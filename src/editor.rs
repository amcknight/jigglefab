//! Scene editor: holds the source of truth for placed beads in Edit mode
//! and produces a fresh `Sim` on Run. See
//! docs/superpowers/specs/2026-05-25-editor-mvp-design.md.

use glam::Vec2;

use crate::chemistry::{parse_chemistry, Chemistry};
use crate::fab::{BeadSpec, Fab};
use crate::sim::Sim;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    Edit,
    Run,
}

/// Chemistries the editor can switch between. Tied to the files in
/// `chemistries/`, baked in at compile time on web (same pattern as fab
/// presets in `app.rs`).
pub const CHEMISTRY_REGISTRY: &[(&str, &str)] = &[
    ("wire", include_str!("../chemistries/wire.toml")),
    ("grey", include_str!("../chemistries/grey.toml")),
    ("sem_basic", include_str!("../chemistries/sem_basic.toml")),
];

pub fn chemistry_toml(name: &str) -> Option<&'static str> {
    CHEMISTRY_REGISTRY.iter().find(|(n, _)| *n == name).map(|(_, t)| *t)
}

pub fn chemistry_names() -> Vec<&'static str> {
    CHEMISTRY_REGISTRY.iter().map(|(n, _)| *n).collect()
}

/// The scene the user is editing. Holds chemistry, world size, the bead
/// list, and the state index that the next click will assign.
pub struct Scene {
    pub chemistry: Chemistry,
    pub chemistry_name: String,
    pub world_size: f32,
    pub beads: Vec<BeadSpec>,
    pub seed: u64,
    pub next_state_idx: u32,
}

impl Scene {
    /// Build a scene from a parsed fab (existing preset) + parsed chemistry.
    pub fn from_fab(fab: &Fab, chemistry: Chemistry, chemistry_name: String) -> Self {
        Self {
            chemistry,
            chemistry_name,
            world_size: fab.meta.world_size.unwrap_or(crate::sim::WORLD_SIZE),
            beads: fab.beads.iter().map(|b| BeadSpec {
                state: b.state.clone(),
                pos: b.pos,
                vel: b.vel,
            }).collect(),
            seed: fab.meta.seed,
            next_state_idx: 0,
        }
    }

    /// Construct a fresh `Sim` from the current scene state.
    pub fn to_sim(&self) -> Sim {
        let fab = Fab {
            meta: crate::fab::Meta {
                name: format!("editor-{}", self.chemistry_name),
                chemistry: self.chemistry_name.clone(),
                seed: self.seed,
                world_size: Some(self.world_size),
            },
            beads: self.beads.clone(),
        };
        Sim::from_fab(&fab, self.chemistry.clone())
    }

    /// Copy a Sim's bead state back into the scene. Velocities are stored
    /// as `Some(...)` so the next `to_sim()` preserves momentum.
    pub fn snapshot_from_sim(&mut self, sim: &Sim) {
        let n = sim.positions.len();
        self.beads.clear();
        self.beads.reserve(n);
        let state_names = &self.chemistry.states;
        for i in 0..n {
            let p = sim.positions[i];
            let v = sim.velocities[i];
            let s = sim.states[i] as usize;
            self.beads.push(BeadSpec {
                state: state_names[s].clone(),
                pos: [p.x, p.y],
                vel: Some([v.x, v.y]),
            });
        }
    }

    /// Append a new bead at `pos` with `self.next_state_idx`. Velocity is
    /// left `None`; `Sim::from_fab` will give it a seeded random unit
    /// direction (matches existing preset convention).
    pub fn place(&mut self, pos: Vec2) {
        let state_name = self.chemistry.states[self.next_state_idx as usize].clone();
        self.beads.push(BeadSpec {
            state: state_name,
            pos: [pos.x, pos.y],
            vel: None,
        });
    }

    /// Switch chemistry. Empties beads because state names from the old
    /// chemistry may not exist in the new one.
    pub fn switch_chemistry(&mut self, chemistry: Chemistry, name: String) {
        self.chemistry = chemistry;
        self.chemistry_name = name;
        self.beads.clear();
        self.next_state_idx = 0;
    }
}

/// Parse a chemistry from the registry by name. Convenience wrapper.
pub fn load_chemistry_by_name(name: &str) -> anyhow::Result<Chemistry> {
    let toml = chemistry_toml(name)
        .ok_or_else(|| anyhow::anyhow!("unknown chemistry: {name}"))?;
    parse_chemistry(toml)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fab::load_fab;

    fn small_wire_fab() -> Fab {
        // 30-bead wire chain, smallest preset we ship.
        load_fab("fabs/wire-30.toml").unwrap()
    }

    #[test]
    fn from_fab_preserves_bead_count() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let scene = Scene::from_fab(&fab, chem, "wire".into());
        assert_eq!(scene.beads.len(), fab.beads.len());
    }

    #[test]
    fn to_sim_preserves_count_and_positions() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let scene = Scene::from_fab(&fab, chem, "wire".into());
        let sim = scene.to_sim();
        assert_eq!(sim.positions.len(), fab.beads.len());
        for (i, b) in fab.beads.iter().enumerate() {
            assert!((sim.positions[i].x - b.pos[0]).abs() < 1e-5);
            assert!((sim.positions[i].y - b.pos[1]).abs() < 1e-5);
        }
    }

    #[test]
    fn snapshot_round_trip_preserves_positions_states_velocities() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        let sim_a = scene.to_sim();
        scene.snapshot_from_sim(&sim_a);
        let sim_b = scene.to_sim();
        assert_eq!(sim_a.positions.len(), sim_b.positions.len());
        for i in 0..sim_a.positions.len() {
            assert!((sim_a.positions[i] - sim_b.positions[i]).length() < 1e-5);
            assert!((sim_a.velocities[i] - sim_b.velocities[i]).length() < 1e-5);
            assert_eq!(sim_a.states[i], sim_b.states[i]);
        }
    }

    #[test]
    fn place_appends_with_chosen_state() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        let before = scene.beads.len();
        scene.next_state_idx = 1; // "on" for wire
        scene.place(Vec2::new(10.0, 10.0));
        assert_eq!(scene.beads.len(), before + 1);
        assert_eq!(scene.beads.last().unwrap().state, "on");
        assert_eq!(scene.beads.last().unwrap().pos, [10.0, 10.0]);
    }

    #[test]
    fn switch_chemistry_empties_beads() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        assert!(!scene.beads.is_empty());
        let grey = load_chemistry_by_name("grey").unwrap();
        scene.switch_chemistry(grey, "grey".into());
        assert!(scene.beads.is_empty());
        assert_eq!(scene.chemistry_name, "grey");
    }

    #[test]
    fn chemistry_registry_has_known_entries() {
        assert!(chemistry_toml("wire").is_some());
        assert!(chemistry_toml("grey").is_some());
        assert!(chemistry_toml("sem_basic").is_some());
        assert!(chemistry_toml("nonexistent").is_none());
    }
}
