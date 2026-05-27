//! Scene editor: holds the source of truth for placed beads in Edit mode
//! and produces a fresh `Sim` on Run. See
//! docs/superpowers/specs/2026-05-25-editor-mvp-design.md.

use std::collections::HashSet;

use glam::Vec2;

use crate::chemistry::{parse_chemistry, Chemistry};
use crate::fab::{BeadSpec, Fab};
use crate::sim::Sim;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    Edit,
    Run,
}

/// The currently-active editor tool. Mutually exclusive: exactly one tool is
/// active at a time. Place is the default and is the only tool that operates
/// during Run mode (matches MVP behaviour).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tool {
    Place,
    Chain,
    Rect,
    Lasso,
}

impl Tool {
    pub fn as_str(self) -> &'static str {
        match self {
            Tool::Place => "place",
            Tool::Chain => "chain",
            Tool::Rect => "rect",
            Tool::Lasso => "lasso",
        }
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "place" => Some(Tool::Place),
            "chain" => Some(Tool::Chain),
            "rect" => Some(Tool::Rect),
            "lasso" => Some(Tool::Lasso),
            _ => None,
        }
    }
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
    /// Canonical (low, high) bond keys. Authoritative; carried through
    /// snapshot/to_sim round-trips so Sim never re-derives from positions
    /// once a Scene has been edited.
    pub bonds: HashSet<(u32, u32)>,
    /// Bead indices in the current selection. Replaced on each Rect/Lasso
    /// gesture; cleared on Run, on switch_chemistry, and on delete.
    pub selection: HashSet<u32>,
    /// Currently-active tool.
    pub tool: Tool,
}

impl Scene {
    /// Build a scene from a parsed fab (existing preset) + parsed chemistry.
    pub fn from_fab(fab: &Fab, chemistry: Chemistry, chemistry_name: String) -> Self {
        let world_size = fab.meta.world_size.unwrap_or(crate::sim::WORLD_SIZE);
        let positions: Vec<glam::Vec2> = fab.beads.iter().map(|b| b.pos()).collect();
        let grid = crate::grid::Grid::new(world_size);
        let bonds = match fab.bonds() {
            Some(explicit) => explicit.iter().map(|p| (p[0].min(p[1]), p[0].max(p[1]))).collect(),
            None => crate::sim::derive_bonds_by_distance(&positions, &grid),
        };
        Self {
            chemistry,
            chemistry_name,
            world_size,
            beads: fab.beads.iter().map(|b| BeadSpec {
                state: b.state.clone(),
                pos: b.pos,
                vel: b.vel,
            }).collect(),
            seed: fab.meta.seed,
            next_state_idx: 0,
            bonds,
            selection: HashSet::new(),
            tool: Tool::Place,
        }
    }

    /// Construct a fresh `Sim` from the current scene state.
    pub fn to_sim(&self) -> Sim {
        let mut bonds_vec: Vec<[u32; 2]> = self.bonds.iter().map(|&(a, b)| [a, b]).collect();
        // Stable order so debug prints / fixture snapshots are deterministic.
        bonds_vec.sort_unstable();
        let fab = Fab {
            meta: crate::fab::Meta {
                name: format!("editor-{}", self.chemistry_name),
                chemistry: self.chemistry_name.clone(),
                seed: self.seed,
                world_size: Some(self.world_size),
                bonds: Some(bonds_vec),
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
        self.bonds = sim.bonds().clone();
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
        self.bonds.clear();
        self.selection.clear();
        self.next_state_idx = 0;
    }
}

/// Parse a chemistry from the registry by name. Convenience wrapper.
pub fn load_chemistry_by_name(name: &str) -> anyhow::Result<Chemistry> {
    let toml = chemistry_toml(name)
        .ok_or_else(|| anyhow::anyhow!("unknown chemistry: {name}"))?;
    parse_chemistry(toml)
}

/// Convert a viewport pixel to world coordinates using the same camera
/// math as `Renderer::update_camera`. Inverse of:
///   ortho(0, w, 0, h) where (w, h) is the aspect-corrected world rect,
///   then translate by (offset_x, offset_y) to center the world inside.
/// Screen y is top-down; world y is bottom-up.
pub fn screen_to_world(
    cursor: (f64, f64),
    viewport: (u32, u32),
    world_size: f32,
) -> Vec2 {
    let (sx, sy) = cursor;
    let (vw, vh) = (viewport.0.max(1) as f32, viewport.1.max(1) as f32);
    let aspect = vw / vh;
    let (w, h) = if aspect >= 1.0 {
        (world_size * aspect, world_size)
    } else {
        (world_size, world_size / aspect)
    };
    let offset_x = (w - world_size) * 0.5;
    let offset_y = (h - world_size) * 0.5;
    let world_x = (sx as f32 / vw) * w - offset_x;
    let world_y = (1.0 - sy as f32 / vh) * h - offset_y;
    // Clamp to world bounds so a click outside the rendered square
    // still produces a placeable position (snapped to the edge).
    Vec2::new(
        world_x.clamp(0.0, world_size),
        world_y.clamp(0.0, world_size),
    )
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

    #[test]
    fn screen_to_world_square_viewport_center() {
        // 100×100 viewport, 30-unit world, cursor at exact center.
        let p = screen_to_world((50.0, 50.0), (100, 100), 30.0);
        assert!((p.x - 15.0).abs() < 1e-4);
        assert!((p.y - 15.0).abs() < 1e-4);
    }

    #[test]
    fn screen_to_world_top_left_maps_to_world_top_left() {
        // Screen (0,0) is top-left; world (0, world_size) is top-left.
        let p = screen_to_world((0.0, 0.0), (100, 100), 30.0);
        assert!((p.x - 0.0).abs() < 1e-4);
        assert!((p.y - 30.0).abs() < 1e-4);
    }

    #[test]
    fn screen_to_world_wide_viewport_clamps_outside_x() {
        // 200×100 viewport, world 30. Aspect=2 → camera-rect width=60, world
        // centered with 15 units of empty space on each side. Cursor at
        // far-left screen edge is at world_x = -15, which clamps to 0.
        let p = screen_to_world((0.0, 50.0), (200, 100), 30.0);
        assert!((p.x - 0.0).abs() < 1e-4);
        assert!((p.y - 15.0).abs() < 1e-4);
    }

    #[test]
    fn scene_from_fab_derives_bonds_for_legacy_preset() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let scene = Scene::from_fab(&fab, chem, "wire".into());
        // wire-30 is a single chain of 30 → 29 consecutive bonds.
        assert_eq!(scene.bonds.len(), 29);
        assert!(scene.selection.is_empty());
    }

    #[test]
    fn scene_to_sim_passes_bonds_verbatim() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        // Hand-edit the bond set so to_sim has something distinctive to pass.
        scene.bonds.clear();
        scene.bonds.insert((0, 1));
        let sim = scene.to_sim();
        assert_eq!(sim.bonds().len(), 1);
        assert!(sim.bonds().contains(&(0, 1)));
    }

    #[test]
    fn scene_snapshot_round_trip_preserves_bonds() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        let original_bonds = scene.bonds.clone();
        let sim = scene.to_sim();
        scene.snapshot_from_sim(&sim);
        assert_eq!(scene.bonds, original_bonds);
    }

    #[test]
    fn scene_tool_default_is_place() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let scene = Scene::from_fab(&fab, chem, "wire".into());
        assert_eq!(scene.tool, Tool::Place);
    }
}
