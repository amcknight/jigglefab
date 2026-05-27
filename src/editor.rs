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

/// Distance between consecutive beads when the Chain tool drops them. Tuned
/// to match the wire-30 preset (0.667) — comfortably under RADIUS=1.0 so the
/// pair starts bonded and `enforce_bonds` never has to repair it.
pub const CHAIN_STEP: f32 = 0.667;

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

    /// Append a new bead at `pos` with `self.next_state_idx`. Derives bonds
    /// from the new bead to any existing bead within RADIUS (Place semantics:
    /// "drop near a chain → it joins"). Returns the new bead's index.
    pub fn place(&mut self, pos: Vec2) -> u32 {
        let state_name = self.chemistry.states[self.next_state_idx as usize].clone();
        let new_idx = self.beads.len() as u32;
        self.beads.push(BeadSpec {
            state: state_name,
            pos: [pos.x, pos.y],
            vel: None,
        });
        let grid = crate::grid::Grid::new(self.world_size);
        for i in 0..(new_idx as usize) {
            let pa = pos;
            let pb_raw = Vec2::from(self.beads[i].pos);
            let pb = pa + grid.min_image(pa, pb_raw);
            if (pb - pa).length() < crate::ccd::RADIUS {
                self.bonds.insert((i as u32, new_idx));
            }
        }
        new_idx
    }

    /// Append a bead at `pos` chain-bonded only to `prev_idx`. Used by the
    /// Chain tool. Unlike `place`, this skips distance-derivation entirely —
    /// nearby non-predecessor beads do NOT form bonds. Returns the new index.
    pub fn append_chain_bead(&mut self, pos: Vec2, prev_idx: u32) -> u32 {
        let state_name = self.chemistry.states[self.next_state_idx as usize].clone();
        let new_idx = self.beads.len() as u32;
        self.beads.push(BeadSpec {
            state: state_name,
            pos: [pos.x, pos.y],
            vel: None,
        });
        let key = if prev_idx < new_idx { (prev_idx, new_idx) } else { (new_idx, prev_idx) };
        self.bonds.insert(key);
        new_idx
    }

    /// Extend an in-progress chain toward `cursor`, dropping beads at CHAIN_STEP
    /// spacing along the segment from the previous bead to the cursor. Returns
    /// the new "last bead" index (== input `last_idx` if no bead was placed).
    pub fn chain_extend(&mut self, last_idx: u32, cursor: Vec2) -> u32 {
        let mut last = last_idx;
        loop {
            let last_pos = Vec2::from(self.beads[last as usize].pos);
            let to_cursor = cursor - last_pos;
            let dist = to_cursor.length();
            if dist < CHAIN_STEP {
                break;
            }
            let dir = to_cursor / dist;
            let new_pos = last_pos + dir * CHAIN_STEP;
            last = self.append_chain_bead(new_pos, last);
        }
        last
    }

    /// Replace the selection with every bead whose center lies inside the
    /// axis-aligned rectangle defined by `a` and `b` (corners in any order).
    pub fn select_rect(&mut self, a: Vec2, b: Vec2) {
        self.selection.clear();
        for (i, bead) in self.beads.iter().enumerate() {
            if point_in_rect(Vec2::from(bead.pos), a, b) {
                self.selection.insert(i as u32);
            }
        }
    }

    /// Replace the selection with every bead whose center lies inside the
    /// closed polygon. Polygons with fewer than 3 vertices select nothing.
    pub fn select_lasso(&mut self, poly: &[Vec2]) {
        self.selection.clear();
        for (i, bead) in self.beads.iter().enumerate() {
            if point_in_polygon(Vec2::from(bead.pos), poly) {
                self.selection.insert(i as u32);
            }
        }
    }

    /// Translate every selected bead by `delta`, then clamp each component to
    /// `[0, world_size]`. Bonds and velocities are untouched (bond indices stay
    /// valid; velocities will be re-derived from positions only if the user
    /// presses Run, and snapshot has already stored them).
    pub fn translate_selection(&mut self, delta: Vec2) {
        let w = self.world_size;
        for &idx in &self.selection {
            let b = &mut self.beads[idx as usize];
            let new_x = (b.pos[0] + delta.x).clamp(0.0, w);
            let new_y = (b.pos[1] + delta.y).clamp(0.0, w);
            b.pos = [new_x, new_y];
        }
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

/// Inclusive point-in-rect using axis-aligned bounds. Accepts either corner
/// ordering — anchor and current can be in any spatial order.
pub fn point_in_rect(p: Vec2, a: Vec2, b: Vec2) -> bool {
    let (xmin, xmax) = if a.x <= b.x { (a.x, b.x) } else { (b.x, a.x) };
    let (ymin, ymax) = if a.y <= b.y { (a.y, b.y) } else { (b.y, a.y) };
    p.x >= xmin && p.x <= xmax && p.y >= ymin && p.y <= ymax
}

/// Classic ray-cast point-in-polygon. Casts a horizontal ray to +x and
/// counts edge crossings; odd → inside. Returns false for polygons with
/// fewer than 3 vertices.
pub fn point_in_polygon(p: Vec2, poly: &[Vec2]) -> bool {
    if poly.len() < 3 {
        return false;
    }
    let mut inside = false;
    let n = poly.len();
    let mut j = n - 1;
    for i in 0..n {
        let pi = poly[i];
        let pj = poly[j];
        // Edge from pj → pi straddles the horizontal line y = p.y?
        let straddles = (pi.y > p.y) != (pj.y > p.y);
        if straddles {
            // x-coordinate of the intersection of that edge with y = p.y.
            let x_cross = pj.x + (p.y - pj.y) * (pi.x - pj.x) / (pi.y - pj.y);
            if p.x < x_cross {
                inside = !inside;
            }
        }
        j = i;
    }
    inside
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

    #[test]
    fn place_derives_bond_to_nearby_bead() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        scene.beads.clear();
        scene.bonds.clear();
        scene.place(Vec2::new(5.0, 5.0));
        scene.place(Vec2::new(5.5, 5.0));  // 0.5 apart < RADIUS=1.0
        assert!(scene.bonds.contains(&(0, 1)), "Place should bond near pairs");
    }

    #[test]
    fn place_no_bond_when_far() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        scene.beads.clear();
        scene.bonds.clear();
        scene.place(Vec2::new(5.0, 5.0));
        scene.place(Vec2::new(10.0, 10.0));
        assert!(scene.bonds.is_empty(), "Place should not bond far pairs");
    }

    #[test]
    fn append_chain_bead_only_bonds_to_predecessor() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        scene.beads.clear();
        scene.bonds.clear();
        let a = scene.place(Vec2::new(0.0, 0.0));
        let b = scene.append_chain_bead(Vec2::new(0.7, 0.0), a);
        let c = scene.append_chain_bead(Vec2::new(0.7, -0.7), b);
        // |a-c| = sqrt(0.49 + 0.49) ≈ 0.99 < RADIUS — but chain MUST NOT bond a-c.
        assert!(scene.bonds.contains(&(a, b)));
        assert!(scene.bonds.contains(&(b, c)));
        assert!(!scene.bonds.contains(&(a, c)), "chain must not form corner triangle");
        assert_eq!(scene.bonds.len(), 2);
    }

    #[test]
    fn chain_extend_single_segment() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        scene.beads.clear();
        scene.bonds.clear();
        let start = scene.place(Vec2::new(0.0, 0.0));
        // Cursor jumps ~2.005 units in one event → expect 3 new beads at 0.667, 1.334, 2.001.
        // (2.005 > 3*CHAIN_STEP; f32 accumulation means exact 2.0 lands just under threshold.)
        let last = scene.chain_extend(start, Vec2::new(2.005, 0.0));
        assert_eq!(scene.beads.len(), 4);  // start + 3 new
        assert_eq!(last, 3);
        let expected_xs = [0.667, 1.334, 2.001];  // 0.667 * (1, 2, 3)
        for (i, x) in expected_xs.iter().enumerate() {
            let p = scene.beads[i + 1].pos;
            assert!((p[0] - x).abs() < 1e-3, "bead {} x = {} expected {}", i + 1, p[0], x);
            assert!(p[1].abs() < 1e-3);
        }
        // Consecutive bonds.
        assert!(scene.bonds.contains(&(0, 1)));
        assert!(scene.bonds.contains(&(1, 2)));
        assert!(scene.bonds.contains(&(2, 3)));
        assert_eq!(scene.bonds.len(), 3);
    }

    #[test]
    fn chain_extend_below_threshold_is_noop() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        scene.beads.clear();
        scene.bonds.clear();
        let start = scene.place(Vec2::new(0.0, 0.0));
        let last = scene.chain_extend(start, Vec2::new(0.1, 0.0));
        assert_eq!(last, start, "no new bead under 0.667");
        assert_eq!(scene.beads.len(), 1);
        assert_eq!(scene.bonds.len(), 0);
    }

    #[test]
    fn chain_extend_pairs_spaced_at_step() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        scene.beads.clear();
        scene.bonds.clear();
        let start = scene.place(Vec2::new(0.0, 0.0));
        scene.chain_extend(start, Vec2::new(3.0, 0.0));
        // Every consecutive pair must be 0.667 apart within float epsilon.
        for w in scene.beads.windows(2) {
            let d = (Vec2::from(w[0].pos) - Vec2::from(w[1].pos)).length();
            assert!((d - 0.667).abs() < 1e-3, "consecutive spacing {} != 0.667", d);
        }
    }

    #[test]
    fn point_in_rect_inside_and_outside() {
        let a = Vec2::new(1.0, 1.0);
        let b = Vec2::new(5.0, 4.0);
        assert!(point_in_rect(Vec2::new(3.0, 2.0), a, b));
        assert!(point_in_rect(Vec2::new(5.0, 4.0), a, b), "boundary counts as in");
        assert!(!point_in_rect(Vec2::new(0.5, 2.0), a, b));
        assert!(!point_in_rect(Vec2::new(3.0, 5.0), a, b));
    }

    #[test]
    fn point_in_rect_handles_inverted_corners() {
        // Drag from bottom-right to top-left: anchor > current. Still works.
        let a = Vec2::new(5.0, 4.0);
        let b = Vec2::new(1.0, 1.0);
        assert!(point_in_rect(Vec2::new(3.0, 2.0), a, b));
    }

    #[test]
    fn point_in_polygon_convex_square() {
        let poly = vec![
            Vec2::new(0.0, 0.0),
            Vec2::new(4.0, 0.0),
            Vec2::new(4.0, 4.0),
            Vec2::new(0.0, 4.0),
        ];
        assert!(point_in_polygon(Vec2::new(2.0, 2.0), &poly));
        assert!(!point_in_polygon(Vec2::new(5.0, 2.0), &poly));
        assert!(!point_in_polygon(Vec2::new(-1.0, 2.0), &poly));
    }

    #[test]
    fn point_in_polygon_concave_u_shape() {
        // "U" shape: outer rectangle minus a notch in the middle top.
        //   (0,0)─────(6,0)
        //     │  ┌───┐  │
        //     │  │   │  │     ← notch from (2,4) to (4,4) descending into (2,2)..(4,2)
        //     │  │   │  │
        //   (0,6)─────(6,6)
        let poly = vec![
            Vec2::new(0.0, 0.0),
            Vec2::new(6.0, 0.0),
            Vec2::new(6.0, 6.0),
            Vec2::new(4.0, 6.0),
            Vec2::new(4.0, 2.0),
            Vec2::new(2.0, 2.0),
            Vec2::new(2.0, 6.0),
            Vec2::new(0.0, 6.0),
        ];
        assert!(point_in_polygon(Vec2::new(1.0, 5.0), &poly), "left arm of U");
        assert!(point_in_polygon(Vec2::new(5.0, 5.0), &poly), "right arm of U");
        assert!(!point_in_polygon(Vec2::new(3.0, 4.0), &poly), "inside notch is outside U");
        assert!(point_in_polygon(Vec2::new(3.0, 1.0), &poly), "base of U");
    }

    #[test]
    fn point_in_polygon_degenerate_returns_false() {
        let empty: Vec<Vec2> = vec![];
        assert!(!point_in_polygon(Vec2::new(0.0, 0.0), &empty));
        let two = vec![Vec2::new(0.0, 0.0), Vec2::new(1.0, 1.0)];
        assert!(!point_in_polygon(Vec2::new(0.5, 0.5), &two));
    }

    #[test]
    fn select_rect_replaces_selection() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        scene.beads.clear();
        scene.bonds.clear();
        scene.place(Vec2::new(2.0, 2.0));  // 0
        scene.place(Vec2::new(8.0, 2.0));  // 1  (far enough not to bond)
        scene.place(Vec2::new(20.0, 20.0)); // 2
        scene.selection.insert(99);  // stale entry — must be overwritten.
        scene.select_rect(Vec2::new(0.0, 0.0), Vec2::new(10.0, 10.0));
        assert_eq!(scene.selection.len(), 2);
        assert!(scene.selection.contains(&0));
        assert!(scene.selection.contains(&1));
        assert!(!scene.selection.contains(&2));
        assert!(!scene.selection.contains(&99));
    }

    #[test]
    fn select_lasso_concave_polygon() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        scene.beads.clear();
        scene.bonds.clear();
        scene.place(Vec2::new(1.0, 5.0));   // 0 — in left arm of U
        scene.place(Vec2::new(5.0, 5.0));   // 1 — in right arm of U
        scene.place(Vec2::new(3.0, 4.0));   // 2 — in notch (outside)
        let poly = vec![
            Vec2::new(0.0, 0.0),
            Vec2::new(6.0, 0.0),
            Vec2::new(6.0, 6.0),
            Vec2::new(4.0, 6.0),
            Vec2::new(4.0, 2.0),
            Vec2::new(2.0, 2.0),
            Vec2::new(2.0, 6.0),
            Vec2::new(0.0, 6.0),
        ];
        scene.select_lasso(&poly);
        assert!(scene.selection.contains(&0));
        assert!(scene.selection.contains(&1));
        assert!(!scene.selection.contains(&2));
    }

    #[test]
    fn translate_selection_shifts_only_selected_beads() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        scene.beads.clear();
        scene.bonds.clear();
        scene.place(Vec2::new(5.0, 5.0));
        scene.place(Vec2::new(10.0, 5.0));
        scene.selection.insert(0);
        scene.translate_selection(Vec2::new(2.0, 0.0));
        assert_eq!(scene.beads[0].pos, [7.0, 5.0]);
        assert_eq!(scene.beads[1].pos, [10.0, 5.0], "unselected bead unchanged");
    }

    #[test]
    fn translate_selection_clamps_to_world() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        scene.beads.clear();
        scene.bonds.clear();
        let world = scene.world_size;
        scene.place(Vec2::new(world - 1.0, 5.0));
        scene.selection.insert(0);
        scene.translate_selection(Vec2::new(10.0, 0.0));  // would push past world edge
        assert!(scene.beads[0].pos[0] <= world);
        assert_eq!(scene.beads[0].pos[0], world);
    }

    #[test]
    fn translate_selection_preserves_bonds() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        let bonds_before = scene.bonds.clone();
        for i in 0..scene.beads.len() as u32 { scene.selection.insert(i); }
        scene.translate_selection(Vec2::new(1.0, 1.0));
        assert_eq!(scene.bonds, bonds_before);
    }
}
