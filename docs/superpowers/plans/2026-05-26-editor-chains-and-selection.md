# Editor — Chains + Selection Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add Chain / Rect / Lasso tools, region selection, drag-to-move, and Delete to the web editor, promoting bonds to first-class data along the way.

**Architecture:** Promote bonds to first-class on `Fab` (optional explicit list) and `Scene` (authoritative `HashSet<(u32, u32)>`). `Sim::from_fab` uses explicit bonds when supplied, falls back to distance-derivation otherwise. The editor gains a `Tool` enum and a `DragState` machine driven by mouse-down/move/up in `App`. Each tool's mouseup commits a mutation on `Scene` (append chain beads + chain bonds, write selection set, translate selection, etc.). The renderer learns one new flag (per-bead `selected: u32`) and one new pipeline (screen-space thin lines for the rect/lasso overlay).

**Tech Stack:** Rust + wgpu + winit, wasm-bindgen bridge (existing), HTML/JS toolbar (existing), serde for Fab TOML.

**Spec:** [docs/superpowers/specs/2026-05-26-editor-chains-and-selection-design.md](../specs/2026-05-26-editor-chains-and-selection-design.md)

---

## Pre-flight

- [ ] **Step 0: Confirm baseline builds**

Run:
```bash
cargo check --target wasm32-unknown-unknown
cargo test --lib
```
Expected: both succeed. If `wasm32-unknown-unknown` isn't installed: `rustup target add wasm32-unknown-unknown`.

---

## Task 1: `Fab.bonds` — TOML round-trip

**Files:**
- Modify: `src/fab.rs`

Add an optional explicit bond list to `Fab`. Existing TOMLs (no `bonds`) must continue to load with `bonds = None`.

- [ ] **Step 1: Write the failing test**

Append to `src/fab.rs` inside the `mod tests` block:
```rust
#[test]
fn fab_round_trips_without_bonds() {
    let fab = load_fab("fabs/grey-30.toml").unwrap();
    assert!(fab.bonds.is_none(), "legacy fab should have no explicit bonds");
}

#[test]
fn fab_parses_explicit_bonds() {
    let toml_text = r#"
[meta]
name = "two"
chemistry = "grey"
seed = 1
bonds = [[0, 1]]

[[bead]]
state = "grey"
pos = [5.0, 5.0]

[[bead]]
state = "grey"
pos = [5.5, 5.0]
"#;
    let fab = parse_fab(toml_text).unwrap();
    assert_eq!(fab.bonds, Some(vec![[0u32, 1u32]]));
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test --lib fab::tests`
Expected: both new tests fail with "no field `bonds` on `Fab`" / "unknown field `bonds`".

- [ ] **Step 3: Add the field**

Edit `src/fab.rs`. Move `bonds` onto `Meta` (TOML places `bonds` under `[meta]` in the test above):
```rust
#[derive(Debug, Deserialize)]
pub struct Fab {
    pub meta: Meta,
    #[serde(rename = "bead")]
    pub beads: Vec<BeadSpec>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Meta {
    pub name: String,
    pub chemistry: String,
    pub seed: u64,
    #[serde(default)]
    pub world_size: Option<f32>,
    #[serde(default)]
    pub bonds: Option<Vec<[u32; 2]>>,
}
```

Then surface `bonds` as a top-level accessor for ergonomics. Add to `Fab`:
```rust
impl Fab {
    pub fn bonds(&self) -> Option<&Vec<[u32; 2]>> {
        self.meta.bonds.as_ref()
    }
}
```

And update the test to use the field exactly as written above (`fab.bonds.is_none()` becomes `fab.bonds().is_none()`, and the equality assertion becomes `assert_eq!(fab.bonds(), Some(&vec![[0u32, 1u32]]));`). Fix the tests now to match:

```rust
#[test]
fn fab_round_trips_without_bonds() {
    let fab = load_fab("fabs/grey-30.toml").unwrap();
    assert!(fab.bonds().is_none(), "legacy fab should have no explicit bonds");
}

#[test]
fn fab_parses_explicit_bonds() {
    let toml_text = r#"
[meta]
name = "two"
chemistry = "grey"
seed = 1
bonds = [[0, 1]]

[[bead]]
state = "grey"
pos = [5.0, 5.0]

[[bead]]
state = "grey"
pos = [5.5, 5.0]
"#;
    let fab = parse_fab(toml_text).unwrap();
    assert_eq!(fab.bonds(), Some(&vec![[0u32, 1u32]]));
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test --lib fab::tests`
Expected: all `fab::tests` tests pass.

- [ ] **Step 5: Commit**

```bash
git add src/fab.rs
git commit -m "feat(fab): optional explicit bonds list for editor round-trip"
```

---

## Task 2: Factor `Sim::from_fab` bond derivation; honour explicit bonds

**Files:**
- Modify: `src/sim.rs:66-98`

Pull the O(N²) bond-derivation loop into `derive_bonds_by_distance`. When `fab.bonds()` is `Some`, use those verbatim; otherwise derive.

- [ ] **Step 1: Write the failing tests**

Append to `src/sim.rs` inside `mod tests`:
```rust
#[test]
fn from_fab_uses_explicit_bonds_when_present() {
    // Three beads in a row at 0.5 spacing — distance derivation would bond
    // (0,1), (1,2), AND (0,2) (|0-2|=1.0 = RADIUS, borderline; flip to 0.9
    // to keep it strictly inside).
    let chem = load_chemistry("chemistries/grey.toml").unwrap();
    let toml_text = r#"
[meta]
name = "explicit"
chemistry = "grey"
seed = 1
bonds = [[0, 1], [1, 2]]

[[bead]]
state = "grey"
pos = [5.0, 5.0]

[[bead]]
state = "grey"
pos = [5.45, 5.0]

[[bead]]
state = "grey"
pos = [5.90, 5.0]
"#;
    let fab = crate::fab::parse_fab(toml_text).unwrap();
    let sim = Sim::from_fab(&fab, chem);
    assert_eq!(sim.bonds().len(), 2);
    assert!(sim.bonds().contains(&(0, 1)));
    assert!(sim.bonds().contains(&(1, 2)));
    assert!(!sim.bonds().contains(&(0, 2)), "explicit bonds must not be widened");
}

#[test]
fn from_fab_without_bonds_matches_distance_derivation() {
    // Regression guard: the no-bonds path must produce the same set as the
    // pre-promotion code on a known preset.
    let chem = load_chemistry("chemistries/wire.toml").unwrap();
    let fab = load_fab("fabs/wire-20x30.toml").unwrap();
    assert!(fab.bonds().is_none());
    let sim = Sim::from_fab(&fab, chem);
    // wire-20x30 = 20 chains of 30, each chain bonds 29 pairs = 580 bonds.
    assert_eq!(sim.bonds().len(), 580);
}
```

- [ ] **Step 2: Run to confirm failure**

Run: `cargo test --lib sim::tests::from_fab_uses_explicit_bonds_when_present sim::tests::from_fab_without_bonds_matches_distance_derivation`
Expected: first test fails (explicit bonds path doesn't exist yet — currently distance-derives and gets 3 bonds incl. (0,2)); second test passes today (regression baseline).

- [ ] **Step 3: Factor and branch**

In `src/sim.rs`, replace the body of `Sim::from_fab` (currently lines ~66-98). Replace:
```rust
        let world_size = fab.meta.world_size.unwrap_or(WORLD_SIZE);
        let grid = Grid::new(world_size);
        let mut bonds = HashSet::new();
        for i in 0..n {
            for j in (i + 1)..n {
                let pa = positions[i];
                let pb = pa + grid.min_image(pa, positions[j]);
                if (pb - pa).length() < RADIUS {
                    bonds.insert((i as u32, j as u32));
                }
            }
        }
```
with:
```rust
        let world_size = fab.meta.world_size.unwrap_or(WORLD_SIZE);
        let grid = Grid::new(world_size);
        let bonds = match fab.bonds() {
            Some(explicit) => explicit.iter().map(|p| (p[0].min(p[1]), p[0].max(p[1]))).collect(),
            None => derive_bonds_by_distance(&positions, &grid),
        };
```

And add the free function just above `impl Sim`:
```rust
/// Distance-derive bonds for legacy presets (no explicit `bonds` field). A
/// pair bonds when their min-image separation is < RADIUS at preset time.
/// Mirrors the Haskell `bbSides` build at `haskell/src/Motion/Point.hs:40-41`.
pub(crate) fn derive_bonds_by_distance(positions: &[Vec2], grid: &Grid) -> HashSet<(u32, u32)> {
    let n = positions.len();
    let mut bonds = HashSet::new();
    for i in 0..n {
        for j in (i + 1)..n {
            let pa = positions[i];
            let pb = pa + grid.min_image(pa, positions[j]);
            if (pb - pa).length() < RADIUS {
                bonds.insert((i as u32, j as u32));
            }
        }
    }
    bonds
}
```

- [ ] **Step 4: Run tests**

Run: `cargo test --lib sim::tests`
Expected: all sim tests pass, including the two new ones.

- [ ] **Step 5: Commit**

```bash
git add src/sim.rs
git commit -m "feat(sim): honour explicit Fab.bonds; factor distance derivation"
```

---

## Task 3: `Scene` gains a bond set and a selection set; `Tool` enum

**Files:**
- Modify: `src/editor.rs`

Add `Scene.bonds`, `Scene.selection`, the `Tool` enum, and update `Scene::from_fab` / `Scene::to_sim` / `Scene::snapshot_from_sim` to round-trip bonds.

- [ ] **Step 1: Write the failing tests**

Append to `src/editor.rs` inside `mod tests`:
```rust
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
```

- [ ] **Step 2: Run to confirm failure**

Run: `cargo test --lib editor::tests`
Expected: four new tests fail with missing fields / missing `Tool` enum.

- [ ] **Step 3: Add `Tool`, `Scene.bonds`, `Scene.selection`, `Scene.tool`**

In `src/editor.rs`, add near the top (after `Mode`):
```rust
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
```

Add to the top of the file:
```rust
use std::collections::HashSet;
```

Extend `Scene`:
```rust
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
```

Update `Scene::from_fab` to seed the new fields:
```rust
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
```

Update `Scene::to_sim` to ship the bonds explicitly:
```rust
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
```

Update `Scene::snapshot_from_sim` to copy bonds back:
```rust
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
```

Update `Scene::switch_chemistry` to clear bonds + selection:
```rust
pub fn switch_chemistry(&mut self, chemistry: Chemistry, name: String) {
    self.chemistry = chemistry;
    self.chemistry_name = name;
    self.beads.clear();
    self.bonds.clear();
    self.selection.clear();
    self.next_state_idx = 0;
}
```

- [ ] **Step 4: Run tests**

Run: `cargo test --lib editor::tests`
Expected: all editor tests pass (including the four new ones).

Also: a few existing tests (`scene_to_sim_passes_bonds_verbatim` etc.) depend on `derive_bonds_by_distance` being reachable as `pub(crate)`. If `cargo build` fails with a visibility error, change `pub(crate)` to `pub` on that fn in `src/sim.rs`.

- [ ] **Step 5: Commit**

```bash
git add src/editor.rs src/sim.rs
git commit -m "feat(editor): Scene.bonds + Scene.selection + Tool enum"
```

---

## Task 4: `Scene::place` derives bonds for the new bead

**Files:**
- Modify: `src/editor.rs`

Today `Scene::place` just appends. With bonds first-class, Place semantics require deriving bonds against existing beads for the newly placed one.

- [ ] **Step 1: Write the failing test**

Append to `mod tests`:
```rust
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
```

- [ ] **Step 2: Run to confirm failure**

Run: `cargo test --lib editor::tests::place_derives_bond_to_nearby_bead editor::tests::place_no_bond_when_far`
Expected: both fail — current `place` does not touch `bonds`.

- [ ] **Step 3: Update `Scene::place`**

Replace the `place` method in `src/editor.rs`:
```rust
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
```

The existing test `place_appends_with_chosen_state` ignores the return value, so the signature change is backward-compatible.

- [ ] **Step 4: Run tests**

Run: `cargo test --lib editor::tests`
Expected: all pass.

- [ ] **Step 5: Commit**

```bash
git add src/editor.rs
git commit -m "feat(editor): Place tool derives bonds for the new bead"
```

---

## Task 5: Chain primitive — `append_chain_bead` + corner anti-triangle test

**Files:**
- Modify: `src/editor.rs`

The chain primitive: append a bead chain-bonded **only** to its predecessor, regardless of geometric proximity to other beads. This is the building block §6 calls for.

- [ ] **Step 1: Write the failing tests**

Append to `mod tests`:
```rust
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
```

- [ ] **Step 2: Run to confirm failure**

Run: `cargo test --lib editor::tests::append_chain_bead_only_bonds_to_predecessor`
Expected: fails — method doesn't exist.

- [ ] **Step 3: Implement `append_chain_bead`**

Add to `impl Scene` in `src/editor.rs`:
```rust
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
```

- [ ] **Step 4: Run tests**

Run: `cargo test --lib editor::tests`
Expected: all pass.

- [ ] **Step 5: Commit**

```bash
git add src/editor.rs
git commit -m "feat(editor): append_chain_bead — chain-only bond, no triangles"
```

---

## Task 6: Chain interpolation — `chain_extend`

**Files:**
- Modify: `src/editor.rs`

Drag-along-path: while the cursor has moved ≥ 0.667 from the last bead, drop a bead 0.667 along the direction and recurse.

- [ ] **Step 1: Write the failing tests**

Append to `mod tests`:
```rust
#[test]
fn chain_extend_single_segment() {
    let fab = small_wire_fab();
    let chem = load_chemistry_by_name("wire").unwrap();
    let mut scene = Scene::from_fab(&fab, chem, "wire".into());
    scene.beads.clear();
    scene.bonds.clear();
    let start = scene.place(Vec2::new(0.0, 0.0));
    // Cursor jumps 2.0 units in one event → expect 3 new beads at 0.667, 1.333, 2.0.
    let last = scene.chain_extend(start, Vec2::new(2.0, 0.0));
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
```

- [ ] **Step 2: Run to confirm failure**

Run: `cargo test --lib editor::tests::chain_extend_single_segment editor::tests::chain_extend_below_threshold_is_noop editor::tests::chain_extend_pairs_spaced_at_step`
Expected: all fail — method doesn't exist.

- [ ] **Step 3: Implement `chain_extend`**

Add to `impl Scene`. Also add a const for the spacing.

Top of `src/editor.rs`, near other consts:
```rust
/// Distance between consecutive beads when the Chain tool drops them. Tuned
/// to match the wire-30 preset (0.667) — comfortably under RADIUS=1.0 so the
/// pair starts bonded and `enforce_bonds` never has to repair it.
pub const CHAIN_STEP: f32 = 0.667;
```

And the method:
```rust
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
```

- [ ] **Step 4: Run tests**

Run: `cargo test --lib editor::tests`
Expected: all pass.

- [ ] **Step 5: Commit**

```bash
git add src/editor.rs
git commit -m "feat(editor): chain_extend — interpolated bead drop at 0.667"
```

---

## Task 7: Selection geometry — `point_in_rect`, `point_in_polygon`

**Files:**
- Modify: `src/editor.rs`

Pure geometric helpers used by Rect / Lasso. Standalone functions so they're easy to unit test.

- [ ] **Step 1: Write the failing tests**

Append to `mod tests`:
```rust
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
```

- [ ] **Step 2: Run to confirm failure**

Run: `cargo test --lib editor::tests::point_in_rect_inside_and_outside`
Expected: fails — functions don't exist.

- [ ] **Step 3: Implement helpers**

Add to `src/editor.rs` (free fns near the bottom, before `#[cfg(test)]`):
```rust
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
```

- [ ] **Step 4: Run tests**

Run: `cargo test --lib editor::tests`
Expected: all pass.

- [ ] **Step 5: Commit**

```bash
git add src/editor.rs
git commit -m "feat(editor): point_in_rect + point_in_polygon for selection"
```

---

## Task 8: Selection writers — `select_rect`, `select_lasso`

**Files:**
- Modify: `src/editor.rs`

Apply the geometric helpers to `Scene.beads` and replace `Scene.selection`.

- [ ] **Step 1: Write the failing tests**

Append to `mod tests`:
```rust
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
```

- [ ] **Step 2: Run to confirm failure**

Run: `cargo test --lib editor::tests::select_rect_replaces_selection editor::tests::select_lasso_concave_polygon`
Expected: fails — methods don't exist.

- [ ] **Step 3: Implement**

Add to `impl Scene`:
```rust
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
```

- [ ] **Step 4: Run tests**

Run: `cargo test --lib editor::tests`
Expected: all pass.

- [ ] **Step 5: Commit**

```bash
git add src/editor.rs
git commit -m "feat(editor): select_rect + select_lasso write Scene.selection"
```

---

## Task 9: Move — `translate_selection` with world-clamp

**Files:**
- Modify: `src/editor.rs`

Translate every selected bead by a delta, clamping final positions to `[0, world_size]` per axis. Bond indices stay the same.

- [ ] **Step 1: Write the failing tests**

Append to `mod tests`:
```rust
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
```

- [ ] **Step 2: Run to confirm failure**

Run: `cargo test --lib editor::tests::translate_selection_shifts_only_selected_beads`
Expected: fails — method doesn't exist.

- [ ] **Step 3: Implement**

Add to `impl Scene`:
```rust
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
```

- [ ] **Step 4: Run tests**

Run: `cargo test --lib editor::tests`
Expected: all pass.

- [ ] **Step 5: Commit**

```bash
git add src/editor.rs
git commit -m "feat(editor): translate_selection with world-edge clamp"
```

---

## Task 10: Delete — bond drop + dense index remap

**Files:**
- Modify: `src/editor.rs`

Removing beads is the index-rewrite-heavy operation. Every bond touching a removed index disappears; survivors get rewritten under the new dense numbering.

- [ ] **Step 1: Write the failing tests**

Append to `mod tests`:
```rust
#[test]
fn delete_drops_touching_bonds_and_remaps_survivors() {
    let fab = small_wire_fab();
    let chem = load_chemistry_by_name("wire").unwrap();
    let mut scene = Scene::from_fab(&fab, chem, "wire".into());
    scene.beads.clear();
    scene.bonds.clear();
    // Chain: 0—1—2—3—4
    let a = scene.place(Vec2::new(0.0, 0.0));
    let b = scene.append_chain_bead(Vec2::new(0.7, 0.0), a);
    let c = scene.append_chain_bead(Vec2::new(1.4, 0.0), b);
    let d = scene.append_chain_bead(Vec2::new(2.1, 0.0), c);
    let _e = scene.append_chain_bead(Vec2::new(2.8, 0.0), d);
    assert_eq!(scene.bonds.len(), 4);

    // Delete the middle bead (index 2). Bonds (1,2) and (2,3) drop.
    // Survivors: old 0,1,3,4 → new 0,1,2,3. Surviving bonds: (0,1), (3,4) → (0,1), (2,3).
    scene.selection.clear();
    scene.selection.insert(c);
    scene.delete_selection();
    assert_eq!(scene.beads.len(), 4);
    assert_eq!(scene.bonds.len(), 2);
    assert!(scene.bonds.contains(&(0, 1)));
    assert!(scene.bonds.contains(&(2, 3)));
    assert!(scene.selection.is_empty(), "selection clears after delete");
}

#[test]
fn delete_all_clears_scene() {
    let fab = small_wire_fab();
    let chem = load_chemistry_by_name("wire").unwrap();
    let mut scene = Scene::from_fab(&fab, chem, "wire".into());
    for i in 0..scene.beads.len() as u32 { scene.selection.insert(i); }
    scene.delete_selection();
    assert!(scene.beads.is_empty());
    assert!(scene.bonds.is_empty());
    assert!(scene.selection.is_empty());
}

#[test]
fn delete_with_empty_selection_is_noop() {
    let fab = small_wire_fab();
    let chem = load_chemistry_by_name("wire").unwrap();
    let mut scene = Scene::from_fab(&fab, chem, "wire".into());
    let before_beads = scene.beads.len();
    let before_bonds = scene.bonds.len();
    scene.delete_selection();
    assert_eq!(scene.beads.len(), before_beads);
    assert_eq!(scene.bonds.len(), before_bonds);
}
```

- [ ] **Step 2: Run to confirm failure**

Run: `cargo test --lib editor::tests::delete_drops_touching_bonds_and_remaps_survivors`
Expected: fails — method doesn't exist.

- [ ] **Step 3: Implement**

Add to `impl Scene`:
```rust
/// Remove every selected bead. Bonds touching a removed index are dropped;
/// surviving bonds are rewritten under the new dense numbering. The
/// selection set is cleared (its indices are stale anyway).
pub fn delete_selection(&mut self) {
    if self.selection.is_empty() {
        return;
    }
    // Build old → new index map. Removed indices map to None.
    let n = self.beads.len();
    let mut remap: Vec<Option<u32>> = Vec::with_capacity(n);
    let mut next = 0u32;
    for i in 0..n as u32 {
        if self.selection.contains(&i) {
            remap.push(None);
        } else {
            remap.push(Some(next));
            next += 1;
        }
    }
    // Rewrite the bead vec in place (retain-and-iterate is cleaner).
    let mut kept_beads = Vec::with_capacity(next as usize);
    for (i, bead) in self.beads.drain(..).enumerate() {
        if !self.selection.contains(&(i as u32)) {
            kept_beads.push(bead);
        }
    }
    self.beads = kept_beads;
    // Rewrite the bond set: keep bonds whose endpoints both survived, remap them.
    let new_bonds: HashSet<(u32, u32)> = self.bonds.iter().filter_map(|&(a, b)| {
        match (remap[a as usize], remap[b as usize]) {
            (Some(na), Some(nb)) => {
                Some(if na < nb { (na, nb) } else { (nb, na) })
            }
            _ => None,
        }
    }).collect();
    self.bonds = new_bonds;
    self.selection.clear();
}
```

- [ ] **Step 4: Run tests**

Run: `cargo test --lib editor::tests`
Expected: all pass.

- [ ] **Step 5: Commit**

```bash
git add src/editor.rs
git commit -m "feat(editor): delete_selection — drop bonds, remap survivors"
```

---

## Task 11: `DragState` enum + gesture API

**Files:**
- Modify: `src/editor.rs`

`App` needs a tagged union to track the in-progress gesture between mouse-down and mouse-up. Move the type into `editor.rs` so it lives next to the data it manipulates.

- [ ] **Step 1: Add the type**

Add to `src/editor.rs`:
```rust
/// In-progress gesture state. `App` holds one of these between mouse-down
/// and mouse-up, picking which to enter based on the active `Tool` and on
/// whether the mouse-down hit a currently-selected bead.
#[derive(Debug, Clone)]
pub enum DragState {
    None,
    /// Chain tool: `last_idx` is the last placed bead (the chain extends
    /// from here on each mousemove).
    Chain { last_idx: u32 },
    /// Rect tool: `anchor` is the world position where the drag started.
    /// `current` updates each mousemove; mouseup commits select_rect.
    Rect { anchor: Vec2, current: Vec2, moved: bool },
    /// Lasso tool: polyline of cursor samples. mouseup closes & commits.
    Lasso { points: Vec<Vec2> },
    /// Move drag: cursor world-pos at the previous mousemove.
    Move { last_cursor: Vec2 },
}

impl Default for DragState {
    fn default() -> Self { DragState::None }
}
```

- [ ] **Step 2: Verify it builds**

Run: `cargo check --lib`
Expected: success.

- [ ] **Step 3: Commit**

```bash
git add src/editor.rs
git commit -m "feat(editor): DragState enum for in-progress gestures"
```

---

## Task 12: Renderer — per-bead `selected` flag + outline ring

**Files:**
- Modify: `src/render.rs:204-228` (update_beads), `shaders/beads.wgsl`

Replace the unused `_pad: u32` on `BeadGpu` with `selected: u32`. The fragment shader draws a white ring at `0.85 < d < 0.95` when `selected != 0`, in addition to the filled disc.

- [ ] **Step 1: Extend `BeadGpu` and `update_beads`**

In `src/render.rs`, change the struct:
```rust
#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
struct BeadGpu {
    pos: [f32; 2],
    state: u32,
    selected: u32,
}
```

Change the `update_beads` signature and body. Replace the existing fn with:
```rust
pub fn update_beads(&mut self, positions: &[Vec2], states: &[u32], selected: &[u32]) {
    debug_assert_eq!(positions.len(), states.len());
    debug_assert_eq!(positions.len(), selected.len());
    if positions.len() > self.bead_capacity {
        self.bead_capacity = positions.len().next_power_of_two();
        self.bead_buf = self.device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("beads"),
            size: (self.bead_capacity * std::mem::size_of::<BeadGpu>()) as u64,
            usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        self.bind_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("beads bg"),
            layout: &self.bind_layout,
            entries: &[
                wgpu::BindGroupEntry { binding: 0, resource: self.camera_buf.as_entire_binding() },
                wgpu::BindGroupEntry { binding: 1, resource: self.bead_buf.as_entire_binding() },
            ],
        });
    }
    let gpu_beads: Vec<BeadGpu> = positions.iter().zip(states.iter()).zip(selected.iter())
        .map(|((p, &s), &sel)| BeadGpu { pos: [p.x, p.y], state: s, selected: sel })
        .collect();
    self.queue.write_buffer(&self.bead_buf, 0, bytemuck::cast_slice(&gpu_beads));
}
```

- [ ] **Step 2: Update WGSL**

Edit `shaders/beads.wgsl`. Replace the file with:
```wgsl
struct Bead {
    pos: vec2<f32>,
    state: u32,
    selected: u32,
};

struct Camera {
    view_proj: mat4x4<f32>,
    radius: f32,
    world_size: f32,
    _pad0: f32,
    _pad1: f32,
    state_colors: array<vec4<f32>, 8>,
};

@group(0) @binding(0) var<uniform> camera: Camera;
@group(0) @binding(1) var<storage, read> beads: array<Bead>;

struct VsIn {
    @location(0) quad_uv: vec2<f32>,
    @builtin(instance_index) inst: u32,
};

struct VsOut {
    @builtin(position) clip: vec4<f32>,
    @location(0) local: vec2<f32>,
    @location(1) @interpolate(flat) state: u32,
    @location(2) @interpolate(flat) selected: u32,
};

@vertex
fn vs_main(in: VsIn) -> VsOut {
    let bead_idx = in.inst / 9u;
    let ghost = in.inst % 9u;
    let gx = f32(i32(ghost % 3u) - 1);
    let gy = f32(i32(ghost / 3u) - 1);
    let bead = beads[bead_idx];
    let center = bead.pos + vec2<f32>(gx, gy) * camera.world_size;
    let world = center + in.quad_uv * camera.radius;
    var out: VsOut;
    out.clip = camera.view_proj * vec4<f32>(world, 0.0, 1.0);
    out.local = in.quad_uv;
    out.state = bead.state;
    out.selected = bead.selected;
    return out;
}

@fragment
fn fs_main(in: VsOut) -> @location(0) vec4<f32> {
    let d = length(in.local);
    if (d > 1.0) {
        discard;
    }
    let body = smoothstep(1.0, 0.95, d);
    let c = camera.state_colors[in.state].rgb;
    // Selection ring: bright white band just outside the body, fading at edges.
    var color = c;
    var alpha = body;
    if (in.selected != 0u) {
        // Band centered around d = 0.90, ~0.06 wide. Outside the band the ring
        // contributes nothing; inside it, blend toward white.
        let ring = smoothstep(0.83, 0.88, d) * (1.0 - smoothstep(0.95, 1.0, d));
        color = mix(color, vec3<f32>(1.0, 1.0, 1.0), ring);
        alpha = max(alpha, ring);
    }
    return vec4<f32>(color, alpha);
}
```

- [ ] **Step 3: Update both `update_beads` call sites in `src/app.rs`**

In `src/app.rs`, the `RedrawRequested` arm has two `renderer.update_beads(...)` calls. Replace them.

The Run branch (currently around line 559):
```rust
let selected: Vec<u32> = match &self.scene {
    Some(s) => (0..sim.positions.len()).map(|i| if s.selection.contains(&(i as u32)) { 1 } else { 0 }).collect(),
    None => vec![0; sim.positions.len()],
};
renderer.update_beads(&sim.positions, &sim.states, &selected);
```

The Edit branch (around line 573):
```rust
let scene = self.scene.as_ref().expect("scene missing in Edit mode");
let positions: Vec<glam::Vec2> = scene.beads.iter()
    .map(|b| glam::Vec2::new(b.pos[0], b.pos[1]))
    .collect();
let states: Vec<u32> = scene.beads.iter()
    .map(|b| scene.chemistry.state_index(&b.state).unwrap_or(0) as u32)
    .collect();
let selected: Vec<u32> = (0..positions.len())
    .map(|i| if scene.selection.contains(&(i as u32)) { 1 } else { 0 })
    .collect();
renderer.update_beads(&positions, &states, &selected);
```

- [ ] **Step 4: Verify build**

Run: `cargo check --target wasm32-unknown-unknown && cargo check --lib`
Expected: both succeed.

- [ ] **Step 5: Commit**

```bash
git add src/render.rs shaders/beads.wgsl src/app.rs
git commit -m "feat(render): per-bead selected flag + outline ring shader"
```

---

## Task 13: Renderer — screen-space overlay pipeline for rect/lasso

**Files:**
- Modify: `src/render.rs`
- Create: `shaders/overlay.wgsl`

A separate line-list pipeline drawn after the bead pass. Up to ~256 segments is plenty (rect = 4, lasso polyline = however many samples; cap at 256).

- [ ] **Step 1: Create the shader**

Create `shaders/overlay.wgsl`:
```wgsl
struct Camera {
    view_proj: mat4x4<f32>,
    radius: f32,
    world_size: f32,
    _pad0: f32,
    _pad1: f32,
    state_colors: array<vec4<f32>, 8>,
};

@group(0) @binding(0) var<uniform> camera: Camera;

struct VsIn {
    @location(0) world: vec2<f32>,
};

struct VsOut {
    @builtin(position) clip: vec4<f32>,
};

@vertex
fn vs_main(in: VsIn) -> VsOut {
    var out: VsOut;
    out.clip = camera.view_proj * vec4<f32>(in.world, 0.0, 1.0);
    return out;
}

@fragment
fn fs_main(_in: VsOut) -> @location(0) vec4<f32> {
    return vec4<f32>(1.0, 1.0, 1.0, 0.7);
}
```

- [ ] **Step 2: Add the pipeline to `Renderer`**

In `src/render.rs`, add fields to `Renderer`:
```rust
overlay_pipeline: wgpu::RenderPipeline,
overlay_buf: wgpu::Buffer,
overlay_capacity: usize,
overlay_vertex_count: u32,
overlay_bind_group: wgpu::BindGroup,
```

In `Renderer::new`, after the bead pipeline is built, add:
```rust
let overlay_capacity: usize = 256;  // enough for any plausible polyline
let overlay_buf = device.create_buffer(&wgpu::BufferDescriptor {
    label: Some("overlay verts"),
    size: (overlay_capacity * std::mem::size_of::<[f32; 2]>()) as u64,
    usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
    mapped_at_creation: false,
});

let overlay_bind_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
    label: Some("overlay bind"),
    entries: &[
        wgpu::BindGroupLayoutEntry {
            binding: 0,
            visibility: wgpu::ShaderStages::VERTEX,
            ty: wgpu::BindingType::Buffer {
                ty: wgpu::BufferBindingType::Uniform,
                has_dynamic_offset: false,
                min_binding_size: None,
            },
            count: None,
        },
    ],
});
let overlay_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
    label: Some("overlay bg"),
    layout: &overlay_bind_layout,
    entries: &[
        wgpu::BindGroupEntry { binding: 0, resource: camera_buf.as_entire_binding() },
    ],
});
let overlay_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
    label: Some("overlay"),
    source: wgpu::ShaderSource::Wgsl(include_str!("../shaders/overlay.wgsl").into()),
});
let overlay_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
    label: Some("overlay layout"),
    bind_group_layouts: &[&overlay_bind_layout],
    push_constant_ranges: &[],
});
let overlay_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
    label: Some("overlay pipeline"),
    layout: Some(&overlay_pipeline_layout),
    vertex: wgpu::VertexState {
        module: &overlay_shader,
        entry_point: Some("vs_main"),
        buffers: &[wgpu::VertexBufferLayout {
            array_stride: 8,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &[wgpu::VertexAttribute {
                offset: 0,
                shader_location: 0,
                format: wgpu::VertexFormat::Float32x2,
            }],
        }],
        compilation_options: Default::default(),
    },
    fragment: Some(wgpu::FragmentState {
        module: &overlay_shader,
        entry_point: Some("fs_main"),
        targets: &[Some(wgpu::ColorTargetState {
            format,
            blend: Some(wgpu::BlendState::ALPHA_BLENDING),
            write_mask: wgpu::ColorWrites::ALL,
        })],
        compilation_options: Default::default(),
    }),
    primitive: wgpu::PrimitiveState {
        topology: wgpu::PrimitiveTopology::LineList,
        ..Default::default()
    },
    depth_stencil: None,
    multisample: wgpu::MultisampleState::default(),
    multiview: None,
    cache: None,
});
```

Add the fields to the `Ok(Self { ... })` return at the bottom of `new`:
```rust
overlay_pipeline,
overlay_buf,
overlay_capacity,
overlay_vertex_count: 0,
overlay_bind_group,
```

- [ ] **Step 3: Add a method to upload overlay vertices**

Add to `impl Renderer`:
```rust
/// Upload a polyline of world-space vertex pairs. Each consecutive pair of
/// vertices defines one line segment (LineList topology). Pass an empty
/// slice to hide the overlay this frame.
pub fn update_overlay(&mut self, segments: &[[f32; 2]]) {
    debug_assert!(segments.len() % 2 == 0, "LineList needs an even vertex count");
    let count = segments.len().min(self.overlay_capacity) as u32;
    self.overlay_vertex_count = count;
    if count == 0 { return; }
    self.queue.write_buffer(
        &self.overlay_buf,
        0,
        bytemuck::cast_slice(&segments[..count as usize]),
    );
}
```

- [ ] **Step 4: Draw the overlay**

In `Renderer::render` (currently at line 267), add a second pass between the bead pass and the queue.submit. Replace the existing render body with:
```rust
pub fn render(&self, bead_count: usize) -> Result<()> {
    let frame = self.surface.get_current_texture()?;
    let view = frame.texture.create_view(&Default::default());
    let mut encoder = self.device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
        label: Some("bead encoder"),
    });
    {
        let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some("bead pass"),
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view: &view,
                resolve_target: None,
                ops: wgpu::Operations {
                    load: wgpu::LoadOp::Clear(wgpu::Color { r: 0.05, g: 0.05, b: 0.07, a: 1.0 }),
                    store: wgpu::StoreOp::Store,
                },
            })],
            depth_stencil_attachment: None,
            timestamp_writes: None,
            occlusion_query_set: None,
        });
        pass.set_pipeline(&self.pipeline);
        pass.set_bind_group(0, &self.bind_group, &[]);
        pass.set_vertex_buffer(0, self.quad_vbuf.slice(..));
        pass.draw(0..6, 0..(bead_count * 9) as u32);
    }
    if self.overlay_vertex_count > 0 {
        let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some("overlay pass"),
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view: &view,
                resolve_target: None,
                ops: wgpu::Operations {
                    load: wgpu::LoadOp::Load,
                    store: wgpu::StoreOp::Store,
                },
            })],
            depth_stencil_attachment: None,
            timestamp_writes: None,
            occlusion_query_set: None,
        });
        pass.set_pipeline(&self.overlay_pipeline);
        pass.set_bind_group(0, &self.overlay_bind_group, &[]);
        pass.set_vertex_buffer(0, self.overlay_buf.slice(..));
        pass.draw(0..self.overlay_vertex_count, 0..1);
    }
    self.queue.submit(std::iter::once(encoder.finish()));
    frame.present();
    Ok(())
}
```

- [ ] **Step 5: Verify build**

Run: `cargo check --target wasm32-unknown-unknown && cargo check --lib`
Expected: both succeed.

- [ ] **Step 6: Commit**

```bash
git add src/render.rs shaders/overlay.wgsl
git commit -m "feat(render): overlay line pipeline for rect/lasso visual"
```

---

## Task 14: `App` — route mouse events through tool + DragState

**Files:**
- Modify: `src/app.rs`

Replace the current single-shot `place_at_cursor` with a state-machine driven by `Tool` and `DragState`. mouse-down picks the gesture (or Move if it hits a selected bead); mouse-move extends it; mouse-up commits.

- [ ] **Step 1: Add fields to `App`**

In `src/app.rs`, extend `App`:
```rust
pub struct App {
    window: Option<Arc<Window>>,
    renderer: Option<Renderer>,
    sim: Option<Sim>,
    scheduler: Box<dyn Scheduler>,
    last_frame: Instant,
    mode: crate::editor::Mode,
    scene: Option<crate::editor::Scene>,
    cursor: winit::dpi::PhysicalPosition<f64>,
    drag: crate::editor::DragState,
    /// True only while the left mouse button is held. mousemove uses this to
    /// know whether to extend the current `drag`.
    mouse_down: bool,
    #[cfg(target_arch = "wasm32")]
    proxy: Option<EventLoopProxy<UserEvent>>,
}
```

Update `App::new`:
```rust
pub fn new() -> Self {
    Self {
        window: None,
        renderer: None,
        sim: None,
        scheduler: Box::new(CpuSequential),
        last_frame: Instant::now(),
        mode: crate::editor::Mode::Run,
        scene: None,
        cursor: winit::dpi::PhysicalPosition::new(0.0, 0.0),
        drag: crate::editor::DragState::None,
        mouse_down: false,
        #[cfg(target_arch = "wasm32")]
        proxy: None,
    }
}
```

- [ ] **Step 2: Replace `place_at_cursor` with the dispatcher and helpers**

Delete the current `place_at_cursor` method. Add this private helper block:
```rust
impl App {
    fn cursor_world(&self) -> Option<glam::Vec2> {
        let window = self.window.as_ref()?;
        let scene = self.scene.as_ref()?;
        let viewport = window.inner_size();
        Some(crate::editor::screen_to_world(
            (self.cursor.x, self.cursor.y),
            (viewport.width, viewport.height),
            scene.world_size,
        ))
    }

    /// True if `world_pos` lies within RADIUS of any currently-selected bead.
    fn hit_selected(scene: &crate::editor::Scene, world_pos: glam::Vec2) -> bool {
        scene.selection.iter().any(|&idx| {
            let p = glam::Vec2::from(scene.beads[idx as usize].pos);
            (p - world_pos).length() <= crate::ccd::RADIUS
        })
    }

    fn rebuild_sim_from_scene(&mut self) {
        let scene = self.scene.as_ref().expect("scene present");
        let new_sim = scene.to_sim();
        #[cfg(target_arch = "wasm32")]
        {
            use crate::chemistry::compile_chemistry;
            use crate::parallel::CpuParallel;
            let compiled = compile_chemistry(new_sim.chemistry()).expect("compile chemistry");
            self.scheduler = Box::new(CpuParallel::new(&new_sim, compiled));
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            self.scheduler = Box::new(CpuSequential);
        }
        self.sim = Some(new_sim);
    }

    fn on_mouse_down(&mut self) {
        self.mouse_down = true;
        let Some(world_pos) = self.cursor_world() else { return };
        let Some(scene) = self.scene.as_mut() else { return };
        // Move drag short-circuits all tools when the mouse-down lands on a selected bead.
        if Self::hit_selected(scene, world_pos) {
            self.drag = crate::editor::DragState::Move { last_cursor: world_pos };
            return;
        }
        // In Run mode only Place is allowed (matches MVP). Other tools are no-ops.
        match (self.mode, scene.tool) {
            (crate::editor::Mode::Run, _) => {
                // Place during Run: snapshot, place, rebuild.
                if let Some(sim) = &self.sim { scene.snapshot_from_sim(sim); }
                scene.place(world_pos);
                self.rebuild_sim_from_scene();
                self.drag = crate::editor::DragState::None;
            }
            (crate::editor::Mode::Edit, crate::editor::Tool::Place) => {
                scene.place(world_pos);
                self.drag = crate::editor::DragState::None;
            }
            (crate::editor::Mode::Edit, crate::editor::Tool::Chain) => {
                let idx = scene.place(world_pos);
                self.drag = crate::editor::DragState::Chain { last_idx: idx };
            }
            (crate::editor::Mode::Edit, crate::editor::Tool::Rect) => {
                self.drag = crate::editor::DragState::Rect { anchor: world_pos, current: world_pos, moved: false };
            }
            (crate::editor::Mode::Edit, crate::editor::Tool::Lasso) => {
                self.drag = crate::editor::DragState::Lasso { points: vec![world_pos] };
            }
        }
    }

    fn on_mouse_move(&mut self) {
        if !self.mouse_down { return; }
        let Some(world_pos) = self.cursor_world() else { return };
        let Some(scene) = self.scene.as_mut() else { return };
        match &mut self.drag {
            crate::editor::DragState::Chain { last_idx } => {
                *last_idx = scene.chain_extend(*last_idx, world_pos);
            }
            crate::editor::DragState::Rect { current, moved, .. } => {
                *current = world_pos;
                *moved = true;
            }
            crate::editor::DragState::Lasso { points } => {
                // Sample only when the cursor has moved by at least ~0.05 world
                // units since the last sample. Keeps the polygon manageable.
                if let Some(last) = points.last() {
                    if (*last - world_pos).length() >= 0.05 {
                        points.push(world_pos);
                    }
                }
            }
            crate::editor::DragState::Move { last_cursor } => {
                let delta = world_pos - *last_cursor;
                scene.translate_selection(delta);
                *last_cursor = world_pos;
            }
            crate::editor::DragState::None => {}
        }
    }

    fn on_mouse_up(&mut self) {
        self.mouse_down = false;
        let drag = std::mem::take(&mut self.drag);
        let Some(scene) = self.scene.as_mut() else { return };
        match drag {
            crate::editor::DragState::Rect { anchor, current, moved } => {
                if moved {
                    scene.select_rect(anchor, current);
                } else {
                    scene.selection.clear();
                }
            }
            crate::editor::DragState::Lasso { points } => {
                if points.len() >= 3 {
                    scene.select_lasso(&points);
                } else {
                    scene.selection.clear();
                }
            }
            crate::editor::DragState::Move { .. } => {
                // Final position already applied in on_mouse_move; nothing to
                // commit. Clamp was applied incrementally in translate_selection.
            }
            crate::editor::DragState::Chain { .. } | crate::editor::DragState::None => {}
        }
    }
}
```

- [ ] **Step 3: Wire the new dispatchers into `window_event`**

In `window_event`, replace the existing `WindowEvent::MouseInput` arm with:
```rust
WindowEvent::MouseInput { state, button, .. } => {
    use winit::event::{ElementState, MouseButton};
    if button == MouseButton::Left {
        match state {
            ElementState::Pressed => self.on_mouse_down(),
            ElementState::Released => self.on_mouse_up(),
        }
    }
}
```

Update the `WindowEvent::CursorMoved` arm to call `on_mouse_move` after recording the cursor:
```rust
WindowEvent::CursorMoved { position, .. } => {
    self.cursor = position;
    self.on_mouse_move();
}
```

- [ ] **Step 4: Emit overlay segments each frame**

The `Rect` and `Lasso` drags need a visible overlay. Compute the segments from `self.drag` and push them to the renderer each frame. Add a helper to `impl App`:

```rust
/// World-space line segments to draw as the rect/lasso overlay this frame.
/// Returns an empty vec when no overlay is active. LineList topology: each
/// pair of consecutive entries defines one segment.
fn overlay_segments(&self) -> Vec<[f32; 2]> {
    match &self.drag {
        crate::editor::DragState::Rect { anchor, current, .. } => {
            let (a, b) = (*anchor, *current);
            let (xmin, xmax) = if a.x <= b.x { (a.x, b.x) } else { (b.x, a.x) };
            let (ymin, ymax) = if a.y <= b.y { (a.y, b.y) } else { (b.y, a.y) };
            // 4 sides, line-list = 8 vertices.
            vec![
                [xmin, ymin], [xmax, ymin],
                [xmax, ymin], [xmax, ymax],
                [xmax, ymax], [xmin, ymax],
                [xmin, ymax], [xmin, ymin],
            ]
        }
        crate::editor::DragState::Lasso { points } => {
            if points.len() < 2 { return Vec::new(); }
            let mut segs = Vec::with_capacity(points.len() * 2);
            for w in points.windows(2) {
                segs.push([w[0].x, w[0].y]);
                segs.push([w[1].x, w[1].y]);
            }
            segs
        }
        _ => Vec::new(),
    }
}
```

In `RedrawRequested`, after both `renderer.update_beads(...)` call sites and before `renderer.render(...)`, add:
```rust
let overlay = self.overlay_segments();
renderer.update_overlay(&overlay);
```

(Add this in both the Run-branch and the Edit-branch.)

- [ ] **Step 5: Verify build**

Run: `cargo check --target wasm32-unknown-unknown && cargo check --lib`
Expected: both succeed.

- [ ] **Step 6: Commit**

```bash
git add src/app.rs
git commit -m "feat(app): route mouse-down/move/up through Tool + DragState"
```

---

## Task 15: `App` — Delete / Backspace + clear selection on Run

**Files:**
- Modify: `src/app.rs`

`Del` and `Backspace` call `Scene::delete_selection`. Run-mode entry must also clear `Scene.selection` (and drop any in-progress drag).

- [ ] **Step 1: Handle keyboard in `window_event`**

In `window_event`, add a new arm before the catch-all `_ => {}`:
```rust
WindowEvent::KeyboardInput { event: key_event, .. } => {
    use winit::event::ElementState;
    use winit::keyboard::{Key, NamedKey};
    if key_event.state == ElementState::Pressed {
        let is_delete = matches!(
            key_event.logical_key,
            Key::Named(NamedKey::Delete) | Key::Named(NamedKey::Backspace)
        );
        if is_delete {
            if self.mode == crate::editor::Mode::Edit {
                if let Some(scene) = self.scene.as_mut() {
                    scene.delete_selection();
                }
            }
        }
    }
}
```

- [ ] **Step 2: Clear selection + drag in `transition_mode` Run branch**

In `transition_mode`, update the `Run` arm to clear selection and drag:
```rust
crate::editor::Mode::Run => {
    if let Some(scene) = self.scene.as_mut() {
        scene.selection.clear();
    }
    self.drag = crate::editor::DragState::None;
    self.mouse_down = false;
    if let Some(scene) = &self.scene {
        let new_sim = scene.to_sim();
        #[cfg(target_arch = "wasm32")]
        {
            use crate::chemistry::compile_chemistry;
            use crate::parallel::CpuParallel;
            let compiled = compile_chemistry(new_sim.chemistry())
                .expect("compile chemistry");
            self.scheduler = Box::new(CpuParallel::new(&new_sim, compiled));
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            self.scheduler = Box::new(CpuSequential);
        }
        self.sim = Some(new_sim);
        self.mode = crate::editor::Mode::Run;
    }
}
```

Also update the chemistry-switch handler in `RedrawRequested` to clear `drag` + `mouse_down` (in addition to the existing `switch_chemistry` call which already clears `selection`):
```rust
if let Some(name) = new_chemistry {
    if let Ok(new_chem) = crate::editor::load_chemistry_by_name(&name) {
        if let Some(scene) = self.scene.as_mut() {
            scene.switch_chemistry(new_chem, name);
        }
        self.sim = None;
        self.mode = crate::editor::Mode::Edit;
        self.drag = crate::editor::DragState::None;
        self.mouse_down = false;
        if let (Some(renderer), Some(scene)) = (self.renderer.as_mut(), self.scene.as_ref()) {
            let palette: Vec<[f32; 3]> = scene.chemistry.colors.clone();
            renderer.update_camera(scene.world_size, &palette);
        }
    } else {
        log::warn!("set_chemistry: unknown chemistry {:?}", name);
    }
}
```

- [ ] **Step 3: Verify build**

Run: `cargo check --target wasm32-unknown-unknown`
Expected: success.

- [ ] **Step 4: Commit**

```bash
git add src/app.rs
git commit -m "feat(app): Del/Backspace deletes selection; Run clears it"
```

---

## Task 16: Bridge globals — `GetTool` / `SetTool` / `SelectionCount`

**Files:**
- Modify: `src/app.rs`

Three new `window.__jigglefab*` functions, plus snapshot wiring so JS can read them.

- [ ] **Step 1: Extend `web_bridge::Snapshot` and `PendingCommands`**

In `src/app.rs`, edit the `web_bridge` module:
```rust
#[cfg(target_arch = "wasm32")]
mod web_bridge {
    use std::cell::RefCell;

    #[derive(Default)]
    pub struct PendingCommands {
        pub set_mode: Option<crate::editor::Mode>,
        pub set_edit_state: Option<u32>,
        pub set_chemistry: Option<String>,
        pub set_tool: Option<crate::editor::Tool>,
    }

    thread_local! {
        pub static COMMANDS: RefCell<PendingCommands> = RefCell::new(PendingCommands::default());
        pub static SNAPSHOT: RefCell<Snapshot> = RefCell::new(Snapshot::default());
    }

    #[derive(Default, Clone)]
    pub struct Snapshot {
        pub mode: &'static str,
        pub bead_count: u32,
        pub palette: Vec<(String, [f32; 3])>,
        pub tool: &'static str,
        pub selection_count: u32,
    }
}
```

- [ ] **Step 2: Add installer fns and snapshot writes**

Add three new installer fns alongside the existing ones:
```rust
#[cfg(target_arch = "wasm32")]
fn install_window_get_tool() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> String {
        web_bridge::SNAPSHOT.with(|s| s.borrow().tool.to_string())
    }) as Box<dyn Fn() -> String>);
    expose_to_window!("__jigglefabGetTool", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_set_tool() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|s: String| {
        if let Some(t) = crate::editor::Tool::from_str(&s) {
            web_bridge::COMMANDS.with(|c| c.borrow_mut().set_tool = Some(t));
        }
    }) as Box<dyn Fn(String)>);
    expose_to_window!("__jigglefabSetTool", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_selection_count() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> u32 {
        web_bridge::SNAPSHOT.with(|s| s.borrow().selection_count)
    }) as Box<dyn Fn() -> u32>);
    expose_to_window!("__jigglefabSelectionCount", cb);
}
```

Call them from `resumed`'s wasm32 block:
```rust
install_window_get_tool();
install_window_set_tool();
install_window_selection_count();
```

(Insert after the existing `install_window_set_chemistry();` call.)

- [ ] **Step 3: Drain `set_tool` in `RedrawRequested` + populate snapshot fields**

In `RedrawRequested`'s wasm32 block, change the COMMANDS drain:
```rust
let (new_mode, edit_state, new_chemistry, new_tool) = web_bridge::COMMANDS.with(|c| {
    let mut cmds = c.borrow_mut();
    (cmds.set_mode.take(), cmds.set_edit_state.take(), cmds.set_chemistry.take(), cmds.set_tool.take())
});
```

And handle `new_tool` immediately after the `new_chemistry` block:
```rust
if let Some(tool) = new_tool {
    if let Some(scene) = self.scene.as_mut() {
        scene.tool = tool;
    }
}
```

Update the snapshot write at the bottom of the wasm32 block:
```rust
let tool_str = self.scene.as_ref().map(|s| s.tool.as_str()).unwrap_or("place");
let selection_count = self.scene.as_ref().map(|s| s.selection.len() as u32).unwrap_or(0);
web_bridge::SNAPSHOT.with(|s| {
    *s.borrow_mut() = web_bridge::Snapshot {
        mode: mode_str,
        bead_count,
        palette,
        tool: tool_str,
        selection_count,
    };
});
```

- [ ] **Step 4: Verify build**

Run: `cargo check --target wasm32-unknown-unknown`
Expected: success.

- [ ] **Step 5: Commit**

```bash
git add src/app.rs
git commit -m "feat(bridge): GetTool/SetTool/SelectionCount window globals"
```

---

## Task 17: HTML toolbar — tool pills, JS wiring, selection HUD

**Files:**
- Modify: `index.html`

Add a `tool` row to `#editor-toolbar` and a small HUD line for selection count.

- [ ] **Step 1: Add the tool row to the toolbar**

In `index.html`, inside `<nav id="editor-toolbar">`, add a new row above the `mode` row:
```html
        <div class="row">
            <span class="group-label">tool</span>
            <a id="btn-tool-place" class="tool active" data-tool="place">Place</a>
            <a id="btn-tool-chain" class="tool" data-tool="chain">Chain</a>
            <a id="btn-tool-rect" class="tool" data-tool="rect">Rect</a>
            <a id="btn-tool-lasso" class="tool" data-tool="lasso">Lasso</a>
        </div>
```

- [ ] **Step 2: Add selection-count line to HUD**

In `#hud .body`, add another `div` after the `beads` line:
```html
            <div><span class="lbl">sel </span><span class="v" id="hud-sel">0</span></div>
```

- [ ] **Step 3: Wire the tool pills in JS**

In the `<script>` block, add a block alongside the existing mode-button wiring:
```javascript
        // Tool pills. Active class follows __jigglefabGetTool(); each click
        // forwards the tool name to wasm via __jigglefabSetTool.
        const toolButtons = document.querySelectorAll("#editor-toolbar a.tool");
        function paintToolButtons(active) {
            toolButtons.forEach(btn => {
                btn.classList.toggle("active", btn.dataset.tool === active);
            });
        }
        toolButtons.forEach(btn => {
            btn.addEventListener("click", (e) => {
                e.preventDefault();
                if (window.__jigglefabSetTool) {
                    window.__jigglefabSetTool(btn.dataset.tool);
                }
                paintToolButtons(btn.dataset.tool);
            });
        });
```

- [ ] **Step 4: Poll tool + selection count in `refreshToolbar`**

Update `refreshToolbar` to read the new globals. After the existing bead-count update:
```javascript
            if (typeof window.__jigglefabGetTool === "function") {
                paintToolButtons(window.__jigglefabGetTool());
            }
            const selEl = document.getElementById("hud-sel");
            if (typeof window.__jigglefabSelectionCount === "function" && selEl) {
                selEl.textContent = window.__jigglefabSelectionCount();
            }
```

- [ ] **Step 5: Manual sanity check (build)**

Run: `cargo build --target wasm32-unknown-unknown`
Expected: success. (The HTML changes don't affect Rust compilation; this confirms the wasm binary still builds for the bundled trunk pipeline.)

- [ ] **Step 6: Commit**

```bash
git add index.html
git commit -m "feat(editor): HTML tool row + selection-count HUD"
```

---

## Task 18: Browser smoke test — chain, rect, delete, lasso, move

**Files:**
- Modify: `scripts/verify-web.py`

Extend the `--editor` block with the new gestures, driving them through the JS bridge (chain draws via mouse.move on the page) and through direct bead-list assertions.

- [ ] **Step 1: Extend the `--editor` block**

In `scripts/verify-web.py`, replace the `if "--editor" in sys.argv:` block with:
```python
        if "--editor" in sys.argv:
            # Editor smoke test (extended for chains / rect / lasso / move / delete).
            await page.wait_for_function("typeof window.__jigglefabSetMode === 'function'", timeout=10000)
            await page.evaluate("window.__jigglefabSetMode('edit')")
            await page.wait_for_function("window.__jigglefabGetMode() === 'edit'")

            box = await page.evaluate(
                "(() => { const c = document.querySelector('canvas');"
                " const r = c.getBoundingClientRect();"
                " return {x: r.left, y: r.top, w: r.width, h: r.height}; })()"
            )
            cx, cy = box["x"] + box["w"] / 2, box["y"] + box["h"] / 2

            # --- Place tool: still works as in MVP. ---
            before = await page.evaluate("window.__jigglefabBeadCount()")
            await page.mouse.click(cx, cy)
            await page.wait_for_function(f"window.__jigglefabBeadCount() === {before + 1}", timeout=2000)

            # --- Chain tool: drag a short path. Expect bead count > +1. ---
            await page.evaluate("window.__jigglefabSetTool('chain')")
            await page.wait_for_function("window.__jigglefabGetTool() === 'chain'")
            chain_before = await page.evaluate("window.__jigglefabBeadCount()")
            await page.mouse.move(cx - 100, cy + 100)
            await page.mouse.down()
            for i in range(1, 8):
                await page.mouse.move(cx - 100 + i * 15, cy + 100 + i * 15)
            await page.mouse.up()
            await page.wait_for_function(f"window.__jigglefabBeadCount() > {chain_before + 1}", timeout=2000)

            # --- Rect tool: drag across the chain. Selection > 0. ---
            await page.evaluate("window.__jigglefabSetTool('rect')")
            await page.wait_for_function("window.__jigglefabGetTool() === 'rect'")
            await page.mouse.move(cx - 150, cy + 50)
            await page.mouse.down()
            await page.mouse.move(cx + 50, cy + 250)
            await page.mouse.up()
            await page.wait_for_function("window.__jigglefabSelectionCount() > 0", timeout=2000)

            # --- Delete: shrink bead count, clear selection. ---
            sel_count = await page.evaluate("window.__jigglefabSelectionCount()")
            beads_before_del = await page.evaluate("window.__jigglefabBeadCount()")
            await page.keyboard.press("Delete")
            await page.wait_for_function(
                f"window.__jigglefabBeadCount() === {beads_before_del - sel_count}", timeout=2000)
            await page.wait_for_function("window.__jigglefabSelectionCount() === 0", timeout=2000)

            # --- Lasso tool: drag a closed loop. Selection > 0. ---
            # Place a fresh bead under the loop so the lasso has something to enclose.
            await page.evaluate("window.__jigglefabSetTool('place')")
            await page.mouse.click(cx + 80, cy + 80)
            await page.evaluate("window.__jigglefabSetTool('lasso')")
            await page.wait_for_function("window.__jigglefabGetTool() === 'lasso'")
            await page.mouse.move(cx + 50, cy + 50)
            await page.mouse.down()
            for (dx, dy) in [(80, 0), (80, 80), (0, 80), (0, 0)]:
                await page.mouse.move(cx + 50 + dx, cy + 50 + dy)
            await page.mouse.up()
            await page.wait_for_function("window.__jigglefabSelectionCount() > 0", timeout=2000)

            # --- Move: drag a selected bead. Run still works after. ---
            # We don't assert the exact translation; just that drag-then-Run preserves count.
            sel_after_lasso = await page.evaluate("window.__jigglefabSelectionCount()")
            beads_after_lasso = await page.evaluate("window.__jigglefabBeadCount()")
            await page.mouse.move(cx + 80, cy + 80)
            await page.mouse.down()
            await page.mouse.move(cx + 120, cy + 120)
            await page.mouse.up()
            assert sel_after_lasso > 0
            await page.evaluate("window.__jigglefabSetMode('run')")
            await page.wait_for_function("window.__jigglefabGetMode() === 'run'", timeout=2000)
            await page.wait_for_function(f"window.__jigglefabBeadCount() === {beads_after_lasso}", timeout=2000)
            await page.wait_for_function("window.__jigglefabSelectionCount() === 0", timeout=2000)

            console_lines.append("[editor] extended smoke test passed")
```

- [ ] **Step 2: Run the smoke test locally**

Build + serve trunk (uses the standard project loop — `trunk serve --release` in another shell), then:
```bash
python scripts/verify-web.py http://127.0.0.1:8080/ --editor --headed
```
Expected: exits 0, with `[editor] extended smoke test passed` in the console section.

If trunk serve isn't currently up, follow the standard local-build path documented in `memory/jigglefab-build-env.md`.

- [ ] **Step 3: Commit**

```bash
git add scripts/verify-web.py
git commit -m "test(web): editor smoke covers chain/rect/lasso/move/delete"
```

---

## Task 19: Perf assertion — explicit-bonds path on large scene

**Files:**
- Modify: `src/sim.rs`

Per spec §8: `Sim::from_fab` with `bonds = Some(explicit)` on a 30k-bead scene completes in < 5 ms. The Run-mode live-edit rebuild path depends on this.

- [ ] **Step 1: Write the failing perf test**

Append to `src/sim.rs` inside `mod tests`:
```rust
#[test]
#[cfg(not(target_arch = "wasm32"))]
fn from_fab_with_explicit_bonds_30k_under_5ms() {
    use std::time::Instant;
    let chem = load_chemistry("chemistries/wire.toml").unwrap();
    // Cold path: derive once so we have a bond list to seed the explicit path with.
    let mut fab = load_fab("fabs/wire-100x30x10.toml").unwrap();
    let sim_warm = Sim::from_fab(&fab, chem.clone());
    let bonds_vec: Vec<[u32; 2]> = sim_warm.bonds().iter().map(|&(a, b)| [a, b]).collect();
    fab.meta.bonds = Some(bonds_vec);
    // Warm-up build (don't measure first iteration — allocator hot path).
    let _ = Sim::from_fab(&fab, chem.clone());
    // Measured build.
    let start = Instant::now();
    let _ = Sim::from_fab(&fab, chem);
    let elapsed = start.elapsed();
    assert!(elapsed.as_millis() < 5,
            "explicit-bonds build on 30k beads took {} ms (budget 5)", elapsed.as_millis());
}
```

This mutates `fab.meta.bonds` directly (the `Meta` struct's `bonds` field is `pub`), then re-builds the `Sim` through the explicit-bonds branch. Simple — no string munging.

- [ ] **Step 2: Run the test**

Run: `cargo test --release --lib sim::tests::from_fab_with_explicit_bonds_30k_under_5ms`
Expected: passes (the explicit-bonds branch is O(N) — pure positions vec + collect).

If the test fails because the explicit-bonds collect path is somehow not O(N), profile with `src/parallel/profile.rs` first per `memory/feedback-profile-before-perf-speculation.md` — don't speculate; measure.

- [ ] **Step 3: Commit**

```bash
git add src/sim.rs
git commit -m "test(sim): assert 30k-bead explicit-bonds build under 5 ms"
```

---

## Task 20: Final verification + push

- [ ] **Step 1: Run full test suite**

Run:
```bash
cargo test --lib
cargo check --target wasm32-unknown-unknown
```
Expected: both succeed.

- [ ] **Step 2: Final smoke test against local build**

```bash
python scripts/verify-web.py http://127.0.0.1:8080/ --editor
```
Expected: exit 0.

- [ ] **Step 3: Push to web branch when satisfied**

(Manual — user-driven; deferred for review per CLAUDE.md instructions on push.)

---

## Self-review notes

- **Spec §1 (tool model):** Task 3 (enum), Task 16 (bridge), Task 17 (HTML).
- **Spec §2 (chain):** Tasks 5 + 6 (`append_chain_bead` + `chain_extend`). Anti-triangle test in Task 5.
- **Spec §3 (selection):** Tasks 7 (geom helpers) + 8 (writers) + 14 (gesture routing) + 13 (overlay render) + 12 (outline ring).
- **Spec §4 (move):** Task 9 (translate_selection) + Task 14 (Move drag).
- **Spec §5 (delete):** Task 10 (`delete_selection`) + Task 15 (Del/Backspace).
- **Spec §6 (bond model promotion):** Tasks 1 (Fab) + 2 (Sim) + 3 (Scene round-trip) + 4 (Place derive).
- **Spec §7 (files):** Covered. `editor.rs`, `app.rs`, `render.rs`, `index.html`, `fab.rs`, `sim.rs`. No new modules.
- **Spec §8 (testing):** Unit fab/sim — Tasks 1, 2, 19. Unit editor — Tasks 3-10. Perf — Task 19. Browser smoke — Task 18.
- **Spec §9 (deferred):** Not implemented (correctly).

**No placeholders.** Each step ships code or a concrete command. Type names match across tasks (`Tool`, `DragState`, `Scene.bonds: HashSet<(u32,u32)>`, `Scene.selection: HashSet<u32>`, `Scene.tool: Tool`).
