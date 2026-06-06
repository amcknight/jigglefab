# Editor Device Library — Core (Rust) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the pure-Rust core of the editor device library — the `Device`/`Suite`/`Library` data model, capture-from-selection, stamp-into-scene (with rotation), rotate-selection, chemistry-compatibility checks, and JSON persistence — all native-unit-testable, with no web wiring yet.

**Architecture:** A new `src/library.rs` module owns the data model + dock/suite mutations + JSON (de)serialization + chemistry hashing/compatibility. Three new methods on `Scene` in `src/editor.rs` bridge the live scene to the library: `extract_device` (selection → device), `instantiate_device` (device → scene), and `rotate_selection`. Everything is `cfg`-agnostic and tested with native `cargo test`. The web bridge, localStorage I/O, and UI are a *separate* follow-up plan that consumes this API.

**Tech Stack:** Rust, `serde` + `serde_json`, `glam::Vec2`, existing `crate::bond::BondPair` / `crate::grid::min_image` / `crate::chemistry::Chemistry`.

**Scope note:** This is plan 1 of 2 for the device-library spec (`docs/superpowers/specs/2026-06-06-editor-device-library-design.md`). It covers §2 (data model), §3 (capture), §4 (stamp), §5 rotation primitive, §7 (compatibility logic), and §9 (serialization logic). Web glue (§1 bridge, §6 UI, §8 bridge globals, §9 localStorage I/O, browser smoke) is plan 2.

---

### Task 1: Library data model + JSON persistence

**Files:**
- Modify: `Cargo.toml` (promote `serde_json` to a real dependency)
- Modify: `src/lib.rs` (declare the module)
- Create: `src/library.rs`

- [ ] **Step 1: Promote `serde_json` to a real dependency**

In `Cargo.toml`, add `serde_json` to `[dependencies]` (it is currently only under `[dev-dependencies]`). Add this line to the `[dependencies]` block (after `toml = "0.8"`):

```toml
serde_json = "1"
```

Leave the existing `[dev-dependencies] serde_json = "1"` line as-is (harmless duplicate; cargo dedupes). 

- [ ] **Step 2: Declare the module**

In `src/lib.rs`, add after the `pub mod grid;` line (line 8):

```rust
pub mod library;
```

- [ ] **Step 3: Write the failing test**

Create `src/library.rs` with ONLY the tests first (the types don't exist yet, so it won't compile — that's the failing state):

```rust
//! In-app device library: reusable sub-assemblies (devices), a working tray
//! (the dock), and saved bundles (suites). Pure data model + logic; the web
//! bridge / localStorage / UI live elsewhere. See
//! docs/superpowers/specs/2026-06-06-editor-device-library-design.md.

use serde::{Deserialize, Serialize};

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_device(id: u32, chem: &str) -> Device {
        Device {
            id,
            name: "elbow".into(),
            chemistry: chem.into(),
            chemistry_hash: 99,
            beads: vec![DeviceBead { state: "wire".into(), pos: [0.0, 0.0] }],
            bonds: vec![],
            ports: vec![],
        }
    }

    #[test]
    fn library_json_round_trips() {
        let lib = Library {
            version: LIBRARY_VERSION,
            next_id: 2,
            dock: vec![sample_device(1, "wire")],
            suites: vec![],
        };
        let json = lib.to_json();
        let back = Library::load_or_default(&json);
        assert_eq!(lib, back);
    }

    #[test]
    fn load_or_default_tolerates_garbage() {
        assert_eq!(Library::load_or_default("not json"), Library::default());
        assert_eq!(Library::load_or_default(""), Library::default());
    }

    #[test]
    fn default_library_is_empty_current_version() {
        let lib = Library::default();
        assert_eq!(lib.version, LIBRARY_VERSION);
        assert_eq!(lib.next_id, 0);
        assert!(lib.dock.is_empty());
        assert!(lib.suites.is_empty());
    }
}
```

- [ ] **Step 4: Run test to verify it fails**

Run: `cargo test --lib library::`
Expected: FAIL — compile error, `Device` / `DeviceBead` / `Library` / `LIBRARY_VERSION` not found.

- [ ] **Step 5: Write minimal implementation**

Insert this ABOVE the `#[cfg(test)]` block in `src/library.rs`:

```rust
/// Current persisted-schema version for `Library`. Bump + add a migration on a
/// breaking JSON change.
pub const LIBRARY_VERSION: u32 = 1;

/// One bead of a device. `pos` is RELATIVE to the device centroid (world units).
/// Velocity is intentionally not stored — devices capture rest shape only.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DeviceBead {
    pub state: String,
    pub pos: [f32; 2],
}

/// A reusable sub-assembly captured from the canvas.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Device {
    pub id: u32,
    pub name: String,
    pub chemistry: String,
    /// Advisory hash of the chemistry definition at save time (see chemistry_hash).
    pub chemistry_hash: u64,
    pub beads: Vec<DeviceBead>,
    /// Internal bonds over local indices `0..beads.len()`.
    pub bonds: Vec<[u32; 2]>,
    /// RESERVED for the future easy-connect feature; always empty in v1.
    #[serde(default)]
    pub ports: Vec<u32>,
}

/// A named bundle of devices, all of one chemistry.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Suite {
    pub name: String,
    pub chemistry: String,
    pub devices: Vec<Device>,
}

/// The whole persisted library: the live dock plus saved suites.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Library {
    pub version: u32,
    pub next_id: u32,
    pub dock: Vec<Device>,
    pub suites: Vec<Suite>,
}

impl Default for Library {
    fn default() -> Self {
        Library { version: LIBRARY_VERSION, next_id: 0, dock: Vec::new(), suites: Vec::new() }
    }
}

impl Library {
    pub fn to_json(&self) -> String {
        serde_json::to_string(self).expect("Library always serializes")
    }

    /// Parse a persisted library. Returns `Library::default()` for empty or
    /// unparseable input so a corrupt/legacy localStorage value never bricks
    /// the editor.
    pub fn load_or_default(s: &str) -> Library {
        serde_json::from_str(s).unwrap_or_default()
    }
}
```

- [ ] **Step 6: Run test to verify it passes**

Run: `cargo test --lib library::`
Expected: PASS (3 tests).

- [ ] **Step 7: Commit**

```bash
git add Cargo.toml src/lib.rs src/library.rs
git commit -m "feat(library): Device/Suite/Library model + JSON persistence"
```

---

### Task 2: Chemistry hash + device compatibility

**Files:**
- Modify: `src/library.rs`

- [ ] **Step 1: Write the failing test**

Add these tests inside the existing `#[cfg(test)] mod tests` block in `src/library.rs`:

```rust
    #[test]
    fn chemistry_hash_is_stable_within_build() {
        let chem = crate::editor::load_chemistry_by_name("wire").unwrap();
        assert_eq!(chemistry_hash(&chem), chemistry_hash(&chem));
    }

    #[test]
    fn device_compatibility_detects_missing_state() {
        let chem = crate::editor::load_chemistry_by_name("wire").unwrap();
        let good = Device {
            id: 0, name: "g".into(), chemistry: "wire".into(),
            chemistry_hash: chemistry_hash(&chem),
            beads: vec![DeviceBead { state: chem.states[0].clone(), pos: [0.0, 0.0] }],
            bonds: vec![], ports: vec![],
        };
        assert!(good.is_compatible_with(&chem));
        assert!(good.missing_states(&chem).is_empty());

        let bad = Device {
            id: 0, name: "b".into(), chemistry: "wire".into(), chemistry_hash: 0,
            beads: vec![DeviceBead { state: "no_such_state".into(), pos: [0.0, 0.0] }],
            bonds: vec![], ports: vec![],
        };
        assert!(!bad.is_compatible_with(&chem));
        assert_eq!(bad.missing_states(&chem), vec!["no_such_state".to_string()]);
    }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test --lib library::`
Expected: FAIL — `chemistry_hash`, `is_compatible_with`, `missing_states` not found.

- [ ] **Step 3: Write minimal implementation**

Add to `src/library.rs`, above the `#[cfg(test)]` block:

```rust
use std::hash::{Hash, Hasher};

/// Advisory, build-stable hash of a chemistry's identity (state names + action
/// table + colors). Used only to *flag* drift in the UI — it never decides
/// compatibility (that is `missing_states`). `DefaultHasher` is not stable
/// across Rust versions, so a value saved by one build may mismatch after a
/// redeploy; that is harmless because a hash-only mismatch is treated as
/// compatible.
pub fn chemistry_hash(chem: &crate::chemistry::Chemistry) -> u64 {
    let mut h = std::collections::hash_map::DefaultHasher::new();
    chem.states.hash(&mut h);
    chem.action_table_flat().hash(&mut h);
    for color in &chem.colors {
        for component in color {
            component.to_bits().hash(&mut h);
        }
    }
    h.finish()
}

impl Device {
    /// State names this device references that are absent from `chem`
    /// (sorted, deduped). Empty → every state still exists.
    pub fn missing_states(&self, chem: &crate::chemistry::Chemistry) -> Vec<String> {
        let mut missing: Vec<String> = self
            .beads
            .iter()
            .map(|b| b.state.clone())
            .filter(|s| chem.state_index(s).is_none())
            .collect();
        missing.sort_unstable();
        missing.dedup();
        missing
    }

    /// True iff every bead state exists in `chem`, so the device can be stamped.
    pub fn is_compatible_with(&self, chem: &crate::chemistry::Chemistry) -> bool {
        self.missing_states(chem).is_empty()
    }
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test --lib library::`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/library.rs
git commit -m "feat(library): chemistry hash + device compatibility check"
```

---

### Task 3: Dock & suite mutations

**Files:**
- Modify: `src/library.rs`

- [ ] **Step 1: Write the failing test**

Add inside `#[cfg(test)] mod tests` in `src/library.rs`:

```rust
    fn bare_device(chem: &str) -> Device {
        Device { id: 0, name: "d".into(), chemistry: chem.into(), chemistry_hash: 0,
                 beads: vec![], bonds: vec![], ports: vec![] }
    }

    #[test]
    fn add_to_dock_assigns_incrementing_ids() {
        let mut lib = Library::default();
        assert_eq!(lib.add_to_dock(bare_device("wire")), 0);
        assert_eq!(lib.add_to_dock(bare_device("wire")), 1);
        assert_eq!(lib.dock[0].id, 0);
        assert_eq!(lib.dock[1].id, 1);
    }

    #[test]
    fn rename_and_remove_device() {
        let mut lib = Library::default();
        let id = lib.add_to_dock(bare_device("wire"));
        lib.rename_device(id, "renamed".into());
        assert_eq!(lib.dock[0].name, "renamed");
        lib.remove_device(id);
        assert!(lib.dock.is_empty());
    }

    #[test]
    fn save_then_load_suite_replaces_only_current_chemistry_slice() {
        let mut lib = Library::default();
        lib.add_to_dock(bare_device("wire"));
        lib.add_to_dock(bare_device("grey"));
        lib.save_suite("s1".into(), "wire");
        // Simulate the user clearing the wire devices from the dock.
        lib.dock.retain(|d| d.chemistry == "grey");
        assert!(lib.load_suite("s1"));
        assert_eq!(lib.dock.iter().filter(|d| d.chemistry == "wire").count(), 1);
        assert_eq!(lib.dock.iter().filter(|d| d.chemistry == "grey").count(), 1);
    }

    #[test]
    fn save_suite_overwrites_same_name() {
        let mut lib = Library::default();
        lib.add_to_dock(bare_device("wire"));
        lib.save_suite("s".into(), "wire");
        lib.add_to_dock(bare_device("wire"));
        lib.save_suite("s".into(), "wire");
        assert_eq!(lib.suites.len(), 1);
        assert_eq!(lib.suites[0].devices.len(), 2);
    }

    #[test]
    fn load_unknown_suite_returns_false() {
        let mut lib = Library::default();
        assert!(!lib.load_suite("nope"));
    }

    #[test]
    fn import_suite_appends_and_overwrites_by_name() {
        let mut lib = Library::default();
        lib.import_suite(Suite { name: "s".into(), chemistry: "wire".into(), devices: vec![] });
        assert_eq!(lib.suites.len(), 1);
        lib.import_suite(Suite { name: "s".into(), chemistry: "wire".into(),
                                 devices: vec![bare_device("wire")] });
        assert_eq!(lib.suites.len(), 1);
        assert_eq!(lib.suites[0].devices.len(), 1);
    }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test --lib library::`
Expected: FAIL — `add_to_dock`, `rename_device`, `remove_device`, `save_suite`, `load_suite`, `import_suite` not found.

- [ ] **Step 3: Write minimal implementation**

Add to the `impl Library { … }` block in `src/library.rs` (alongside `to_json`/`load_or_default`):

```rust
    /// Add a device to the dock, assigning it the next library id. Returns the
    /// assigned id (overwrites any incoming `device.id`).
    pub fn add_to_dock(&mut self, mut device: Device) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        device.id = id;
        self.dock.push(device);
        id
    }

    pub fn rename_device(&mut self, id: u32, name: String) {
        if let Some(d) = self.dock.iter_mut().find(|d| d.id == id) {
            d.name = name;
        }
    }

    pub fn remove_device(&mut self, id: u32) {
        self.dock.retain(|d| d.id != id);
    }

    /// Snapshot the dock's devices for `chemistry` into a named suite,
    /// overwriting any existing suite of the same name. Device ids are cloned
    /// as-is (suite ids are not authoritative; `load_suite` reassigns).
    pub fn save_suite(&mut self, name: String, chemistry: &str) {
        let devices: Vec<Device> = self
            .dock
            .iter()
            .filter(|d| d.chemistry == chemistry)
            .cloned()
            .collect();
        let suite = Suite { name: name.clone(), chemistry: chemistry.to_string(), devices };
        match self.suites.iter_mut().find(|s| s.name == name) {
            Some(existing) => *existing = suite,
            None => self.suites.push(suite),
        }
    }

    /// Replace the dock's slice for the suite's chemistry with the suite's
    /// devices (each given a fresh id). Devices of *other* chemistries are left
    /// untouched. Returns false if no suite by that name exists.
    pub fn load_suite(&mut self, name: &str) -> bool {
        let Some(suite) = self.suites.iter().find(|s| s.name == name).cloned() else {
            return false;
        };
        self.dock.retain(|d| d.chemistry != suite.chemistry);
        for mut d in suite.devices {
            let id = self.next_id;
            self.next_id += 1;
            d.id = id;
            self.dock.push(d);
        }
        true
    }

    /// Add an imported suite, overwriting any existing suite of the same name.
    /// Does not touch the dock.
    pub fn import_suite(&mut self, suite: Suite) {
        match self.suites.iter_mut().find(|s| s.name == suite.name) {
            Some(existing) => *existing = suite,
            None => self.suites.push(suite),
        }
    }
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test --lib library::`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/library.rs
git commit -m "feat(library): dock + suite mutations (add/rename/remove/save/load/import)"
```

---

### Task 4: `Scene::extract_device` — capture selection → device

**Files:**
- Modify: `src/editor.rs`

- [ ] **Step 1: Write the failing test**

Add inside `#[cfg(test)] mod tests` in `src/editor.rs` (the `test_scene` helper there builds a wire scene):

```rust
    #[test]
    fn extract_device_recenters_and_keeps_internal_bonds() {
        let mut scene = test_scene(128.0);
        // 0—1—2 elbow at the chain step spacing.
        let a = scene.place(Vec2::new(10.0, 10.0));
        let b = scene.append_chain_bead(Vec2::new(10.667, 10.0), a);
        let c = scene.append_chain_bead(Vec2::new(10.667, 9.333), b);
        scene.selection.insert(a);
        scene.selection.insert(b);
        scene.selection.insert(c);

        let dev = scene.extract_device("elbow".into()).unwrap();
        assert_eq!(dev.beads.len(), 3);
        assert_eq!(dev.chemistry, "wire");
        assert_eq!(dev.bonds.len(), 2);
        assert!(dev.bonds.contains(&[0, 1]));
        assert!(dev.bonds.contains(&[1, 2]));
        // Recentred: mean position is the origin.
        let mx: f32 = dev.beads.iter().map(|b| b.pos[0]).sum::<f32>() / 3.0;
        let my: f32 = dev.beads.iter().map(|b| b.pos[1]).sum::<f32>() / 3.0;
        assert!(mx.abs() < 1e-4 && my.abs() < 1e-4, "centroid not at origin: {mx},{my}");
    }

    #[test]
    fn extract_device_drops_bonds_to_unselected() {
        let mut scene = test_scene(128.0);
        let a = scene.place(Vec2::new(5.0, 5.0));
        let _b = scene.append_chain_bead(Vec2::new(5.667, 5.0), a); // bonded a—b
        scene.selection.insert(a); // select only a
        let dev = scene.extract_device("x".into()).unwrap();
        assert_eq!(dev.beads.len(), 1);
        assert!(dev.bonds.is_empty());
    }

    #[test]
    fn extract_device_empty_selection_is_none() {
        let scene = test_scene(128.0);
        assert!(scene.extract_device("x".into()).is_none());
    }

    #[test]
    fn extract_device_centroid_correct_across_seam() {
        let mut scene = test_scene(128.0);
        // Two beads straddling the seam: 127.5 and 0.5 (min-image distance 1.0).
        let a = scene.place(Vec2::new(127.5, 10.0));
        let b = scene.place(Vec2::new(0.5, 10.0));
        scene.selection.insert(a);
        scene.selection.insert(b);
        let dev = scene.extract_device("pair".into()).unwrap();
        // Recentred pair sits at ±0.5 on x, not ±63.5 (which a naive mean gives).
        let xs: Vec<f32> = dev.beads.iter().map(|d| d.pos[0]).collect();
        for x in &xs {
            assert!(x.abs() < 0.51, "seam-straddling bead not recentred: {x}");
        }
    }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test --lib editor::tests::extract_device`
Expected: FAIL — `extract_device` not found on `Scene`.

- [ ] **Step 3: Write minimal implementation**

Add this method to `impl Scene` in `src/editor.rs`:

```rust
    /// Capture the current selection as a reusable `Device` (rest shape only).
    /// Positions are recentred on the selection's torus-aware centroid and
    /// velocities dropped. Only bonds with both endpoints selected are kept,
    /// remapped to local indices `0..n`. The returned device has `id = 0`; the
    /// library assigns a real id on `add_to_dock`. Returns `None` if the
    /// selection is empty.
    pub fn extract_device(&self, name: String) -> Option<crate::library::Device> {
        if self.selection.is_empty() {
            return None;
        }
        // Stable local order: selected global indices, ascending.
        let mut sel: Vec<u32> = self.selection.iter().copied().collect();
        sel.sort_unstable();

        // Torus-aware centroid: express each selected position relative to the
        // first via min-image, average those offsets.
        let anchor = Vec2::from(self.beads[sel[0] as usize].pos);
        let mut sum = Vec2::ZERO;
        let offsets: Vec<Vec2> = sel
            .iter()
            .map(|&i| {
                let off = crate::grid::min_image(
                    anchor,
                    Vec2::from(self.beads[i as usize].pos),
                    self.world_size,
                );
                sum += off;
                off
            })
            .collect();
        let centroid_off = sum / sel.len() as f32;

        // old global index -> new local index
        let mut remap = std::collections::HashMap::new();
        for (local, &g) in sel.iter().enumerate() {
            remap.insert(g, local as u32);
        }

        let beads = sel
            .iter()
            .enumerate()
            .map(|(k, &g)| {
                let rel = offsets[k] - centroid_off;
                crate::library::DeviceBead {
                    state: self.beads[g as usize].state.clone(),
                    pos: [rel.x, rel.y],
                }
            })
            .collect();

        let mut bonds: Vec<[u32; 2]> = self
            .bonds
            .iter()
            .filter_map(|b| match (remap.get(&b.lo()), remap.get(&b.hi())) {
                (Some(&la), Some(&lb)) => Some(BondPair::new(la, lb).as_array()),
                _ => None,
            })
            .collect();
        bonds.sort_unstable();

        Some(crate::library::Device {
            id: 0,
            name,
            chemistry: self.chemistry_name.clone(),
            chemistry_hash: crate::library::chemistry_hash(&self.chemistry),
            beads,
            bonds,
            ports: Vec::new(),
        })
    }
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test --lib editor::tests::extract_device`
Expected: PASS (4 tests).

- [ ] **Step 5: Commit**

```bash
git add src/editor.rs
git commit -m "feat(editor): Scene::extract_device (selection -> device, torus-aware)"
```

---

### Task 5: `Scene::instantiate_device` — stamp device → scene

**Files:**
- Modify: `src/editor.rs`

- [ ] **Step 1: Write the failing test**

Add inside `#[cfg(test)] mod tests` in `src/editor.rs`:

```rust
    fn two_bead_device() -> crate::library::Device {
        crate::library::Device {
            id: 1, name: "pair".into(), chemistry: "wire".into(), chemistry_hash: 0,
            beads: vec![
                crate::library::DeviceBead { state: "off".into(), pos: [-0.3, 0.0] },
                crate::library::DeviceBead { state: "off".into(), pos: [ 0.3, 0.0] },
            ],
            bonds: vec![[0, 1]],
            ports: vec![],
        }
    }

    #[test]
    fn instantiate_appends_isolated_with_internal_bond() {
        let mut scene = test_scene(128.0);
        // A pre-existing lone bead right at the drop point — must NOT bond.
        scene.place(Vec2::new(20.0, 20.0));
        let before = scene.beads.len();
        let base = scene.instantiate_device(&two_bead_device(), Vec2::new(20.0, 20.0), 0.0);
        assert_eq!(scene.beads.len(), before + 2);
        assert_eq!(base, before as u32);
        // The device's own internal bond is present...
        assert!(scene.bonds.contains(&BondPair::new(base, base + 1)));
        // ...and no accidental bond to the pre-existing bead.
        assert_eq!(scene.bonds.len(), 1);
        // Selection becomes exactly the two new beads.
        assert_eq!(scene.selection.len(), 2);
        assert!(scene.selection.contains(&base));
        assert!(scene.selection.contains(&(base + 1)));
    }

    #[test]
    fn instantiate_rotates_90_degrees() {
        let mut scene = test_scene(128.0);
        let dev = crate::library::Device {
            id: 1, name: "p".into(), chemistry: "wire".into(), chemistry_hash: 0,
            beads: vec![crate::library::DeviceBead { state: "off".into(), pos: [1.0, 0.0] }],
            bonds: vec![], ports: vec![],
        };
        let base = scene.instantiate_device(&dev, Vec2::new(10.0, 10.0), std::f32::consts::FRAC_PI_2);
        let p = Vec2::from(scene.beads[base as usize].pos);
        // (1,0) rotated +90° -> (0,1); + drop (10,10) -> (10,11).
        assert!((p.x - 10.0).abs() < 1e-4, "x={}", p.x);
        assert!((p.y - 11.0).abs() < 1e-4, "y={}", p.y);
    }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test --lib editor::tests::instantiate`
Expected: FAIL — `instantiate_device` not found on `Scene`.

- [ ] **Step 3: Write minimal implementation**

Add this method to `impl Scene` in `src/editor.rs`:

```rust
    /// Stamp `device` into the scene at world position `drop`, rotated by
    /// `angle` radians about the device origin. Appends fresh beads (vel None)
    /// and the device's internal bonds (remapped to the new indices). Does NOT
    /// bond to pre-existing beads (isolated drop). Replaces the selection with
    /// the newly-placed indices. Returns the first new bead's index.
    pub fn instantiate_device(
        &mut self,
        device: &crate::library::Device,
        drop: Vec2,
        angle: f32,
    ) -> u32 {
        let base = self.beads.len() as u32;
        let (s, c) = angle.sin_cos();
        for db in &device.beads {
            let (x, y) = (db.pos[0], db.pos[1]);
            let rx = x * c - y * s;
            let ry = x * s + y * c;
            let world = wrap_vec(Vec2::new(rx + drop.x, ry + drop.y), self.world_size);
            self.beads.push(BeadSpec {
                state: db.state.clone(),
                pos: [world.x, world.y],
                vel: None,
            });
        }
        for b in &device.bonds {
            self.bonds.insert(BondPair::new(base + b[0], base + b[1]));
        }
        self.selection.clear();
        for i in 0..device.beads.len() as u32 {
            self.selection.insert(base + i);
        }
        base
    }
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test --lib editor::tests::instantiate`
Expected: PASS (2 tests).

- [ ] **Step 5: Commit**

```bash
git add src/editor.rs
git commit -m "feat(editor): Scene::instantiate_device (stamp device, rotated, isolated)"
```

---

### Task 6: `Scene::rotate_selection` — rotate selection about centroid

**Files:**
- Modify: `src/editor.rs`

- [ ] **Step 1: Write the failing test**

Add inside `#[cfg(test)] mod tests` in `src/editor.rs`:

```rust
    #[test]
    fn rotate_selection_360_is_identity() {
        let mut scene = test_scene(128.0);
        let a = scene.place(Vec2::new(10.0, 10.0));
        let b = scene.place(Vec2::new(14.0, 10.0)); // 4 apart: no bond
        scene.selection.insert(a);
        scene.selection.insert(b);
        let (pa, pb) = (scene.beads[a as usize].pos, scene.beads[b as usize].pos);
        scene.rotate_selection(std::f32::consts::TAU);
        assert!((scene.beads[a as usize].pos[0] - pa[0]).abs() < 1e-3);
        assert!((scene.beads[a as usize].pos[1] - pa[1]).abs() < 1e-3);
        assert!((scene.beads[b as usize].pos[0] - pb[0]).abs() < 1e-3);
        assert!((scene.beads[b as usize].pos[1] - pb[1]).abs() < 1e-3);
    }

    #[test]
    fn rotate_selection_90_about_centroid() {
        let mut scene = test_scene(128.0);
        let a = scene.place(Vec2::new(10.0, 10.0));
        let b = scene.place(Vec2::new(14.0, 10.0));
        scene.selection.insert(a);
        scene.selection.insert(b);
        scene.rotate_selection(std::f32::consts::FRAC_PI_2);
        let pa = Vec2::from(scene.beads[a as usize].pos);
        let pb = Vec2::from(scene.beads[b as usize].pos);
        // Centroid (12,10); a rel (-2,0) -> (0,-2) -> (12,8); b -> (12,12).
        assert!((pa.x - 12.0).abs() < 1e-3 && (pa.y - 8.0).abs() < 1e-3, "{pa:?}");
        assert!((pb.x - 12.0).abs() < 1e-3 && (pb.y - 12.0).abs() < 1e-3, "{pb:?}");
    }

    #[test]
    fn rotate_selection_empty_is_noop() {
        let mut scene = test_scene(128.0);
        let a = scene.place(Vec2::new(10.0, 10.0));
        let pa = scene.beads[a as usize].pos;
        scene.rotate_selection(std::f32::consts::FRAC_PI_2); // nothing selected
        assert_eq!(scene.beads[a as usize].pos, pa);
    }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test --lib editor::tests::rotate_selection`
Expected: FAIL — `rotate_selection` not found on `Scene`.

- [ ] **Step 3: Write minimal implementation**

Add this method to `impl Scene` in `src/editor.rs`:

```rust
    /// Rotate the selected beads by `angle` radians about their torus-aware
    /// centroid, wrapping each result into the world. Bonds are unaffected
    /// (indices don't move). No-op on an empty selection. Snapping to fixed
    /// increments is the caller's responsibility.
    pub fn rotate_selection(&mut self, angle: f32) {
        if self.selection.is_empty() {
            return;
        }
        let mut sel: Vec<u32> = self.selection.iter().copied().collect();
        sel.sort_unstable();
        let anchor = Vec2::from(self.beads[sel[0] as usize].pos);
        let mut sum = Vec2::ZERO;
        let offsets: Vec<Vec2> = sel
            .iter()
            .map(|&i| {
                let off = crate::grid::min_image(
                    anchor,
                    Vec2::from(self.beads[i as usize].pos),
                    self.world_size,
                );
                sum += off;
                off
            })
            .collect();
        let centroid = anchor + sum / sel.len() as f32;
        let (s, c) = angle.sin_cos();
        for (k, &i) in sel.iter().enumerate() {
            let rel = (anchor + offsets[k]) - centroid;
            let rx = rel.x * c - rel.y * s;
            let ry = rel.x * s + rel.y * c;
            let np = wrap_vec(Vec2::new(centroid.x + rx, centroid.y + ry), self.world_size);
            self.beads[i as usize].pos = [np.x, np.y];
        }
    }
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test --lib editor::tests::rotate_selection`
Expected: PASS (3 tests).

- [ ] **Step 5: Commit**

```bash
git add src/editor.rs
git commit -m "feat(editor): Scene::rotate_selection about torus-aware centroid"
```

---

### Task 7: Full-suite green + round-trip integration test

**Files:**
- Modify: `src/library.rs`

- [ ] **Step 1: Write the failing test**

Add inside `#[cfg(test)] mod tests` in `src/library.rs` — an end-to-end test that captures from a scene, stores in the library, serializes, reloads, and stamps back:

```rust
    #[test]
    fn capture_persist_reload_stamp_round_trip() {
        use crate::editor::{load_chemistry_by_name, Scene};
        use glam::Vec2;

        // Build a 3-bead elbow scene and select all of it.
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene {
            chemistry: chem,
            chemistry_name: "wire".into(),
            world_size: 128.0,
            beads: Vec::new(),
            seed: 0,
            next_state_idx: 0,
            bonds: std::collections::HashSet::new(),
            selection: std::collections::HashSet::new(),
            tool: crate::editor::Tool::Place,
        };
        let a = scene.place(Vec2::new(10.0, 10.0));
        let b = scene.append_chain_bead(Vec2::new(10.667, 10.0), a);
        let c = scene.append_chain_bead(Vec2::new(10.667, 9.333), b);
        scene.selection.insert(a);
        scene.selection.insert(b);
        scene.selection.insert(c);

        // Capture -> dock -> serialize -> reload.
        let dev = scene.extract_device("elbow".into()).unwrap();
        let mut lib = Library::default();
        lib.add_to_dock(dev);
        let lib = Library::load_or_default(&lib.to_json());
        let reloaded = &lib.dock[0];
        assert_eq!(reloaded.beads.len(), 3);
        assert_eq!(reloaded.bonds.len(), 2);

        // Stamp it into a fresh empty scene.
        let mut target = Scene {
            chemistry: load_chemistry_by_name("wire").unwrap(),
            chemistry_name: "wire".into(),
            world_size: 128.0,
            beads: Vec::new(),
            seed: 0,
            next_state_idx: 0,
            bonds: std::collections::HashSet::new(),
            selection: std::collections::HashSet::new(),
            tool: crate::editor::Tool::Place,
        };
        target.instantiate_device(reloaded, Vec2::new(50.0, 50.0), 0.0);
        assert_eq!(target.beads.len(), 3);
        assert_eq!(target.bonds.len(), 2);
        assert_eq!(target.selection.len(), 3);
    }
```

- [ ] **Step 2: Run test to verify it fails or passes**

Run: `cargo test --lib`
Expected: This integration test should PASS immediately (all the pieces exist from Tasks 1–6). If it fails, it has caught a real integration bug — fix the offending method, not the test. This task's value is the cross-module assertion.

- [ ] **Step 3: Run the full library + editor suite**

Run: `cargo test --lib library:: editor::`
Expected: PASS — all library and editor tests green.

- [ ] **Step 4: Confirm a clean full build**

Run: `cargo build`
Expected: builds with no errors (warnings about the as-yet-unused `Library` methods on native are acceptable — they are wired up in plan 2).

- [ ] **Step 5: Commit**

```bash
git add src/library.rs
git commit -m "test(library): end-to-end capture->persist->reload->stamp round-trip"
```

---

## Self-Review

**Spec coverage (core slice):**
- §2 data model — Task 1 (`Device`/`DeviceBead`/`Suite`/`Library`, `ports` reserved, `version`). ✅
- §3 capture — Task 4 (`extract_device`, torus-aware centroid, internal-bonds-only, rest shape, `id=0`). ✅
- §4 stamp — Task 5 (`instantiate_device`, rotation, isolated, selection = new beads). ✅
- §5 rotation primitive — Task 6 (`rotate_selection`, snapping left to caller). ✅
- §7 compatibility — Task 2 (`missing_states`/`is_compatible_with`; `chemistry_hash` advisory). ✅
- §9 serialization — Task 1 (`to_json`/`load_or_default` with corrupt-input fallback). ✅
- Dock/suite mutations underpinning §6 — Task 3 (`add_to_dock`/`rename`/`remove`/`save_suite`/`load_suite`/`import_suite`). ✅
- Deferred to plan 2 (not in this plan, by design): §1 bridge architecture, §6 UI, §8 bridge globals, §9 localStorage *I/O*, browser smoke, ghost overlay, placement/rotation input wiring.

**Placeholder scan:** No TBD/TODO; every code step shows complete code. ✅

**Type consistency:** `Device`/`DeviceBead`/`Suite`/`Library` field names and method signatures (`extract_device(&self, String) -> Option<Device>`, `instantiate_device(&mut self, &Device, Vec2, f32) -> u32`, `rotate_selection(&mut self, f32)`, `add_to_dock(Device) -> u32`, `save_suite(String, &str)`, `load_suite(&str) -> bool`, `chemistry_hash(&Chemistry) -> u64`) are used identically across tasks and tests. `wrap_vec` and `BondPair`/`min_image` are pre-existing in their modules. ✅

**Note on `wrap_vec`:** it is a private free fn in `src/editor.rs`; `extract_device`/`instantiate_device`/`rotate_selection` live in the same module so they can call it directly.
