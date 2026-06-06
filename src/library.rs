//! In-app device library: reusable sub-assemblies (devices), a working tray
//! (the dock), and saved bundles (suites). Pure data model + logic; the web
//! bridge / localStorage / UI live elsewhere. See
//! docs/superpowers/specs/2026-06-06-editor-device-library-design.md.

use std::hash::{Hash, Hasher};

use serde::{Deserialize, Serialize};

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
    #[serde(default)]
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

    /// Add a device to the dock, assigning it the next library id. Returns the
    /// assigned id (overwrites any incoming `device.id`).
    pub fn add_to_dock(&mut self, mut device: Device) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        device.id = id;
        self.dock.push(device);
        id
    }

    /// Rename the dock device with the given id. No-op if the id is absent.
    pub fn rename_device(&mut self, id: u32, name: String) {
        if let Some(d) = self.dock.iter_mut().find(|d| d.id == id) {
            d.name = name;
        }
    }

    /// Remove the dock device with the given id. No-op if the id is absent.
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
}

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
        lib.add_to_dock(bare_device("wire")); // id 0
        lib.add_to_dock(bare_device("grey")); // id 1
        lib.save_suite("s1".into(), "wire");
        // Simulate the user clearing the wire devices from the dock.
        lib.dock.retain(|d| d.chemistry == "grey");
        let grey_id_before = lib.dock.iter().find(|d| d.chemistry == "grey").unwrap().id;

        assert!(lib.load_suite("s1"));

        assert_eq!(lib.dock.iter().filter(|d| d.chemistry == "wire").count(), 1);
        assert_eq!(lib.dock.iter().filter(|d| d.chemistry == "grey").count(), 1);
        // The untouched grey device keeps its id.
        let grey_id_after = lib.dock.iter().find(|d| d.chemistry == "grey").unwrap().id;
        assert_eq!(grey_id_before, grey_id_after);
        // The reloaded wire device gets a fresh id, not its original 0.
        let wire_id_after = lib.dock.iter().find(|d| d.chemistry == "wire").unwrap().id;
        assert!(wire_id_after > 1, "reloaded device should get a fresh id, got {wire_id_after}");
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
}
