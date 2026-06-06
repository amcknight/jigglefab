//! In-app device library: reusable sub-assemblies (devices), a working tray
//! (the dock), and saved bundles (suites). Pure data model + logic; the web
//! bridge / localStorage / UI live elsewhere. See
//! docs/superpowers/specs/2026-06-06-editor-device-library-design.md.

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
}
