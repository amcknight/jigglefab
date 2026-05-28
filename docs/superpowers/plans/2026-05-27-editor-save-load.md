# Editor Save / Load Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let the user download the current Edit-mode scene as a `fabs/*.toml` file and load such a file back, replacing the scene.

**Architecture:** Add `Serialize` to `Fab`/`Meta`/`BeadSpec`, then a pure `Scene::to_toml` (serialize) and `Scene::from_toml` (parse + strict validation against the static chemistry registry). The web layer follows the existing `__jigglefab*` bridge: a `save` command serializes the live scene and triggers a browser download via `web_sys`; `__jigglefabLoadToml(text)` validates+builds a `Scene` synchronously and queues a `load_scene` command for the app loop to install.

**Tech Stack:** Rust + serde + the `toml` crate (0.8), wasm-bindgen / web-sys / js-sys bridge, HTML/JS toolbar, Playwright (`scripts/verify-web.py`).

**Spec:** [docs/superpowers/specs/2026-05-27-editor-save-load-design.md](../specs/2026-05-27-editor-save-load-design.md)

---

## Pre-flight

- [ ] **Step 0: Confirm baseline builds**

Run:
```bash
cargo test --lib
cargo check --target wasm32-unknown-unknown
```
Expected: both succeed. If `wasm32-unknown-unknown` isn't installed: `rustup target add wasm32-unknown-unknown`.

---

## Task 1: `Serialize` on `Fab` / `Meta` / `BeadSpec`

**Files:**
- Modify: `src/fab.rs`

`Fab`, `Meta`, and `BeadSpec` currently derive only `Deserialize`. Add `Serialize` so a `Fab` can be written back to TOML, and skip `None` optionals so output stays clean.

- [ ] **Step 1: Write the failing test**

Append to `src/fab.rs` inside `mod tests`:
```rust
#[test]
fn fab_serializes_and_reparses() {
    let toml_text = r#"
[meta]
name = "two"
chemistry = "grey"
seed = 7
world_size = 30.0
bonds = [[0, 1]]

[[bead]]
state = "grey"
pos = [5.0, 5.0]

[[bead]]
state = "grey"
pos = [5.5, 5.0]
"#;
    let fab = parse_fab(toml_text).unwrap();
    let out = toml::to_string_pretty(&fab).unwrap();
    let reparsed = parse_fab(&out).unwrap();
    assert_eq!(reparsed.meta.name, "two");
    assert_eq!(reparsed.meta.chemistry, "grey");
    assert_eq!(reparsed.meta.seed, 7);
    assert_eq!(reparsed.meta.world_size, Some(30.0));
    assert_eq!(reparsed.bonds(), Some(&vec![[0u32, 1u32]]));
    assert_eq!(reparsed.beads.len(), 2);
    assert_eq!(reparsed.beads[0].pos, [5.0, 5.0]);
    // vel is None on every bead, so the skip attribute must omit it entirely.
    assert!(!out.contains("vel"), "None vel must be skipped, got:\n{out}");
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test --lib fab::tests::fab_serializes_and_reparses`
Expected: FAIL — `toml::to_string_pretty` requires `Fab: Serialize`, which isn't derived yet (compile error "the trait `Serialize` is not implemented for `Fab`").

- [ ] **Step 3: Add the derives + skip attributes**

Edit `src/fab.rs`. Change the import line:
```rust
use serde::{Deserialize, Serialize};
```

Add `Serialize` to each derive and skip-if-none to the optionals:
```rust
#[derive(Debug, Serialize, Deserialize)]
pub struct Fab {
    pub meta: Meta,
    #[serde(rename = "bead")]
    pub beads: Vec<BeadSpec>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Meta {
    pub name: String,
    pub chemistry: String,
    pub seed: u64,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub world_size: Option<f32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub bonds: Option<Vec<[u32; 2]>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BeadSpec {
    pub state: String,
    pub pos: [f32; 2],
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub vel: Option<[f32; 2]>,
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test --lib fab::tests`
Expected: PASS (all fab tests, including the new one).

- [ ] **Step 5: Commit**

```bash
git add src/fab.rs
git commit -m "feat(fab): derive Serialize + skip None optionals for save"
```

---

## Task 2: `Scene::to_toml`

**Files:**
- Modify: `src/editor.rs`

Mirror `Scene::to_sim` (`src/editor.rs:160`), but force velocities off and always emit the explicit bond list (even empty) so a reload never falls back to distance-derivation.

- [ ] **Step 1: Write the failing tests**

Append to `src/editor.rs` inside `mod tests`:
```rust
#[test]
fn to_toml_emits_sorted_bonds_no_vel() {
    let fab = small_wire_fab();
    let chem = load_chemistry_by_name("wire").unwrap();
    let mut scene = Scene::from_fab(&fab, chem, "wire".into());
    scene.beads.clear();
    scene.bonds.clear();
    let a = scene.place(Vec2::new(5.0, 5.0));
    let b = scene.append_chain_bead(Vec2::new(5.667, 5.0), a);
    scene.append_chain_bead(Vec2::new(6.334, 5.0), b);
    let out = scene.to_toml();
    let fab2 = crate::fab::parse_fab(&out).unwrap();
    assert_eq!(fab2.meta.chemistry, "wire");
    assert_eq!(fab2.beads.len(), 3);
    assert_eq!(fab2.bonds(), Some(&vec![[0u32, 1u32], [1u32, 2u32]]));
    assert!(!out.contains("vel"), "to_toml must not emit velocities");
}

#[test]
fn to_toml_emits_empty_bonds_list_when_no_bonds() {
    // A bead with no bonds must still emit `bonds = []` so a reload uses the
    // explicit empty set instead of distance-deriving from positions.
    let fab = small_wire_fab();
    let chem = load_chemistry_by_name("wire").unwrap();
    let mut scene = Scene::from_fab(&fab, chem, "wire".into());
    scene.beads.clear();
    scene.bonds.clear();
    scene.place(Vec2::new(5.0, 5.0));
    let out = scene.to_toml();
    let fab2 = crate::fab::parse_fab(&out).unwrap();
    assert_eq!(fab2.bonds(), Some(&vec![]));
}
```

- [ ] **Step 2: Run to confirm failure**

Run: `cargo test --lib editor::tests::to_toml_emits_sorted_bonds_no_vel`
Expected: FAIL — `to_toml` doesn't exist (no method named `to_toml`).

- [ ] **Step 3: Implement `to_toml`**

Add to `impl Scene` in `src/editor.rs` (next to `to_sim`):
```rust
/// Serialize this scene to `fabs/*.toml` format: positions, states, and the
/// explicit bond list (sorted ascending). Never emits velocities. The bond
/// list is ALWAYS present — even when empty — because `Scene::from_fab`
/// distance-derives bonds when the field is absent, which would wrongly bond
/// nearby-but-unbonded beads (e.g. after a Move) on reload.
pub fn to_toml(&self) -> String {
    let mut bonds_vec: Vec<[u32; 2]> = self.bonds.iter().map(|&(a, b)| [a, b]).collect();
    bonds_vec.sort_unstable();
    let fab = Fab {
        meta: crate::fab::Meta {
            name: "editor scene".to_string(),
            chemistry: self.chemistry_name.clone(),
            seed: self.seed,
            world_size: Some(self.world_size),
            bonds: Some(bonds_vec),
        },
        beads: self.beads.iter().map(|b| BeadSpec {
            state: b.state.clone(),
            pos: b.pos,
            vel: None,
        }).collect(),
    };
    toml::to_string_pretty(&fab).expect("Fab always serializes")
}
```

- [ ] **Step 4: Run tests**

Run: `cargo test --lib editor::tests`
Expected: PASS (all editor tests, including the two new ones).

- [ ] **Step 5: Commit**

```bash
git add src/editor.rs
git commit -m "feat(editor): Scene::to_toml — serialize scene to fab TOML"
```

---

## Task 3: `LoadError` + `Scene::from_toml`

**Files:**
- Modify: `src/editor.rs`

Strict, atomic parse: malformed TOML, an unknown chemistry, or a bead state not in that chemistry's palette each abort before any `Scene` is built.

- [ ] **Step 1: Write the failing tests**

Append to `src/editor.rs` inside `mod tests`:
```rust
#[test]
fn from_toml_round_trips_scene() {
    let fab = small_wire_fab();
    let chem = load_chemistry_by_name("wire").unwrap();
    let mut scene = Scene::from_fab(&fab, chem, "wire".into());
    scene.beads.clear();
    scene.bonds.clear();
    let a = scene.place(Vec2::new(5.0, 5.0));
    let b = scene.append_chain_bead(Vec2::new(5.667, 5.0), a);
    scene.append_chain_bead(Vec2::new(6.334, 5.0), b);
    let out = scene.to_toml();
    let loaded = Scene::from_toml(&out).expect("valid scene loads");
    assert_eq!(loaded.chemistry_name, "wire");
    assert_eq!(loaded.beads.len(), scene.beads.len());
    assert_eq!(loaded.bonds, scene.bonds);
    assert_eq!(loaded.beads[1].pos, scene.beads[1].pos);
}

#[test]
fn from_toml_rejects_unknown_chemistry() {
    let toml_text = r#"
[meta]
name = "x"
chemistry = "nonexistent"
seed = 1
bonds = []

[[bead]]
state = "off"
pos = [5.0, 5.0]
"#;
    match Scene::from_toml(toml_text) {
        Err(LoadError::UnknownChemistry(n)) => assert_eq!(n, "nonexistent"),
        other => panic!("expected UnknownChemistry, got {other:?}"),
    }
}

#[test]
fn from_toml_rejects_unknown_state() {
    let toml_text = r#"
[meta]
name = "x"
chemistry = "wire"
seed = 1
bonds = []

[[bead]]
state = "off"
pos = [5.0, 5.0]

[[bead]]
state = "not_a_state"
pos = [5.5, 5.0]
"#;
    match Scene::from_toml(toml_text) {
        Err(LoadError::UnknownState { bead, state }) => {
            assert_eq!(bead, 1);
            assert_eq!(state, "not_a_state");
        }
        other => panic!("expected UnknownState, got {other:?}"),
    }
}

#[test]
fn from_toml_rejects_malformed() {
    assert!(matches!(Scene::from_toml("not valid toml {{{"), Err(LoadError::Parse(_))));
}

#[test]
fn from_toml_loads_legacy_preset() {
    // A shipped preset (no `bonds` field) still loads: from_fab derives bonds.
    let text = include_str!("../fabs/wire-30.toml");
    let loaded = Scene::from_toml(text).expect("legacy preset loads");
    assert_eq!(loaded.chemistry_name, "wire");
    assert_eq!(loaded.beads.len(), 30);
    assert_eq!(loaded.bonds.len(), 29); // single chain of 30 -> 29 derived bonds
}

#[test]
fn empty_scene_round_trips() {
    let fab = small_wire_fab();
    let chem = load_chemistry_by_name("wire").unwrap();
    let mut scene = Scene::from_fab(&fab, chem, "wire".into());
    scene.beads.clear();
    scene.bonds.clear();
    let out = scene.to_toml();
    let loaded = Scene::from_toml(&out).expect("empty scene loads");
    assert!(loaded.beads.is_empty());
    assert!(loaded.bonds.is_empty());
}
```

- [ ] **Step 2: Run to confirm failure**

Run: `cargo test --lib editor::tests::from_toml_round_trips_scene`
Expected: FAIL — `LoadError` and `Scene::from_toml` don't exist (compile errors).

- [ ] **Step 3: Implement `LoadError` + `from_toml`**

Add to `src/editor.rs`. Put `LoadError` near the top (after the `Tool` enum), and the method inside `impl Scene` (next to `to_toml`):
```rust
/// Why a `Scene::from_toml` load was rejected. Load is atomic — on any of
/// these the caller's existing scene must be left untouched.
#[derive(Debug)]
pub enum LoadError {
    /// TOML did not parse as a `Fab`.
    Parse(String),
    /// `meta.chemistry` is not in the embedded chemistry registry.
    UnknownChemistry(String),
    /// A bead's `state` is not in the chosen chemistry's palette.
    UnknownState { bead: usize, state: String },
}

impl std::fmt::Display for LoadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoadError::Parse(e) => write!(f, "could not parse TOML: {e}"),
            LoadError::UnknownChemistry(name) => write!(f, "unknown chemistry: {name}"),
            LoadError::UnknownState { bead, state } => {
                write!(f, "bead {bead} uses a state not in this chemistry: \"{state}\"")
            }
        }
    }
}
```

Inside `impl Scene`:
```rust
/// Parse a saved scene from `fabs/*.toml` text. Strict: malformed TOML, an
/// unknown chemistry, or any bead whose state is not in that chemistry's
/// palette aborts with a `LoadError` and builds nothing.
pub fn from_toml(text: &str) -> Result<Scene, LoadError> {
    let fab = crate::fab::parse_fab(text).map_err(|e| LoadError::Parse(e.to_string()))?;
    let chem = load_chemistry_by_name(&fab.meta.chemistry)
        .map_err(|_| LoadError::UnknownChemistry(fab.meta.chemistry.clone()))?;
    for (i, bead) in fab.beads.iter().enumerate() {
        if chem.state_index(&bead.state).is_none() {
            return Err(LoadError::UnknownState { bead: i, state: bead.state.clone() });
        }
    }
    let name = fab.meta.chemistry.clone();
    Ok(Scene::from_fab(&fab, chem, name))
}
```

- [ ] **Step 4: Run tests**

Run: `cargo test --lib editor::tests`
Expected: PASS (all editor tests, including the six new ones).

- [ ] **Step 5: Commit**

```bash
git add src/editor.rs
git commit -m "feat(editor): Scene::from_toml + LoadError — strict atomic load"
```

---

## Task 4: Bridge command fields (`save`, `load_scene`)

**Files:**
- Modify: `src/app.rs:21-29`

Extend the wasm-only `PendingCommands` struct with the two new commands.

- [ ] **Step 1: Add the fields**

In `src/app.rs`, change `PendingCommands` (currently lines 21-29) to:
```rust
    /// Pending commands from the JS toolbar, drained by the App each frame.
    #[derive(Default)]
    pub struct PendingCommands {
        pub set_mode: Option<crate::editor::Mode>,
        pub set_edit_state: Option<u32>,
        pub set_chemistry: Option<String>,
        pub set_tool: Option<crate::editor::Tool>,
        pub clear: bool,
        pub revert: bool,
        pub save: bool,
        pub load_scene: Option<crate::editor::Scene>,
    }
```
(`Option<Scene>` defaults to `None` without `Scene: Default`, so `#[derive(Default)]` still holds.)

- [ ] **Step 2: Verify it builds**

Run: `cargo check --target wasm32-unknown-unknown`
Expected: success.

- [ ] **Step 3: Commit**

```bash
git add src/app.rs
git commit -m "feat(bridge): save + load_scene pending commands"
```

---

## Task 5: web-sys download helper + features

**Files:**
- Modify: `Cargo.toml:53`, `src/app.rs`

Saving triggers a browser download from Rust. That needs a few more `web-sys` interface features and a small helper.

- [ ] **Step 1: Add web-sys features**

In `Cargo.toml`, replace the `web-sys` line (line 53) with:
```toml
web-sys = { version = "0.3", features = ["Document", "Element", "HtmlCanvasElement", "HtmlElement", "HtmlAnchorElement", "Blob", "Url", "Window", "Location"] }
```

- [ ] **Step 2: Add the download helper + timestamp helper**

In `src/app.rs`, add these two wasm-only free functions near the other `#[cfg(target_arch = "wasm32")]` helpers (e.g. just below the `expose_to_window!` macro definition):
```rust
/// Trigger a browser "save file" download of `contents` under `filename`.
/// Builds an object URL from a Blob and clicks a transient <a download>.
#[cfg(target_arch = "wasm32")]
fn trigger_download(filename: &str, contents: &str) {
    use wasm_bindgen::JsCast;
    let parts = js_sys::Array::new();
    parts.push(&wasm_bindgen::JsValue::from_str(contents));
    let blob = match web_sys::Blob::new_with_str_sequence(&parts) {
        Ok(b) => b,
        Err(e) => { log::warn!("save: blob create failed: {e:?}"); return; }
    };
    let url = match web_sys::Url::create_object_url_with_blob(&blob) {
        Ok(u) => u,
        Err(e) => { log::warn!("save: object URL failed: {e:?}"); return; }
    };
    if let Some(document) = web_sys::window().and_then(|w| w.document()) {
        if let Ok(el) = document.create_element("a") {
            let anchor: web_sys::HtmlAnchorElement = el.unchecked_into();
            anchor.set_href(&url);
            anchor.set_download(filename);
            anchor.click();
        }
    }
    let _ = web_sys::Url::revoke_object_url(&url);
}

/// Local-time `YYYYMMDD-HHMMSS` stamp for save filenames.
#[cfg(target_arch = "wasm32")]
fn save_timestamp() -> String {
    let d = js_sys::Date::new_0();
    format!(
        "{:04}{:02}{:02}-{:02}{:02}{:02}",
        d.get_full_year(),
        d.get_month() + 1,
        d.get_date(),
        d.get_hours(),
        d.get_minutes(),
        d.get_seconds(),
    )
}
```

Note: `js_sys::Date` component getters return `u32` in js-sys 0.3, so `get_month() + 1` and the `{:02}` integer padding compile as-is. If the build reports a float-arithmetic type error here, the getters are `f64` in the pinned version — change to `d.get_month() as u32 + 1` and cast the others with `as u32`.

- [ ] **Step 3: Verify it builds**

Run: `cargo check --target wasm32-unknown-unknown`
Expected: success. The helpers are unused for now — expect dead-code warnings, which Task 6 resolves.

- [ ] **Step 4: Commit**

```bash
git add Cargo.toml src/app.rs
git commit -m "feat(app): web-sys download helper + save_timestamp"
```

---

## Task 6: Bridge installers + command handlers

**Files:**
- Modify: `src/app.rs` (installers ~155-308 region; setup calls ~653-659; command drain + handlers ~727-777)

Expose `__jigglefabSave` / `__jigglefabLoadToml`, register them, and handle the drained commands in the redraw loop.

- [ ] **Step 1: Add the two installer functions**

In `src/app.rs`, add near the other `install_window_*` functions (e.g. after `install_window_can_revert`):
```rust
#[cfg(target_arch = "wasm32")]
fn install_window_save() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().save = true);
    }) as Box<dyn Fn()>);
    expose_to_window!("__jigglefabSave", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_load() {
    use wasm_bindgen::closure::Closure;
    // Returns "" on success (and queues the built Scene), else an error string.
    let cb = Closure::wrap(Box::new(|text: String| -> String {
        match crate::editor::Scene::from_toml(&text) {
            Ok(scene) => {
                web_bridge::COMMANDS.with(|c| c.borrow_mut().load_scene = Some(scene));
                String::new()
            }
            Err(e) => e.to_string(),
        }
    }) as Box<dyn Fn(String) -> String>);
    expose_to_window!("__jigglefabLoadToml", cb);
}
```

- [ ] **Step 2: Register them in setup**

In `src/app.rs`, find the block of `install_window_*();` calls (around line 653-659, where `install_window_set_mode()` etc. are called) and add:
```rust
            install_window_save();
            install_window_load();
```

- [ ] **Step 3: Drain the new commands**

In `src/app.rs`, replace the command-drain block (currently lines 727-732) with one that also takes `save` and `load_scene`:
```rust
                    let (new_mode, edit_state, new_chemistry, new_tool, clear_scene, revert, save, load_scene) = web_bridge::COMMANDS.with(|c| {
                        let mut cmds = c.borrow_mut();
                        let clr = std::mem::replace(&mut cmds.clear, false);
                        let rev = std::mem::replace(&mut cmds.revert, false);
                        let sav = std::mem::replace(&mut cmds.save, false);
                        let load = cmds.load_scene.take();
                        (cmds.set_mode.take(), cmds.set_edit_state.take(), cmds.set_chemistry.take(), cmds.set_tool.take(), clr, rev, sav, load)
                    });
```

- [ ] **Step 4: Handle the new commands**

In `src/app.rs`, immediately after the `if revert { self.revert_to_snapshot(); }` block (currently line 774-776) and before the closing of the `#[cfg(target_arch = "wasm32")]` block (line 777), add:
```rust
                    if let Some(new_scene) = load_scene {
                        self.scene = Some(new_scene);
                        self.sim = None;
                        self.pre_run_snapshot = None;
                        self.mode = crate::editor::Mode::Edit;
                        self.drag = crate::editor::DragState::None;
                        self.mouse_down = false;
                        if let (Some(renderer), Some(scene)) = (self.renderer.as_mut(), self.scene.as_ref()) {
                            let palette: Vec<[f32; 3]> = scene.chemistry.colors.clone();
                            renderer.update_camera(scene.world_size, &palette);
                        }
                    }
                    if save {
                        if let Some(scene) = self.scene.as_ref() {
                            let contents = scene.to_toml();
                            let filename = format!("jigglefab-{}-{}.toml", scene.chemistry_name, save_timestamp());
                            trigger_download(&filename, &contents);
                        }
                    }
```

- [ ] **Step 5: Verify it builds**

Run: `cargo check --target wasm32-unknown-unknown && cargo check --lib`
Expected: both succeed, no dead-code warnings for the Task 5 helpers (now used).

- [ ] **Step 6: Commit**

```bash
git add src/app.rs
git commit -m "feat(bridge): __jigglefabSave + __jigglefabLoadToml wiring"
```

---

## Task 7: Toolbar — Save / Load pills + file input

**Files:**
- Modify: `index.html`

Add the two pills next to Clear/Revert, a hidden file input, the JS wiring, and Save's disabled-state polling.

- [ ] **Step 1: Add the buttons + file input**

In `index.html`, in the `mode` row (currently lines 206-212), add after the Revert anchor (line 211):
```html
            <a id="btn-save" class="disabled">Save</a>
            <a id="btn-load">Load</a>
            <input type="file" id="file-load" accept=".toml" style="display:none">
```

- [ ] **Step 2: Wire the buttons**

In `index.html`, after the Revert button handler block (ends at line 494, `});`), add:
```javascript
        // Save button. Disabled unless Edit mode with beads (see refreshToolbar).
        // Forwards to wasm, which serializes the scene and triggers a download.
        const saveBtn = document.getElementById("btn-save");
        saveBtn.addEventListener("click", (e) => {
            e.preventDefault();
            if (saveBtn.classList.contains("disabled")) return;
            if (window.__jigglefabSave) window.__jigglefabSave();
        });

        // Load button -> hidden file input. Confirms before replacing a
        // non-empty scene, mirroring Clear/Revert.
        const loadBtn = document.getElementById("btn-load");
        const fileInput = document.getElementById("file-load");
        loadBtn.addEventListener("click", (e) => {
            e.preventDefault();
            const beads = (window.__jigglefabBeadCount && window.__jigglefabBeadCount()) || 0;
            if (beads > 0 && !window.confirm(
                `Load a scene? This replaces the current ${beads} bead${beads === 1 ? "" : "s"}.`
            )) return;
            fileInput.value = "";  // allow re-selecting the same file
            fileInput.click();
        });
        fileInput.addEventListener("change", () => {
            const file = fileInput.files && fileInput.files[0];
            if (!file) return;
            const reader = new FileReader();
            reader.onload = () => {
                const err = window.__jigglefabLoadToml
                    ? window.__jigglefabLoadToml(reader.result)
                    : "load unavailable";
                if (err) window.alert("Could not load scene: " + err);
            };
            reader.readAsText(file);
        });
```

- [ ] **Step 3: Poll Save's disabled state in `refreshToolbar`**

In `index.html`, inside `refreshToolbar` (after the revert-disabled block, currently lines 542-545), add:
```javascript
            const saveBtn2 = document.getElementById("btn-save");
            if (saveBtn2 && typeof window.__jigglefabGetMode === "function"
                && typeof window.__jigglefabBeadCount === "function") {
                const canSave = window.__jigglefabGetMode() === "edit"
                    && window.__jigglefabBeadCount() > 0;
                saveBtn2.classList.toggle("disabled", !canSave);
            }
```

- [ ] **Step 4: Verify the wasm build still compiles**

Run: `cargo build --target wasm32-unknown-unknown`
Expected: success (HTML changes don't affect Rust, but this confirms the bundled trunk binary still builds).

- [ ] **Step 5: Commit**

```bash
git add index.html
git commit -m "feat(editor): Save / Load toolbar pills + file input"
```

---

## Task 8: Browser smoke test — load + save round-trip

**Files:**
- Modify: `scripts/verify-web.py`

Extend the `--editor` block to exercise load (valid + invalid) through the bridge and capture a Save download with Playwright.

- [ ] **Step 1: Add the assertions**

In `scripts/verify-web.py`, find the line (currently 210):
```python
            console_lines.append("[editor] extended smoke test passed")
```
Insert the following BEFORE that line (same indentation — inside the `if "--editor" in sys.argv:` block):
```python
            # --- Load: feed a known TOML through the bridge (bypasses the file
            # dialog). Scene replaces, mode is edit, bead count matches. ---
            scene_toml = (
                '[meta]\n'
                'name = "smoke"\n'
                'chemistry = "wire"\n'
                'seed = 1\n'
                'world_size = 30.0\n'
                'bonds = [[0, 1], [1, 2]]\n\n'
                '[[bead]]\nstate = "off"\npos = [10.0, 10.0]\n\n'
                '[[bead]]\nstate = "off"\npos = [10.667, 10.0]\n\n'
                '[[bead]]\nstate = "off"\npos = [11.334, 10.0]\n'
            )
            load_err = await page.evaluate("(t) => window.__jigglefabLoadToml(t)", scene_toml)
            assert load_err == "", f"valid load returned error: {load_err!r}"
            await page.wait_for_function("window.__jigglefabGetMode() === 'edit'", timeout=2000)
            await page.wait_for_function("window.__jigglefabBeadCount() === 3", timeout=2000)

            # --- Load rejection: unknown chemistry returns a non-empty error
            # string and leaves the scene untouched. ---
            bad_toml = '[meta]\nname = "x"\nchemistry = "nope"\nseed = 1\nbonds = []\n\n[[bead]]\nstate = "off"\npos = [5.0, 5.0]\n'
            bad_err = await page.evaluate("(t) => window.__jigglefabLoadToml(t)", bad_toml)
            assert bad_err, "invalid load should return a non-empty error string"
            assert await page.evaluate("window.__jigglefabBeadCount()") == 3, \
                "rejected load must not change the scene"

            # --- Save: triggers a .toml download. Capture it with Playwright. ---
            async with page.expect_download(timeout=5000) as dl_info:
                await page.evaluate("window.__jigglefabSave()")
            download = await dl_info.value
            assert download.suggested_filename.endswith(".toml"), \
                f"unexpected download name: {download.suggested_filename}"
```

- [ ] **Step 2: Run the smoke test locally**

Build + serve trunk (standard project loop — `trunk serve --release` in another shell per `memory/jigglefab-build-env.md`), then:
```bash
python scripts/verify-web.py http://127.0.0.1:8080/ --editor --headed
```
Expected: exits 0, with `[editor] extended smoke test passed` in the console section.

- [ ] **Step 3: Commit**

```bash
git add scripts/verify-web.py
git commit -m "test(web): editor smoke covers save download + load round-trip"
```

---

## Task 9: Final verification

- [ ] **Step 1: Full native test suite**

Run:
```bash
cargo test --lib
```
Expected: all pass (new `fab::tests` + `editor::tests` included).

- [ ] **Step 2: wasm build**

Run:
```bash
cargo check --target wasm32-unknown-unknown
```
Expected: success.

- [ ] **Step 3: Smoke test against local build**

```bash
python scripts/verify-web.py http://127.0.0.1:8080/ --editor
```
Expected: exit 0.

- [ ] **Step 4: Push (manual — user-driven)**

Deferred for review per the CLAUDE.md push policy. Pushing to `web` triggers the GH Actions deploy.

---

## Self-review notes

- **Spec §1 (file format):** Task 1 (Serialize + skip-None); Task 2 emits sorted bonds, always-present bond list, no vel; `meta.name = "editor scene"`, `seed` carried.
- **Spec §2 (save):** Task 2 (`to_toml`), Task 5 (download helper + timestamp), Task 6 (`save` command + handler), Task 7 (button + disabled poll). Filename `jigglefab-<chemistry>-<YYYYMMDD-HHMMSS>.toml`.
- **Spec §3 (load):** Task 3 (`from_toml` + `LoadError`, strict/atomic), Task 6 (`__jigglefabLoadToml` + `load_scene` handler: swap scene, Edit mode, drop sim, clear snapshot), Task 7 (file input + confirm-on-replace + error alert).
- **Spec §4 (bridge):** Task 4 (command fields), Task 6 (`__jigglefabSave`, `__jigglefabLoadToml`).
- **Spec §5 (UI):** Task 7 (pills next to Clear/Revert; Save disabled unless Edit + beads; Load always enabled).
- **Spec §6 (files):** `src/fab.rs`, `src/editor.rs`, `src/app.rs`, `index.html`, `scripts/verify-web.py`, plus `Cargo.toml` (web-sys features) — no new modules.
- **Spec §7 (testing):** round-trip, sorted-bonds/no-vel, unknown-chemistry, unknown-state, malformed, legacy-preset, empty-scene unit tests (Tasks 1-3); browser load+save smoke (Task 8).
- **Spec §8 (deferred):** localStorage, savestate/velocities, lenient load, drag-drop — none implemented (correct).

**Type consistency:** `Scene::to_toml(&self) -> String`, `Scene::from_toml(&str) -> Result<Scene, LoadError>`, `LoadError::{Parse, UnknownChemistry, UnknownState{bead,state}}`, `PendingCommands.save: bool`, `PendingCommands.load_scene: Option<Scene>`, `trigger_download(&str, &str)`, `save_timestamp() -> String`, bridge globals `__jigglefabSave` / `__jigglefabLoadToml` — names match across Tasks 3-8.

**No placeholders.** Every code step ships complete code; every run step has an exact command and expected result.
