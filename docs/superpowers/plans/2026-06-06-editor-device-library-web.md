# Editor Device Library — Web Integration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Wire the plan-1 device-library core to the browser editor — a left-edge dock sidebar with beads-only thumbnails, click-arm/ghost/click-place stamping, Shift+scroll/bracket rotation, suites, export/import, and localStorage persistence.

**Architecture:** Rust owns the `Library` (a field on `App`); JS owns localStorage + file I/O. The `App` mirrors library-derived data (full JSON, a dock projection, suite names, armed id, a mutation `rev`) into the existing `SNAPSHOT` each frame; JS reads it via new `__jigglefab*` getters and drives mutations via new `COMMANDS`. A `library_rev` counter lets the existing per-frame `refreshToolbar` poll re-persist and re-render only on change. No new `web-sys` features.

**Tech Stack:** Rust + wasm-bindgen/js-sys (existing bridge pattern in `src/app.rs`), `serde_json` (already a dependency from plan 1), vanilla JS in `index.html`, Playwright smoke in `scripts/verify-web.py`.

**Spec:** `docs/superpowers/specs/2026-06-06-editor-device-library-web-design.md`. Core (already shipped on this branch): `docs/superpowers/specs/2026-06-06-editor-device-library-design.md`.

## Build & verify notes (read first)

- cargo isn't on the default Bash PATH. Prepend every cargo command with:
  `export PATH="$PATH:/c/Users/thedo/.cargo/bin"` (Bash only — PowerShell is denied).
- **Almost all new code in `src/app.rs` is under `#[cfg(target_arch = "wasm32")]`.** Native `cargo build`/`cargo test` will NOT compile it. The compile gate for every Rust task is:
  `cargo check --target wasm32-unknown-unknown`
  If that errors with "target may not be installed", run `rustup target add wasm32-unknown-unknown` once, then retry.
- The **cfg-agnostic** additions (the `App` fields, `apply_rotation`, the `overlay_segments` ghost branch, the `on_mouse_down` armed branch) also compile natively, so run `cargo test --lib` too after those tasks to confirm no native breakage.
- This is integration/glue, not unit-TDD: the behavioral gate is the browser smoke in Task 6 (`scripts/verify-web.py --editor`), mirroring how the existing editor tools are tested. Tasks 1–5 build the glue and gate on `cargo check --target wasm32-unknown-unknown`; Task 6 proves behavior end-to-end.
- Commit trailer on every commit:
  `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>`

## File structure

- `src/app.rs` — `App` gains `library`, `armed_device`, `ghost_angle`, `shift_held`, `library_rev`. New bridge fields (PendingCommands/Snapshot + a `DockEntry`), installers, the command drain, the snapshot projection, the ghost overlay branch, place-while-armed, and rotation input.
- `index.html` — `#device-dock` sidebar markup + styles; thumbnail drawing; action controls; rev-driven render + persist; startup load; export/import.
- `scripts/verify-web.py` — extend the `--editor` block.

No changes to `src/library.rs` or `src/editor.rs` (the core is complete). No new dependencies or `web-sys` features.

---

### Task 1: `App` state + bridge plumbing (load / persist / rev)

Adds the `Library` to `App`, the persistence bridge, and the snapshot mirror — no UI yet. Verified by a smoke step that hand-feeds a library JSON and reads it back.

**Files:** Modify `src/app.rs`

- [ ] **Step 1: Add command + snapshot fields + `DockEntry` to the `web_bridge` module**

In `src/app.rs`, in the `#[cfg(target_arch = "wasm32")] mod web_bridge` block: extend `PendingCommands`, add a `DockEntry` struct, and extend `Snapshot`.

Add these fields to `PendingCommands` (after the existing `pub revert: bool,`):
```rust
        pub load_library: Option<String>,
        pub save_to_dock: Option<String>,
        pub rename_device: Option<(u32, String)>,
        pub remove_device: Option<u32>,
        pub arm_device: Option<u32>,
        pub disarm: bool,
        pub save_suite: Option<String>,
        pub load_suite: Option<String>,
        pub import_suite: Option<String>,
```

Add this struct inside the `web_bridge` module (e.g. just above `Snapshot`):
```rust
    /// One dock device, projected for the JS thumbnail renderer. `beads` is a
    /// list of (relative position, rgb color) pairs; `compatible` is computed
    /// against the scene's current chemistry.
    #[derive(Clone)]
    pub struct DockEntry {
        pub id: u32,
        pub name: String,
        pub chemistry: String,
        pub beads: Vec<([f32; 2], [f32; 3])>,
        pub compatible: bool,
    }
```

Add these fields to `Snapshot` (after the existing `pub grid_alpha: f32,`):
```rust
        pub library_json: String,
        pub library_rev: u32,
        pub armed_id: i32,
        pub dock: Vec<DockEntry>,
        pub suite_names: Vec<String>,
```
(`Snapshot` derives `Default, Clone`; `String`/`Vec`/`i32` all satisfy both, and `DockEntry` is `Clone`.)

- [ ] **Step 2: Add the `App` fields**

In `struct App`, add (after `pre_run_snapshot`):
```rust
    /// In-app device library. Persisted to localStorage by JS; mutated here.
    library: crate::library::Library,
    /// The device currently armed for stamping (Edit mode), if any.
    armed_device: Option<crate::library::Device>,
    /// Accumulated ghost rotation in radians (reset to 0 on each fresh arm).
    ghost_angle: f32,
    /// True while Shift is held — turns scroll into rotate-the-selection.
    shift_held: bool,
    /// Bumped on every library mutation so JS re-persists + re-renders.
    library_rev: u32,
```

In `App::new()`, initialize them (after `pre_run_snapshot: None,`):
```rust
            library: crate::library::Library::default(),
            armed_device: None,
            ghost_angle: 0.0,
            shift_held: false,
            library_rev: 0,
```

- [ ] **Step 3: Add the bridge installer functions**

Add these `#[cfg(target_arch = "wasm32")]` functions alongside the other `install_window_*` fns in `src/app.rs`:
```rust
#[cfg(target_arch = "wasm32")]
fn install_window_load_library() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|json: String| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().load_library = Some(json));
    }) as Box<dyn Fn(String)>);
    expose_to_window!("__jigglefabLoadLibrary", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_get_library_json() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> String {
        web_bridge::SNAPSHOT.with(|s| s.borrow().library_json.clone())
    }) as Box<dyn Fn() -> String>);
    expose_to_window!("__jigglefabGetLibraryJson", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_get_library_rev() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> u32 {
        web_bridge::SNAPSHOT.with(|s| s.borrow().library_rev)
    }) as Box<dyn Fn() -> u32>);
    expose_to_window!("__jigglefabGetLibraryRev", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_armed_id() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> i32 {
        web_bridge::SNAPSHOT.with(|s| s.borrow().armed_id)
    }) as Box<dyn Fn() -> i32>);
    expose_to_window!("__jigglefabArmedId", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_get_suite_names() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> js_sys::Array {
        let arr = js_sys::Array::new();
        web_bridge::SNAPSHOT.with(|s| {
            for name in &s.borrow().suite_names {
                arr.push(&wasm_bindgen::JsValue::from_str(name));
            }
        });
        arr
    }) as Box<dyn Fn() -> js_sys::Array>);
    expose_to_window!("__jigglefabGetSuiteNames", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_get_dock() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> js_sys::Array {
        let outer = js_sys::Array::new();
        web_bridge::SNAPSHOT.with(|s| {
            for d in &s.borrow().dock {
                let entry = js_sys::Object::new();
                let _ = js_sys::Reflect::set(&entry, &"id".into(), &wasm_bindgen::JsValue::from_f64(d.id as f64));
                let _ = js_sys::Reflect::set(&entry, &"name".into(), &wasm_bindgen::JsValue::from_str(&d.name));
                let _ = js_sys::Reflect::set(&entry, &"chemistry".into(), &wasm_bindgen::JsValue::from_str(&d.chemistry));
                let _ = js_sys::Reflect::set(&entry, &"compatible".into(), &wasm_bindgen::JsValue::from_bool(d.compatible));
                let beads = js_sys::Array::new();
                for (pos, color) in &d.beads {
                    let b = js_sys::Object::new();
                    let p = js_sys::Array::new();
                    p.push(&wasm_bindgen::JsValue::from_f64(pos[0] as f64));
                    p.push(&wasm_bindgen::JsValue::from_f64(pos[1] as f64));
                    let _ = js_sys::Reflect::set(&b, &"pos".into(), &p);
                    let col = js_sys::Array::new();
                    col.push(&wasm_bindgen::JsValue::from_f64(color[0] as f64));
                    col.push(&wasm_bindgen::JsValue::from_f64(color[1] as f64));
                    col.push(&wasm_bindgen::JsValue::from_f64(color[2] as f64));
                    let _ = js_sys::Reflect::set(&b, &"color".into(), &col);
                    beads.push(&b);
                }
                let _ = js_sys::Reflect::set(&entry, &"beads".into(), &beads);
                outer.push(&entry);
            }
        });
        outer
    }) as Box<dyn Fn() -> js_sys::Array>);
    expose_to_window!("__jigglefabGetDock", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_save_to_dock() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|name: String| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().save_to_dock = Some(name));
    }) as Box<dyn Fn(String)>);
    expose_to_window!("__jigglefabSaveToDock", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_rename_device() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|id: u32, name: String| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().rename_device = Some((id, name)));
    }) as Box<dyn Fn(u32, String)>);
    expose_to_window!("__jigglefabRenameDevice", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_remove_device() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|id: u32| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().remove_device = Some(id));
    }) as Box<dyn Fn(u32)>);
    expose_to_window!("__jigglefabRemoveDevice", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_arm_device() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|id: u32| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().arm_device = Some(id));
    }) as Box<dyn Fn(u32)>);
    expose_to_window!("__jigglefabArmDevice", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_disarm() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().disarm = true);
    }) as Box<dyn Fn()>);
    expose_to_window!("__jigglefabDisarm", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_save_suite() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|name: String| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().save_suite = Some(name));
    }) as Box<dyn Fn(String)>);
    expose_to_window!("__jigglefabSaveSuite", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_load_suite() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|name: String| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().load_suite = Some(name));
    }) as Box<dyn Fn(String)>);
    expose_to_window!("__jigglefabLoadSuite", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_import_suite() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|json: String| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().import_suite = Some(json));
    }) as Box<dyn Fn(String)>);
    expose_to_window!("__jigglefabImportSuite", cb);
}
```

- [ ] **Step 4: Call the new installers**

In `resumed()`, in the `#[cfg(target_arch = "wasm32")]` block, after the existing `install_window_grid_alpha();` line (~line 794), add:
```rust
            install_window_load_library();
            install_window_get_library_json();
            install_window_get_library_rev();
            install_window_armed_id();
            install_window_get_suite_names();
            install_window_get_dock();
            install_window_save_to_dock();
            install_window_rename_device();
            install_window_remove_device();
            install_window_arm_device();
            install_window_disarm();
            install_window_save_suite();
            install_window_load_suite();
            install_window_import_suite();
```

- [ ] **Step 5: Drain the library commands**

In `window_event`'s `WindowEvent::RedrawRequested` arm, inside the existing `#[cfg(target_arch = "wasm32")]` block, immediately AFTER the existing command-handling (after the `if revert { self.revert_to_snapshot(); }` line, ~line 921), add:
```rust
                    let (load_library, save_to_dock, rename_device, remove_device,
                         arm_device, disarm, save_suite, load_suite, import_suite) =
                        web_bridge::COMMANDS.with(|c| {
                            let mut cmds = c.borrow_mut();
                            let dis = std::mem::replace(&mut cmds.disarm, false);
                            (cmds.load_library.take(), cmds.save_to_dock.take(),
                             cmds.rename_device.take(), cmds.remove_device.take(),
                             cmds.arm_device.take(), dis, cmds.save_suite.take(),
                             cmds.load_suite.take(), cmds.import_suite.take())
                        });
                    let mut lib_changed = false;
                    if let Some(json) = load_library {
                        self.library = crate::library::Library::load_or_default(&json);
                        lib_changed = true;
                    }
                    if let Some(name) = save_to_dock {
                        if let Some(scene) = self.scene.as_ref() {
                            if let Some(dev) = scene.extract_device(name) {
                                self.library.add_to_dock(dev);
                                lib_changed = true;
                            }
                        }
                    }
                    if let Some((id, name)) = rename_device {
                        self.library.rename_device(id, name);
                        lib_changed = true;
                    }
                    if let Some(id) = remove_device {
                        self.library.remove_device(id);
                        lib_changed = true;
                    }
                    if let Some(name) = save_suite {
                        let chem = self.scene.as_ref().map(|s| s.chemistry_name.clone()).unwrap_or_default();
                        self.library.save_suite(name, &chem);
                        lib_changed = true;
                    }
                    if let Some(name) = load_suite {
                        self.library.load_suite(&name);
                        lib_changed = true;
                    }
                    if let Some(json) = import_suite {
                        match serde_json::from_str::<crate::library::Suite>(&json) {
                            Ok(suite) => { self.library.import_suite(suite); lib_changed = true; }
                            Err(e) => log::warn!("importSuite: parse error: {e}"),
                        }
                    }
                    if let Some(id) = arm_device {
                        let dev = self.scene.as_ref().and_then(|scene| {
                            self.library.dock.iter()
                                .find(|d| d.id == id)
                                .filter(|d| d.is_compatible_with(&scene.chemistry))
                                .cloned()
                        });
                        if let Some(dev) = dev {
                            self.armed_device = Some(dev);
                            self.ghost_angle = 0.0;
                        }
                    }
                    if disarm { self.armed_device = None; }
                    if lib_changed { self.library_rev = self.library_rev.wrapping_add(1); }
```

- [ ] **Step 6: Mirror library data into the snapshot**

In the same RedrawRequested arm, in the `#[cfg(target_arch = "wasm32")]` snapshot-write block (~line 964), BEFORE the `web_bridge::SNAPSHOT.with(...)` call, add the projections:
```rust
                    let library_json = self.library.to_json();
                    let library_rev = self.library_rev;
                    let armed_id = self.armed_device.as_ref().map(|d| d.id as i32).unwrap_or(-1);
                    let dock: Vec<web_bridge::DockEntry> = match self.scene.as_ref() {
                        Some(scene) => self.library.dock.iter().map(|d| {
                            let beads = d.beads.iter().map(|b| {
                                let color = scene.chemistry.state_index(&b.state)
                                    .map(|si| scene.chemistry.colors[si])
                                    .unwrap_or([0.5, 0.5, 0.5]);
                                (b.pos, color)
                            }).collect();
                            web_bridge::DockEntry {
                                id: d.id,
                                name: d.name.clone(),
                                chemistry: d.chemistry.clone(),
                                beads,
                                compatible: d.is_compatible_with(&scene.chemistry),
                            }
                        }).collect(),
                        None => Vec::new(),
                    };
                    let suite_names: Vec<String> = match self.scene.as_ref() {
                        Some(scene) => self.library.suites.iter()
                            .filter(|s| s.chemistry == scene.chemistry_name)
                            .map(|s| s.name.clone())
                            .collect(),
                        None => Vec::new(),
                    };
```
Then add these to the `Snapshot { ... }` literal (after `grid_alpha: self.grid_alpha,`):
```rust
                            library_json,
                            library_rev,
                            armed_id,
                            dock,
                            suite_names,
```

- [ ] **Step 7: Verify it compiles for wasm and natively**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cargo check --target wasm32-unknown-unknown`
Expected: compiles (warnings about unused `armed_device`/`ghost_angle`/`shift_held` are fine — later tasks use them).
Run: `cargo test --lib`
Expected: native lib tests still pass (the new `App` fields compile natively; the wasm blocks are skipped).

- [ ] **Step 8: Commit**
```bash
git add src/app.rs
git commit -m "feat(app): Library in App + persistence/dock/suite bridge plumbing"
```

---

### Task 2: Dock sidebar UI (thumbnails, save-to-dock, rename/remove, filter)

Adds the visible left sidebar driven by the Task-1 bridge. No arming/placement yet (Task 3).

**Files:** Modify `index.html`

- [ ] **Step 1: Add the sidebar styles**

In the `<style>` block of `index.html`, after the `#editor-toolbar a.state-pill .dot { ... }` rule, add:
```css
        #device-dock {
            position: absolute;
            top: 8px;
            left: 8px;
            width: 150px;
            max-height: calc(100vh - 16px);
            display: flex;
            flex-direction: column;
            gap: 6px;
            padding: 6px;
            background: rgba(0, 0, 0, 0.45);
            border-radius: 10px;
            font: 12px/1.2 ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
            z-index: 10;
        }
        #device-dock .dock-actions { display: flex; flex-wrap: wrap; gap: 4px; }
        #device-dock .dock-actions a {
            color: #cfd; text-decoration: none; padding: 3px 8px; border-radius: 6px;
            background: rgba(255, 255, 255, 0.06); border: 1px solid transparent; cursor: pointer;
        }
        #device-dock .dock-actions a:hover { background: rgba(255, 255, 255, 0.12); }
        #device-dock .dock-actions a.disabled { opacity: 0.4; cursor: default; pointer-events: none; }
        #device-dock .dock-list { display: flex; flex-direction: column; gap: 4px; overflow-y: auto; }
        #device-dock .dock-empty { color: #888; padding: 6px 2px; }
        #device-dock .dock-item {
            display: flex; align-items: center; gap: 6px; padding: 3px;
            border-radius: 6px; border: 1px solid transparent; cursor: pointer;
        }
        #device-dock .dock-item:hover { background: rgba(255, 255, 255, 0.08); }
        #device-dock .dock-item.active { background: #2a4d3a; border-color: #4a8; }
        #device-dock .dock-item.incompatible { opacity: 0.4; cursor: default; }
        #device-dock .dock-item canvas { background: rgba(255,255,255,0.04); border-radius: 4px; }
        #device-dock .dock-item .name { flex: 1; color: #cfd; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
        #device-dock .dock-item .rm { color: #a88; padding: 0 4px; }
        #device-dock .dock-item .rm:hover { color: #f88; }
```

- [ ] **Step 2: Add the sidebar markup**

In `index.html`, after the `<nav id="editor-toolbar"> ... </nav>` element (before `<nav id="speed-picker">`), add:
```html
    <aside id="device-dock">
        <div class="dock-actions">
            <a id="dock-save-sel" class="disabled">Save selection</a>
            <a id="dock-save-suite">Save suite</a>
            <a id="dock-load-suite">Load suite</a>
            <a id="dock-export">Export</a>
            <a id="dock-import">Import</a>
        </div>
        <div class="dock-list" id="dock-list"></div>
        <input id="dock-import-file" type="file" accept=".json,application/json" style="display:none" />
    </aside>
```

- [ ] **Step 3: Add the dock JS (render driven by `library_rev`)**

In the `<script>` block, after the tool-button wiring (after the `toolButtons.forEach(...)` block, before the `let lastChemName = "";` line), add:
```javascript
        // ---- Device dock ----
        const dockList = document.getElementById("dock-list");
        const dockSaveSel = document.getElementById("dock-save-sel");
        let lastDockRev = -1;

        function drawThumb(canvas, beads) {
            const ctx = canvas.getContext("2d");
            const W = canvas.width, H = canvas.height;
            ctx.clearRect(0, 0, W, H);
            if (!beads.length) return;
            let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
            for (const b of beads) {
                minX = Math.min(minX, b.pos[0]); maxX = Math.max(maxX, b.pos[0]);
                minY = Math.min(minY, b.pos[1]); maxY = Math.max(maxY, b.pos[1]);
            }
            const pad = 4, span = Math.max(maxX - minX, maxY - minY, 1e-3);
            const scale = (Math.min(W, H) - 2 * pad) / span;
            const ox = (W - (maxX - minX) * scale) / 2 - minX * scale;
            const oy = (H - (maxY - minY) * scale) / 2 - minY * scale;
            for (const b of beads) {
                const x = b.pos[0] * scale + ox, y = b.pos[1] * scale + oy;
                ctx.fillStyle = `rgb(${Math.round(b.color[0]*255)},${Math.round(b.color[1]*255)},${Math.round(b.color[2]*255)})`;
                ctx.beginPath();
                ctx.arc(x, y, 2.5, 0, Math.PI * 2);
                ctx.fill();
            }
        }

        function renderDock() {
            if (typeof window.__jigglefabGetDock !== "function") return;
            const chem = (window.__jigglefabGetChemistryName && window.__jigglefabGetChemistryName()) || "";
            const armed = (window.__jigglefabArmedId && window.__jigglefabArmedId()) ?? -1;
            const all = window.__jigglefabGetDock();
            const devices = all.filter(d => d.chemistry === chem);
            dockList.innerHTML = "";
            if (!devices.length) {
                const empty = document.createElement("div");
                empty.className = "dock-empty";
                empty.textContent = "Select beads, then Save selection";
                dockList.appendChild(empty);
                return;
            }
            for (const d of devices) {
                const item = document.createElement("div");
                item.className = "dock-item"
                    + (d.id === armed ? " active" : "")
                    + (d.compatible ? "" : " incompatible");
                if (!d.compatible) item.title = "Incompatible with the current chemistry";
                const canvas = document.createElement("canvas");
                canvas.width = 32; canvas.height = 32;
                drawThumb(canvas, d.beads);
                item.appendChild(canvas);
                const name = document.createElement("span");
                name.className = "name";
                name.textContent = d.name;
                item.appendChild(name);
                const rm = document.createElement("span");
                rm.className = "rm";
                rm.textContent = "×";
                rm.title = "Remove from dock";
                rm.addEventListener("click", (e) => {
                    e.stopPropagation();
                    if (window.confirm(`Remove "${d.name}" from the dock?`)) {
                        window.__jigglefabRemoveDevice(d.id);
                    }
                });
                item.appendChild(rm);
                // Double-click name to rename.
                name.addEventListener("dblclick", (e) => {
                    e.stopPropagation();
                    const next = window.prompt("Rename device", d.name);
                    if (next && next !== d.name) window.__jigglefabRenameDevice(d.id, next);
                });
                // Click body to arm/disarm (wired in Task 3; placeholder no-op now).
                item.dataset.id = d.id;
                item.dataset.compatible = d.compatible;
                dockList.appendChild(item);
            }
        }

        // Save selection → prompt for a name → saveToDock.
        dockSaveSel.addEventListener("click", () => {
            if (dockSaveSel.classList.contains("disabled")) return;
            const count = (window.__jigglefabGetDock && window.__jigglefabGetDock().length) || 0;
            const name = window.prompt("Device name", "device " + (count + 1));
            if (name) window.__jigglefabSaveToDock(name);
        });
```

- [ ] **Step 4: Drive dock render + save-button enablement from the poll**

In `refreshToolbar()`, just before the closing `requestAnimationFrame(refreshToolbar);` line, add:
```javascript
            if (typeof window.__jigglefabGetLibraryRev === "function") {
                const rev = window.__jigglefabGetLibraryRev();
                const chemName = (window.__jigglefabGetChemistryName && window.__jigglefabGetChemistryName()) || "";
                const sig = rev + "|" + chemName + "|" + ((window.__jigglefabArmedId && window.__jigglefabArmedId()) ?? -1);
                if (sig !== lastDockRev) { lastDockRev = sig; renderDock(); }
            }
            if (dockSaveSel && typeof window.__jigglefabSelectionCount === "function") {
                const mode = (window.__jigglefabGetMode && window.__jigglefabGetMode()) || "run";
                const ok = mode === "edit" && window.__jigglefabSelectionCount() > 0;
                dockSaveSel.classList.toggle("disabled", !ok);
            }
```
(`lastDockRev` is declared in Step 3; the signature folds in chemistry + armed id so the list re-renders on chemistry switch and arm/disarm, not just library mutations.)

- [ ] **Step 5: Verify in a browser build (manual smoke)**

This task has no Rust changes, so the gate is a visual/manual check (the automated smoke lands in Task 6). If a local web build is available:
Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cargo check --target wasm32-unknown-unknown`
Expected: still compiles (no Rust changed, but confirms nothing was broken).
Then, if `trunk` is set up, `trunk serve` and confirm: switch to Edit, place + Rect-select a few beads, the "Save selection" pill enables, clicking it and naming adds a thumbnail tile to the left sidebar. If `trunk` isn't available locally, defer the visual check to Task 6's automated smoke and proceed.

- [ ] **Step 6: Commit**
```bash
git add index.html
git commit -m "feat(web): device dock sidebar (thumbnails, save-to-dock, rename/remove, filter)"
```

---

### Task 3: Arm, ghost preview, place, disarm

Makes the dock interactive: arm a device, show a ghost at the cursor, click to stamp, Esc/re-click to disarm.

**Files:** Modify `src/app.rs`, `index.html`

- [ ] **Step 1: Place-while-armed in `on_mouse_down`**

In `src/app.rs`, in `fn on_mouse_down`, immediately after `let Some(world_pos) = self.cursor_world() else { return };` (and before `let Some(scene) = self.scene.as_mut() else { return };`), insert:
```rust
        // Armed device takes priority: stamp a copy and stay armed (Edit only).
        if self.mode == crate::editor::Mode::Edit {
            if let Some(dev) = self.armed_device.clone() {
                if let Some(scene) = self.scene.as_mut() {
                    scene.instantiate_device(&dev, world_pos, self.ghost_angle);
                }
                self.drag = crate::editor::DragState::None;
                return;
            }
        }
```

- [ ] **Step 2: Ghost overlay in `overlay_segments`**

In `fn overlay_segments`, just before the final `out` return, add:
```rust
        // Armed-device ghost: a small cross per device bead at the cursor,
        // turned by the accumulated ghost angle. Beads-only (matches thumbnails).
        if let (Some(dev), Some(cursor)) = (self.armed_device.as_ref(), self.cursor_world()) {
            let (s, c) = self.ghost_angle.sin_cos();
            let r = 0.15; // half-length of each cross arm, world units
            for b in &dev.beads {
                let gx = b.pos[0] * c - b.pos[1] * s + cursor.x;
                let gy = b.pos[0] * s + b.pos[1] * c + cursor.y;
                out.push(OverlayVertex { pos: [gx - r, gy], shade: 0.8 });
                out.push(OverlayVertex { pos: [gx + r, gy], shade: 0.8 });
                out.push(OverlayVertex { pos: [gx, gy - r], shade: 0.8 });
                out.push(OverlayVertex { pos: [gx, gy + r], shade: 0.8 });
            }
        }
```

- [ ] **Step 3: Disarm on mode→Run and on chemistry switch**

In `fn transition_mode`, in the `crate::editor::Mode::Run` arm, after `self.mouse_down = false;`, add:
```rust
                self.armed_device = None;
```
In `window_event`'s RedrawRequested wasm block, in the `if let Some(name) = new_chemistry { ... }` success branch (where it sets `self.mode = Mode::Edit;` etc.), add:
```rust
                            self.armed_device = None;
```
And in the `if clear_scene { ... }` block, after `self.mouse_down = false;`, add:
```rust
                        self.armed_device = None;
```

- [ ] **Step 4: Esc disarms (keyboard handler)**

In `fn window_event`, `WindowEvent::KeyboardInput` arm, inside the `if pressed { ... }` block, add (alongside the existing delete/`0` handling):
```rust
                    if matches!(key_event.logical_key, Key::Named(NamedKey::Escape)) {
                        self.armed_device = None;
                    }
```

- [ ] **Step 5: Wire dock-item click to arm/disarm (JS)**

In `index.html`'s `renderDock()`, replace the placeholder comment line
```javascript
                // Click body to arm/disarm (wired in Task 3; placeholder no-op now).
                item.dataset.id = d.id;
                item.dataset.compatible = d.compatible;
```
with:
```javascript
                item.dataset.id = d.id;
                if (d.compatible) {
                    item.addEventListener("click", () => {
                        const armed = (window.__jigglefabArmedId && window.__jigglefabArmedId()) ?? -1;
                        if (armed === d.id) window.__jigglefabDisarm();
                        else window.__jigglefabArmDevice(d.id);
                    });
                }
```

- [ ] **Step 6: Verify compiles (wasm + native)**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cargo check --target wasm32-unknown-unknown`
Expected: compiles. `armed_device`/`ghost_angle` now used.
Run: `cargo test --lib`
Expected: native lib tests pass (the `on_mouse_down`/`overlay_segments` additions are cfg-agnostic and compile natively).

- [ ] **Step 7: Commit**
```bash
git add src/app.rs index.html
git commit -m "feat(editor): arm device + ghost preview + click-to-stamp + disarm"
```

---

### Task 4: Rotation — Shift+scroll and brackets (15° snap)

Adds the rotation primitive routing (ghost while armed, else selection), the Shift modifier, and the input wiring.

**Files:** Modify `src/app.rs`

- [ ] **Step 1: Rotation constant + routing helper**

In `src/app.rs`, in `impl App`, add a constant near the other consts (e.g. above `fn overlay_segments`) and a helper method (place it near `apply`-style helpers, e.g. after `note_camera_activity`):
```rust
    /// One rotation notch: 15°, in radians.
    const ROTATE_SNAP_RAD: f32 = std::f32::consts::PI / 12.0;

    /// Rotate the current rotation target by `delta` radians: the armed ghost if
    /// one is armed, otherwise the current selection in the scene.
    fn apply_rotation(&mut self, delta: f32) {
        if self.armed_device.is_some() {
            self.ghost_angle += delta;
        } else if let Some(scene) = self.scene.as_mut() {
            scene.rotate_selection(delta);
        }
    }
```

- [ ] **Step 2: Track Shift; reset on focus loss**

In `fn window_event`, `WindowEvent::Focused(false)` arm, add (alongside `self.space_held = false;`):
```rust
                self.shift_held = false;
```
In `WindowEvent::KeyboardInput`, after the existing Space tracking (`if matches!(key_event.logical_key, Key::Named(NamedKey::Space)) { self.space_held = pressed; }`), add:
```rust
                if matches!(key_event.logical_key, Key::Named(NamedKey::Shift)) {
                    self.shift_held = pressed;
                }
```

- [ ] **Step 3: Shift+scroll rotates instead of zooming**

In `fn window_event`, `WindowEvent::MouseWheel` arm, replace the existing body:
```rust
                if scroll != 0.0 {
                    if let Some(viewport) = self.viewport() {
                        let ws = self.world_size();
                        let factor = crate::camera::ZOOM_STEP.powf(scroll);
                        self.camera.zoom_at((self.cursor.x, self.cursor.y), factor, viewport, ws);
                        self.refresh_camera();
                        self.note_camera_activity();
                    }
                }
```
with:
```rust
                if scroll != 0.0 {
                    if self.shift_held {
                        // Rotate the ghost/selection; plain scroll stays zoom so
                        // you can zoom for precise placement while a ghost is up.
                        self.apply_rotation(scroll.signum() * Self::ROTATE_SNAP_RAD);
                    } else if let Some(viewport) = self.viewport() {
                        let ws = self.world_size();
                        let factor = crate::camera::ZOOM_STEP.powf(scroll);
                        self.camera.zoom_at((self.cursor.x, self.cursor.y), factor, viewport, ws);
                        self.refresh_camera();
                        self.note_camera_activity();
                    }
                }
```

- [ ] **Step 4: Bracket keys rotate**

In `WindowEvent::KeyboardInput`, inside the `if pressed { ... }` block, add:
```rust
                    if let Key::Character(ch) = &key_event.logical_key {
                        match ch.as_str() {
                            "[" => self.apply_rotation(-Self::ROTATE_SNAP_RAD),
                            "]" => self.apply_rotation(Self::ROTATE_SNAP_RAD),
                            _ => {}
                        }
                    }
```
(The existing `"0"` camera-reset check uses a separate `matches!`; leave it as-is — this new `if let` is additive.)

- [ ] **Step 5: Verify compiles (wasm + native)**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cargo check --target wasm32-unknown-unknown`
Expected: compiles; `shift_held` now used.
Run: `cargo test --lib`
Expected: native lib tests pass.

- [ ] **Step 6: Commit**
```bash
git add src/app.rs
git commit -m "feat(editor): Shift+scroll and bracket-key rotation (15° snap)"
```

---

### Task 5: Suites + export / import (JS)

Wires the suite controls and file export/import. Suite save/load are already drained in Task 1; this adds the JS controls and the file I/O + localStorage persistence.

**Files:** Modify `index.html`

- [ ] **Step 1: localStorage load at startup + persist on rev change**

In `index.html`'s `<script>`, near the top of the dock section (right after `let lastDockRev = -1;` from Task 2), add:
```javascript
        const LIBRARY_KEY = "jigglefab.library.v1";
        let libraryLoaded = false;
        let lastPersistRev = -1;
        function maybeLoadLibrary() {
            if (libraryLoaded || typeof window.__jigglefabLoadLibrary !== "function") return;
            const saved = localStorage.getItem(LIBRARY_KEY);
            if (saved) window.__jigglefabLoadLibrary(saved);
            libraryLoaded = true;
        }
        function maybePersistLibrary() {
            if (typeof window.__jigglefabGetLibraryRev !== "function") return;
            const rev = window.__jigglefabGetLibraryRev();
            if (rev === lastPersistRev) return;
            lastPersistRev = rev;
            if (typeof window.__jigglefabGetLibraryJson === "function") {
                localStorage.setItem(LIBRARY_KEY, window.__jigglefabGetLibraryJson());
            }
        }
```

In `refreshToolbar()`, just before the dock-render block added in Task 2, add:
```javascript
            maybeLoadLibrary();
            maybePersistLibrary();
```
(Order matters: load before the first persist so a freshly-loaded library isn't immediately overwritten. `maybeLoadLibrary` runs once; `lastPersistRev` starts at -1 and the loaded library's rev is 0, so the first persist writes the loaded value back — idempotent.)

- [ ] **Step 2: Save suite / Load suite controls**

In the `<script>`, after the `dockSaveSel.addEventListener(...)` block from Task 2, add:
```javascript
        document.getElementById("dock-save-suite").addEventListener("click", () => {
            const name = window.prompt("Suite name");
            if (name) window.__jigglefabSaveSuite(name);
        });

        document.getElementById("dock-load-suite").addEventListener("click", () => {
            const names = (window.__jigglefabGetSuiteNames && window.__jigglefabGetSuiteNames()) || [];
            if (!names.length) { window.alert("No saved suites for this chemistry."); return; }
            const name = window.prompt("Load which suite?\n" + names.join("\n"), names[0]);
            if (!name || !names.includes(name)) return;
            const dockCount = (window.__jigglefabGetDock && window.__jigglefabGetDock()
                .filter(d => d.chemistry === ((window.__jigglefabGetChemistryName && window.__jigglefabGetChemistryName()) || "")).length) || 0;
            if (dockCount > 0 && !window.confirm(
                `Load suite "${name}"? This replaces the ${dockCount} device${dockCount === 1 ? "" : "s"} in the dock for this chemistry.`
            )) return;
            window.__jigglefabLoadSuite(name);
        });
```

- [ ] **Step 3: Export — slice one suite out of the library JSON, download**

In the `<script>`, add:
```javascript
        document.getElementById("dock-export").addEventListener("click", () => {
            const names = (window.__jigglefabGetSuiteNames && window.__jigglefabGetSuiteNames()) || [];
            if (!names.length) { window.alert("No saved suites for this chemistry to export."); return; }
            const name = window.prompt("Export which suite?\n" + names.join("\n"), names[0]);
            if (!name) return;
            const lib = JSON.parse(window.__jigglefabGetLibraryJson());
            const suite = (lib.suites || []).find(s => s.name === name);
            if (!suite) { window.alert("Suite not found."); return; }
            const blob = new Blob([JSON.stringify(suite, null, 2)], { type: "application/json" });
            const url = URL.createObjectURL(blob);
            const a = document.createElement("a");
            a.href = url;
            a.download = "jigglefab-suite-" + name + ".json";
            document.body.appendChild(a);
            a.click();
            a.remove();
            URL.revokeObjectURL(url);
        });
```

- [ ] **Step 4: Import — read a file, hand JSON to wasm**

In the `<script>`, add:
```javascript
        const importFile = document.getElementById("dock-import-file");
        document.getElementById("dock-import").addEventListener("click", () => importFile.click());
        importFile.addEventListener("change", () => {
            const file = importFile.files && importFile.files[0];
            if (!file) return;
            const reader = new FileReader();
            reader.onload = () => {
                window.__jigglefabImportSuite(String(reader.result));
                importFile.value = ""; // allow re-importing the same file
            };
            reader.readAsText(file);
        });
```

- [ ] **Step 5: Verify compiles**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cargo check --target wasm32-unknown-unknown`
Expected: compiles (no Rust changed; confirms the tree is still green).

- [ ] **Step 6: Commit**
```bash
git add index.html
git commit -m "feat(web): suites + export/import + localStorage persistence"
```

---

### Task 6: Browser smoke test

Extends `scripts/verify-web.py --editor` to exercise the full flow end-to-end. This is the behavioral gate for the whole web layer.

**Files:** Modify `scripts/verify-web.py`

- [ ] **Step 1: Read the existing `--editor` block**

Read `scripts/verify-web.py` (the `if "--editor" in sys.argv:` block, ~lines 88–200) to match its style: it uses `await page.evaluate(...)`, `await page.wait_for_function(...)`, `await page.mouse.*`, and accepts dialogs via `page.once("dialog", lambda d: d.accept())`. Find where the editor block ends (it leaves the page in a known state) and append the new steps there. Note the `cx, cy` canvas-center variables computed near line 105–112; reuse them if still in scope, else recompute the same way.

- [ ] **Step 2: Add the device-library smoke steps**

Append inside the `if "--editor" in sys.argv:` block (after the existing editor assertions, before it falls through to the end of the block). This assumes Edit mode + Place tool; it resets that explicitly to be self-contained:
```python
            # ---- Device library ----
            print("editor: device library")
            await page.wait_for_function("typeof window.__jigglefabGetDock === 'function'", timeout=10000)
            await page.evaluate("window.__jigglefabSetMode('edit')")
            await page.wait_for_function("window.__jigglefabGetMode() === 'edit'")
            # Clear to a known empty scene.
            if await page.evaluate("window.__jigglefabBeadCount()") > 0:
                page.once("dialog", lambda d: d.accept())
                await page.evaluate("document.getElementById('btn-clear').click()")
                await page.wait_for_function("window.__jigglefabBeadCount() === 0", timeout=2000)

            # Recompute canvas center.
            box = await page.evaluate(
                "() => { const c = document.querySelector('canvas');"
                " const r = c.getBoundingClientRect();"
                " return {x: r.x + r.width/2, y: r.y + r.height/2}; }"
            )
            cx, cy = box["x"], box["y"]

            # Place 3 beads with the Place tool, then Rect-select them.
            await page.evaluate("window.__jigglefabSetTool('place')")
            await page.wait_for_function("window.__jigglefabGetTool() === 'place'")
            for (dx, dy) in [(-20, 0), (0, 0), (20, 0)]:
                await page.mouse.click(cx + dx, cy + dy)
            await page.wait_for_function("window.__jigglefabBeadCount() === 3", timeout=2000)
            await page.evaluate("window.__jigglefabSetTool('rect')")
            await page.wait_for_function("window.__jigglefabGetTool() === 'rect'")
            await page.mouse.move(cx - 60, cy - 40)
            await page.mouse.down()
            await page.mouse.move(cx + 60, cy + 40)
            await page.mouse.up()
            await page.wait_for_function("window.__jigglefabSelectionCount() === 3", timeout=2000)

            # Save selection to the dock (auto-accept the name prompt).
            await page.evaluate("window.__jigglefabSaveToDock('smoke-device')")
            await page.wait_for_function("window.__jigglefabGetDock().length === 1", timeout=2000)
            dock0 = await page.evaluate("window.__jigglefabGetDock()[0]")
            assert dock0["beads"] and len(dock0["beads"]) == 3, f"expected 3 device beads, got {dock0}"

            # Persistence: localStorage should hold the library after the save.
            await page.wait_for_function(
                "() => { const v = localStorage.getItem('jigglefab.library.v1');"
                " return v && JSON.parse(v).dock.length === 1; }", timeout=2000)

            # Arm + stamp: bead count grows by 3, no cross-bonds (isolated).
            dev_id = dock0["id"]
            await page.evaluate(f"window.__jigglefabArmDevice({dev_id})")
            await page.wait_for_function(f"window.__jigglefabArmedId() === {dev_id}", timeout=2000)
            beads_before_stamp = await page.evaluate("window.__jigglefabBeadCount()")
            await page.mouse.click(cx, cy + 80)
            await page.wait_for_function(
                f"window.__jigglefabBeadCount() === {beads_before_stamp + 3}", timeout=2000)
            await page.evaluate("window.__jigglefabDisarm()")
            await page.wait_for_function("window.__jigglefabArmedId() === -1", timeout=2000)

            # Suite save → remove device → load suite restores it.
            await page.evaluate("window.__jigglefabSaveSuite('smoke-suite')")
            await page.wait_for_function(
                "window.__jigglefabGetSuiteNames().includes('smoke-suite')", timeout=2000)
            await page.evaluate(f"window.__jigglefabRemoveDevice({dev_id})")
            await page.wait_for_function("window.__jigglefabGetDock().length === 0", timeout=2000)
            await page.evaluate("window.__jigglefabLoadSuite('smoke-suite')")
            await page.wait_for_function("window.__jigglefabGetDock().length === 1", timeout=2000)

            # Import a device with a bogus state → present but incompatible.
            bad = (
                '{"name":"bad-suite","chemistry":"' 
            )
            chem_now = await page.evaluate("window.__jigglefabGetChemistryName()")
            bad_json = (
                '{"name":"bad-suite","chemistry":"' + chem_now + '",'
                '"devices":[{"id":0,"name":"bogus","chemistry":"' + chem_now + '",'
                '"chemistry_hash":0,"beads":[{"state":"__no_such_state__","pos":[0,0]}],'
                '"bonds":[],"ports":[]}]}'
            )
            await page.evaluate("(j) => window.__jigglefabImportSuite(j)", bad_json)
            await page.wait_for_function(
                "window.__jigglefabGetSuiteNames().includes('bad-suite')", timeout=2000)
            await page.evaluate("window.__jigglefabLoadSuite('bad-suite')")
            await page.wait_for_function(
                "window.__jigglefabGetDock().some(d => d.compatible === false)", timeout=2000)
            print("editor: device library OK")
```
(The unused `bad = (...)` fragment above is a leftover — delete it; only `bad_json` is used. Final code should define `chem_now` then `bad_json` then evaluate.)

- [ ] **Step 3: Clean up the leftover fragment**

Remove the dangling
```python
            bad = (
                '{"name":"bad-suite","chemistry":"' 
            )
```
lines so only `chem_now` and `bad_json` remain. Re-read the block to confirm it's syntactically clean Python.

- [ ] **Step 4: Run the smoke test**

The smoke needs the web app served. Per the project's deploy/verify flow, run it the same way the existing `--editor` smoke is run (e.g. against a local `trunk serve` or the configured `URL`). If a local serve is available:
Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && python scripts/verify-web.py --editor`
Expected: prints "editor: device library OK" and the script exits 0.
If the harness can't serve the web build in this environment, report DONE_WITH_CONCERNS noting the smoke wasn't run here, and flag that the user should run `python scripts/verify-web.py --editor` (or the deploy smoke) to confirm. Do NOT weaken assertions to make it pass.

- [ ] **Step 5: Commit**
```bash
git add scripts/verify-web.py
git commit -m "test(web): device-library smoke (save/arm/stamp/suite/import) in verify-web"
```

---

## Self-Review

**Spec coverage:**
- §1 Rust-owns-data / JS-owns-I/O / rev counter / no new web-sys — Task 1 (snapshot mirror, getters, drain) + Task 5 (JS localStorage). ✅
- §2 library lifecycle in App (`loadLibrary`, `getLibraryJson`, rev bump) — Task 1. ✅
- §3 capture / save-to-dock — Task 1 drain + Task 2 control. ✅
- §4 arm/ghost/place/disarm (Edit only; gestures suspended while armed; Esc/re-click) — Task 3. ✅
- §5 rotation (Shift+scroll + brackets, 15°, ghost vs selection, plain-scroll-zoom-always) — Task 4. ✅
- §6 dock sidebar (left, beads-only thumbnails, actions, chemistry filter, incompatible greying, empty state, rev-driven render, confirms) — Task 2 (+ arm wiring in Task 3). ✅
- §7 suites + export/import — Task 5. ✅
- §8 bridge reference — getters/commands across Task 1 (note: per the design's simplification, commands are fire-and-forget; JS gates invalid calls — Save-selection disabled unless Edit+selection, Load/Export list only known suites, incompatible entries un-armable; import parse errors log a warning). Export is done entirely in JS by slicing the library JSON, so no Rust export getter is needed. ✅
- §9 testing (native covered by plan 1; browser smoke) — Task 6. ✅
- §11 phasing — Tasks 1–6 map 1:1 to the six phases. ✅

**Placeholder scan:** No TBD/TODO. The one intentional "fix this leftover" is Task 6 Step 3, which explicitly instructs removing a dangling fragment — it is a cleanup step, not a placeholder, and the correct final shape is specified.

**Type consistency:** `library`/`armed_device`/`ghost_angle`/`shift_held`/`library_rev` on `App`; `PendingCommands` fields and `Snapshot` fields (`library_json`, `library_rev`, `armed_id: i32`, `dock: Vec<DockEntry>`, `suite_names`) used identically in installers, drain, and snapshot write. Bridge names (`__jigglefabGetDock`, `__jigglefabArmDevice`, `__jigglefabSaveToDock`, `__jigglefabGetLibraryJson`, `__jigglefabGetLibraryRev`, `__jigglefabArmedId`, `__jigglefabGetSuiteNames`, `__jigglefabSaveSuite`, `__jigglefabLoadSuite`, `__jigglefabImportSuite`, `__jigglefabRenameDevice`, `__jigglefabRemoveDevice`, `__jigglefabDisarm`, `__jigglefabLoadLibrary`) match between the Rust installers and the JS callers. `apply_rotation`/`ROTATE_SNAP_RAD` consistent across Task 4. Plan-1 APIs used (`extract_device`, `instantiate_device`, `rotate_selection`, `add_to_dock`, `rename_device`, `remove_device`, `save_suite`, `load_suite`, `import_suite`, `is_compatible_with`, `to_json`, `load_or_default`) match their plan-1 signatures.

**Deviation from spec noted:** the spec's §8 implied some commands return error strings; this plan uses fire-and-forget commands + JS-side gating (simpler, matches the existing bridge), with import parse errors logged. Behavior is equivalent for the gated UI. Documented here and in §8 coverage above.
