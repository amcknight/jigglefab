# Revert to before Run — implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a Revert button that restores the scene captured at the most recent Edit→Run transition, persisting until the next Run.

**Architecture:** App owns an `Option<ScenePayload>` snapshot captured at the Run-arm of `transition_mode` (right before `rebuild_sim_from_scene`). A new `revert_to_snapshot()` method writes it back into the live scene, drops the sim, and forces Edit. Two new JS bridges expose the snapshot's existence and let the toolbar fire the command. Two existing dispatch arms (`clear` and `set_chemistry`) gain a single `self.pre_run_snapshot = None`. UI is a single button next to Clear, polled each frame.

**Tech Stack:** Rust (winit App, wasm-bindgen bridges), HTML/CSS/vanilla JS, Python Playwright for web smoke.

**Spec:** `docs/superpowers/specs/2026-05-27-revert-to-before-run-design.md`.

**Build:** `export PATH="$PATH:/c/Users/thedo/.cargo/bin"` first. Unit tests: `cargo test --lib`. Web smoke: `trunk serve --address 127.0.0.1 --port 8765` in one shell, `python scripts/verify-web.py http://127.0.0.1:8765/ --editor` in another.

---

## File map

- **Modify** `src/editor.rs` — add `ScenePayload` struct + `Scene::capture_payload` / `Scene::restore_payload` + unit tests (Task 1).
- **Modify** `src/app.rs` — add `App.pre_run_snapshot`, capture in `transition_mode`, invalidate in `clear` and `set_chemistry` dispatch arms, add `App::revert_to_snapshot`, extend `PendingCommands` and `web_bridge::Snapshot`, add two `install_window_*` bridges (Tasks 2–4).
- **Modify** `index.html` — `.disabled` CSS, Revert button DOM, click handler, `refreshToolbar` polling (Task 5).
- **Modify** `scripts/verify-web.py` — append revert smoke sequence to the `--editor` block (Task 6).

---

## Task 1: Add ScenePayload + capture/restore on Scene

**Files:**
- Modify: `src/editor.rs`
- Test: `src/editor.rs` (existing `#[cfg(test)] mod tests`)

- [ ] **Step 1.1: Write the failing tests**

Add the following two tests inside the existing `mod tests` block in `src/editor.rs`, alongside the other `Scene` tests (e.g. just below `switch_chemistry_empties_beads`):

```rust
    #[test]
    fn capture_payload_round_trips_through_restore() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        // Tweak fields that should round-trip.
        scene.next_state_idx = 1;
        let payload = scene.capture_payload();

        // Mutate the scene to confirm restore actually overwrites.
        scene.beads.clear();
        scene.bonds.clear();
        scene.next_state_idx = 0;

        scene.restore_payload(&payload);

        assert_eq!(scene.beads.len(), payload.beads.len());
        for (a, b) in scene.beads.iter().zip(payload.beads.iter()) {
            assert_eq!(a.state, b.state);
            assert!((a.pos[0] - b.pos[0]).abs() < 1e-6);
            assert!((a.pos[1] - b.pos[1]).abs() < 1e-6);
        }
        assert_eq!(scene.bonds, payload.bonds);
        assert!((scene.world_size - payload.world_size).abs() < 1e-6);
        assert_eq!(scene.seed, payload.seed);
        assert_eq!(scene.next_state_idx, payload.next_state_idx);
    }

    #[test]
    fn restore_payload_clears_selection() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        let payload = scene.capture_payload();
        scene.selection.insert(0);
        scene.selection.insert(1);
        scene.restore_payload(&payload);
        assert!(scene.selection.is_empty());
    }
```

- [ ] **Step 1.2: Run tests to verify they fail**

```bash
export PATH="$PATH:/c/Users/thedo/.cargo/bin"
cargo test --lib editor::tests::capture_payload_round_trips_through_restore editor::tests::restore_payload_clears_selection 2>&1 | tail -20
```

Expected: compile error — `no method named 'capture_payload' found for struct 'Scene'`.

- [ ] **Step 1.3: Add the `ScenePayload` struct**

In `src/editor.rs`, just after the `Scene` struct definition (the block ending `pub tool: Tool, }` around line 113), add:

```rust
/// Snapshot of the parts of a Scene that round-trip through Revert.
/// Excludes `chemistry`, `selection`, and `tool` — chemistry is held
/// invariant by snapshot invalidation rules (see App), selection is
/// ephemeral, tool is UI state.
#[derive(Debug, Clone)]
pub struct ScenePayload {
    pub chemistry_name: String,
    pub world_size: f32,
    pub seed: u64,
    pub next_state_idx: u32,
    pub beads: Vec<BeadSpec>,
    pub bonds: HashSet<(u32, u32)>,
}
```

- [ ] **Step 1.4: Add the `Scene::capture_payload` and `Scene::restore_payload` methods**

Add the following inside the `impl Scene` block (place near `switch_chemistry`, e.g. just above `pub fn switch_chemistry`):

```rust
    /// Take a snapshot of the round-trippable fields.
    pub fn capture_payload(&self) -> ScenePayload {
        ScenePayload {
            chemistry_name: self.chemistry_name.clone(),
            world_size: self.world_size,
            seed: self.seed,
            next_state_idx: self.next_state_idx,
            beads: self.beads.clone(),
            bonds: self.bonds.clone(),
        }
    }

    /// Overwrite the round-trippable fields from a snapshot and clear
    /// the selection. Leaves `chemistry` and `tool` untouched — see
    /// ScenePayload docs.
    pub fn restore_payload(&mut self, payload: &ScenePayload) {
        self.chemistry_name = payload.chemistry_name.clone();
        self.world_size = payload.world_size;
        self.seed = payload.seed;
        self.next_state_idx = payload.next_state_idx;
        self.beads = payload.beads.clone();
        self.bonds = payload.bonds.clone();
        self.selection.clear();
    }
```

- [ ] **Step 1.5: Run tests to verify they pass**

```bash
cargo test --lib editor 2>&1 | tail -10
```

Expected: `test result: ok. NN passed; 0 failed; ...` — all editor tests including the two new ones pass.

- [ ] **Step 1.6: Commit**

```bash
git add src/editor.rs
git commit -m "$(cat <<'EOF'
feat(editor): ScenePayload + capture/restore methods on Scene

Snapshot of the round-trippable fields (chemistry_name, world_size,
seed, next_state_idx, beads, bonds). restore_payload clears selection.
chemistry and tool intentionally not snapshotted — chemistry is held
invariant by App-side invalidation rules, tool is UI state.

Prep for Revert button in editor toolbar.
EOF
)"
```

---

## Task 2: Add pre_run_snapshot field and capture on Edit→Run

**Files:**
- Modify: `src/app.rs`

- [ ] **Step 2.1: Add `pre_run_snapshot` field to `App`**

In `src/app.rs`, find the `pub struct App {` block (around line 284). Add this field just after the `drag` / `mouse_down` fields and before the `#[cfg(target_arch = "wasm32")] proxy` field:

```rust
    /// Scene payload captured at the most recent Edit→Run transition.
    /// `Some` means Revert is available. Cleared on chemistry switch or Clear.
    pre_run_snapshot: Option<crate::editor::ScenePayload>,
```

- [ ] **Step 2.2: Initialize the field in `App::new`**

In the `impl App { pub fn new() -> Self { Self { ... } } }` block (around line 302), add the field initializer alongside the others, e.g. just above the `#[cfg(target_arch = "wasm32")] proxy:` line:

```rust
            pre_run_snapshot: None,
```

- [ ] **Step 2.3: Capture the snapshot in `transition_mode` Run arm**

Find `fn transition_mode(&mut self, ...)` (around line 491). In the `Mode::Run` arm, right before the `self.rebuild_sim_from_scene();` call, add the capture. The current arm reads:

```rust
            crate::editor::Mode::Run => {
                if let Some(scene) = self.scene.as_mut() {
                    scene.selection.clear();
                }
                self.drag = crate::editor::DragState::None;
                self.mouse_down = false;
                if self.scene.is_some() {
                    self.rebuild_sim_from_scene();
                    self.mode = crate::editor::Mode::Run;
                }
            }
```

Change it to:

```rust
            crate::editor::Mode::Run => {
                if let Some(scene) = self.scene.as_mut() {
                    scene.selection.clear();
                }
                self.drag = crate::editor::DragState::None;
                self.mouse_down = false;
                if let Some(scene) = self.scene.as_ref() {
                    self.pre_run_snapshot = Some(scene.capture_payload());
                }
                if self.scene.is_some() {
                    self.rebuild_sim_from_scene();
                    self.mode = crate::editor::Mode::Run;
                }
            }
```

- [ ] **Step 2.4: Invalidate the snapshot in `clear` and `set_chemistry` dispatch arms**

In the `WindowEvent::RedrawRequested` arm (around line 681), find the `set_chemistry` arm of the command-dispatch (around line 700–716) and add `self.pre_run_snapshot = None;` inside the success branch, after the `self.sim = None;` line:

Existing:
```rust
                    if let Some(name) = new_chemistry {
                        if let Ok(new_chem) = crate::editor::load_chemistry_by_name(&name) {
                            if let Some(scene) = self.scene.as_mut() {
                                scene.switch_chemistry(new_chem, name);
                            }
                            self.sim = None;
                            self.mode = crate::editor::Mode::Edit;
                            ...
```

Updated:
```rust
                    if let Some(name) = new_chemistry {
                        if let Ok(new_chem) = crate::editor::load_chemistry_by_name(&name) {
                            if let Some(scene) = self.scene.as_mut() {
                                scene.switch_chemistry(new_chem, name);
                            }
                            self.sim = None;
                            self.pre_run_snapshot = None;
                            self.mode = crate::editor::Mode::Edit;
                            ...
```

Then find the `if clear_scene {` arm (around line 722) and add the same line after `self.sim = None;`:

Existing:
```rust
                    if clear_scene {
                        if let Some(scene) = self.scene.as_mut() {
                            scene.clear();
                        }
                        self.sim = None;
                        self.mode = crate::editor::Mode::Edit;
                        self.drag = crate::editor::DragState::None;
                        self.mouse_down = false;
                    }
```

Updated:
```rust
                    if clear_scene {
                        if let Some(scene) = self.scene.as_mut() {
                            scene.clear();
                        }
                        self.sim = None;
                        self.pre_run_snapshot = None;
                        self.mode = crate::editor::Mode::Edit;
                        self.drag = crate::editor::DragState::None;
                        self.mouse_down = false;
                    }
```

- [ ] **Step 2.5: Verify the build**

```bash
cargo build --lib 2>&1 | tail -10
cargo test --lib 2>&1 | tail -5
```

Expected: build OK, all tests pass (no behavioral change yet — snapshot is captured but unused).

- [ ] **Step 2.6: Commit**

```bash
git add src/app.rs
git commit -m "$(cat <<'EOF'
feat(app): capture pre-Run scene snapshot on Edit→Run

App gains pre_run_snapshot: Option<ScenePayload>, populated in
transition_mode's Run arm before rebuild_sim_from_scene. Cleared in
the clear_scene and set_chemistry dispatch arms so the snapshot can't
outlive the chemistry it was captured under.

Snapshot is captured but not yet exposed — Revert command + bridges
land in follow-up commits.
EOF
)"
```

---

## Task 3: Add revert command and `App::revert_to_snapshot`

**Files:**
- Modify: `src/app.rs`

- [ ] **Step 3.1: Add `revert` to `PendingCommands` and `can_revert` to `Snapshot`**

In `src/app.rs`, find the `web_bridge` module (around lines 22–46). Update both structs:

```rust
    #[derive(Default, Clone)]
    pub struct PendingCommands {
        pub set_mode: Option<crate::editor::Mode>,
        pub set_edit_state: Option<u32>,
        pub set_chemistry: Option<String>,
        pub set_tool: Option<crate::editor::Tool>,
        pub clear: bool,
        pub revert: bool,
    }
```

```rust
    #[derive(Default, Clone)]
    pub struct Snapshot {
        pub mode: &'static str,
        pub bead_count: u32,
        pub palette: Vec<(String, [f32; 3])>,
        pub tool: &'static str,
        pub selection_count: u32,
        pub chemistry_name: String,
        pub can_revert: bool,
    }
```

- [ ] **Step 3.2: Add `App::revert_to_snapshot` method**

Add this method to `impl App`, placed just below `fn rebuild_sim_from_scene` (around line 368):

```rust
    fn revert_to_snapshot(&mut self) {
        let Some(payload) = self.pre_run_snapshot.as_ref() else { return };
        if let Some(scene) = self.scene.as_mut() {
            scene.restore_payload(payload);
        }
        self.sim = None;
        self.mode = crate::editor::Mode::Edit;
        self.drag = crate::editor::DragState::None;
        self.mouse_down = false;
        if let (Some(renderer), Some(scene)) = (self.renderer.as_mut(), self.scene.as_ref()) {
            let palette: Vec<[f32; 3]> = scene.chemistry.colors.clone();
            renderer.update_camera(scene.world_size, &palette);
        }
    }
```

- [ ] **Step 3.3: Dispatch the revert command each frame**

In the `WindowEvent::RedrawRequested` arm (around line 687), the existing tuple destructure pulls `clr` (clear) out separately because it's a bool. Add the same treatment for `revert`. The existing block is:

```rust
                    let (new_mode, edit_state, new_chemistry, new_tool, clear_scene) = web_bridge::COMMANDS.with(|c| {
                        let mut cmds = c.borrow_mut();
                        let clr = std::mem::replace(&mut cmds.clear, false);
                        (cmds.set_mode.take(), cmds.set_edit_state.take(), cmds.set_chemistry.take(), cmds.set_tool.take(), clr)
                    });
```

Change it to:

```rust
                    let (new_mode, edit_state, new_chemistry, new_tool, clear_scene, revert) = web_bridge::COMMANDS.with(|c| {
                        let mut cmds = c.borrow_mut();
                        let clr = std::mem::replace(&mut cmds.clear, false);
                        let rev = std::mem::replace(&mut cmds.revert, false);
                        (cmds.set_mode.take(), cmds.set_edit_state.take(), cmds.set_chemistry.take(), cmds.set_tool.take(), clr, rev)
                    });
```

Then, immediately after the existing `if clear_scene { ... }` block (around line 730), add:

```rust
                    if revert {
                        self.revert_to_snapshot();
                    }
```

- [ ] **Step 3.4: Populate `can_revert` in the per-frame Snapshot write**

Find the snapshot-write block (around line 779). The existing call constructs a `Snapshot` literal. Add `can_revert` to it:

Existing:
```rust
                    let chemistry_name = self.scene.as_ref().map(|s| s.chemistry_name.clone()).unwrap_or_default();
                    web_bridge::SNAPSHOT.with(|s| {
                        *s.borrow_mut() = web_bridge::Snapshot {
                            mode: mode_str,
                            bead_count,
                            palette,
                            tool: tool_str,
                            selection_count,
                            chemistry_name,
                        };
                    });
```

Updated:
```rust
                    let chemistry_name = self.scene.as_ref().map(|s| s.chemistry_name.clone()).unwrap_or_default();
                    let can_revert = self.pre_run_snapshot.is_some();
                    web_bridge::SNAPSHOT.with(|s| {
                        *s.borrow_mut() = web_bridge::Snapshot {
                            mode: mode_str,
                            bead_count,
                            palette,
                            tool: tool_str,
                            selection_count,
                            chemistry_name,
                            can_revert,
                        };
                    });
```

- [ ] **Step 3.5: Verify the build**

```bash
cargo build --lib 2>&1 | tail -10
cargo test --lib 2>&1 | tail -5
```

Expected: build OK, all tests pass.

- [ ] **Step 3.6: Commit**

```bash
git add src/app.rs
git commit -m "$(cat <<'EOF'
feat(app): revert command + App::revert_to_snapshot

Adds PendingCommands.revert and Snapshot.can_revert, plus
App::revert_to_snapshot that writes the captured ScenePayload back
into the scene, drops sim, forces Edit. Dispatched each frame
alongside the existing clear arm.

JS bridges land next.
EOF
)"
```

---

## Task 4: Install the JS bridges

**Files:**
- Modify: `src/app.rs`

- [ ] **Step 4.1: Add `install_window_revert`**

Add this function in `src/app.rs` next to `install_window_clear` (around line 272). Both are `#[cfg(target_arch = "wasm32")]`:

```rust
#[cfg(target_arch = "wasm32")]
fn install_window_revert() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().revert = true);
    }) as Box<dyn Fn()>);
    expose_to_window!("__jigglefabRevert", cb);
}
```

- [ ] **Step 4.2: Add `install_window_can_revert`**

Add this just below `install_window_revert`:

```rust
#[cfg(target_arch = "wasm32")]
fn install_window_can_revert() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> bool {
        web_bridge::SNAPSHOT.with(|s| s.borrow().can_revert)
    }) as Box<dyn Fn() -> bool>);
    expose_to_window!("__jigglefabCanRevert", cb);
}
```

- [ ] **Step 4.3: Call both installers from `resumed`**

Find the block of `install_window_*();` calls in `resumed` (around line 605–614, just below `install_window_clear();`). Add both new installers immediately after `install_window_clear();`:

```rust
            install_window_clear();
            install_window_revert();
            install_window_can_revert();
```

- [ ] **Step 4.4: Verify the wasm build**

```bash
cargo build --target wasm32-unknown-unknown --release 2>&1 | tail -10
```

Expected: build OK with the existing benign warnings only. If `wasm32-unknown-unknown` target isn't installed: `rustup target add wasm32-unknown-unknown`.

- [ ] **Step 4.5: Commit**

```bash
git add src/app.rs
git commit -m "$(cat <<'EOF'
feat(bridge): __jigglefabRevert + __jigglefabCanRevert

Two new window globals so the editor toolbar can fire the revert
command and reflect whether a snapshot exists. Matches the
install_window_clear / install_window_get_mode pattern.
EOF
)"
```

---

## Task 5: Revert button in the editor toolbar

**Files:**
- Modify: `index.html`

- [ ] **Step 5.1: Add `.disabled` CSS for editor-toolbar buttons**

In `index.html`, find the editor-toolbar style block (around line 144). Add a `.disabled` rule alongside the existing `a:hover` and `a.active` rules:

Existing:
```css
        #editor-toolbar a:hover { background: rgba(255, 255, 255, 0.12); }
        #editor-toolbar a.active { background: #2a4d3a; color: #fff; border-color: #4a8; }
```

Append a new rule directly below them:

```css
        #editor-toolbar a.disabled {
            opacity: 0.4;
            cursor: default;
            pointer-events: none;
        }
```

(`pointer-events: none` is enough to neutralise hover + click without re-implementing both behaviours.)

- [ ] **Step 5.2: Add the Revert button to the mode row**

Find the mode row in `#editor-toolbar` (around line 201):

```html
        <div class="row">
            <span class="group-label">mode</span>
            <a id="btn-edit" class="mode">Edit</a>
            <a id="btn-run" class="mode">Run</a>
            <a id="btn-clear">Clear</a>
        </div>
```

Add the Revert button after Clear:

```html
        <div class="row">
            <span class="group-label">mode</span>
            <a id="btn-edit" class="mode">Edit</a>
            <a id="btn-run" class="mode">Run</a>
            <a id="btn-clear">Clear</a>
            <a id="btn-revert" class="disabled">Revert</a>
        </div>
```

(Starts disabled — no snapshot at page load.)

- [ ] **Step 5.3: Wire the click handler**

Find the existing Clear-button handler block (around line 457):

```javascript
        document.getElementById("btn-clear").addEventListener("click", (e) => {
            e.preventDefault();
            if (!window.__jigglefabClear) return;
            const beads = (window.__jigglefabBeadCount && window.__jigglefabBeadCount()) || 0;
            if (beads > 0 && !window.confirm(`Clear scene? This removes ${beads} bead${beads === 1 ? "" : "s"}.`)) return;
            window.__jigglefabClear();
        });
```

Immediately below it, add the Revert handler:

```javascript
        // Revert button. No-op when disabled (snapshot absent). Confirms
        // before discarding a non-empty scene, mirroring Clear's pattern.
        document.getElementById("btn-revert").addEventListener("click", (e) => {
            e.preventDefault();
            const btn = document.getElementById("btn-revert");
            if (btn.classList.contains("disabled")) return;
            if (!window.__jigglefabRevert) return;
            const beads = (window.__jigglefabBeadCount && window.__jigglefabBeadCount()) || 0;
            if (beads > 0 && !window.confirm(
                `Revert to pre-Run scene? This discards ${beads} bead${beads === 1 ? "" : "s"}.`
            )) return;
            window.__jigglefabRevert();
        });
```

- [ ] **Step 5.4: Poll `__jigglefabCanRevert` in `refreshToolbar`**

Find `refreshToolbar` (around line 488). Just below the chemistry-pill block (added in the prior chemistry-fix commit), add a Revert-state poll. The current end of the function reads:

```javascript
            if (typeof window.__jigglefabGetTool === "function") {
                paintToolButtons(window.__jigglefabGetTool());
            }
            const selEl = document.getElementById("hud-sel");
            if (typeof window.__jigglefabSelectionCount === "function" && selEl) {
                selEl.textContent = window.__jigglefabSelectionCount();
            }
            requestAnimationFrame(refreshToolbar);
```

Insert the Revert poll right before the `selEl` block:

```javascript
            if (typeof window.__jigglefabGetTool === "function") {
                paintToolButtons(window.__jigglefabGetTool());
            }
            const revertBtn = document.getElementById("btn-revert");
            if (revertBtn && typeof window.__jigglefabCanRevert === "function") {
                revertBtn.classList.toggle("disabled", !window.__jigglefabCanRevert());
            }
            const selEl = document.getElementById("hud-sel");
            if (typeof window.__jigglefabSelectionCount === "function" && selEl) {
                selEl.textContent = window.__jigglefabSelectionCount();
            }
            requestAnimationFrame(refreshToolbar);
```

- [ ] **Step 5.5: Drive the page manually to verify behavior**

Start the dev server:

```bash
export PATH="$PATH:/c/Users/thedo/.cargo/bin"
trunk serve --address 127.0.0.1 --port 8765 --no-autoreload
```

Open `http://127.0.0.1:8765/` in Chrome. Verify by inspection:

1. On load, the Revert button is greyed out (opacity 0.4, no hover).
2. Click Run. Revert becomes solid/enabled.
3. Click Edit. Revert stays enabled.
4. Click Revert. The scene returns to the chain layout you had at the moment of Run; mode flips to Edit. Revert stays enabled.
5. Click Clear. Confirm. Revert greys out again.
6. Run, then switch chemistry from `wire` to `grey` and confirm. Revert greys out (snapshot invalidated).

Stop trunk (Ctrl+C in its terminal) after verifying.

- [ ] **Step 5.6: Commit**

```bash
git add index.html
git commit -m "$(cat <<'EOF'
feat(editor): Revert button restores pre-Run scene

Sits next to Clear in the mode row. Reads __jigglefabCanRevert each
frame to toggle a .disabled class (opacity + pointer-events: none).
Confirms before discarding a non-empty scene.
EOF
)"
```

---

## Task 6: Web smoke for the revert round-trip

**Files:**
- Modify: `scripts/verify-web.py`

- [ ] **Step 6.1: Append revert assertions to the `--editor` block**

In `scripts/verify-web.py`, find the end of the `if "--editor" in sys.argv:` block — specifically the line:

```python
            console_lines.append("[editor] extended smoke test passed")
```

Replace that line with the revert smoke sequence followed by the same log line:

```python
            # --- Revert: snapshot at Edit→Run is restored, persists across reverts,
            # and is invalidated by Clear. ---
            # Establish a known scene: drop into Edit, place one bead.
            await page.evaluate("window.__jigglefabSetMode('edit')")
            await page.wait_for_function("window.__jigglefabGetMode() === 'edit'", timeout=2000)
            await page.evaluate("window.__jigglefabSetTool('place')")
            beads_pre_run = await page.evaluate("window.__jigglefabBeadCount()")
            await page.mouse.click(cx + 30, cy - 30)
            await page.wait_for_function(
                f"window.__jigglefabBeadCount() === {beads_pre_run + 1}", timeout=2000)
            snap_count = beads_pre_run + 1

            # Run, let the sim mutate, then Revert.
            await page.evaluate("window.__jigglefabSetMode('run')")
            await page.wait_for_function("window.__jigglefabGetMode() === 'run'", timeout=2000)
            await page.wait_for_function("window.__jigglefabCanRevert() === true", timeout=2000)
            await page.wait_for_timeout(300)

            await page.evaluate("window.__jigglefabRevert()")
            await page.wait_for_function("window.__jigglefabGetMode() === 'edit'", timeout=2000)
            assert await page.evaluate("window.__jigglefabBeadCount()") == snap_count, \
                "revert did not restore the pre-Run bead count"
            assert await page.evaluate("window.__jigglefabCanRevert()") is True, \
                "snapshot should persist across reverts"

            # Clear should invalidate the snapshot.
            page.once("dialog", lambda d: d.accept())
            await page.evaluate(
                "document.getElementById('btn-clear').click()"
            )
            await page.wait_for_function("window.__jigglefabCanRevert() === false", timeout=2000)

            console_lines.append("[editor] extended smoke test passed")
```

- [ ] **Step 6.2: Run the smoke**

In one shell:

```bash
export PATH="$PATH:/c/Users/thedo/.cargo/bin"
trunk serve --address 127.0.0.1 --port 8765 --no-autoreload
```

Wait until trunk reports `INFO success`. In another shell:

```bash
python scripts/verify-web.py http://127.0.0.1:8765/ --editor
```

Expected: the printout's `Console:` section ends with `[editor] extended smoke test passed`. The Per-timepoint snapshot SHA-256 lines are not all-identical (the sim is rendering).

Stop trunk (Ctrl+C).

- [ ] **Step 6.3: Commit**

```bash
git add scripts/verify-web.py
git commit -m "$(cat <<'EOF'
test(web): editor smoke covers revert round-trip + invalidation

Places a bead, runs, reverts, asserts bead count and mode restored
and snapshot persists. Then Clear and assert the snapshot is gone.
EOF
)"
```

---

## Done. Bundle for deploy.

After all six tasks land, the chemistry-fix commit (already on `main`) plus the six new commits are ready to push to the `web` branch when you want the Revert button live. (Main is push-blocked per project convention; `web` triggers the deploy via GitHub Actions.)
