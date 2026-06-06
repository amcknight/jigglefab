# Editor Fade-on-Zoom Adaptive Grid Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the always-on torus seam line with an adaptive scale grid that fades in while the camera zooms/pans and fades out when it settles.

**Architecture:** A pure `grid_segments` generator (camera.rs) emits weighted grid lines whose spacing adapts to zoom. The app animates a `grid_alpha` from per-frame `dt` (reset on camera activity), multiplies it into the overlay's per-vertex `shade`, and skips the grid entirely at rest. Reuses the overlay pipeline's `shade` from the torus work.

**Tech Stack:** Rust, `glam`, `winit`, `web_time::Instant`, `wgpu` overlay pipeline.

**Build/test commands** (cargo not on default Bash PATH):
```bash
export PATH="$PATH:/c/Users/thedo/.cargo/bin"
cd /c/Users/thedo/git/jigglefab
cargo test --lib                                   # unit tests
cargo check --lib                                  # native type-check (NOT cargo build --lib — debug link quirk)
cargo check --lib --target wasm32-unknown-unknown  # web type-check
```

---

## File Structure
- **`src/camera.rs`** — add `grid_segments` (adaptive, weighted) + `world_per_px`; later remove the superseded `seam_segments`.
- **`src/app.rs`** — fade state (`grid_alpha`/`idle_since`), `note_camera_activity`, pure `grid_fade_step` + constants, per-frame fade step, grid-aware `overlay_segments`, `__jigglefabGridAlpha` bridge.
- **`scripts/verify-web.py`** — grid-fade smoke assertion.

Each task compiles and stays green: Task 1 adds the new camera fns (seam_segments still used). Task 2 switches the app to the grid + fade and removes seam_segments. Task 3 adds the bridge + smoke.

---

## Task 1: Camera — `grid_segments` + `world_per_px`

Pure math. Adds the adaptive weighted grid generator and the world-units-per-pixel helper. `seam_segments` stays (app still calls it until Task 2).

**Files:**
- Modify: `src/camera.rs`
- Test: `src/camera.rs` (`#[cfg(test)]`)

- [ ] **Step 1: Write the failing tests**

Add to `src/camera.rs`'s `mod tests` (reuse the existing `WS` const = 128.0):
```rust
    #[test]
    fn grid_segments_level0_only_boundary_lines() {
        // Large world_per_px ⇒ ideal≈1 ⇒ level 0 ⇒ spacing = world_size ⇒ only
        // the domain-boundary lines (x=0, x=128 / y=0, y=128), all weight 1.0.
        // ideal = WS / (80 * wpp); wpp = 1.6 ⇒ ideal = 1.0 ⇒ level 0.
        let segs = grid_segments(Vec2::new(-10.0, -10.0), Vec2::new(138.0, 138.0), WS, 1.6);
        assert_eq!(segs.len(), 8, "got {:?}", segs); // 2 vertical + 2 horizontal × 2 verts
        assert!(segs.iter().all(|(_, w)| (*w - 1.0).abs() < 1e-6), "all boundary weight 1.0");
        assert!(segs.iter().any(|(p, _)| p[0] == 0.0));
        assert!(segs.iter().any(|(p, _)| p[0] == 128.0));
    }

    #[test]
    fn grid_segments_subdivides_when_zoomed_in() {
        // Small world_per_px ⇒ ideal = WS/(80*0.2) = 8 ⇒ level 3 ⇒ spacing = 16.
        let segs = grid_segments(Vec2::new(0.0, 0.0), Vec2::new(128.0, 128.0), WS, 0.2);
        // A boundary line (x=0 or x=128) has weight 1.0; an interior line (x=16) weight 0.5.
        assert!(segs.iter().any(|(p, w)| p[0] == 0.0 && (*w - 1.0).abs() < 1e-6), "boundary 1.0");
        assert!(segs.iter().any(|(p, w)| p[0] == 16.0 && (*w - 0.5).abs() < 1e-6), "interior 0.5");
        // spacing divides world_size: every emitted x is a multiple of 16.
        assert!(segs.iter().all(|(p, _)| (p[0] % 16.0).abs() < 1e-3 || (16.0 - (p[0] % 16.0)).abs() < 1e-3));
    }

    #[test]
    fn grid_segments_level_clamps_for_extreme_zoom() {
        // Without the MAX_LEVEL=6 clamp this tiny world_per_px would push the level
        // to ~14 (spacing ≈ 0.008) and emit hundreds of lines over the 2-unit view.
        // Clamped to level 6 (spacing = world_size/64 = 2) it stays a handful.
        let segs = grid_segments(Vec2::new(0.0, 0.0), Vec2::new(2.0, 2.0), WS, 0.0001);
        assert!(segs.len() <= 12, "level not clamped, too many lines: {}", segs.len());
    }

    #[test]
    fn world_per_px_matches_extent_over_width() {
        let cam = Camera { zoom: 2.0, center: Vec2::new(64.0, 64.0) };
        let wpp = cam.world_per_px((800, 800), WS);
        // square viewport, zoom 2 ⇒ visible.x = WS/2 = 64 ⇒ wpp = 64/800.
        assert!((wpp - 64.0 / 800.0).abs() < 1e-6, "wpp {wpp}");
    }
```

- [ ] **Step 2: Run them to verify failure**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cd /c/Users/thedo/git/jigglefab && cargo test --lib camera::tests::grid_segments`
Expected: FAIL — `cannot find function grid_segments` / `world_per_px`.

- [ ] **Step 3: Implement**

Add `world_per_px` to `impl Camera` (near `visible_world_rect`):
```rust
    /// World units per physical pixel along x (drives adaptive grid spacing).
    pub fn world_per_px(&self, viewport: (u32, u32), world_size: f32) -> f32 {
        let vis = self.visible_extent(viewport, world_size);
        vis.x / viewport.0.max(1) as f32
    }
```
Add the free `grid_segments` fn (next to `seam_segments`):
```rust
/// Adaptive scale-grid lines for the visible rect `[min, max]`, as LineList
/// vertex pairs each tagged with a weight: 1.0 on world-tile boundaries
/// (multiples of `world_size`), 0.5 on interior subdivision lines. Spacing snaps
/// to `world_size / 2^level` so cells stay near `TARGET_PX` on screen and always
/// align to the domain. Pure — `world_per_px` is the only zoom input.
pub fn grid_segments(min: Vec2, max: Vec2, world_size: f32, world_per_px: f32) -> Vec<([f32; 2], f32)> {
    const TARGET_PX: f32 = 80.0;
    const MAX_LEVEL: i32 = 6;
    let wpp = world_per_px.max(1e-9);
    let ideal = world_size / (TARGET_PX * wpp);
    let level = (ideal.max(1e-9).log2().round() as i32).clamp(0, MAX_LEVEL);
    let spacing = world_size / 2f32.powi(level);
    let weight = |c: f32| -> f32 {
        let r = (c / world_size).round();
        if (c - r * world_size).abs() < spacing * 1e-3 { 1.0 } else { 0.5 }
    };
    let mut segs = Vec::new();
    let fx = (min.x / spacing).ceil() as i32;
    let lx = (max.x / spacing).floor() as i32;
    for k in fx..=lx {
        let x = k as f32 * spacing;
        let w = weight(x);
        segs.push(([x, min.y], w));
        segs.push(([x, max.y], w));
    }
    let fy = (min.y / spacing).ceil() as i32;
    let ly = (max.y / spacing).floor() as i32;
    for k in fy..=ly {
        let y = k as f32 * spacing;
        let w = weight(y);
        segs.push(([min.x, y], w));
        segs.push(([max.x, y], w));
    }
    segs
}
```

- [ ] **Step 4: Run to verify pass**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cd /c/Users/thedo/git/jigglefab && cargo test --lib camera && cargo check --lib && cargo check --lib --target wasm32-unknown-unknown`
Expected: all camera tests pass (the 4 new + existing, including the still-present `seam_segments_*`); both checks clean.

- [ ] **Step 5: Commit**
```bash
git add src/camera.rs
git commit -m "feat(camera): adaptive weighted grid_segments + world_per_px"
```

---

## Task 2: App — fade state + render the grid (replace seam line)

Animate `grid_alpha` from camera activity and render the adaptive grid through the overlay, faded. Removes the always-on `seam_segments` path.

**Files:**
- Modify: `src/app.rs`
- Modify: `src/camera.rs` (remove `seam_segments` + its tests, now unused)
- Test: `src/app.rs` (`#[cfg(test)]` for `grid_fade_step`)

- [ ] **Step 1: Write the failing test for `grid_fade_step`**

In `src/app.rs`, add a test module at the end of the file (if one already exists, add into it):
```rust
#[cfg(test)]
mod fade_tests {
    use super::{grid_fade_step, GRID_FADE_IN_S, GRID_HOLD_S};

    #[test]
    fn fades_in_while_active() {
        // idle_since < HOLD ⇒ target 1.0; one fade-in time-constant ⇒ ~0.63.
        let a = grid_fade_step(0.0, 0.0, GRID_FADE_IN_S);
        assert!((a - (1.0 - (-1.0f32).exp())).abs() < 1e-3, "got {a}");
        // repeated steps keep rising toward 1.0
        let mut alpha = a;
        for _ in 0..30 { alpha = grid_fade_step(alpha, 0.0, 0.016); }
        assert!(alpha > 0.95, "should be nearly full: {alpha}");
    }

    #[test]
    fn fades_out_when_idle() {
        let mut alpha = 1.0;
        for _ in 0..120 { alpha = grid_fade_step(alpha, GRID_HOLD_S + 1.0, 0.016); }
        assert!(alpha < 0.05, "should fade out: {alpha}");
    }

    #[test]
    fn large_dt_does_not_overshoot() {
        // A huge frame gap snaps toward target without passing it.
        let a = grid_fade_step(0.0, 0.0, 100.0);
        assert!(a <= 1.0 + 1e-6 && a > 0.99, "no overshoot, near target: {a}");
        let b = grid_fade_step(1.0, GRID_HOLD_S + 1.0, 100.0);
        assert!(b >= -1e-6 && b < 0.01, "no overshoot below 0: {b}");
    }
}
```

- [ ] **Step 2: Run to verify failure**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cd /c/Users/thedo/git/jigglefab && cargo test --lib fade_tests`
Expected: FAIL — `grid_fade_step` / consts not found.

- [ ] **Step 3: Add the fade helper + constants (module level in `src/app.rs`)**

Add near the top of `src/app.rs` (module level, after the `use` lines):
```rust
/// Seconds the grid stays fully in after the last camera move before fading.
const GRID_HOLD_S: f32 = 0.4;
const GRID_FADE_IN_S: f32 = 0.12;
const GRID_FADE_OUT_S: f32 = 0.40;

/// Advance the grid fade one frame (frame-rate independent exponential ease).
/// `idle_since` = seconds since the last zoom/pan. Returns the new alpha in [0,1].
fn grid_fade_step(alpha: f32, idle_since: f32, dt: f32) -> f32 {
    let target = if idle_since < GRID_HOLD_S { 1.0 } else { 0.0 };
    let tau = if target > alpha { GRID_FADE_IN_S } else { GRID_FADE_OUT_S };
    let k = 1.0 - (-dt / tau.max(1e-6)).exp();
    alpha + (target - alpha) * k
}
```

- [ ] **Step 4: Add `App` fields + init + `note_camera_activity`**

In the `App` struct, after `pan_button`, add:
```rust
    /// Adaptive grid fade: current alpha and seconds since the last camera move.
    grid_alpha: f32,
    idle_since: f32,
```
In `App::new()`, after `pan_button: None,`:
```rust
            grid_alpha: 0.0,
            idle_since: 1.0e9, // start fully idle (grid hidden)
```
Add a method to `impl App` (near `refresh_camera`):
```rust
    /// Mark a camera move (zoom/pan) so the scale grid fades in this frame.
    fn note_camera_activity(&mut self) {
        self.idle_since = 0.0;
    }
```

- [ ] **Step 5: Call `note_camera_activity` from the camera gestures**

In the `WindowEvent::MouseWheel` arm, after `self.refresh_camera();` (inside the `if let Some(viewport)` block):
```rust
                        self.refresh_camera();
                        self.note_camera_activity();
```
In `WindowEvent::CursorMoved`'s pan branch, after the pan `self.refresh_camera();`:
```rust
                        self.refresh_camera();
                        self.note_camera_activity(); // pan trigger — delete this line for zoom-only
```

- [ ] **Step 6: Step the fade once per frame (start of `RedrawRequested`)**

In the `WindowEvent::RedrawRequested` arm, immediately after `let Some(window_arc) = self.window.clone() else { return };` and BEFORE the `#[cfg(target_arch = "wasm32")]` command block, insert:
```rust
                // Advance the adaptive-grid fade from this frame's dt (last_frame
                // is reset at the end of this arm). Runs in Run and Edit.
                let grid_dt = self.last_frame.elapsed().as_secs_f32();
                self.idle_since += grid_dt;
                self.grid_alpha = grid_fade_step(self.grid_alpha, self.idle_since, grid_dt);
```

- [ ] **Step 7: Render the faded grid in `overlay_segments`; remove `SEAM_SHADE`**

Replace the `const SEAM_SHADE: f32 = 0.25;` with:
```rust
    /// Peak shade of the fully-faded-in scale grid (boundary lines); interior
    /// subdivision lines get half. The overlay fragment also multiplies by 0.7.
    const GRID_SHADE: f32 = 0.35;
```
Replace the seam-grid portion of `overlay_segments` (the `if let Some(viewport)` block that loops over `seam_segments`) with the faded adaptive grid:
```rust
    fn overlay_segments(&self) -> Vec<crate::render::OverlayVertex> {
        use crate::render::OverlayVertex;
        let mut out: Vec<OverlayVertex> = Vec::new();
        // Adaptive scale grid, faded by camera activity. Skipped entirely at rest.
        if self.grid_alpha > 0.001 {
            if let Some(viewport) = self.viewport() {
                let ws = self.world_size();
                let (min, max) = self.camera.visible_world_rect(viewport, ws);
                let wpp = self.camera.world_per_px(viewport, ws);
                for (pos, weight) in crate::camera::grid_segments(min, max, ws, wpp) {
                    out.push(OverlayVertex { pos, shade: self.grid_alpha * weight * Self::GRID_SHADE });
                }
            }
        }
        // Drag overlay (bright).
        let bright = |p: [f32; 2]| OverlayVertex { pos: p, shade: 1.0 };
        match &self.drag {
            crate::editor::DragState::Rect { anchor, current, .. } => {
                let (a, b) = (*anchor, *current);
                let (xmin, xmax) = if a.x <= b.x { (a.x, b.x) } else { (b.x, a.x) };
                let (ymin, ymax) = if a.y <= b.y { (a.y, b.y) } else { (b.y, a.y) };
                for p in [
                    [xmin, ymin], [xmax, ymin],
                    [xmax, ymin], [xmax, ymax],
                    [xmax, ymax], [xmin, ymax],
                    [xmin, ymax], [xmin, ymin],
                ] {
                    out.push(bright(p));
                }
            }
            crate::editor::DragState::Lasso { points } => {
                if points.len() >= 2 {
                    for w in points.windows(2) {
                        out.push(bright([w[0].x, w[0].y]));
                        out.push(bright([w[1].x, w[1].y]));
                    }
                    let last = points[points.len() - 1];
                    let first = points[0];
                    out.push(bright([last.x, last.y]));
                    out.push(bright([first.x, first.y]));
                }
            }
            _ => {}
        }
        out
    }
```

- [ ] **Step 8: Remove the now-unused `seam_segments` from `src/camera.rs`**

Delete the `seam_segments` free fn and its two tests (`seam_segments_covers_boundaries_in_view`, `seam_segments_fit_view_shows_outer_box`). Confirm nothing else references it: `rg 'seam_segments' src/` → empty.

- [ ] **Step 9: Build, test, and a quick native run**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cd /c/Users/thedo/git/jigglefab && cargo test --lib && cargo check --lib && cargo check --lib --target wasm32-unknown-unknown`
Expected: clean; `fade_tests` + all camera/editor tests pass; no `seam_segments` / `SEAM_SHADE` / dead-code-on-`grid_alpha` warnings (grid_alpha is read in overlay_segments and written each frame).

Optional native run (if a GUI is available): `cargo run --release` — wheel-zoom and watch a faint grid fade in then out; pan and watch the same; at rest the view is clean. If no GUI, say so; the browser smoke (Task 3) is the behavioral gate.

- [ ] **Step 10: Commit**
```bash
git add src/app.rs src/camera.rs
git commit -m "feat(editor): fade-on-zoom adaptive scale grid (replaces seam line)"
```

---

## Task 3: `__jigglefabGridAlpha` bridge + browser smoke

Expose the grid alpha and assert the fade in/out on the deployed path.

**Files:**
- Modify: `src/app.rs` (Snapshot field + populate + installer + init call)
- Modify: `scripts/verify-web.py`

- [ ] **Step 1: Add the bridge**

(a) In `web_bridge::Snapshot`, after `center_y: f32,` add: `pub grid_alpha: f32,`.

(b) In the `RedrawRequested` snapshot-write struct literal (after `center_y: self.camera.center.y,`) add: `grid_alpha: self.grid_alpha,`.

(c) Add an installer next to `install_window_get_center_y`:
```rust
#[cfg(target_arch = "wasm32")]
fn install_window_grid_alpha() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> f32 {
        web_bridge::SNAPSHOT.with(|s| s.borrow().grid_alpha)
    }) as Box<dyn Fn() -> f32>);
    expose_to_window!("__jigglefabGridAlpha", cb);
}
```

(d) In the wasm init block, next to `install_window_get_center_y();`, add `install_window_grid_alpha();`.

- [ ] **Step 2: Build-check both targets**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cd /c/Users/thedo/git/jigglefab && cargo check --lib --target wasm32-unknown-unknown && cargo test --lib`
Expected: clean; tests pass.

- [ ] **Step 3: Add the smoke assertion (`scripts/verify-web.py`)**

In the `--editor` block, after the torus-pan smoke (find `[editor] torus pan OK`) and before `[editor] extended smoke test passed`, insert:
```python
            # --- Grid fade: ~0 at rest, rises while zooming, falls after idle. ---
            await page.wait_for_timeout(1000)  # let any prior camera motion settle
            a_rest = await page.evaluate("window.__jigglefabGridAlpha()")
            await page.mouse.move(cx, cy)
            await page.mouse.wheel(0, -300)
            await page.wait_for_timeout(100)
            a_zoom = await page.evaluate("window.__jigglefabGridAlpha()")
            await page.wait_for_timeout(1200)  # idle: HOLD + fade-out
            a_idle = await page.evaluate("window.__jigglefabGridAlpha()")
            assert a_rest < 0.1, f"grid not hidden at rest: {a_rest}"
            assert a_zoom > 0.1, f"grid did not fade in on zoom: {a_zoom}"
            assert a_idle < 0.1, f"grid did not fade out when idle: {a_idle}"
            console_lines.append(f"[editor] grid fade OK: rest={a_rest} zoom={a_zoom} idle={a_idle}")
```

- [ ] **Step 4: Run the browser smoke (don't fake it)**

```bash
export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cd /c/Users/thedo/git/jigglefab
trunk serve --release --port 8080 &
# wait for the build + server, then:
python scripts/verify-web.py http://localhost:8080/ --editor
```
Expected: prints `[editor] grid fade OK: rest=… zoom=… idle=…` (rest/idle < 0.1, zoom > 0.1) and `[editor] extended smoke test passed`, exit 0. Stop the `trunk serve` afterward. If trunk/WebGPU is unavailable, report exactly what failed; the wasm `cargo check` + unit tests are the minimum gate. (Note: `trunk serve` watches sources and auto-reloads — fine for a one-shot scripted run; don't leave it running while editing files.)

- [ ] **Step 5: Commit**
```bash
git add src/app.rs scripts/verify-web.py
git commit -m "test(editor): __jigglefabGridAlpha bridge + grid-fade smoke"
```

---

## Self-Review notes
- **Spec coverage:** fade state machine (T2 §1) · adaptive weighted geometry (T1 §2) · bold boundary via weight (T1) · render via overlay shade × grid_alpha, skip at rest (T2 §3) · remove seam (T2 §8) · bridge + smoke (T3 §4) · pan trigger isolated to one deletable line (T2 §5). All spec §1–§6 covered.
- **Deferred (spec §Out) absent:** labels/ruler, two-level crossfade, UI config.
- **Type consistency:** `grid_segments(min,max,world_size,world_per_px) -> Vec<([f32;2],f32)>` defined T1, consumed T2; `grid_fade_step(alpha,idle_since,dt)` + `GRID_HOLD_S/FADE_IN/FADE_OUT` defined+tested T2; `GRID_SHADE` (app) defined T2; `world_per_px` defined T1 used T2; `grid_alpha`/`idle_since` fields defined T2 used T2/T3.
- **Each task compiles:** T1 adds (seam_segments retained); T2 swaps app→grid and deletes seam_segments together; T3 is additive bridge/test.
- **Open verification gap:** the grid's *visual* subtlety (GRID_SHADE/TARGET_PX/timings) is machine-checked for behavior (fade alpha, geometry) but its look is a tuning judgment — confirm in the Task 2 native run or Task 3 `--headed`/screenshot and adjust constants if needed.
