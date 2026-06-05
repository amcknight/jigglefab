# Editor Live Zoom + Pan Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a live, cursor-anchored mouse-wheel zoom plus middle/space-drag pan to the editor/sim view, working in both Run and Edit modes on native and web.

**Architecture:** Introduce one `Camera` struct (`src/camera.rs`) that owns the world↔screen transform as pure math, replacing the two hard-coded copies in `render.rs::update_camera` and `editor.rs::screen_to_world`. `App` holds a `Camera`, drives it from winit wheel/mouse/key events, and feeds it to the renderer. Zoom is clamped to `[1.0, 16.0]` (1.0 = today's fit-world view); pan is clamped so the world can't leave the viewport.

**Tech Stack:** Rust, `glam` (Mat4/Vec2), `winit` (input, native + wasm canvas), `wgpu` (orthographic projection).

**Build/test commands** (this repo — cargo is not on the default Bash PATH):
```bash
export PATH="$PATH:/c/Users/thedo/.cargo/bin"
cd /c/Users/thedo/git/jigglefab
cargo test --lib                # unit tests, debug OK
cargo build --lib              # type-check
```

---

## File Structure

- **Create `src/camera.rs`** — the `Camera` struct, constants, all transform/zoom/pan math, and its unit tests. One responsibility: the view transform. No winit, no wgpu, no app state — only `glam`.
- **Modify `src/lib.rs`** — add `pub mod camera;`.
- **Modify `src/render.rs`** — `update_camera` takes `&Camera` and builds `view_proj` from it instead of the hard-coded fit matrix.
- **Modify `src/editor.rs`** — delete the free `screen_to_world` (logic moves into `Camera`); adapt the tests that covered it.
- **Modify `src/app.rs`** — `App` gains `camera`, `space_held`, `pan_anchor` fields; new `MouseWheel` handling; middle-button + space pan; `0`-key reset; camera reset on chemistry switch; re-clamp on resize; `cursor_world`/`update_camera` call sites use `self.camera`.
- **Modify `src/app.rs` (wasm bridge) + `scripts/verify-web.py`** — expose `__jigglefabGetZoom()` and add a wheel/reset smoke assertion.

---

## Task 1: `Camera` struct — fit, view_proj, screen_to_world

The pure foundation: a camera that, at `zoom = 1.0`, reproduces today's fit-world transform exactly. `view_proj` and `screen_to_world` are mutual inverses derived from one visible-rect definition, so they cannot drift.

**Files:**
- Create: `src/camera.rs`
- Modify: `src/lib.rs` (add `pub mod camera;`)
- Test: `src/camera.rs` (`#[cfg(test)]` module)

**Math reference (do not skip — the formulas below are the contract the tests check):**

For viewport `(vw, vh)` and aspect `a = vw/vh`, the *base* visible extent at `zoom = 1` matches today's letterboxing:
- `base_w = world_size * a.max(1.0)`
- `base_h = world_size * (1.0/a).max(1.0)`

At zoom `z`, visible extent is `visible = (base_w/z, base_h/z)`. The visible world rect is centred on `self.center` with those dimensions. Screen `(sx, sy)` is top-down (0 at top).

- `fx = sx/vw`, `fy = sy/vh`
- `world_x = center.x - visible.x/2 + fx*visible.x`
- `world_y = center.y + visible.y/2 - fy*visible.y` (screen-y down ⇒ world-y up)

`view_proj` is `Mat4::orthographic_rh(left, right, bottom, top, -1, 1)` over that same rect.

- [ ] **Step 1: Write the failing tests**

Create `src/camera.rs` with the test module first (the impl in later steps makes them pass):

```rust
use glam::{Mat4, Vec2};

pub const MIN_ZOOM: f32 = 1.0;
pub const MAX_ZOOM: f32 = 16.0;
/// Multiplicative zoom factor applied per unit of scroll.
pub const ZOOM_STEP: f32 = 1.1;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Camera {
    /// 1.0 = fit-world (whole world fills the viewport — today's behavior).
    pub zoom: f32,
    /// World point shown at the centre of the viewport.
    pub center: Vec2,
}

#[cfg(test)]
mod tests {
    use super::*;

    const WS: f32 = 128.0;

    fn approx(a: Vec2, b: Vec2) -> bool {
        (a - b).length() < 1e-3
    }

    #[test]
    fn fit_centers_world() {
        let cam = Camera::fit(WS);
        assert_eq!(cam.zoom, 1.0);
        assert!(approx(cam.center, Vec2::new(WS / 2.0, WS / 2.0)));
    }

    #[test]
    fn fit_center_pixel_maps_to_world_center() {
        // Square viewport: the centre pixel is the world centre.
        let cam = Camera::fit(WS);
        let w = cam.screen_to_world((400.0, 400.0), (800, 800), WS);
        assert!(approx(w, Vec2::new(WS / 2.0, WS / 2.0)), "got {w:?}");
    }

    #[test]
    fn fit_top_left_pixel_is_world_top_left() {
        // Screen (0,0) is top-left; world (0, WS) is top-left on a square view.
        let cam = Camera::fit(WS);
        let w = cam.screen_to_world((0.0, 0.0), (800, 800), WS);
        assert!(approx(w, Vec2::new(0.0, WS)), "got {w:?}");
    }

    #[test]
    fn screen_world_round_trip_when_zoomed() {
        // world -> screen -> world is identity away from the edge clamp.
        let cam = Camera { zoom: 4.0, center: Vec2::new(40.0, 90.0) };
        let viewport = (1024, 768);
        let world_in = Vec2::new(42.0, 88.0);
        let screen = cam.world_to_screen(world_in, viewport, WS);
        let world_out = cam.screen_to_world((screen.0 as f64, screen.1 as f64), viewport, WS);
        assert!(approx(world_in, world_out), "in {world_in:?} out {world_out:?}");
    }

    #[test]
    fn view_proj_is_inverse_of_screen_to_world() {
        // A world point projected to clip, then mapped clip->screen->world,
        // returns the original. Guards render/input consistency.
        let cam = Camera { zoom: 2.5, center: Vec2::new(70.0, 60.0) };
        let viewport = (800, 600);
        let world_in = Vec2::new(72.0, 55.0);
        let clip = cam.view_proj(viewport, WS) * world_in.extend(0.0).extend(1.0);
        // clip.xy in [-1,1]; convert to screen (y flips).
        let sx = (clip.x * 0.5 + 0.5) * viewport.0 as f32;
        let sy = (1.0 - (clip.y * 0.5 + 0.5)) * viewport.1 as f32;
        let world_out = cam.screen_to_world((sx as f64, sy as f64), viewport, WS);
        assert!(approx(world_in, world_out), "in {world_in:?} out {world_out:?}");
    }
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cargo test --lib camera`
Expected: FAIL — `no function or associated item named 'fit'` / `screen_to_world` / `world_to_screen` / `view_proj` found for `Camera`.

- [ ] **Step 3: Write the implementation**

Add to `src/camera.rs` (above the test module):

```rust
impl Camera {
    pub fn fit(world_size: f32) -> Self {
        Self { zoom: 1.0, center: Vec2::new(world_size * 0.5, world_size * 0.5) }
    }

    /// Width/height of the world rect currently visible, in world units.
    fn visible_extent(&self, viewport: (u32, u32), world_size: f32) -> Vec2 {
        let vw = viewport.0.max(1) as f32;
        let vh = viewport.1.max(1) as f32;
        let a = vw / vh;
        let base_w = world_size * a.max(1.0);
        let base_h = world_size * (1.0 / a).max(1.0);
        Vec2::new(base_w / self.zoom, base_h / self.zoom)
    }

    /// Convert a screen pixel to a world point. Result is NOT clamped — callers
    /// that want edge-snapping clamp the return value. (Internal anchor math
    /// needs the raw value.)
    fn screen_to_world_raw(&self, cursor: (f64, f64), viewport: (u32, u32), world_size: f32) -> Vec2 {
        let vw = viewport.0.max(1) as f32;
        let vh = viewport.1.max(1) as f32;
        let vis = self.visible_extent(viewport, world_size);
        let fx = cursor.0 as f32 / vw;
        let fy = cursor.1 as f32 / vh;
        Vec2::new(
            self.center.x - vis.x * 0.5 + fx * vis.x,
            self.center.y + vis.y * 0.5 - fy * vis.y,
        )
    }

    /// Public screen->world, clamped to `[0, world_size]` per axis so a click
    /// outside the rendered world still yields a placeable (edge-snapped) point.
    pub fn screen_to_world(&self, cursor: (f64, f64), viewport: (u32, u32), world_size: f32) -> Vec2 {
        let w = self.screen_to_world_raw(cursor, viewport, world_size);
        Vec2::new(w.x.clamp(0.0, world_size), w.y.clamp(0.0, world_size))
    }

    /// Inverse of `screen_to_world_raw`, for tests/overlay math.
    pub fn world_to_screen(&self, world: Vec2, viewport: (u32, u32), world_size: f32) -> (f32, f32) {
        let vw = viewport.0.max(1) as f32;
        let vh = viewport.1.max(1) as f32;
        let vis = self.visible_extent(viewport, world_size);
        let fx = (world.x - self.center.x + vis.x * 0.5) / vis.x;
        let fy = (self.center.y + vis.y * 0.5 - world.y) / vis.y;
        (fx * vw, fy * vh)
    }

    /// World→clip orthographic projection for the current view.
    pub fn view_proj(&self, viewport: (u32, u32), world_size: f32) -> Mat4 {
        let vis = self.visible_extent(viewport, world_size);
        let left = self.center.x - vis.x * 0.5;
        let right = self.center.x + vis.x * 0.5;
        let bottom = self.center.y - vis.y * 0.5;
        let top = self.center.y + vis.y * 0.5;
        Mat4::orthographic_rh(left, right, bottom, top, -1.0, 1.0)
    }
}
```

- [ ] **Step 4: Add the module to the crate**

In `src/lib.rs`, add alongside the other `pub mod` lines:

```rust
pub mod camera;
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cargo test --lib camera`
Expected: PASS — 5 tests in `camera::tests`.

- [ ] **Step 6: Commit**

```bash
git add src/camera.rs src/lib.rs
git commit -m "feat(camera): Camera transform (fit/view_proj/screen_to_world)"
```

---

## Task 2: Cursor-anchored zoom

`zoom_at` multiplies zoom (clamped), then moves `center` so the world point under the cursor stays under the cursor. Pan-clamp is added in Task 3; for now `center` is set directly.

**Files:**
- Modify: `src/camera.rs`
- Test: `src/camera.rs` (`#[cfg(test)]`)

- [ ] **Step 1: Write the failing tests**

Add to `mod tests`:

```rust
#[test]
fn zoom_at_keeps_cursor_world_point_fixed() {
    let mut cam = Camera::fit(WS);
    let viewport = (1024, 768);
    let cursor = (300.0, 500.0);
    let before = cam.screen_to_world(cursor, viewport, WS);
    cam.zoom_at(cursor, 2.0, viewport, WS);
    let after = cam.screen_to_world(cursor, viewport, WS);
    assert!((cam.zoom - 2.0).abs() < 1e-4, "zoom {}", cam.zoom);
    assert!(approx(before, after), "anchor moved: {before:?} -> {after:?}");
}

#[test]
fn zoom_at_clamps_to_max() {
    let mut cam = Camera::fit(WS);
    let viewport = (800, 800);
    for _ in 0..100 {
        cam.zoom_at((400.0, 400.0), 2.0, viewport, WS);
    }
    assert!((cam.zoom - MAX_ZOOM).abs() < 1e-4, "zoom {}", cam.zoom);
}

#[test]
fn zoom_at_clamps_to_min_and_anchor_exact_at_clamp() {
    // Already at min; zooming out further is a no-op, so the centre cannot drift.
    let mut cam = Camera::fit(WS);
    let viewport = (800, 800);
    cam.zoom_at((100.0, 700.0), 0.5, viewport, WS);
    assert!((cam.zoom - MIN_ZOOM).abs() < 1e-4, "zoom {}", cam.zoom);
    assert!(approx(cam.center, Vec2::new(WS / 2.0, WS / 2.0)), "center {:?}", cam.center);
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cargo test --lib camera::tests::zoom`
Expected: FAIL — `no method named 'zoom_at'`.

- [ ] **Step 3: Write the implementation**

Add to `impl Camera`:

```rust
/// Multiply zoom by `factor` (clamped to [MIN_ZOOM, MAX_ZOOM]) while keeping the
/// world point under `cursor` fixed on screen.
pub fn zoom_at(&mut self, cursor: (f64, f64), factor: f32, viewport: (u32, u32), world_size: f32) {
    let new_zoom = (self.zoom * factor).clamp(MIN_ZOOM, MAX_ZOOM);
    if (new_zoom - self.zoom).abs() < f32::EPSILON {
        return; // at a clamp: no zoom change ⇒ no centre shift (anchor stays exact)
    }
    let anchor = self.screen_to_world_raw(cursor, viewport, world_size);
    self.zoom = new_zoom;
    // Solve for the centre that puts `anchor` back under `cursor` at the new zoom.
    let vw = viewport.0.max(1) as f32;
    let vh = viewport.1.max(1) as f32;
    let vis = self.visible_extent(viewport, world_size);
    let fx = cursor.0 as f32 / vw;
    let fy = cursor.1 as f32 / vh;
    self.center = Vec2::new(
        anchor.x - (fx - 0.5) * vis.x,
        anchor.y + (fy - 0.5) * vis.y,
    );
    self.clamp_pan(viewport, world_size);
}
```

Add a `clamp_pan` stub so Task 2 compiles (Task 3 fills it in):

```rust
fn clamp_pan(&mut self, _viewport: (u32, u32), _world_size: f32) {}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cargo test --lib camera`
Expected: PASS — Task 1 + Task 2 tests (8 total).

- [ ] **Step 5: Commit**

```bash
git add src/camera.rs
git commit -m "feat(camera): cursor-anchored zoom_at with clamp"
```

---

## Task 3: Pan + reset + pan-clamp

Fill in `clamp_pan`, add `pan_by` and `reset`. After this, the camera is feature-complete.

**Files:**
- Modify: `src/camera.rs`
- Test: `src/camera.rs` (`#[cfg(test)]`)

**Clamp rule:** with visible half-extent `h = visible/2`: if `visible <= world_size` on an axis, clamp `center` to `[h, world_size - h]`; otherwise (visible exceeds the world — always true on the letterboxed axis at zoom 1) pin `center` to `world_size/2`.

**Pan delta:** for a cursor screen-delta `(dsx, dsy)` (content follows the cursor): `center.x -= dsx * visible.x/vw`, `center.y += dsy * visible.y/vh` (screen-y inverted vs world-y).

- [ ] **Step 1: Write the failing tests**

Add to `mod tests`:

```rust
#[test]
fn pan_clamp_keeps_world_in_view() {
    let mut cam = Camera { zoom: 4.0, center: Vec2::new(WS / 2.0, WS / 2.0) };
    let viewport = (800, 800);
    // Huge pan up-left; centre must stay so the visible rect is inside [0,WS].
    cam.pan_by((10_000.0, 10_000.0), viewport, WS);
    let half = WS / (2.0 * cam.zoom); // square viewport ⇒ visible = WS/zoom
    assert!(cam.center.x >= half - 1e-3 && cam.center.x <= WS - half + 1e-3, "x {}", cam.center.x);
    assert!(cam.center.y >= half - 1e-3 && cam.center.y <= WS - half + 1e-3, "y {}", cam.center.y);
}

#[test]
fn pan_letterboxed_axis_pins_to_center() {
    // Wide viewport at zoom 1: x-extent exceeds world ⇒ centre.x pinned.
    let mut cam = Camera::fit(WS);
    let viewport = (1600, 800); // aspect 2 ⇒ visible.x = 2*WS > WS
    cam.pan_by((500.0, 0.0), viewport, WS);
    assert!((cam.center.x - WS / 2.0).abs() < 1e-3, "x {}", cam.center.x);
}

#[test]
fn pan_moves_center_opposite_to_cursor_x() {
    let mut cam = Camera { zoom: 4.0, center: Vec2::new(WS / 2.0, WS / 2.0) };
    let viewport = (800, 800);
    let before = cam.center.x;
    cam.pan_by((20.0, 0.0), viewport, WS); // drag cursor right
    assert!(cam.center.x < before, "center.x should decrease: {before} -> {}", cam.center.x);
}

#[test]
fn reset_equals_fit() {
    let mut cam = Camera { zoom: 7.0, center: Vec2::new(10.0, 10.0) };
    cam.reset(WS);
    assert_eq!(cam, Camera::fit(WS));
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cargo test --lib camera::tests::pan`
Expected: FAIL — `no method named 'pan_by'` (and `reset`).

- [ ] **Step 3: Write the implementation**

Replace the `clamp_pan` stub from Task 2 with the real body, and add `pan_by` + `reset`:

```rust
fn clamp_pan(&mut self, viewport: (u32, u32), world_size: f32) {
    let vis = self.visible_extent(viewport, world_size);
    let clamp_axis = |c: f32, ext: f32| -> f32 {
        let h = ext * 0.5;
        if ext <= world_size {
            c.clamp(h, world_size - h)
        } else {
            world_size * 0.5 // visible exceeds world ⇒ pin to centre
        }
    };
    self.center.x = clamp_axis(self.center.x, vis.x);
    self.center.y = clamp_axis(self.center.y, vis.y);
}

/// Pan by a cursor screen-delta in physical pixels (content follows the cursor).
pub fn pan_by(&mut self, screen_delta: (f32, f32), viewport: (u32, u32), world_size: f32) {
    let vw = viewport.0.max(1) as f32;
    let vh = viewport.1.max(1) as f32;
    let vis = self.visible_extent(viewport, world_size);
    self.center.x -= screen_delta.0 * vis.x / vw;
    self.center.y += screen_delta.1 * vis.y / vh;
    self.clamp_pan(viewport, world_size);
}

pub fn reset(&mut self, world_size: f32) {
    *self = Camera::fit(world_size);
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cargo test --lib camera`
Expected: PASS — all 12 camera tests.

- [ ] **Step 5: Commit**

```bash
git add src/camera.rs
git commit -m "feat(camera): pan_by, reset, pan clamp"
```

---

## Task 4: Wire the camera into render + editor + app (behavior-preserving)

Swap the two hard-coded transforms for the camera. At `zoom = 1`, the app looks and behaves exactly as before — this task adds **no** new gestures, only the plumbing. Verifying existing tests still pass is the safety net.

**Files:**
- Modify: `src/render.rs` (`update_camera` signature + body)
- Modify: `src/editor.rs` (remove free `screen_to_world`; adapt its tests)
- Modify: `src/app.rs` (add `camera` field; init; route `cursor_world`; all `update_camera` call sites)

- [ ] **Step 1: Change `render.rs::update_camera` to take `&Camera`**

In `src/render.rs`, replace the signature and the matrix build. Find ([src/render.rs:316-329](../../../src/render.rs#L316-L329)):

```rust
    pub fn update_camera(&mut self, world_size: f32, palette: &[[f32; 3]]) {
        // Orthographic projection covering the whole world, square, centered.
        let aspect = self.size.width as f32 / self.size.height as f32;
        let (w, h) = if aspect >= 1.0 {
            (world_size * aspect, world_size)
        } else {
            (world_size, world_size / aspect)
        };
        let proj = Mat4::orthographic_rh(0.0, w, 0.0, h, -1.0, 1.0);
        // Center the world inside the view if aspect > 1.
        let offset_x = (w - world_size) * 0.5;
        let offset_y = (h - world_size) * 0.5;
        let view = Mat4::from_translation(glam::Vec3::new(offset_x, offset_y, 0.0));
        let vp = proj * view;
```

with:

```rust
    pub fn update_camera(&mut self, camera: &crate::camera::Camera, world_size: f32, palette: &[[f32; 3]]) {
        let vp = camera.view_proj((self.size.width, self.size.height), world_size);
```

The rest of the function (the `state_colors` loop, `CameraUbo` build, `write_buffer`) is unchanged. The now-unused `Mat4`/`Vec2` imports may need pruning — if `cargo build` warns about unused imports in `render.rs`, remove the offending names from the `use glam::...` line (keep any still used by other code).

- [ ] **Step 2: Add the `camera` field to `App` and initialize it**

In `src/app.rs`, add to the `App` struct ([src/app.rs:315-332](../../../src/app.rs#L315-L332)), after `cursor`:

```rust
    camera: crate::camera::Camera,
    /// True while Space is held — turns a left-drag into a pan.
    space_held: bool,
    /// Some(last_cursor) while a pan drag (middle-button or space+left) is active.
    pan_anchor: Option<winit::dpi::PhysicalPosition<f64>>,
```

In `App::new()` ([src/app.rs:335-351](../../../src/app.rs#L335-L351)), add after `cursor: ...`:

```rust
            camera: crate::camera::Camera::fit(crate::sim::WORLD_SIZE),
            space_held: false,
            pan_anchor: None,
```

- [ ] **Step 3: Route `cursor_world` through the camera**

Replace `cursor_world` ([src/app.rs:358-367](../../../src/app.rs#L358-L367)):

```rust
    fn cursor_world(&self) -> Option<glam::Vec2> {
        let window = self.window.as_ref()?;
        let scene = self.scene.as_ref()?;
        let viewport = window.inner_size();
        Some(self.camera.screen_to_world(
            (self.cursor.x, self.cursor.y),
            (viewport.width, viewport.height),
            scene.world_size,
        ))
    }
```

- [ ] **Step 4: Update every `update_camera` call site**

There are five call sites; each must now pass `&self.camera` (or `&camera`/`&renderer`-local as appropriate). Update them:

1. Native init ([src/app.rs:620](../../../src/app.rs#L620)):
   `renderer.update_camera(world_size, &palette);` → `renderer.update_camera(&self.camera, world_size, &palette);`
   But `self.camera` was initialized to `WORLD_SIZE`; set it to this scene's world first. Immediately before this line add:
   `self.camera = crate::camera::Camera::fit(world_size);`

2. Wasm init ([src/app.rs:673](../../../src/app.rs#L673)) — this runs inside `spawn_local`, which moves `world_size`/`palette` but not `self`. Capture the camera by value before the closure. Immediately before `let proxy = ...` ([src/app.rs:667](../../../src/app.rs#L667)) add:
   `self.camera = crate::camera::Camera::fit(world_size);`
   `let camera = self.camera;`
   and change the call inside the closure to:
   `renderer.update_camera(&camera, world_size, &palette);`

3. `RendererReady` ([src/app.rs:694](../../../src/app.rs#L694)):
   `renderer.update_camera(sim.world_size(), &sim.palette());` → `renderer.update_camera(&self.camera, sim.world_size(), &sim.palette());`
   (`self.camera` is not borrowed elsewhere here — the surrounding `if let` borrows `self.window`/`self.sim` immutably; reading `self.camera` by shared ref is fine. If the borrow checker objects, copy it first: `let camera = self.camera;` before the `if let`.)

4. `Resized` ([src/app.rs:715-720](../../../src/app.rs#L715-L720)) — re-clamp pan for the new aspect, then update:

```rust
            WindowEvent::Resized(size) => {
                let Some(renderer) = &mut self.renderer else { return };
                let Some(sim) = &mut self.sim else { return };
                renderer.resize(size);
                self.camera.pan_by((0.0, 0.0), (size.width, size.height), sim.world_size());
                renderer.update_camera(&self.camera, sim.world_size(), &sim.palette());
            }
```
(`pan_by` with a zero delta just re-applies `clamp_pan` for the new viewport.)

5. Chemistry switch ([src/app.rs:751-754](../../../src/app.rs#L751-L754)) — reset the camera to the new scene's fit, then update. Replace that `if let` block:

```rust
                            if let (Some(renderer), Some(scene)) = (self.renderer.as_mut(), self.scene.as_ref()) {
                                let palette: Vec<[f32; 3]> = scene.chemistry.colors.clone();
                                self.camera = crate::camera::Camera::fit(scene.world_size);
                                renderer.update_camera(&self.camera, scene.world_size, &palette);
                            }
```
Note: `self.camera = ...` then `self.renderer`/`self.scene` are already borrowed by the `if let`. To avoid a borrow conflict, compute the reset before the `if let`:

```rust
                            if let Some(scene) = self.scene.as_ref() {
                                self.camera = crate::camera::Camera::fit(scene.world_size);
                            }
                            if let (Some(renderer), Some(scene)) = (self.renderer.as_mut(), self.scene.as_ref()) {
                                let palette: Vec<[f32; 3]> = scene.chemistry.colors.clone();
                                renderer.update_camera(&self.camera, scene.world_size, &palette);
                            }
```

- [ ] **Step 5: Remove the free `editor::screen_to_world` and adapt its tests**

In `src/editor.rs`, delete the `pub fn screen_to_world(...)` function ([src/editor.rs:421-444](../../../src/editor.rs#L421-L444)). Its three viewport tests (`screen_to_world_square_viewport_center`, `screen_to_world_top_left_maps_to_world_top_left`, `screen_to_world_wide_viewport_clamps_outside_x`, around [src/editor.rs:580-600](../../../src/editor.rs#L580-L600)) now belong to the camera. Move their assertions into `src/camera.rs`'s `mod tests`, rewritten against `Camera::fit(ws).screen_to_world(...)`. For the wide-viewport clamp test, assert the x result is clamped to `[0, world_size]`:

```rust
#[test]
fn wide_viewport_clamps_x_outside_world() {
    // Aspect 2 view: a pixel at the far left maps left of the world, clamped to 0.
    let cam = Camera::fit(WS);
    let w = cam.screen_to_world((0.0, 400.0), (1600, 800), WS);
    assert!(w.x >= 0.0 && w.x <= WS, "x {}", w.x);
    assert!((w.x - 0.0).abs() < 1e-3, "expected left-edge clamp, got {}", w.x);
}
```

Delete the now-removed tests from `editor.rs`. If any other code in the crate still calls `editor::screen_to_world`, grep and update it:
Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && rg 'screen_to_world' src/`

- [ ] **Step 6: Build and run the full unit suite**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cargo build --lib && cargo test --lib`
Expected: PASS — camera tests plus all pre-existing `--lib` tests (the editor tests minus the three moved ones). No unused-import or unused-variable warnings in `render.rs`/`editor.rs`.

- [ ] **Step 7: Commit**

```bash
git add src/render.rs src/editor.rs src/app.rs
git commit -m "refactor(view): route render + editor through Camera (zoom=1 unchanged)"
```

---

## Task 5: Input — wheel zoom, middle/space pan, 0-reset

Now the gestures. All arrive as winit `WindowEvent`s (native + web). Pan is mutually exclusive with edit gestures: while `pan_anchor.is_some()`, mouse-move/up route to pan, not the editor.

**Files:**
- Modify: `src/app.rs` (`window_event` match arms; `on_mouse_move`; pan helpers)

- [ ] **Step 1: Add viewport / world-size / palette / refresh helpers**

In `src/app.rs`, add these methods to `impl App` (near `cursor_world`):

```rust
    fn viewport(&self) -> Option<(u32, u32)> {
        let s = self.window.as_ref()?.inner_size();
        Some((s.width, s.height))
    }

    fn world_size(&self) -> f32 {
        self.scene.as_ref().map(|s| s.world_size)
            .or_else(|| self.sim.as_ref().map(|s| s.world_size()))
            .unwrap_or(crate::sim::WORLD_SIZE)
    }

    fn palette_for_camera(&self) -> Vec<[f32; 3]> {
        if let Some(scene) = &self.scene {
            scene.chemistry.colors.clone()
        } else if let Some(sim) = &self.sim {
            sim.palette()
        } else {
            Vec::new()
        }
    }

    /// Re-upload the current camera to the renderer. Locals are pulled out first
    /// so the `&mut self.renderer` borrow doesn't overlap the `&self` reads
    /// (`palette_for_camera`/`world_size` borrow self whole; `Camera` is `Copy`).
    fn refresh_camera(&mut self) {
        let ws = self.world_size();
        let palette = self.palette_for_camera();
        let camera = self.camera;
        if let Some(r) = &mut self.renderer {
            r.update_camera(&camera, ws, &palette);
        }
    }
```

(`scene.chemistry.colors` is the palette used at [src/app.rs:752](../../../src/app.rs#L752), so it exists.)

- [ ] **Step 2: Handle the mouse wheel**

In `window_event`, add a new arm alongside the others (e.g. after the `MouseInput` arm at [src/app.rs:858-866](../../../src/app.rs#L858-L866)):

```rust
            WindowEvent::MouseWheel { delta, .. } => {
                use winit::event::MouseScrollDelta;
                let scroll = match delta {
                    MouseScrollDelta::LineDelta(_, y) => y,
                    MouseScrollDelta::PixelDelta(p) => (p.y as f32) / 50.0,
                };
                if scroll != 0.0 {
                    if let Some(viewport) = self.viewport() {
                        let ws = self.world_size();
                        let factor = crate::camera::ZOOM_STEP.powf(scroll);
                        self.camera.zoom_at((self.cursor.x, self.cursor.y), factor, viewport, ws);
                        self.refresh_camera();
                    }
                }
            }
```

(`refresh_camera` and `palette_for_camera` were added to `impl App` in Step 1.)

- [ ] **Step 3: Track Space and start/stop pan on middle button**

Extend the `KeyboardInput` arm ([src/app.rs:867-883](../../../src/app.rs#L867-L883)) to track Space and handle the `0` reset. Replace that arm:

```rust
            WindowEvent::KeyboardInput { event: key_event, .. } => {
                use winit::event::ElementState;
                use winit::keyboard::{Key, NamedKey};
                let pressed = key_event.state == ElementState::Pressed;
                if matches!(key_event.logical_key, Key::Named(NamedKey::Space)) {
                    self.space_held = pressed;
                }
                if pressed {
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
                    if matches!(&key_event.logical_key, Key::Character(c) if c.as_str() == "0") {
                        self.camera.reset(self.world_size());
                        self.refresh_camera();
                    }
                }
            }
```

Extend the `MouseInput` arm ([src/app.rs:858-866](../../../src/app.rs#L858-L866)) to handle middle-button pan and space+left pan:

```rust
            WindowEvent::MouseInput { state, button, .. } => {
                use winit::event::{ElementState, MouseButton};
                match (button, state) {
                    (MouseButton::Middle, ElementState::Pressed) => {
                        self.pan_anchor = Some(self.cursor);
                    }
                    (MouseButton::Middle, ElementState::Released) => {
                        self.pan_anchor = None;
                    }
                    (MouseButton::Left, ElementState::Pressed) => {
                        if self.space_held {
                            self.pan_anchor = Some(self.cursor);
                        } else {
                            self.on_mouse_down();
                        }
                    }
                    (MouseButton::Left, ElementState::Released) => {
                        if self.pan_anchor.is_some() {
                            self.pan_anchor = None;
                        } else {
                            self.on_mouse_up();
                        }
                    }
                    _ => {}
                }
            }
```

- [ ] **Step 4: Route cursor-move to pan when a pan is active**

In `CursorMoved` ([src/app.rs:854-857](../../../src/app.rs#L854-L857)), pan before falling through to edit-move:

```rust
            WindowEvent::CursorMoved { position, .. } => {
                let prev = self.cursor;
                self.cursor = position;
                if self.pan_anchor.is_some() {
                    let dx = (position.x - prev.x) as f32;
                    let dy = (position.y - prev.y) as f32;
                    if let Some(viewport) = self.viewport() {
                        let ws = self.world_size();
                        self.camera.pan_by((dx, dy), viewport, ws);
                        self.refresh_camera();
                    }
                } else {
                    self.on_mouse_move();
                }
            }
```

- [ ] **Step 5: Build and smoke-check with the unit suite**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cargo build --lib && cargo test --lib`
Expected: PASS, no warnings. (No new unit tests here — the camera math is covered in Tasks 1-3; this task is winit wiring, verified manually in Step 6 and by the browser smoke in Task 6.)

- [ ] **Step 6: Manual verification (native)**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cargo run --release`
Confirm by observation:
- Scroll wheel up over a bead → zooms in toward the cursor; down → out; stops at whole-world (can't zoom into void).
- Middle-drag pans; Space+left-drag pans; world can't be dragged fully off-screen.
- Press `0` → snaps back to fit.
- Start the sim (Run) and confirm zoom/pan still work while it's running.

- [ ] **Step 7: Commit**

```bash
git add src/app.rs
git commit -m "feat(editor): wheel zoom + middle/space pan + 0-reset"
```

---

## Task 6: Browser smoke — `__jigglefabGetZoom` + verify-web assertion

Expose the zoom level to the web bridge so the smoke test can assert the wheel works on the deployed path, then add the assertion.

**Files:**
- Modify: `src/app.rs` (wasm bridge: `install_window_get_zoom`, call it in the wasm init block)
- Modify: `scripts/verify-web.py` (wheel + reset assertion under `--editor`)

- [ ] **Step 1: Add the `__jigglefabGetZoom` bridge global**

In `src/app.rs`, find an existing simple read-only bridge installer to copy the pattern — e.g. `install_window_bead_count` (referenced at [src/app.rs:656](../../../src/app.rs#L656)). Add a parallel `install_window_get_zoom` that returns the current camera zoom. Since the camera lives on `App` (not a thread-local), follow the same mechanism the other getters use to read app state (they read from the shared `web_bridge` snapshot updated each frame). Concretely:

  1. Locate the per-frame snapshot struct that backs the other getters (the `HUD`/snapshot written near [src/app.rs:835-848](../../../src/app.rs#L835-L848), which already carries `bead_count`, `mode`, etc.). Add a `zoom: f32` field to it and set `zoom: self.camera.zoom` where that struct is populated.
  2. Add `install_window_get_zoom` mirroring `install_window_bead_count`, reading `zoom` from that snapshot and returning it as a JS number.
  3. Call `install_window_get_zoom();` in the wasm init list ([src/app.rs:656-665](../../../src/app.rs#L656-L665)).

Build the wasm target to confirm it compiles:
Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cargo build --lib --target wasm32-unknown-unknown`
Expected: success.

- [ ] **Step 2: Add the smoke assertion**

In `scripts/verify-web.py`, inside the `--editor` path, after the page/editor is ready, add:

```python
# Zoom: wheel over the canvas should raise the zoom level; 0 resets to ~1.
z0 = page.evaluate("() => window.__jigglefabGetZoom()")
page.mouse.move(canvas_cx, canvas_cy)   # center of the canvas (reuse existing vars)
page.mouse.wheel(0, -300)               # negative dy = scroll up = zoom in
page.wait_for_timeout(100)
z1 = page.evaluate("() => window.__jigglefabGetZoom()")
assert z1 > z0, f"wheel did not zoom in: {z0} -> {z1}"
page.keyboard.press("0")
page.wait_for_timeout(100)
z2 = page.evaluate("() => window.__jigglefabGetZoom()")
assert abs(z2 - 1.0) < 0.01, f"reset did not return to fit: {z2}"
print("zoom smoke OK:", z0, z1, z2)
```

If `canvas_cx`/`canvas_cy` (or equivalent canvas-center coordinates) don't already exist in the editor smoke, derive them from the canvas bounding box the same way the existing rect/lasso drag steps in that script do.
Run: `rg 'canvas' scripts/verify-web.py` to find the existing canvas-coordinate pattern and match it.

- [ ] **Step 3: Run the browser smoke**

Run: `python scripts/verify-web.py --editor`
(Build the web bundle first if the script doesn't do it itself — check the script's header/usage; follow the same build step the existing editor smoke uses.)
Expected: the run prints `zoom smoke OK: ...` with `z1 > z0` and `z2 ≈ 1.0`, and the rest of the editor smoke still passes.

- [ ] **Step 4: Commit**

```bash
git add src/app.rs scripts/verify-web.py
git commit -m "test(editor): browser smoke for wheel zoom + 0-reset"
```

---

## Self-Review notes

- **Spec coverage:** wheel zoom (Task 5) · cursor anchor (Task 2) · pan middle/space (Task 5) · pan clamp (Task 3) · zoom clamp 1–16 (Task 2) · reset `0` (Task 5) · single Camera source of truth (Tasks 1–4) · render + overlay + wrap-ghosts follow camera (Task 4, free via `view_proj`) · Run+Edit + native+web (Task 5 winit path; Task 6 web smoke) · resize re-clamp + chemistry reset persistence (Task 4 Step 4). All spec §1–§6 requirements map to a task.
- **Deferred (per spec §Scope/§7), intentionally absent:** pinch, +/- buttons, keyboard +/- zoom, animated easing, zoom-during-lasso, camera persisted to saved scenes.
- **Open implementation detail to confirm during Task 6:** the exact per-frame snapshot mechanism the wasm getters use — Step 1 describes the pattern but the engineer must match the real field/struct names in `app.rs`/`web_bridge`.
