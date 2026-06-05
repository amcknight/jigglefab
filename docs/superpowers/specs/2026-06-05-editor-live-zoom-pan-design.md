# Editor — live zoom + pan

Status: approved 2026-06-05. Implementation plan: TBD.

Adds a live, interactive camera to the editor/sim view. Today the world is rendered
as a fixed orthographic square that always fits the viewport; there is no camera
state. This introduces a real camera (zoom + pan) driven by mouse wheel and
drag, working in both Run and Edit modes on both the native and web builds.

## Goal

Let the user (1) zoom the view in and out with the mouse wheel, anchored to the
cursor, and (2) pan around once zoomed in. Works while the sim runs and while
editing; orthogonal to the active tool. A key snaps back to the default fit-world
view.

## Scope

**In:**
- Mouse-wheel / trackpad-scroll zoom, anchored to the cursor.
- Pan via middle-button drag, or hold `Space` + left-drag.
- Reset-to-fit on the `0` key.
- Zoom clamped: minimum = fit-world (whole world fills the viewport, the current
  behavior); maximum ~16×.
- Pan clamped so the world cannot be moved fully off-screen.
- A single `Camera` struct as the one source of truth for the world↔screen
  transform, replacing the two hard-coded copies in `render.rs` and `editor.rs`.
- Works in both Run and Edit modes, and on both native and web (all input flows
  through winit `WindowEvent`s, which winit translates from canvas events on web).

**Out (deferred):**
- Pinch-to-zoom (touch) and on-screen +/- buttons. Wheel-only for v1.
- Keyboard `+`/`-` zoom.
- Zoom/pan during a region-select or chain drag (camera gestures and edit
  gestures are mutually exclusive per press; you don't zoom mid-lasso).
- Smooth/animated zoom easing — each wheel notch is an immediate step.
- Persisting camera state to a saved scene / save-load round-trip.
- Rotating the view.

## §1 — `Camera`: single source of truth

The world→screen transform is currently hard-coded and **duplicated**:
- `render.rs::update_camera` ([src/render.rs:316-347](../../../src/render.rs#L316-L347))
  builds an orthographic matrix that fits the world square into the viewport,
  centered, aspect-corrected.
- `editor.rs::screen_to_world` ([src/editor.rs:421-444](../../../src/editor.rs#L421-L444))
  is the hand-written inverse of that same transform, used to convert mouse
  positions to world coordinates.

These must agree exactly, and adding zoom/pan to each independently is how they
drift apart. v1 collapses both into one struct.

```rust
// src/camera.rs (new module; lib.rs adds `pub mod camera;`)
use glam::{Mat4, Vec2};

pub struct Camera {
    /// 1.0 = fit-world (whole world fills the viewport — today's behavior).
    /// Larger = zoomed in. Clamped to [MIN_ZOOM, MAX_ZOOM].
    pub zoom: f32,
    /// World point shown at the center of the viewport. Clamped so the world
    /// cannot leave the view.
    pub center: Vec2,
}

pub const MIN_ZOOM: f32 = 1.0;
pub const MAX_ZOOM: f32 = 16.0;
/// Multiplicative zoom factor applied per unit of scroll.
pub const ZOOM_STEP: f32 = 1.1;
```

Methods (all pure, all take `viewport: (u32, u32)` and `world_size: f32` so the
camera holds no viewport state of its own):

| Method | Purpose |
| --- | --- |
| `Camera::fit(world_size) -> Camera` | Default: `zoom = 1.0`, `center = (world_size/2, world_size/2)`. |
| `view_proj(viewport, world_size) -> Mat4` | World→clip matrix. Replaces the body of `update_camera`'s matrix build. |
| `screen_to_world(cursor, viewport, world_size) -> Vec2` | Inverse. Replaces `editor::screen_to_world`. Result clamped to `[0, world_size]` per axis (preserves today's edge-snap behavior). |
| `zoom_at(cursor, factor, viewport, world_size)` | Multiply `zoom` by `factor` (clamped), then adjust `center` so the world point under `cursor` stays under `cursor`. Re-clamps pan. |
| `pan_by(screen_delta, viewport, world_size)` | Shift `center` by `screen_delta` converted to world units at the current zoom. Re-clamps pan. |
| `reset(world_size)` | `*self = Camera::fit(world_size)`. |

**The fit geometry** (preserved from today). For viewport aspect `a = vw/vh`, the
visible world extent at `zoom = 1` is:
- `a >= 1`: width `world_size * a`, height `world_size` (letterboxed left/right).
- `a < 1`: width `world_size`, height `world_size / a` (letterboxed top/bottom).

At zoom `z`, the visible extent is that divided by `z`. `view_proj` is the
orthographic projection of the visible rect (derived from `center`, `zoom`,
aspect) into clip space; `screen_to_world` inverts it.

**Zoom clamp.** `zoom` is clamped to `[MIN_ZOOM, MAX_ZOOM]` before `center` is
adjusted, so `zoom_at` past the bounds is a no-op on zoom (and the anchor stays
exact because no zoom change means no center shift).

**Pan clamp.** After any `zoom_at`/`pan_by`, `center` is clamped so the visible
rect stays within `[0, world_size]²`. When the visible extent on an axis exceeds
`world_size` (always true on the letterboxed axis at zoom 1, or on both axes if a
future change lets you zoom below fit), `center` on that axis is pinned to
`world_size/2` rather than clamped to an empty interval.

## §2 — Camera lives on `App`, orthogonal to mode and tool

The camera is **view** state, not scene state. It lives on `App`
([src/app.rs](../../../src/app.rs)), not in `editor::DragState`, and is independent
of the active tool and of Run/Edit mode.

```rust
// src/app.rs — App fields
camera: crate::camera::Camera,
space_held: bool,
pan_anchor: Option<winit::dpi::PhysicalPosition<f64>>, // Some while panning
```

**Lifecycle:**
- Initialized to `Camera::fit(world_size)` when the first scene/sim loads.
- **Persists** across Run↔Stop (you keep your view when you start/stop the sim).
- **Resets** to `Camera::fit(new_world_size)` on chemistry switch (the scene and
  possibly `world_size` change).
- **Re-clamps** on window resize: `zoom` and `center` are kept, but the fit
  geometry changes with aspect, so pan is re-clamped (and the effective view
  updates) — implemented by re-running the pan clamp with the new viewport.

`cursor_world()` ([src/app.rs:358-366](../../../src/app.rs#L358-L366)) switches
from calling the free `editor::screen_to_world` to `self.camera.screen_to_world`.
`update_camera` is called with the camera each frame it changes.

## §3 — Interaction

All gestures arrive as winit `WindowEvent`s (works on native and web alike).

**Wheel zoom.** New `WindowEvent::MouseWheel { delta, .. }` arm. Normalize delta:
```rust
let scroll = match delta {
    MouseScrollDelta::LineDelta(_, y) => y,
    MouseScrollDelta::PixelDelta(p)   => (p.y as f32) / 50.0, // px → ~notches
};
let factor = ZOOM_STEP.powf(scroll);
self.camera.zoom_at((self.cursor.x, self.cursor.y), factor, viewport, world_size);
```
Scroll up (positive `y`) zooms in. The cursor's world point stays fixed.

**Pan.** Two ways, both producing a pan drag:
- Middle-button: `WindowEvent::MouseInput` with `MouseButton::Middle`, Pressed →
  set `pan_anchor = Some(cursor)`; Released → `pan_anchor = None`.
- Space+left: track `space_held` from `KeyboardInput` (`NamedKey::Space`). In
  `on_mouse_down`, if `space_held` is true, start a pan (set `pan_anchor`) and
  **do not** route the press to the editor tool. On left-release while panning,
  end the pan.

While `pan_anchor.is_some()`, `CursorMoved` calls
`camera.pan_by(cursor - last, viewport, world_size)` (drag the world with the
cursor) and updates the anchor; it does **not** route to the editor's
`on_mouse_move`. A pan press is mutually exclusive with an edit gesture: if a pan
is active, Place/Chain/Rect/Lasso/Move see nothing.

**Reset.** In the `KeyboardInput` Pressed arm, `Key::Character("0")` (and
`NamedKey`'s numpad 0 if distinct) → `self.camera.reset(world_size)`. Works in any
mode.

**Redraw.** Any camera change calls `renderer.update_camera(&self.camera, …)` and
requests a redraw.

## §4 — Render & overlay (free)

`render.rs::update_camera` changes signature to take the camera (plus the existing
palette) and builds `view_proj` via `camera.view_proj(self.size, world_size)`
instead of the hard-coded fit matrix. The `world_size` field in the `CameraUbo`
(used by the wrap-ghost logic in the shader) is unchanged.

The rect/lasso overlay (world-space `LineList` segments via `update_overlay`) and
the 9× torus wrap-ghosts both already render through `view_proj`, so they track
zoom and pan with no further change. Selection rings (per-bead, in the shader)
likewise scale with the view automatically.

## §5 — Files

- **`src/camera.rs`** (new): `Camera`, constants, the six methods, unit tests.
- **`src/lib.rs`**: `pub mod camera;`.
- **`src/render.rs`**: `update_camera` takes `&Camera`; build `view_proj` from it.
  Update both call sites' signatures.
- **`src/editor.rs`**: remove the free `screen_to_world` (moved into `Camera`), or
  keep a thin shim that constructs a fit camera for any remaining non-camera
  callers. Tests that exercised `screen_to_world` move/adapt to `Camera`.
- **`src/app.rs`**: `camera`, `space_held`, `pan_anchor` fields; `MouseWheel` arm;
  middle-button + space handling in `MouseInput`/`KeyboardInput`; pan branch in
  `CursorMoved`; `0`-key reset; camera reset on chemistry switch; re-clamp on
  resize; `cursor_world` and `update_camera` call sites use `self.camera`.

Untouched: sim, scheduler, chemistry, fab presets, bond model, `index.html`
(no JS changes — gestures are pure canvas input through winit).

## §6 — Testing

**Unit — `camera.rs` (pure math):**
- `fit`: `screen_to_world` of the viewport center returns the world center;
  corners map to the letterboxed world extent for both aspect branches.
- **Round-trip:** for a sampling of `(zoom, center, cursor)`, world→screen→world
  is the identity (within float epsilon), away from the edge-clamp region.
- **Zoom anchor invariant:** after `zoom_at(cursor, factor, …)`, the world point
  that was under `cursor` is still under `cursor` (the defining property of
  cursor-anchored zoom).
- **Zoom clamp:** `zoom_at` with a large `factor` stops at `MAX_ZOOM`; with a tiny
  `factor` stops at `MIN_ZOOM = 1.0`; at the clamp the anchor is exact (no center
  drift).
- **Pan clamp:** after `pan_by` with a large delta, the visible rect still lies
  within `[0, world_size]²`; on a letterboxed axis `center` pins to
  `world_size/2`.
- **Reset:** `reset` equals `fit`.
- **Aspect:** wide and tall viewports both round-trip and clamp correctly
  (port the existing `screen_to_world_*` viewport tests in `editor.rs` to
  `Camera`).

**Browser smoke** (`scripts/verify-web.py --editor`):
- Expose a read-only bridge global `__jigglefabGetZoom() -> f32` for assertions.
- Dispatch a wheel event over the canvas; assert `__jigglefabGetZoom()` increased.
- Reset via the `0` key; assert `__jigglefabGetZoom()` returns to ~1.0.
- (Pan is hard to assert headlessly; covered by unit tests. If a `__jigglefabGetCenter()`
  is cheap to add, assert a middle-drag moved it, else rely on unit coverage and
  note the gap.)

**Manual / parity:**
- Native and web: wheel zooms toward cursor; middle-drag and space+left-drag pan;
  `0` resets; zoom can't go below fit; world can't be panned off-screen; gestures
  work while the sim runs.

## §7 — Open / deferred questions

- **Pan key conflict:** `Space` is otherwise unused in the editor today; if a
  future feature wants it (e.g. play/pause), revisit. Middle-drag is the
  conflict-free primary.
- **Reset key:** `0` chosen over `Home`/`F` for reachability; trivial to change.
- **Zoom below fit:** intentionally disallowed (`MIN_ZOOM = 1.0`). The pan-clamp
  math already handles "visible extent exceeds world" so relaxing this later is a
  one-constant change plus a center-pin on both axes.
- **Trackpad pixel-delta tuning:** the `/50.0` px→notch divisor is a guess; tune
  against a real trackpad during implementation.
- **Camera in saved scenes:** save/load (separate spec) does not yet persist
  camera state; a loaded scene opens at fit. Revisit if users expect their view
  to round-trip.
