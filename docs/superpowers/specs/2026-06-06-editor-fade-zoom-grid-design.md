# Editor — fade-on-zoom adaptive scale grid

Status: approved 2026-06-06. Implementation plan: TBD.

Refines the torus seam line (`docs/superpowers/specs/2026-06-06-editor-torus-pan-edit-design.md` §4), which drew an always-on faint box at the domain boundary. This **replaces** it with an adaptive scale grid that is invisible at rest and **fades in while the camera is moving** (zoom or pan), then fades out when the camera settles — giving a sense of scale exactly when it's useful, without permanent clutter.

## Goal

While zooming or panning, briefly show a subtle grid whose spacing adapts to the zoom level (so cells stay a sensible on-screen size), with the world-tile boundary drawn a touch bolder. Fade it out shortly after the camera goes idle.

## Scope

**In:**
- An adaptive grid: spacing = `world_size / 2^level`, `level` chosen so on-screen cell size stays near a target; always aligns to the domain so `world_size` multiples can be drawn bolder.
- A fade driven by camera activity: zoom (wheel) or pan resets an idle timer; `grid_alpha` eases in (~120 ms) while active and out (~400 ms) after a ~0.4 s hold.
- Reuse the overlay pipeline's per-vertex `shade` (from the torus work): grid `shade = grid_alpha × line_weight × GRID_SHADE`.
- Removal of the always-on `seam_segments`/`SEAM_SHADE` path.

**Out (deferred):**
- Numeric scale labels / a ruler.
- Cross-fading between two adjacent grid levels (single level per frame; the overall fade masks level "pops").
- Grid during the drag overlay only (it's a camera affordance, independent of edit tool).
- Making the grid configurable in the UI.

**Tunable, flagged:** the user is unsure about **pan** (vs zoom-only) as a trigger. Pan-triggering is included but isolated to a single call (`note_camera_activity()` in the pan branch) so it can be dropped trivially after seeing it live.

## §1 — Fade state machine

Two new `App` fields:
```rust
grid_alpha: f32,   // current animated alpha, 0..1
idle_since: f32,   // seconds since the last camera move
```
`fn note_camera_activity(&mut self)` sets `self.idle_since = 0.0`. Called from the `MouseWheel` handler (after `zoom_at`) and the pan branch of `CursorMoved` (after `pan_by`). **The pan call is the one the user wants to be able to remove easily** — it's a single line.

Once per frame, in the `RedrawRequested` handler (where `self.last_frame` is already managed via `web_time::Instant`), compute the frame `dt` and step the fade with a pure helper:

```rust
// constants
const GRID_HOLD_S: f32 = 0.4;     // stay fully in for this long after the last move
const GRID_FADE_IN_S: f32 = 0.12; // ease-in time constant
const GRID_FADE_OUT_S: f32 = 0.40;// ease-out time constant

/// Advance the grid fade one frame. Returns the new alpha in [0,1].
fn grid_fade_step(alpha: f32, idle_since: f32, dt: f32) -> f32 {
    let target = if idle_since < GRID_HOLD_S { 1.0 } else { 0.0 };
    let tau = if target > alpha { GRID_FADE_IN_S } else { GRID_FADE_OUT_S };
    // exponential approach, frame-rate independent; clamp the step at 1.0
    let k = (dt / tau).min(1.0);
    alpha + (target - alpha) * k
}
```
Per frame: `let dt = self.last_frame.elapsed().as_secs_f32();` (already computed for sim timing) → `self.idle_since += dt;` → `self.grid_alpha = grid_fade_step(self.grid_alpha, self.idle_since, dt);`. The app redraws every frame in Poll mode, so this animates in both Run and Edit.

## §2 — Adaptive grid geometry

A pure generator replaces `seam_segments`:
```rust
/// Adaptive grid lines for the visible rect, as (LineList vertex pairs) tagged
/// with a per-line weight (1.0 for world-tile boundaries, ~0.5 for interior
/// subdivision lines). `world_per_px` = visible world width / viewport pixels.
pub fn grid_segments(min: Vec2, max: Vec2, world_size: f32, world_per_px: f32)
    -> Vec<([f32; 2], f32)>;   // (position, weight) per vertex
```
Spacing selection:
- `TARGET_PX = 80.0`, `MAX_LEVEL = 6`.
- `level = round(log2(world_size / (TARGET_PX * world_per_px)))`, clamped to `[0, MAX_LEVEL]`.
- `spacing = world_size / 2f32.powi(level)`.
- Level 0 (zoomed out far) → spacing = `world_size` → only domain-boundary lines. Higher levels add finer subdivisions. Because `spacing` always divides `world_size`, a line at coordinate `c` is a **boundary** iff `c` is a multiple of `world_size` → weight `1.0`; otherwise weight `0.5`.

Geometry: like `seam_segments`, emit a vertical line `(x,min.y)-(x,max.y)` for every `x = k*spacing ∈ [min.x, max.x]`, and horizontals likewise, each vertex tagged with its line's weight. At the chosen `level`, the count of visible lines is bounded (target ~80 px spacing over the viewport ⇒ a few-to-~dozen lines per axis).

`world_per_px` comes from the camera: add `Camera::world_per_px(viewport, world_size) -> f32` returning `visible_extent(viewport, world_size).x / viewport.0`.

## §3 — Rendering & app wiring

`overlay_segments` (returns `Vec<crate::render::OverlayVertex>`):
- If `grid_alpha > 0.001` and a viewport exists: compute `(min,max)` from `Camera::visible_world_rect`, `wpp` from `Camera::world_per_px`, then for each `(pos, weight)` from `grid_segments`, push `OverlayVertex { pos, shade: self.grid_alpha * weight * GRID_SHADE }`. Skipped entirely at rest (zero verts when faded out).
- Then the bright drag overlay (rect/lasso) at `shade: 1.0`, unchanged.

`GRID_SHADE` (e.g. `0.35`) sets the fully-faded-in subtlety; boundary lines reach `grid_alpha*0.35`, interior `grid_alpha*0.175`. (Overlay fragment already multiplies by `0.7`, so peak boundary alpha ≈ `0.7*0.35 ≈ 0.25`.)

Removed: `seam_segments` and the `SEAM_SHADE` constant (superseded). The overlay capacity (512) still bounds the combined grid+drag vertex count; the adaptive level cap keeps the grid small.

## §4 — Web bridge

Add `__jigglefabGridAlpha() -> f32` mirroring `__jigglefabGetZoom`: a `grid_alpha: f32` field on `web_bridge::Snapshot`, populated from `self.grid_alpha` in the snapshot write, exposed via an installer called in the wasm init block. Used by the smoke test to assert the fade in/out without pixel inspection.

## §5 — Files

- **`src/camera.rs`**: replace `seam_segments` with `grid_segments`; add `world_per_px`. Update/replace its tests.
- **`src/app.rs`**: `grid_alpha`/`idle_since` fields + init; `note_camera_activity` (called in wheel + pan handlers); per-frame `grid_fade_step` in `RedrawRequested`; `overlay_segments` emits the faded weighted grid; remove `SEAM_SHADE`, add `GRID_SHADE`; `grid_fade_step` + fade constants; `__jigglefabGridAlpha` bridge (Snapshot field + installer + init call).
- **`scripts/verify-web.py`**: grid-fade smoke assertion.

Untouched: shader (`overlay.wgsl` already does `0.7 * shade`), render pipeline, sim, editor gestures, torus pan/edit behavior.

## §6 — Testing

**Unit — `grid_segments` (camera.rs):**
- Zoomed out (`world_per_px` large ⇒ level 0): only `world_size`-multiple lines, all weight 1.0.
- Zoomed in (`world_per_px` small ⇒ level ≥ 1): interior subdivision lines present at weight 0.5; boundary lines still weight 1.0.
- Spacing always divides `world_size` (a subdivision line's coordinate is a multiple of `world_size/2^level`).
- Level clamps at `MAX_LEVEL` for extreme zoom-in (no unbounded line count).
- `world_per_px` matches `visible_extent.x / viewport_width`.

**Unit — `grid_fade_step` (app.rs or a small module):**
- Activity (`idle_since = 0`) drives alpha toward 1.0 over successive steps.
- Idle (`idle_since > GRID_HOLD_S`) drives alpha toward 0.0.
- Frame-rate independence: a large `dt` clamps the step (no overshoot past target).
- From `alpha=0`, one `GRID_FADE_IN_S`-sized `dt` reaches ~0.63 (1 - 1/e) — i.e. eases, not steps.

**Browser smoke (`scripts/verify-web.py --editor`):**
- Read `__jigglefabGridAlpha()` at rest → ~0.
- Wheel-zoom; within ~100 ms assert alpha rose > 0.1.
- Idle ~0.8 s; assert alpha returned to < 0.05 (faded out).

## §7 — Open / deferred questions

- **Pan as a trigger:** included but isolated (§1). Evaluate live; if it feels noisy, delete the one `note_camera_activity()` call in the pan branch (zoom-only remains).
- **Constants** (`TARGET_PX=80`, `GRID_SHADE=0.35`, hold/fade times): starting values; tune against the dark background during implementation so the grid reads as a hairline that appears/vanishes gently.
- **Level pop:** crossing a `level` threshold mid-zoom swaps spacing in one frame; the simultaneous fade hides it. Revisit with a two-level crossfade only if it's visibly distracting.
- **Torus wrap:** the grid is drawn in raw visible-rect coordinates (multiples of `world_size`, including negative tiles when panned across the seam) — consistent with the torus rendering, same as the seam line it replaces.
