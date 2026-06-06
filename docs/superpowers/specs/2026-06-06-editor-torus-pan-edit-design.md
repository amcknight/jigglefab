# Editor — torus-native pan + edit

Status: approved 2026-06-06. Implementation plan: TBD.

Builds on the live zoom + pan feature (`docs/superpowers/specs/2026-06-05-editor-live-zoom-pan-design.md`). That shipped with the camera **clamped** to the `[0, world_size]²` domain — but the simulation is a true torus (`grid.wrap_pos`, min-image pair distances, 3×3 ghost rendering). The clamp fights that topology: it pins the view and all editing to one arbitrary cut of a seamless wrapping space, producing an invisible wall when you pan or draw near the edge. This makes the editor torus-native: pan wraps seamlessly, you can draw/select/move anywhere, and a faint seam line keeps you oriented.

## Goal

Remove the boundary wall. Panning wraps forever with no void; all edit gestures (place, chain, rect/lasso select, move) work across the seam; a faint grid line marks the repeating domain boundary for orientation.

## Scope

**In:**
- **Pan wraps:** camera center wraps mod `world_size`; no clamp. Seamless because the 3×3 ghost ring always renders a full neighborhood.
- **Raw cursor coords:** `Camera::screen_to_world` returns continuous, unclamped world coords. Wrapping happens at bead-commit time.
- **Edit anywhere, torus-aware:** place/chain wrap committed positions and use min-image so seam-crossing chains stay correctly spaced; rect/lasso select a bead if any of its ghost copies lies in the (raw) region; move hit-test and translate use min-image / wrap.
- **Faint seam line:** the domain-boundary grid (lines at multiples of `world_size`) rendered dim through the overlay pipeline, for every boundary intersecting the view.

**Out (deferred):**
- Changing the min/max zoom range (stays `[1.0, 16.0]`).
- Per-bead "which tile am I in" labels or a minimap.
- Snapping draws to the seam, or any seam-aware editing affordance beyond correctness.
- Saving camera/pan state with a scene.

## Background: the torus primitives (already exist)

- `Grid::wrap_pos(pos)` — wraps a position into `[0, world_size)` per axis ([src/grid.rs:160](../../../src/grid.rs#L160)).
- `Grid::min_image(from, to)` — shortest torus displacement from `from` to `to`, components in `[-world_size/2, world_size/2]`; works on raw (out-of-domain) inputs since it only wraps the delta ([src/grid.rs:217](../../../src/grid.rs#L217)).
- `Scene::place` already derives bonds via `min_image` ([src/editor.rs:213](../../../src/editor.rs#L213)).
- Renderer draws each bead 9× at `±world_size` offsets (the ghost ring) so wrapped content is visible.

## §1 — Pan wraps (camera)

`Camera::pan_by` drops `clamp_pan` and instead wraps the center per axis:
```rust
self.center.x = (self.center.x - screen_delta.0 * vis.x / vw).rem_euclid(world_size);
self.center.y = (self.center.y + screen_delta.1 * vis.y / vh).rem_euclid(world_size);
```
`zoom_at` likewise wraps the center it computes (instead of `clamp_pan`). Wrapping the center by a multiple of `world_size` shifts the view by whole tiles, which—because of ghost tiling—shows identical content, so the world point under the cursor stays visually fixed (it lands on an identical ghost). The anchor invariant therefore holds **modulo `world_size`**, not as raw equality. `Camera::fit` and `reset` are unchanged (center = world center, zoom 1).

`clamp_pan` is **removed** (the letterbox-pin logic it held is no longer needed — wrapping handles every case). The `Resized` re-clamp call in `app.rs` becomes a no-op to drop (or a harmless `rem_euclid` re-wrap); the spec drops the resize re-clamp entirely since a wrapped center is always valid.

Min zoom stays `1.0`. Panning at zoom 1 wraps the (whole-world) view harmlessly. The 3×3 ghost ring covers any view window up to ~3×`world_size`, which bounds the visible extent at zoom ≥ 1 on any aspect — so no additional ghost copies are needed.

## §2 — Raw cursor coordinates

`Camera::screen_to_world` currently clamps its result to `[0, world_size]`. Change it to return the **raw, continuous** world coordinate (delete the clamp). `screen_to_world_raw` and the new public method become identical, so collapse them into one un-clamped `screen_to_world`. The zoom anchor math (`zoom_at`) already used the raw value, so it is unaffected.

Consequence: callers that previously relied on the clamp must now wrap at the point a position becomes persistent scene data (§3). Cursor positions used purely for hit-testing or as gesture anchors stay raw.

## §3 — Edit anywhere (torus-aware gestures)

All gesture geometry is computed in raw coords; positions are wrapped into the domain only when stored as a bead. `Scene` constructs a `Grid::new(self.world_size)` for min-image/wrap (as `place` already does).

- **`place(pos)`** ([src/editor.rs:201](../../../src/editor.rs#L201)): wrap on entry — `let pos = grid.wrap_pos(pos);` before pushing. Bond derivation already uses `min_image`, so unchanged otherwise.
- **`append_chain_bead(pos, prev)`** ([src/editor.rs:224](../../../src/editor.rs#L224)): wrap `pos` before pushing.
- **`chain_extend(last_idx, cursor)`** ([src/editor.rs:239](../../../src/editor.rs#L239)): replace `to_cursor = cursor - last_pos` with `to_cursor = grid.min_image(last_pos, cursor)`. Stepping then follows the *short* torus path, so a chain dragged across the seam keeps `CHAIN_STEP` spacing and wraps each bead into the domain via `append_chain_bead`. The consecutive bond is valid; the torus sim handles the seam.
- **`select_rect(a, b)` / `select_lasso(poly)`** ([src/editor.rs:257](../../../src/editor.rs#L257), [:268](../../../src/editor.rs#L268)): a bead is selected if **any of its 9 ghost copies** (`pos + (k,l)*world_size`, `k,l ∈ {-1,0,1}`) lies inside the raw region. Add a helper, e.g. `fn any_ghost<F: Fn(Vec2)->bool>(pos: Vec2, world_size: f32, inside: F) -> bool`, and call it with `point_in_rect(.., a, b)` / `point_in_polygon(.., poly)`. `a`/`b`/`poly` stay raw. (Selection runs on mouse-up, so the 9× test is not a hot path.)
- **`translate_selection(delta)`** ([src/editor.rs:281](../../../src/editor.rs#L281)): replace the `.clamp(0.0, w)` with wrap — `grid.wrap_pos(Vec2::new(b.pos[0]+delta.x, b.pos[1]+delta.y))`.
- **`hit_selected` (app.rs)**: the Move hit-test currently uses `(p - world_pos).length()`; change to `grid.min_image(world_pos, p).length() <= RADIUS` so you can grab a selected bead by whichever ghost is under the cursor. (Build a `Grid` from `scene.world_size`.)

Gesture-point storage in `app.rs`/`DragState` (rect anchor/current, lasso points, chain reference) stays raw — no wrapping during the drag.

## §4 — Faint seam line

Render the domain-boundary grid through the existing thin-line overlay pipeline, dimmed.

**Overlay pipeline change:** the overlay vertex is currently `[f32; 2]` (world pos) and the fragment hardcodes `vec4(1,1,1,0.7)` ([shaders/overlay.wgsl:29](../../../shaders/overlay.wgsl#L29)). Add a per-vertex `shade: f32` (alpha multiplier): vertex becomes `{ pos: [f32;2], shade: f32 }`; fragment returns `vec4(1.0, 1.0, 1.0, 0.7 * in.shade)`. `Renderer::update_overlay` takes the new vertex type. Existing drag overlay uses `shade = 1.0` (unchanged look); seam lines use a small shade (≈ 0.25 → ~0.18 alpha).

**Seam-line geometry (per frame, app-side):** expose `Camera::visible_world_rect(viewport, world_size) -> (Vec2 min, Vec2 max)` (the raw rect `center ± vis/2`). The overlay builder emits, for each integer `m` with `m*world_size ∈ [min.x, max.x]`, a vertical segment from `(m*world_size, min.y)` to `(m*world_size, max.y)`; likewise horizontals for `m*world_size ∈ [min.y, max.y]`. These are appended (shade ≈ 0.25) to whatever drag-overlay segments exist (shade 1.0), and the combined list is uploaded each frame. At zoom ≥ 1 only a handful of boundary lines are ever visible.

The seam grid is purely cosmetic: it never blocks pan or edit (those are unclamped per §1–§3).

## §5 — Files

- **`src/camera.rs`**: `pan_by`/`zoom_at` wrap center instead of `clamp_pan`; remove `clamp_pan`; `screen_to_world` returns raw (collapse the `_raw` split); add `visible_world_rect`. Update/extend tests.
- **`src/editor.rs`**: wrap in `place`/`append_chain_bead`/`translate_selection`; min-image in `chain_extend`; 9-ghost `any_ghost` helper used by `select_rect`/`select_lasso`. Update/extend tests.
- **`src/app.rs`**: `hit_selected` uses min-image; drop the resize re-clamp; overlay builder appends seam-line segments with shade and uses the new overlay vertex type.
- **`src/render.rs`**: overlay vertex gains `shade`; `update_overlay` signature; overlay vertex buffer layout (+1 `f32` attribute).
- **`shaders/overlay.wgsl`**: `VsIn`/`VsOut` carry `shade`; fragment multiplies alpha.

Untouched: sim, scheduler, chemistry, fab presets, bond model.

## §6 — Testing

**Camera (unit):**
- `pan_by` wraps: a pan that would push center past `world_size` leaves `center` in `[0, world_size)`; a pan of exactly `+world_size` returns to the start (seamless).
- `screen_to_world` no longer clamps: a cursor mapping to a raw coord outside `[0, world_size]` returns that raw value (not the edge).
- `visible_world_rect` matches `center ± visible_extent/2`.
- Zoom-anchor test updated to assert the cursor's world point is fixed **modulo `world_size`** (min-image distance ≈ 0), since the wrapped center may differ from the pre-zoom anchor by a whole tile. Round-trip tests still pass.

**Editor (unit):**
- `place` wraps a beyond-domain position into `[0, world_size)`.
- `chain_extend` across the seam: a cursor one min-image step beyond `world_size` from `last_pos` places a bead at `CHAIN_STEP` spacing on the **short** side; assert min-image distance between consecutive beads ≈ `CHAIN_STEP`, not `world_size − CHAIN_STEP`.
- `select_rect` / `select_lasso` with a region straddling the seam (raw coords spanning `world_size`) select a bead whose domain position is on the far side (ghost-in-region).
- `select_rect` not crossing the seam still selects exactly the in-rect beads (regression).
- `translate_selection` wraps: moving a selected bead past the boundary lands it on the opposite side (not clamped to the edge).

**Browser smoke (`scripts/verify-web.py --editor`):**
- Pan past the edge (middle-drag a large delta): bead count unchanged and the canvas still renders content (not the empty clear color) — confirms seamless wrap, no void.
- Draw a chain that crosses a boundary; assert bead count increased and beads exist on both sides of the seam.
- Rect-select across the seam; assert selection count > 0.

## §7 — Open / deferred questions

- **Seam-grid density at low zoom:** at exactly zoom 1 the whole world fills the view and only the outer boundary shows; that's fine. No adaptive density needed within `[1, 16]`.
- **Seam shade value:** `0.25` is a starting guess; tune against the dark clear color during implementation so it reads as a hairline, not a wall.
- **Concave lasso spanning >1×world_size:** the 9-ghost test is exhaustive for any region the view can produce at zoom ≥ 1 (visible extent ≤ ~2×world_size), so no higher ghost ring is needed.
- **Selection visualization across the seam:** selected beads already draw their ring on every ghost copy (per-bead shader flag), so a selection straddling the seam highlights correctly with no extra work.
