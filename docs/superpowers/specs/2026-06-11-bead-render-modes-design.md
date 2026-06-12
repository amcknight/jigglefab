# Bead Render Modes — Design

**Date:** 2026-06-11
**Status:** spec, awaiting implementation plan

## Motivation

The current renderer draws each bead as a soft-edged instanced disc. When
beads sit close together their discs overlap, which violates a property the
user wants to make permanent: **no overlap, no order-precedence — every pixel
is computed from scene state alone, never from draw order.**

The user wants to experiment with several field-based render modes
(Voronoi, metaballs, …) under a runtime toggle, rather than committing to one
"best" look. The visual variety is part of the product; the toggle is a
permanent feature, not a debug switch.

This spec covers the v1 set: six render modes selectable from a runtime
toggle, sharing one expanded bead buffer.

## Scope

### In scope (v1)

Six render modes:

1. **Disc** — current renderer, kept as a baseline option.
2. **Voronoi (crisp)** — hard nearest-bead cells, clipped to local extent.
3. **Soft Voronoi** — hard cells with fuzzy seams based on contestedness.
4. **Metaball-blend** — bond-aware sum-of-fields, color is field-weighted
   average within the bonded component.
5. **Metaball-argmax** — same merged shape as blend, color from max-field
   bead (crisp seams, soft skin).
6. **Worley** — `d₂ − d₁` glow, decorative, clipped to local extent.

Universal constraint: **color ends locally** in every mode. Voronoi /
Soft-Voronoi / Worley clip when nearest distance exceeds `1.5 × R`. Metaball
modes naturally clip at iso threshold.

Bead buffer carries velocity (`vel: vec2<f32>`) from day one, even though no
v1 mode reads it. Adding anisotropic-velocity modes later costs no buffer
churn.

### Out of scope (v1)

- Anisotropic-velocity render modes (velocity-trail metaballs,
  velocity-weighted Voronoi). Data is wired through; modes ship later.
- Per-mode live sliders (radius, iso threshold, falloff curve) as user UI.
  Constants are tuned in code for v1.
- 3D rendering.
- Bond/edge decoration beyond what the editor already shows.
- A separate render-mode toggle for sim view vs. editor view — they share the
  renderer and thus the mode.

## Architecture

### Two pipelines, selected on the CPU

The `Renderer` holds a `mode: RenderMode` enum on its public surface. Each
frame, `render.rs` binds one of two GPU pipelines based on `mode`:

- **Disc pipeline (existing, unchanged).** Instanced quads, one per bead,
  3×3 toroidal ghost wrap. Runs only when `mode == Disc`.
- **Field pipeline (new).** Single full-screen quad, fragment shader does all
  field math. Runs for all other five modes. The shader switches on a `u32`
  uniform to pick the mode's color math.

Two pipelines (not one branched) because the draw call shape differs: Disc is
per-bead instanced, Field is per-pixel full-screen. Forcing one pipeline to
do both produces awkward stub geometry. Both pipelines read the same bead
buffer; Disc just ignores the new fields.

### Shader pipeline (Field)

The fragment shader is a small per-pixel pipeline:

```
enumerate beads → accumulate field → branch on mode → write color
```

**Field accumulation (mode-independent first pass).** One pass over the bead
buffer fills a `FieldAccum` struct with everything every mode needs to
*decide whether to draw and in roughly what shape*:

```wgsl
struct FieldAccum {
    nearest_idx: u32,         // smallest distance — Voronoi family
    nearest_d: f32,
    second_d: f32,            // second-smallest — Worley, Soft-Voronoi seam
    argmax_idx: u32,          // largest field — metaball-argmax color, and
                              // anchors the component for both metaball modes
    argmax_f: f32,
    total_f_in_comp: f32,     // field sum, restricted to argmax bead's component
}
```

**Metaball-blend takes a second pass.** The blend formula
`Σ f_i · color_i / Σ f_i` needs each in-component bead's weighted color
contribution, which cannot fit in a fixed-size struct. So mode 3 walks the
bead buffer once more, summing weighted colors over beads with
`component_id == component_id[argmax_idx]`. This is the only mode with a
second pass; all others read from `FieldAccum` alone.

**Toroidal wrap inside the accumulator.** Each bead is considered 9 times
(3×3 ghost grid), matching how the existing Disc renderer handles wrap.

**Mode dispatch.** After accumulation, one switch on the `mode` uniform
picks the color function:

```wgsl
switch (mode) {
    case 0u: { color = voronoi_color(acc); }          // hard cell, clip far
    case 1u: { color = soft_voronoi_color(acc); }     // hard, fuzzy seam
    case 2u: { color = worley_color(acc); }           // d2 - d1 glow
    case 3u: { color = metaball_blend_color(p, acc); }  // weighted color
    case 4u: { color = metaball_argmax_color(acc); }  // argmax color
    default: { discard; }
}
```

Each `*_color` function is ~10 lines.

### Field math — concrete rules

**Falloff (compact support):**
```
f(d) = (1 - d²/R²)²    for d < R
f(d) = 0               otherwise
```
Compact support guarantees distant beads contribute zero. This is the reason
metaball-blend does not need a K-nearest cap: only beads close enough to
overlap fields can color a pixel at all.

**Voronoi (mode 0):**
- Color = `state_colors[bead[nearest_idx].state]` if `nearest_d < 1.5 × R`,
  else background.

**Soft Voronoi (mode 1):**
- Same as Voronoi, but seam-soften: `contest = 1 - clamp((second_d - nearest_d) / 0.04R, 0, 1)`.
- Blend toward second-nearest's color by `0.5 × contest` (never fully — keeps
  ownership crisp).
- Clip when `nearest_d > 1.5 × R`.

**Worley (mode 2):**
- `intensity = clamp((second_d - nearest_d) × 4, 0, 1)`.
- Color = nearest bead's color × intensity (cracks glow as low-intensity
  borders).
- Clip when `nearest_d > 1.5 × R`.

**Metaball-blend (mode 3) — bond-aware:**
1. Find argmax bead `i*`. Let `C* = component_id[i*]`.
2. Sum `total_f = Σ f_i(p)` over beads with `component_id == C*`.
3. If `total_f < ISO`, discard.
4. Color = `Σ f_i(p) · color_i / total_f` over the same restricted set.
5. Apply soft edge: scale color toward background as `total_f` approaches
   `ISO` from above.

**Metaball-argmax (mode 4) — bond-aware:**
1–3. Same as blend.
4. Color = `state_colors[bead[argmax_idx].state]`.
5. Same soft edge as blend.

The bond-aware restriction means two unbonded beads that drift close together
do **not** fuse — each renders as its own blob. A 5-bead chain renders as one
welded shape with smooth color blending throughout.

## Data layout

### `BeadGpu` (CPU + WGSL)

```rust
#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
struct BeadGpu {
    pos: [f32; 2],        // existing
    vel: [f32; 2],        // new — wired but unused by v1 modes
    state: u32,           // existing — color index
    selected: u32,        // existing
    component_id: u32,    // new — bond-graph connected component
    _pad: u32,            // alignment to 8
}
```

32 bytes per bead. At 5k beads = 160 KB — negligible.

WGSL mirror:
```wgsl
struct Bead {
    pos: vec2<f32>,
    vel: vec2<f32>,
    state: u32,
    selected: u32,
    component_id: u32,
    _pad: u32,
};
```

### `CameraUbo`

Add two fields to the existing UBO:

```rust
struct CameraUbo {
    view_proj: [[f32; 4]; 4],
    radius: f32,
    world_size: f32,
    bead_count: u32,                // new — fragment shader needs explicit N
    mode: u32,                      // new — RenderMode discriminant
    state_colors: [[f32; 4]; 8],
}
```

`mode` lives in the camera UBO rather than its own binding — cheap, already
bound.

### Connected-component computation

`component_id` is computed CPU-side via union-find over the bond graph and
written into each `BeadGpu`.

- Runs only when bonds change (dirty flag from the sim).
- Cost when it does run: O(N + bonds·α). At 5k beads / 5k bonds, well under
  1ms in wasm.
- Steady state with no bond changes: zero per-frame cost.
- If profiling ever shows this as a bottleneck, two escape hatches:
  incremental union-find on bond add, or moving the computation to a GPU
  compute shader. Neither in v1.

No bond pair buffer is uploaded to the GPU. The shader only ever needs to
compare `component_id` values, which is one `u32` per bead.

## A → C upgrade path (deferred)

If a measured workload (5k+ beads, dense bonds, low-end GPU) shows the
Field-pipeline fragment shader is bound by the O(N) bead loop, swap to a
spatial-grid lookup:

- The simulation already has a coarse spatial grid in [src/grid.rs](src/grid.rs).
- Expose the grid as a GPU storage buffer (cell offsets + bead indices).
- Replace the `accumulate_field` function body in the Field shader: instead
  of iterating `0..bead_count`, iterate the bead indices in the 3×3 grid
  cells around the pixel's cell.
- All other shader code (mode math, color logic, `FieldAccum` struct,
  toroidal wrap, bond-aware restriction) is unchanged.

Estimated effort: half-day. Risk: low — the accumulator is a single function
boundary.

This is not v1 work. It is called out so v1 doesn't accidentally fork the
shader in a way that makes the swap painful.

## UI surface

### Mode picker

A "Render" chip group on the editor side panel, in the scene-level controls
section alongside the chemistry / state pickers (not in the device library
dock — render mode is a view setting, not a scene-content control). Six
labelled options:
`Disc | Voronoi | Soft Voronoi | Metaball Blend | Metaball Argmax | Worley`.

Default: **Disc** (no surprise change for existing users / saved fabs).

### Persistence

The chosen mode is saved to `localStorage` under a key like
`jigglefab:render-mode`. Matches the existing device-library localStorage
pattern. Mode is restored on page reload.

### Keybind

- `R` cycles modes forward.
- `Shift+R` cycles modes backward.
- A small status text near the toggle shows the current mode name so cycling
  is discoverable.

If `R` collides with an existing binding, the planning step picks an
unused key. (Quick check during plan-writing.)

### Selection rings

Disc mode renders selection rings as today (in the existing disc shader's
fragment branch on `selected`).

Field modes render selection rings as a separate post-pass after the field
draw: a per-selected-bead instanced quad with a fragment shader that draws a
thin ring at distance `R` from the bead centre. The post-pass uses its own
small pipeline (a stripped-down version of the existing disc pipeline) so
the field shader stays focused on field math. The ring renders on top of the
field, regardless of merge state, so a selected bead inside a fused blob is
still visibly outlined.

## Testing

### Web smoke test additions

Extend [scripts/verify-web.py](scripts/verify-web.py) with one section per
mode. For each mode:

1. Load the editor, place a known bead cluster: one isolated bead, one
   2-bond bead pair, one 3-bond bead triple.
2. Select the mode from the picker.
3. Take a screenshot.
4. Assert gross properties:
   - Non-background pixel count in the bead region is non-zero (the mode is
     drawing something).
   - Non-background pixel count *far* from any bead (`d > 2 × R` from every
     bead) is zero. This is the **color-ends-locally** invariant — the most
     important regression guard.

### Rust unit tests

- **Connected-component computation:** given a `BondPair` list, every bead's
  `component_id` is correct. Two bonded beads share an id; two isolated
  beads have different ids; a 3-bead chain has one shared id.
- **`BeadGpu` layout:** assert `size_of::<BeadGpu>() == 32`. Round-trip
  `pos`/`vel`/`state` through bytemuck.
- **`RenderMode` serialisation:** for localStorage round-trip.

### Shader golden tests

- A 5-bead canonical test scene rendered in each mode at a fixed camera
  pose. Committed under `tests/golden/render-modes/`.
- `cargo test` reads the goldens and compares against current shader output
  with a small per-pixel tolerance (allows minor driver / precision drift).
- Goldens regenerated explicitly via
  `cargo test -- --ignored regenerate_goldens` when the design changes
  intentionally.

### Perf sanity check (manual)

After implementation: render a 5k-bead bond-heavy scene in each mode and
eyeball framerate. If a mode tanks, that's the signal to start the A → C
grid migration. Not a blocking test.

### Not tested

- Visual aesthetics — the toggle is the answer; eyes are the judge.
- Exact pixel values — goldens use a tolerance; only gross locality /
  presence is strictly asserted.

## Risks & mitigations

| Risk | Likelihood | Mitigation |
|---|---|---|
| Full-screen pass slow at 5k beads / low-end GPU | Medium | A → C swap. Single shader function. Half-day. |
| Connected-component CPU cost in chemistry that flips bonds every frame | Low | Dirty-flag gate. Incremental union-find or GPU scan if it ever shows in profile. |
| Falloff radius `R` constant doesn't suit all chemistry visuals | Medium | v1 constant. Sliders explicitly out of scope; revisit if multiple users report mismatch. |
| Keybind `R` collides with existing UI | Low | Plan step verifies; falls back to alt key. |
| Goldens drift across GPU drivers and cause spurious failures | Medium | Tolerance window; goldens regeneratable. Local-dev escape hatch documented. |

## Future hooks (not v1)

- **Anisotropic-velocity Voronoi:** distance metric stretched along `vel`.
  Velocity already in the buffer, one shader function swap to consume it.
- **Velocity-trail metaballs:** falloff stretched backwards along `vel`.
  Same buffer.
- **Live tweakable sliders:** falloff radius, iso threshold, soft-seam
  width. Constants today, easy to surface later.
- **Bond-only metaballs:** only fuse along actual bond edges (rather than
  whole component). Requires uploading bond pairs as well — buffer addition,
  no shader rearchitecture.
- **Spatial-grid acceleration (the A → C swap):** the upgrade path called
  out above.
