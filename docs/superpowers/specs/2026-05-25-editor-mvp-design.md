# Editor MVP — design

Status: approved 2026-05-25. Implementation plan: TBD.

## Goal

Add a minimal scene editor to the web build so the user can click to place
beads and run their own scene, instead of being limited to baked-in preset
fabs. This is v1 of an editor; line/freehand drawing, save/load, selection,
and region transforms are explicitly out of scope and will come later.

## Scope

**In:**
- Chemistry picker (changing chemistry empties the scene).
- State picker, auto-built from the current chemistry's palette.
- Single-click bead placement at the cursor (one bead per click).
- Run / Edit toolbar buttons. "Stop" = pressing Edit while running.
- Live edits during Run (clicks place beads transparently, see §6).
- Web build only. The native build's GPU CCD demo path is untouched.

**Out (deferred to later milestones):**
- Save / load (download TOML, localStorage).
- Drawing modes (line, freehand with 2/3-unit spacing snap).
- Selection (rectangle select, copy, paste, delete, rotate).
- Undo / redo.
- Resizing the world.
- Native-build editor parity.

## UI surface (HTML)

A new nav bar sits above the existing `#picker` preset row. The preset
picker stays — it's the way to seed the scene with a non-trivial starting
point. The editor toolbar is always visible.

```
[ Edit ]  [ Run ]    chemistry: ( wire | grey | sem_basic )    state: ( off | on )
```

- `Edit` / `Run` are mode buttons; exactly one is active at any time.
  Pressing the inactive one performs the transition described in §4.
- Chemistry pills follow the existing speed-picker pill style
  (`#speed-picker` in `index.html`). Selecting a different chemistry
  empties the scene after a `window.confirm()` if the scene is non-empty.
- State pills are rebuilt whenever chemistry changes, reading
  `__jigglefabGetPalette()`. The active pill is the state that
  newly-placed beads receive.
- Canvas clicks place a single bead at the cursor:
  - During Edit: append to the Scene; renderer picks it up next frame.
  - During Run: snapshot Sim → Scene, append, rebuild Sim, keep running.

## Data model

New file `src/editor.rs`:

```rust
pub enum Mode { Edit, Run }

pub struct Scene {
    pub chemistry: Chemistry,        // owned, used to rebuild Sim
    pub chemistry_name: String,      // exposed to JS picker
    pub world_size: f32,             // fixed at scene creation; taken from initial preset
    pub beads: Vec<BeadSpec>,        // existing fab::BeadSpec
    pub next_state_idx: u32,         // which state the next click places
}

impl Scene {
    pub fn from_fab(fab: &Fab, chem: Chemistry) -> Self;
    pub fn empty(chem: Chemistry, chemistry_name: String, world_size: f32) -> Self;
    pub fn snapshot_from_sim(&mut self, sim: &Sim);   // copy positions/velocities/states back on Stop
    pub fn to_sim(&self) -> Sim;                      // build on Run
    pub fn place(&mut self, pos: Vec2);               // append BeadSpec at cursor using next_state_idx
}
```

`BeadSpec` already supports an optional `vel: Option<[f32; 2]>`
(`src/fab.rs:25`). `snapshot_from_sim` writes `Some(velocity)` for each
bead so that Run/Stop round-trips preserve momentum.

New beads placed during Edit get a random unit-speed velocity, matching
the convention in `Sim::from_fab` (`src/sim.rs:75-79`).

## State machine

```
                  click (Scene.place)
                  ↓
       ┌──────[ Edit ]──────┐
       │                    │
       │ Stop (Edit btn):   │ Run (Run btn):
       │ - Scene.snapshot   │ - sim = Scene.to_sim()
       │   _from_sim(sim)   │ - scheduler = rebuild(sim)
       │ - drop sim         │ - mode = Run
       │ - mode = Edit      │
       │                    │
       └───[ Run ]──────────┘
                  ↑
        click: snapshot, place, rebuild Sim, continue (see §6)
```

`App` (in `src/app.rs`) gains a `mode: Mode` field, an `Option<Scene>`,
and the existing `sim: Option<Sim>` becomes `None` while in Edit mode.

Render loop branch:
- **Edit**: skip `scheduler.step`. Renderer reads positions/states from
  `Scene.beads`.
- **Run**: step scheduler, render from `Sim` as today.

## JS ↔ Rust bridge

Extends the existing `window.__jigglefab*` pattern from
`src/app.rs:79-123`. New globals exposed via the `expose_to_window!` macro:

| Function                          | Returns / behaviour                                           |
| --------------------------------- | ------------------------------------------------------------- |
| `__jigglefabGetMode()`            | `"edit"` or `"run"` — for toolbar to reflect current state    |
| `__jigglefabSetMode(s)`           | `"edit"` performs Stop, `"run"` performs Run                  |
| `__jigglefabGetPalette()`         | `[{ name: string, color: [r,g,b] }, ...]` for state picker    |
| `__jigglefabSetEditState(idx)`    | Sets `Scene.next_state_idx`                                   |
| `__jigglefabGetChemistries()`     | `["wire", "grey", "sem_basic"]` for chemistry picker          |
| `__jigglefabSetChemistry(name)`   | Unconditionally swaps chemistry and empties scene (preserves world_size). JS guards with `confirm()` first. |
| `__jigglefabBeadCount()`          | Current bead count (Scene or Sim depending on mode), for HUD  |

Chemistry TOML files (`chemistries/*.toml`) are `include_str!`'d at
build time, same pattern as the fab presets.

Canvas clicks do **not** go through JS. They're handled in Rust via
existing `WindowEvent::MouseInput` in `app.rs:295`. Screen→world
conversion uses the camera transform the renderer already builds for
`update_camera`.

## Live edits

Clicks always place — including during Run. This is the "cheap if free"
default; the user can ignore it by hitting Stop first.

- **Edit**: trivial append, no rebuild.
- **Run**: triggers `snapshot_from_sim` → `place` → `to_sim` → swap.
  One `Sim` allocation. Bond derivation in `Sim::from_fab` is O(N²)
  (`src/sim.rs:87-96`); at 600 beads (`wire-20x30`) this is microseconds
  and invisible. At ~30k beads (`wire-100x30x10`) it's tens of ms and
  hitches on each click. Acceptable for MVP; if it becomes annoying we
  replace the bond derivation with a grid-accelerated version.

The scheduler also rebuilds on every live edit, since `CpuParallel::new`
takes `&sim`. Same cost profile.

## Renderer

No changes needed. `update_beads` in `src/render.rs:204-228` already
grows the storage buffer on demand (next power of two) when the bead
count exceeds capacity. `render(bead_count)` in `src/render.rs:267`
takes the per-frame count as a parameter. The editor just calls these
the same way today's code does, passing the current count from either
the Scene (Edit mode) or the Sim (Run mode).

## Files

- **New**: `src/editor.rs` (Mode, Scene, transitions, click → place).
- **Modified**:
  - `src/app.rs` — mode state, click handler, transition handlers, new
    bridge globals, render-loop branch on mode.
  - `src/chemistry/mod.rs` — `#[derive(Clone)]` on `Chemistry` so a
    `Scene` can hold one and clone-pass to `Sim::from_fab` per Run.
  - `src/lib.rs` — `pub mod editor;`.
  - `index.html` — editor toolbar + JS for pills, mode buttons, palette
    rendering, chemistry switch confirmation.
- **Untouched**: `src/sim.rs`, `src/scheduler*.rs`, `src/parallel/*`,
  `src/chemistry/*`, all `fabs/*.toml`, all `chemistries/*.toml`, native
  demo path in `app.rs`.

## Behavioural details

- **World boundary**: clicks outside the world square are clamped to
  world bounds. No way to grow the world in MVP.
- **Bead overlap on placement**: if a click lands on or near an
  existing bead, place it anyway. Physics will resolve. No special-case
  code.
- **Chemistry switch on non-empty scene**: `window.confirm()` warns
  before emptying. If declined, picker reverts to the active chemistry.
- **Toolbar visibility**: editor toolbar is always shown, even on
  initial page load. The existing preset picker remains for seeding
  scenes from a baseline.
- **HUD bead count**: switches to read `__jigglefabBeadCount()` so it
  reflects edits live.

## Testing

- **Unit**: `Scene::from_fab` round-trip via `snapshot_from_sim` →
  `to_sim` preserves bead count, positions (within float epsilon), and
  states. Bond set matches between original `Sim::from_fab` and the
  round-tripped version.
- **Manual / browser**: drive `scripts/verify-web.py` extended with an
  editor smoke test:
  1. Page loads in Run mode with default preset.
  2. Click Edit. Sim freezes.
  3. Click in the canvas. New bead appears at cursor.
  4. Click Run. Sim resumes, includes the new bead.
  5. Click in canvas during Run. New bead appears, sim keeps running.
  6. Switch chemistry to `grey`. Confirmation appears. Scene empties.
  7. Place a bead with `grey` state. Run. Confirm the bead jiggles per
     grey rules.

## Open / deferred questions

- **Save/load format**: not in MVP. When added, the natural choice is
  download as TOML matching the existing `fabs/*.toml` schema, since
  `Scene` is already isomorphic to `Fab`.
- **Grid-accelerated bond derivation**: revisit when live edits at
  large bead counts become a UX problem, not before.
- **Chemistry switch UX**: `window.confirm()` is functional but ugly.
  Replace with a styled inline confirmation when we touch the toolbar
  for a later feature.
