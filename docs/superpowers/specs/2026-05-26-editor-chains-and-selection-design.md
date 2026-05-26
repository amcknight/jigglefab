# Editor — chain draw + selection (rect / lasso / move / delete)

Status: approved 2026-05-26. Implementation plan: TBD.

Pulls forward features the editor MVP (`docs/superpowers/specs/2026-05-25-editor-mvp-design.md`) listed as deferred: chain drawing, region selection, move, delete. Promotes the bond set to first-class data along the way so chains stay chains across sharp corners.

## Goal

Let the user (1) drag out a chain of beads that bonds as a chain — even around sharp turns — and (2) select a region of beads with a rectangle or lasso, then move or delete them. Edit-mode only; the existing place-on-click affordance and live-edit-during-Run stay unchanged.

## Scope

**In:**
- Tool row in the editor toolbar: `Place` / `Chain` / `Rect` / `Lasso`. Exactly one active.
- Chain tool: freehand drag, beads spawned along the cursor path at ~0.667-unit spacing, consecutive pairs bond, **no** triangle bonds.
- Rect tool: drag-rectangle selection; replaces previous selection on release.
- Lasso tool: freehand polygon; closes on release; point-in-polygon assigns membership.
- Drag from a selected bead translates the whole selection; positions clamped to world at drop time; velocities preserved.
- `Del` / `Backspace` deletes the current selection.
- Bonds promoted to first-class data: `Fab.bonds: Option<Vec<(u32,u32)>>`, Scene maintains its own bond set, Sim accepts it directly.

**Out (deferred):**
- Copy / paste, rotate.
- Undo / redo.
- Save / load.
- Multi-select via shift-add.
- Snap-to-angle, smoothing, polyline (click-to-vertex) chain modes.
- Selection / chain / move / delete during Run mode (Run mode keeps Place-only behavior from MVP).
- Native-build parity.

## §1 — Tool model

Add a tool row to the existing `#editor-toolbar`:

```
[ Place ] [ Chain ] [ Rect ] [ Lasso ]
```

- Exactly one tool active. `Place` is the default and preserves MVP behavior exactly (single-click places a bead; allowed during Run via the existing snapshot-rebuild).
- `Chain` / `Rect` / `Lasso` are Edit-only. In Run mode the buttons remain clickable (you can pre-select a tool while paused) but canvas input under those tools is a no-op until the user enters Edit.
- Switching tool does NOT clear the selection. Switching chemistry or pressing Run does.

New bridge globals:
| Function | Returns / behaviour |
| --- | --- |
| `__jigglefabGetTool()` | `"place"` \| `"chain"` \| `"rect"` \| `"lasso"` |
| `__jigglefabSetTool(s)` | Sets the active tool |
| `__jigglefabSelectionCount()` | Number of currently-selected beads, for HUD |

## §2 — Chain tool

Freehand drag. While the mouse is held:

- `mousedown` in Edit: place a bead at the cursor (state = `next_state_idx`, `vel: None` — same as Place); record it as the chain's "last bead".
- `mousemove`: **while** `|cursor - last_bead.pos| ≥ 0.667` world units, place a new bead 0.667 units along the vector from `last_bead.pos` toward `cursor`, append `(last_bead_idx, new_idx)` to `Scene.bonds`, set "last bead" to that new bead, and loop. This handles fast cursor motion (a single 2-unit `mousemove` event drops 3 beads on the segment, not one stretched 2 units past `RADIUS`).
- `mouseup`: end the chain.

A press-and-release with no movement (mousedown → mouseup without crossing 0.667) places exactly one bead — Chain degenerates to Place for a single click.

0.667 is the wire-30 preset spacing — comfortably under the bond threshold `RADIUS = 1.0` (`src/ccd.rs:3`). Each bond is added explicitly. No distance derivation happens during chain draw, so non-consecutive close pairs (sharp corners, U-turns, self-crossings) do **not** become triangle bonds. Stretched-bond drift handling (`enforce_bonds` in `src/sim.rs:116`) never triggers on chain-drawn pairs because they're always placed at exactly 0.667 < `RADIUS`.

Crosses-existing-beads: place anyway, same MVP convention.

## §3 — Selection (rect + lasso)

Both tools produce the same kind of result: a `HashSet<u32>` of bead indices in `Scene.beads`. Drawing either replaces the previous selection.

**Rect tool**
- `mousedown` anchors corner A.
- `mousemove` updates corner B; renderer draws a rectangle overlay between A and B.
- `mouseup` runs point-in-rect on every bead center; writes matches to `Scene.selection`.

**Lasso tool**
- `mousedown` starts a path.
- `mousemove` appends points (sampled when the cursor has moved ≥ ~2 screen pixels since the last sample, to keep the polygon manageable); renderer draws the running polyline.
- `mouseup` closes the polygon (last point → first), runs ray-cast point-in-polygon per bead, writes matches.

**Empty gesture deselects:** a press-and-release with no movement on Rect or Lasso clears the selection. Same for clicking outside any selected bead's hit-radius in Move (§4).

**Selection rendering:** selected beads draw a white outline ring. Implementation: extend the per-bead storage struct with `selected: u32`, branch in the bead fragment shader to draw a 1–2 px ring outside the bead radius when set.

**Drag overlay rendering:** new screen-space thin-line pipeline in `src/render.rs` that draws the rect's four sides or the lasso's polyline during the drag. Removed when the drag ends.

## §4 — Move

Once `Scene.selection` is non-empty, **drag from a selected bead** translates the whole selection.

- Hit-test on `mousedown`: if the cursor is within `RADIUS` of any selected bead's center, enter Move drag, ignoring which tool button is active. Otherwise, the active tool handles the press normally (new Rect/Lasso, or Place, or Chain).
- `mousemove` translates every selected bead by `cursor - last_cursor`.
- `mouseup` ends the Move; final positions are clamped to `[0, world_size]` per axis.
- Velocities preserved as-is. Bond indices unchanged.

## §5 — Delete

While `Scene.selection` is non-empty:

- `Del` or `Backspace` removes selected beads.
- Every bond `(i, j)` touching a removed index is dropped.
- Remaining indices are remapped to the new dense numbering (a bead at old index `k` whose `k` is not deleted goes to its position in the surviving subsequence); bonds rewritten with the remapped indices.
- `Scene.selection` clears (the indices are stale anyway).

## §6 — Bond model (promotion to first-class)

The Haskell project stored bond intent as a tagged `bbSides :: M.Map (P Int) Side` map (`haskell/src/Motion/Model.hs:36`), built once at `buildModel` time from initial geometry via `side ps = if furtherThan 1 ps then Out else In` (`haskell/src/Motion/Point.hs:40-41`). Rust mirrors this — `Sim::from_fab` distance-derives a `HashSet<(u32,u32)>` at init (`src/sim.rs:87-96`) and keeps it for the life of the sim. The chain editor needs to bypass the distance derivation for chain-drawn pairs without giving up the distance derivation for legacy presets.

**Fab change:**
```rust
// src/fab.rs
pub struct Fab {
    pub meta: Meta,
    pub beads: Vec<BeadSpec>,
    pub bonds: Option<Vec<(u32, u32)>>,  // NEW
}
```
TOML round-trip: absent → `None`. Existing fab TOMLs (`fabs/*.toml`) have no `bonds` field and continue to load with `bonds = None`.

**Sim change:**
```rust
// src/sim.rs — small refactor in from_fab
let bonds: HashSet<(u32, u32)> = match &fab.bonds {
    Some(explicit) => explicit.iter().copied().collect(),
    None => derive_bonds_by_distance(&positions, &grid),  // existing logic, factored out
};
```

**Scene change:**
```rust
pub struct Scene {
    // existing fields...
    pub bonds: HashSet<(u32, u32)>,  // canonical (low, high)
    pub selection: HashSet<u32>,     // bead indices
}
```

**Lifecycle:**
- `Scene::from_fab(preset)`: distance-derive `bonds` once from preset positions (matches today's behavior for wire-30, wire-100x30, etc.).
- Chain tool: appends `(prev, new)` only.
- Place tool: distance-derives bonds for the **new bead** against existing beads (preserves Place's "drop near a chain → it joins" intuition). Known consequence: dropping a bead next to a chain interior can produce a triangle bond. That's Place semantics; use Chain to avoid it.
- Delete: drops touching bonds, remaps remaining indices.
- Move: bond indices unchanged.
- Run → Stop snapshot: bonds round-trip via `Fab.bonds = Some(scene.bonds)`.
- Switch chemistry: bonds clear (beads clear too, same as today).

## §7 — Files

- **`src/fab.rs`**: add `bonds: Option<Vec<(u32, u32)>>`; serde round-trip.
- **`src/sim.rs`**: factor distance-derivation into `fn derive_bonds_by_distance`; `from_fab` uses `fab.bonds` if `Some`, else calls the factored fn.
- **`src/editor.rs`**: add `Tool` enum, `DragState` (`None | Chain { last_pos, last_idx } | Rect { anchor } | Lasso { points } | Move { last_cursor }`), `Scene.bonds`, `Scene.selection`, gesture handlers, `point_in_polygon`, place/chain/delete bond bookkeeping helpers.
- **`src/app.rs`**: route mouse-down / move / up + `Del`/`Backspace` through editor; new bridge globals (`__jigglefabGetTool`, `__jigglefabSetTool`, `__jigglefabSelectionCount`). Cursor change per tool optional.
- **`src/render.rs`**: per-bead `selected: u32`, outline ring in shader; new thin-line overlay pipeline for the rect/lasso visual.
- **`index.html`**: tool-row pills + JS wiring.

Untouched: scheduler, chemistry, parallel, fab presets, chemistry TOMLs, native demo path.

## §8 — Testing

**Unit — fab/sim:**
- `Fab` round-trips through TOML with and without an explicit `bonds` field; absent field → `None`.
- `Sim::from_fab` with `bonds = Some(explicit)` uses them verbatim (no derivation).
- `Sim::from_fab` with `bonds = None` produces the **exact same** bond set as the pre-change code on every existing preset. (Both correctness and regression guard for the cold-load path.)

**Unit — editor:**
- `point_in_polygon` on convex, concave, and U-shape polygons; on-edge cases documented.
- Chain draw: simulate cursor samples 0.1 units apart over a 1-unit total path → 1 bead placed; samples 1.0 units apart over 5 units → 5 beads placed.
- Chain interpolation under fast motion: a single `mousemove` jumping 2 world units from `last_bead.pos` places 3 beads at 0.667, 1.333, 2.0 along the segment; assert every consecutive pair is exactly 0.667 units apart (within float epsilon).
- **Chain corner anti-triangle**: place beads at `(0,0), (0.7,0), (0.7,-0.7)` via the chain tool; assert `bonds = {(0,1), (1,2)}` only — assert `(0,2)` is **absent** even though `|A-C| ≈ 0.99 < RADIUS`.
- **Place tool near a chain interior** *does* create the close-neighbor bond (regression guard for Place semantics).
- Rect select: build a scene, run rect, assert selected indices.
- Lasso select: same, with a non-convex polygon.
- Move: translate selection, assert positions shifted, velocities preserved, bond indices unchanged.
- Delete: remove a selection, assert remaining bead indices contiguous, bonds touching deleted indices gone, surviving bonds correctly remapped.
- Snapshot round-trip: Scene → Sim → snapshot → Scene preserves `bonds` exactly.

**Perf:**
- `Sim::from_fab(wire-100x30x10)` with `bonds = None` does not regress vs. pre-change (criterion bench or wall-time assert).
- `Sim::from_fab` with `bonds = Some(explicit)` on a 30k-bead scene completes in < 5 ms (the Run-mode live-edit rebuild path; eliminates the O(N²) hitch the MVP spec flagged).

**Browser smoke** (`scripts/verify-web.py --editor`):
- Switch to Chain tool, drag along a path, assert `__jigglefabBeadCount()` increased and `__jigglefabSelectionCount()` is 0.
- Switch to Rect, drag over the chain, assert `__jigglefabSelectionCount() > 0`.
- Press `Delete`; assert bead count dropped and selection count is 0.
- Switch to Lasso, drag a closed loop, assert selection count > 0.
- Drag a selected bead, release, click Run; assert sim resumes and includes the moved beads.

## §9 — Open / deferred questions

- **Multi-chain crossing**: two chains drawn close enough that beads from each are within `RADIUS` of beads of the other will still distance-derive bonds when the second chain is *placed* — but only because each new chain bead currently appends via Chain logic which adds only the consecutive bond. Cross-chain bonds therefore do **not** form via the Chain tool. They DO form if a user drops Place-tool beads between chains, which is consistent with the §6 Place semantics. No follow-up needed unless complaint.
- **Selection visualization during Run**: deferred; selection clears on Run entry, so this isn't a problem yet. Revisit if selection-persists-across-Run is added.
- **Chain spacing tunable**: 0.667 is hard-coded for v1. If users want denser or sparser chains, expose as a toolbar field later.
- **Delete confirmation**: none in v1. Undo would be the right answer; until then, users live with it.
