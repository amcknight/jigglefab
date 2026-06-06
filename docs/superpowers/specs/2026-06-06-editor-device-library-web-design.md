# Editor — device library, web integration

Status: approved 2026-06-06. Implementation plan: TBD.

The browser-facing half of the device-library feature. The pure-Rust core —
`Device`/`Suite`/`Library`, capture (`Scene::extract_device`), stamp
(`Scene::instantiate_device`), `Scene::rotate_selection`, chemistry
compatibility, and JSON persistence — already shipped on branch
`feat/editor-device-library` (see
`docs/superpowers/specs/2026-06-06-editor-device-library-design.md` and the core
plan `docs/superpowers/plans/2026-06-06-editor-device-library-core.md`). This
spec wires that core to the web UI: a left-edge **dock** sidebar, a ghost-based
placement interaction, rotation input, suites, export/import, and persistence.

## Goal

Make the device library usable in the browser editor: select beads → save as a
device into a dock with a thumbnail, click-arm a device and stamp oriented copies
onto the canvas, manage named suites, and have everything survive a refresh.

## Scope

**In:**
- A `Library` held in `App`, loaded from `localStorage` at startup and re-saved
  after every mutation.
- Save-selection-to-dock; rename / remove dock devices.
- Click-arm a dock device → ghost preview at cursor → click to stamp (isolated).
- Rotation of the armed ghost and of any selection: Shift+scroll and `[`/`]`,
  15° snap. Plain scroll stays zoom (so you can zoom for precise placement while
  a ghost is up).
- Whatever is stamped is left selected for immediate tweaking.
- Left sidebar dock UI: beads-only thumbnails, action controls, chemistry filter,
  greyed-out incompatible devices, empty-state hint.
- Suites: save the dock as a named suite, load a suite (replaces the current
  chemistry's dock slice).
- Export a suite to a file; import a suite from a file.

**Out (deferred):**
- **Ports / easy-connect**: auto-bonding a stamped device into existing
  structure. v1 drops *isolated*.
- **Bond-on-place highlighting**: previewing which existing beads a device would
  bond to. Meaningless until ports exist; record it with the ports work.
- Drag-and-drop from the dock (v1 is click-arm/click-place).
- Nested / hierarchical devices, collapse/expand.
- IndexedDB (localStorage is enough for hand-built libraries).
- Native-build parity (web editor only, like the rest of the editor).

## §1 — Architecture: Rust owns data, JS owns I/O

The plan-1 core is the single source of truth. The browser is a thin shell, in
the established `window.__jigglefab*` + `COMMANDS`/`SNAPSHOT` pattern
(`src/app.rs`, polled each frame by `refreshToolbar` in `index.html`).

- `App` gains a `library: Library` field (the type is `cfg`-agnostic; the field
  exists on all targets, persistence is wasm-only). All mutations call the
  plan-1 `Library`/`Scene` methods inside the app loop — **no library logic in
  JS**.
- **Persistence I/O lives in JS; data lives in Rust.** This deliberately avoids
  calling `localStorage`/`Blob` from Rust (no new `web-sys` features, and the
  logic stays in the already-tested core):
  - *Startup:* JS reads `localStorage["jigglefab.library.v1"]` and calls
    `__jigglefabLoadLibrary(json)` (empty/missing → the library stays default).
  - *After any change:* the app loop bumps a `library_rev: u32` counter exposed
    in `SNAPSHOT`. `refreshToolbar` compares it to the last seen value; on a
    change it calls `__jigglefabGetLibraryJson()` and writes the string back to
    `localStorage`.
  - *Export/import:* Rust serializes/parses; JS does the file download/upload.
- **No new `web-sys` features** are required by this spec.

## §2 — Library lifecycle in `App`

- New field `library: Library`, initialized to `Library::default()`.
- `__jigglefabLoadLibrary(json: &str)` command: `self.library =
  Library::load_or_default(json)` (corrupt/empty → default). Called once at
  startup by JS.
- Every mutating command (below) mutates `self.library`, then bumps
  `self.library_rev` (mirrored into `SNAPSHOT.library_rev`).
- `__jigglefabGetLibraryJson() -> String`: returns `self.library.to_json()` for
  JS to persist.

## §3 — Capture: save selection to dock

- `__jigglefabSaveToDock(name: String) -> String` command. The loop calls
  `scene.extract_device(name)`; `None` (empty selection) → returns an error
  string; otherwise `library.add_to_dock(device)`, bump rev, return `""`.
- JS enables the **Save selection** control only when
  `__jigglefabSelectionCount() > 0` (the getter already exists), and prompts for
  a name (default `"device N"`, N = dock length + 1).

## §4 — Arm, ghost, place (Edit mode only)

Armed-device state lives in `App` (e.g. `armed_device: Option<Device>` plus a
`ghost_angle: f32` accumulator). Run mode forces disarm.

- **Arm:** `__jigglefabArmDevice(id: u32) -> String`. If the device is
  incompatible with the current chemistry (`Device::is_compatible_with`), refuse
  and return an error string; else clone it into `armed_device`, reset
  `ghost_angle = 0.0`, and suspend Place/Chain handling. `SNAPSHOT.armed_id:
  Option<u32>` reflects the armed device so JS can highlight the entry.
- **Ghost preview:** while armed, the app feeds the overlay
  (`Renderer::update_overlay`) a small mark per device bead — a short two-segment
  cross — positioned at `rotate(bead.pos, ghost_angle) + cursor_world`, updated
  on cursor-move. Beads-only (no bond lines), matching the thumbnail style.
- **Place:** a canvas click while armed calls `scene.instantiate_device(&device,
  cursor_world, ghost_angle)` (plan-1 stamp: isolated; selection becomes the new
  beads). Stays armed so multiple copies can be dropped. **Esc** or re-arming the
  same id (toggle) → `__jigglefabDisarm()`, clearing `armed_device` and the
  ghost overlay.
- The existing place/chain/select gesture handling is gated on
  `armed_device.is_none()` so arming cleanly suspends them.

## §5 — Rotation (general, not ghost-specific)

Rotation acts on whatever rotation target exists, sharing one input:

- **Target:** if `armed_device` is `Some`, rotation adjusts `ghost_angle` (the
  ghost turns live before placement). Otherwise it calls
  `scene.rotate_selection(delta)` on the current selection (so a just-dropped
  device, or anything selected later, rotates in place).
- **Input:** **Shift+scroll** rotates by ±15° per notch (sign from scroll
  direction); **`[`** = −15°, **`]`** = +15°. Implemented by extending the
  existing wheel and keyboard handlers in `src/app.rs` to check the Shift
  modifier and the bracket keys.
- **Plain scroll = zoom, always** (armed or not), preserving precise-placement
  zoom while a ghost is up.
- 15° is a `const ROTATE_SNAP_DEG`. Angles accumulate; placing several copies
  reuses the current angle (consistent orientation); fresh-arming resets to 0.

## §6 — Dock sidebar UI (`index.html`)

A new `#device-dock` panel down the left edge (the free edge: HUD top-left,
toolbar top-right, pickers bottom-center), styled like the existing toolbar
pills.

- **Header actions:** **Save selection** (enabled when selection non-empty),
  **Save suite…** (prompts a name), **Load suite ▾** (dropdown of suite names for
  the current chemistry), **Export** (download current/selected suite),
  **Import** (file input).
- **Device list:** vertical, scrollable. Each entry = a small beads-only
  thumbnail (a per-entry `<canvas>` drawing colored dots fit-to-bounds from the
  entry's `beads`) + the device name + a rename affordance + a remove (×).
- **Arming:** clicking an entry sends `__jigglefabArmDevice(id)`; the entry whose
  id equals `SNAPSHOT.armed_id` gets an `active` highlight.
- **Chemistry filter:** only entries whose `chemistry` matches the scene's
  current chemistry are shown; switching chemistry swaps the visible set.
- **Incompatible devices:** entries with `compatible: false` render greyed and
  un-armable, with a `title` tooltip naming the missing state(s).
- **Empty state:** when no devices exist for the current chemistry, show a hint:
  "Select beads, then Save selection".
- **Rendering cadence:** `refreshToolbar` reads `__jigglefabGetLibraryRev()`; the
  list is rebuilt only when the rev changes (the cheap-signature trick the state
  pills already use). Thumbnails redraw with the list.
- **Confirms** (reuse `window.confirm`): remove-device confirms; Load-suite
  confirms when it would replace a non-empty dock; Save actions don't.

`__jigglefabGetDock() -> Array` returns, per visible-or-all device:
`{ id, name, chemistry, beads: [{ pos: [x,y], color: [r,g,b] }], compatible }`.
(JS does the chemistry-filter for display; `compatible` is computed Rust-side
against the current chemistry.)

## §7 — Suites + export / import

- `__jigglefabSaveSuite(name: String)`: `library.save_suite(name,
  current_chemistry)` (plan-1: snapshots the current chemistry's dock slice,
  overwrites same-named), bump rev.
- `__jigglefabLoadSuite(name: String) -> String`: `library.load_suite(name)`
  (plan-1: replaces the current chemistry's dock slice with fresh ids; leaves
  other chemistries untouched). Returns `""` or an error if the name is unknown.
  JS confirms first when the dock is non-empty.
- **Export:** `__jigglefabExportSuiteJson(name: String) -> String` returns the
  suite serialized as JSON (`serde_json`); JS builds a `Blob` and triggers a
  download named `jigglefab-suite-<name>.json`.
- **Import:** JS reads a chosen file's text and calls
  `__jigglefabImportSuite(json: String) -> String`; the loop parses it to a
  `Suite` and calls `library.import_suite(suite)` (overwrite same-named), bump
  rev. Returns `""` or a parse-error string; JS surfaces a non-empty return via
  `alert`.

## §8 — Bridge reference

Getters (read `SNAPSHOT`; installed via the existing `expose_to_window!` macro):

| Getter | Returns |
| --- | --- |
| `__jigglefabGetDock()` | array of `{id,name,chemistry,beads:[{pos,color}],compatible}` |
| `__jigglefabGetLibraryJson()` | the whole library as a JSON string |
| `__jigglefabGetLibraryRev()` | `u32` mutation counter |
| `__jigglefabGetSuiteNames()` | array of suite names for the current chemistry |
| `__jigglefabArmedId()` | `id` or `-1` if none |
| `__jigglefabExportSuiteJson(name)` | suite JSON string (`""` if unknown) |

Commands (push to `COMMANDS`, drained by the loop; string-returning ones run
synchronously and report `""`/error like the existing load path):

| Command | Effect |
| --- | --- |
| `__jigglefabLoadLibrary(json)` | replace `library` from JSON (startup) |
| `__jigglefabSaveToDock(name)` | extract selection → dock |
| `__jigglefabRenameDevice(id,name)` / `__jigglefabRemoveDevice(id)` | mutate dock |
| `__jigglefabArmDevice(id)` / `__jigglefabDisarm()` | set/clear armed device |
| `__jigglefabSaveSuite(name)` / `__jigglefabLoadSuite(name)` | snapshot / restore dock slice |
| `__jigglefabImportSuite(json)` | add/overwrite a suite |

New `SNAPSHOT` fields: `library_rev: u32`, `armed_id: Option<u32>`, and the dock
projection backing `__jigglefabGetDock()`. Existing `selection_count`, `mode`,
and `chemistry_name` cover action enablement.

## §9 — Testing

- **Native:** plan 1 already covers all library/scene logic. Plan 2's `App`
  command handlers are thin glue; any non-trivial projection helper (e.g.
  building the dock snapshot, or the armed→`-1` mapping) gets a small unit test
  where it is pure Rust.
- **Browser smoke** (`scripts/verify-web.py --editor`, extending the existing
  block):
  - Place 3 beads, select them, `__jigglefabSaveToDock("t")` → assert
    `__jigglefabGetDock()` length 1 and the entry's bead count is 3.
  - Assert `localStorage["jigglefab.library.v1"]` is non-empty after the save
    (persistence round-trip).
  - `__jigglefabArmDevice(id)`, simulate a canvas click → assert bead count grew
    by 3 and no cross-bonds formed (isolated).
  - `__jigglefabSaveSuite("s")`, clear the dock, `__jigglefabLoadSuite("s")` →
    dock restored.
  - `__jigglefabImportSuite(<json with a bogus state>)` → the entry appears with
    `compatible:false` and `__jigglefabArmDevice` on it returns an error.

## §10 — Files touched

- `src/app.rs` — `library` + `armed_device` + `ghost_angle` + `library_rev`
  fields; the bridge getters/commands; startup load hook; ghost overlay feed;
  place-while-armed in the canvas-click handler; Shift+scroll / bracket rotation
  in the wheel + keyboard handlers; dock snapshot projection.
- `index.html` — `#device-dock` sidebar markup + styles; thumbnail canvas
  drawing; action controls; suite dropdown; import file input; rev-driven
  re-render; localStorage read at startup + write on rev change; export download.
- `scripts/verify-web.py` — extend the `--editor` smoke block (§9).
- No changes to `src/library.rs` or `src/editor.rs` (the core is complete). No
  new dependencies or `web-sys` features.

## §11 — Phasing (for the implementation plan)

1. `Library` in `App` + load/persist bridge (`loadLibrary`, `getLibraryJson`,
   `library_rev`) + JS startup-load and rev-driven save. (No UI yet; verified by
   a smoke step that round-trips a hand-fed library.)
2. Dock sidebar + thumbnails + `saveToDock` + rename/remove + chemistry filter +
   incompatible greying + empty state.
3. Arm + ghost overlay + place + disarm; gate existing gestures on not-armed.
4. Rotation: Shift+scroll + brackets, ghost vs selection targeting, 15° snap.
5. Suites + export/import.
6. Browser smoke (`verify-web.py`) covering the full flow.

Each phase leaves something demonstrable.

## §12 — Open / deferred questions

- **Ports / easy-connect** and the **bond-on-place highlight** (show which
  existing beads a device would bond to) are the natural next feature; v1's
  isolated drop is the deliberate stepping stone.
- **Suite-load granularity**: replaces the current chemistry's dock slice (not a
  full wipe), matching the core's `load_suite`. A future "merge into dock" is
  possible.
- **Thumbnail polish** (dot size, padding, retina scaling) is left to
  implementation.
- **localStorage quota**: fine for hand-built libraries; revisit IndexedDB only
  if libraries grow large.
