# Editor — device library (dock + suites)

Status: approved 2026-06-06. Implementation plan: TBD.

An in-app library of reusable **devices** (knots, ribosomes, whatever we invent),
stamped from the canvas into a sidebar **dock**, and saved/loaded as named
**suites**. This supersedes the earlier file-only save/load idea
(`docs/superpowers/specs/2026-05-27-editor-save-load-design.md`, never
implemented): instead of persisting whole scenes as `.toml`, we persist
*sub-assemblies* the user can re-stamp anywhere. It is a prerequisite step on the
road to the universal constructor — the library is where composable parts live —
but the library is **not** the constructor (see Non-goals).

## Goal

Let the user select a sub-assembly on the canvas, save it as a reusable device
with a thumbnail, keep a tray (the dock) of such devices, stamp copies back onto
the canvas (positioned and rotated), and persist groups of devices as named
suites that survive a browser refresh and can be shared via file export/import.

## Scope

**In (v1):**
- **Capture**: turn the current selection into a `Device` (idealized rest shape +
  states + internal bonds + chemistry), recentred to its centroid.
- **Stamp**: click-arm a dock device, ghost-preview follows the cursor, click to
  drop a copy; **isolated** (its own internal bonds only — no auto-bond to
  existing beads).
- **Rotate after drop**: the freshly-dropped device is selected; scroll wheel (or
  `[` / `]`) rotates it about its centroid in **snapped 15° increments**.
- **Dock**: sidebar tray of devices with on-the-fly thumbnails; rename / remove.
- **Suites**: save the dock as a named suite; load a suite (replaces the dock,
  with confirm if non-empty).
- **Persistence**: the whole library lives in `localStorage` (survives refresh).
- **Export / import**: a suite ↔ a file, for sharing or committing into the repo.
- **Chemistry filter**: the dock shows only devices matching the scene's current
  chemistry.
- **Chemistry compatibility**: detect when a saved device's states no longer
  exist in the (evolved) chemistry; flag the device as incompatible rather than
  crash or silently delete.

**Out (deferred to later specs):**
- **Ports / easy-connect**: designated connection beads that snap and auto-bond
  devices together. (The `Device` model reserves a `ports` field for this.)
- **Auto-bond on drop** by proximity.
- **Nested / hierarchical devices**: a device built from other devices, with
  collapse/expand. The v1 model must not wall this off, but it ships flat.
- **State-rename migration tables** (remapping an old state name to a new one).
  v1 only *detects* drift; it does not repair it.
- **The universal constructor** itself (reads a chemistry-encoded, RNA-like
  description and builds the thing). Separate, later work.
- **Live/running capture**: capture reads rest-state positions; capturing a
  jiggling sim is not supported (see §3 caveat).

## Non-goals / framing

The library stores **devices**, which are small-to-large hand-built
sub-assemblies. The **universal constructor** is a different future machine that
consumes descriptions expressed *within a chemistry* (think RNA) and physically
builds the described structure. The library is upstream of that work (a place to
keep parts and, eventually, nested assemblies), not the constructor.

## §1 — Architecture

**Rust owns the library.** All logic — capture, stamp, dock/suite management,
chemistry filtering and validation — lives in Rust as pure, unit-testable
functions, consistent with the existing `Scene` code and the
`COMMANDS`/`SNAPSHOT` + `window.__jigglefab*` bridge pattern in `src/app.rs`.
JavaScript stays thin: it renders the dock from a snapshot and sends commands.

- Persistence is JSON in `localStorage`, written from Rust via `web_sys`
  (`Storage` feature), read once at startup.
- Thumbnails are **not** stored. JS draws each one from the device's `beads` +
  chemistry colors onto a small `<canvas>`, so storage stays tiny and thumbnails
  never drift from the data.

Rejected alternative: JS owning the library data structure — it scatters untested
business logic (suite management, chemistry filtering) into `index.html`.

## §2 — Data model (`src/library.rs`)

```rust
struct DeviceBead { state: String, pos: [f32; 2] }   // pos RELATIVE to centroid

struct Device {
    id: u32,                 // library-assigned, stable for the library's life
    name: String,            // editable; defaults "device N"
    chemistry: String,       // chemistry name this device belongs to
    chemistry_hash: u64,     // hash of the chemistry definition at save time
    beads: Vec<DeviceBead>,  // recentred; no velocities
    bonds: Vec<[u32; 2]>,    // local indices 0..beads.len()
    ports: Vec<u32>,         // RESERVED for easy-connect; always empty in v1
}

struct Suite {
    name: String,
    chemistry: String,       // all devices in a suite share one chemistry
    devices: Vec<Device>,
}

struct Library {
    version: u32,            // schema version; drives future migration
    next_id: u32,            // monotonic device-id source
    dock: Vec<Device>,       // the live tray (flat; UI filters by chemistry)
    suites: Vec<Suite>,
}
```

- Serialized with serde to JSON (add `serde_json` if not already a dep) under the
  `localStorage` key `jigglefab.library.v1`.
- `version` starts at `1`. A future breaking schema change bumps it and adds a
  migration on read.
- `ports` is serialized but unused in v1; reserving it keeps the easy-connect
  feature additive.

## §3 — Capture: selection → device (`Scene::extract_device`)

Pure and native-unit-testable.

1. Collect the selection (`HashSet<u32>`), sort into a stable local order
   `0..n`.
2. Compute the centroid **torus-aware**: accumulate each selected position via
   `grid::min_image` relative to the first selected bead, average, wrap. This
   keeps a selection that straddles the world seam from getting a garbage centre.
3. For each selected bead emit `DeviceBead { state, pos: world_pos − centroid }`
   (min-image relative to the centroid). No velocity.
4. Keep only bonds whose **both** endpoints are selected; remap global→local
   indices (the inverse of `delete_selection`'s remap).
5. Set `chemistry` = `scene.chemistry_name`, `chemistry_hash` = hash of the
   current chemistry definition.

Returns a `Device` (without an id); the `Library` assigns `id = next_id++` when it
is pushed onto the dock.

**Caveat — rest state only.** Capture reads the scene's *current* bead positions.
In Edit mode these are the clean placed positions. After Run without Revert,
positions are the jiggled ones. The contract is "capture from a rest scene"; this
is documented, not enforced with relaxation machinery.

## §4 — Stamp: device → scene (`Scene::instantiate_device`)

`instantiate_device(device, drop_pos, rotation_radians)`, pure and testable:

1. For each device bead: `world = wrap( rotate(bead.pos, rotation) + drop_pos )`;
   append a new `BeadSpec { state, pos: world, vel: None }`.
2. Remap the device's local bonds to the freshly-appended global indices
   (`base + local_idx`) and insert into `scene.bonds`.
3. **Isolated**: no proximity bonding to pre-existing beads.
4. Replace `scene.selection` with exactly the newly-appended indices, so
   rotate-after-drop acts on the drop with no extra clicks.

Stamping a device whose chemistry is incompatible with the scene is refused at
the command layer (see §7); `instantiate_device` itself assumes a validated
device.

## §5 — Placement & rotation interaction (desktop)

- **Arm**: clicking a dock thumbnail arms that device. A translucent **ghost** of
  its beads/bonds follows the cursor over the canvas, drawn through the existing
  editor overlay path (the one selection rect/lasso already use). Arming suspends
  Place/Chain.
- **Drop**: click on the canvas stamps a copy at that point. The device stays
  armed (drop several); **Esc** or re-clicking the dock entry disarms.
- **Rotate after drop** (`Scene::rotate_selection(angle)`): rotates the selected
  beads about their centroid (torus-aware), then wraps. Bonds are untouched
  (indices don't move). Driven by the scroll wheel (or `[` / `]`) in snapped
  **15°** increments. The primitive is general — it rotates any selection, not
  just a fresh drop.

## §6 — Dock / suite UI (`index.html`)

Same pill + poll (`refreshToolbar`-style) approach as the current toolbar; a new
sidebar panel.

- **Dock panel**: grid of per-device thumbnails (small `<canvas>` drawn from
  `SNAPSHOT.dock`) with names. Per entry: arm (click), rename, remove-from-dock.
- **Actions**:
  - **Save selection to dock** — enabled only when the selection is non-empty;
    prompts for a name (defaults "device N").
  - **Save dock as suite…** — prompts a name; snapshots the current dock.
  - **Load suite ▾** — picker of saved suites; **replaces** the dock, with a
    `window.confirm` when the dock is non-empty (mirrors Clear/Revert).
  - **Export suite** / **Import suite** — file download / upload (reuse the
    `web_sys` download + hidden-file-input helpers sketched in the prior
    save/load spec).
- **Chemistry filter**: only devices whose `chemistry` matches the scene are
  shown; switching chemistry swaps the visible set. Hidden, never deleted.

## §7 — Chemistry compatibility

Chemistries are baked-in TOML that will evolve. A `Device` pins state-name
strings, so drift must degrade gracefully.

- On load and on chemistry switch, for each device:
  - Validate every `bead.state` against the current chemistry's palette.
  - **All present** → usable. A `chemistry_hash` mismatch *alone* (compatible
    tweak) clears silently.
  - **Any state missing/renamed** → device shown but **incompatible**: greyed,
    un-armable, tooltip naming the missing state(s). Never auto-deleted; never
    crashes a stamp.
- The command layer refuses to arm/stamp an incompatible device.
- True rename *migration* (a remap table) is out of scope; storing
  `chemistry_hash` + per-state validation leaves it additive.

## §8 — Bridge additions (`src/app.rs`)

Following the existing `window.__jigglefab*` + `COMMANDS`/`SNAPSHOT` pattern.

| Global | Effect |
| --- | --- |
| `__jigglefabSaveToDock(name) -> String` | extract selection → device → dock; persist; `""` ok / error text |
| `__jigglefabRenameDevice(id, name)` / `__jigglefabRemoveDevice(id)` | mutate dock; persist |
| `__jigglefabArmDevice(id) -> String` | arm (refused if incompatible); `""` ok / error |
| `__jigglefabDisarm()` | clear the armed stamp |
| `__jigglefabSaveSuite(name)` / `__jigglefabLoadSuite(name) -> String` | snapshot dock / replace dock |
| `__jigglefabExportSuite(name)` | serialize suite → browser download |
| `__jigglefabImportSuite(json) -> String` | parse + add suite; `""` ok / error text |
| `__jigglefabRotateSelection(deg)` | snapped rotate of the current selection |

`SNAPSHOT.dock`: `[{ id, name, beads, bonds, colors, compatible }]` — everything
JS needs to draw thumbnails and render disabled/incompatible states. Reuses the
existing `bead_count` / `mode` snapshot fields for action enablement.

`COMMANDS` gains entries for the mutating verbs above; the app loop (which holds
the `Scene` and `Library`) applies them, then persists the `Library` to
`localStorage`.

## §9 — Persistence

- The whole `Library` is serialized to JSON and written to `localStorage`
  (`jigglefab.library.v1`) after every mutation: save-to-dock, rename, remove,
  save-suite, load-suite, import.
- Read once at startup; on parse failure (corrupt/old), start empty and log
  rather than crash (a future `version` bump adds a real migration step here).
- Export writes a single suite as a JSON file; import parses one back and appends
  it (deduping by suite name with a confirm-overwrite).

## §10 — Files touched

- **new** `src/library.rs` — `Device`, `DeviceBead`, `Suite`, `Library`, JSON
  (de)serialization, chemistry-hash + compatibility checks, dock/suite mutations.
- `src/editor.rs` — `Scene::extract_device`, `Scene::instantiate_device`,
  `Scene::rotate_selection`.
- `src/app.rs` — new commands + bridge installers, armed-device state, the ghost
  overlay feed, `localStorage` read/write via `web_sys`, download/upload helpers.
- `index.html` — dock sidebar, thumbnail canvases, action pills, suite picker,
  import file input.
- `scripts/verify-web.py` — extend the `--editor` block.
- `Cargo.toml` — `serde_json` (if absent); `web-sys` `Storage` feature.

## §11 — Testing

**Native unit (the weight of coverage, TDD):**
- `extract_device` → `instantiate_device` round-trip preserves states, relative
  shape, and internal bonds.
- Centroid recentring is correct, including a selection straddling the seam.
- `rotate_selection` by 0° (identity), 90°, 360° (identity within ε) about the
  centroid, with wrap.
- `instantiate_device` is isolated: stamping near existing beads adds **no**
  cross-bonds; bead count grows by exactly the device size.
- Chemistry validation flags a device with an unknown state as incompatible and
  passes a clean one; `chemistry_hash` mismatch with all-states-present stays
  usable.
- `Library` JSON round-trips (dock + suites + ids + version).
- `save_suite` then `load_suite` replaces the dock with the suite's devices.

**Browser smoke (`scripts/verify-web.py --editor`):**
- Select 3 beads → `__jigglefabSaveToDock` → assert `SNAPSHOT.dock` length 1.
- Arm + place → assert bead count grew by 3 with no new cross-bonds.
- Save a suite, clear the dock, load it back → dock restored.
- Feed an import with a bogus state → device present but flagged incompatible and
  refused on arm.

## §12 — Open / deferred questions

- **Suite load = replace** in v1. Merge-into-dock is a later option.
- **Thumbnail rendering detail** (bond lines vs. dots only, fixed vs.
  fit-to-bounds scale) is left to implementation; it draws from `beads` + colors.
- **localStorage quota**: hand-built devices are small; if libraries grow large,
  revisit IndexedDB. Out of scope now.
- **Nested devices & ports** are explicitly future; the `ports` field and flat
  `Device` model are chosen to keep them additive.
