# Editor — save / load scenes (.toml download / upload)

Status: approved 2026-05-27. Implementation plan: TBD.

Pulls forward the save/load feature listed as deferred in both the editor MVP (`docs/superpowers/specs/2026-05-25-editor-mvp-design.md`) and the chains+selection spec (`docs/superpowers/specs/2026-05-26-editor-chains-and-selection-design.md`). With bonds now first-class on `Fab`/`Scene`, a saved scene round-trips cleanly through the existing `fabs/*.toml` schema.

## Goal

Let the user persist an edited scene to a `.toml` file they can keep, share, commit into `fabs/`, or reload later — and load such a file back into the editor. Saved files are interchangeable with the built-in presets.

## Scope

**In:**
- `Save` button: serialize the current Edit-mode scene to a `.toml` file and trigger a browser download.
- `Load` button: pick a `.toml` file, validate it strictly, and replace the current scene.
- File format is the existing `fabs/*.toml` schema (a saved file *is* a valid `Fab`).
- Save captures a clean layout: positions + states + bonds. No velocities.
- Load is atomic: it either fully succeeds or leaves the current scene untouched.

**Out (deferred):**
- localStorage / IndexedDB slots (files only for v1).
- Saving live/running state (velocities, mid-sim savestate).
- Lenient load (coercing unknown bead states); v1 rejects.
- Multi-file management, thumbnails, a "recent files" list.
- Native-build parity (web editor only).
- Drag-and-drop file load onto the canvas.

## §1 — File format

A saved file reuses the `fabs/*.toml` schema verbatim, so it is a valid `Fab` and interchangeable with presets:

```toml
[meta]
name = "editor scene"
chemistry = "wire"
seed = 42
world_size = 30.0
bonds = [[0, 1]]

[[bead]]
state = "wire"
pos = [5.0, 5.0]

[[bead]]
state = "wire"
pos = [5.667, 5.0]
```

- `Fab`, `Meta`, and `BeadSpec` gain `#[derive(Serialize)]` (today they are `Deserialize` only).
- `#[serde(skip_serializing_if = "Option::is_none")]` on `Meta.world_size`, `Meta.bonds`, and `BeadSpec.vel` keeps output clean. Editor scenes always emit `bonds` (chains require explicit bonds); `vel` is never emitted (see §2).
- `bonds` is emitted sorted (`Vec<[u32; 2]>`, ascending) for deterministic, diff-friendly files.
- `meta.name` defaults to `"editor scene"`. `seed` is carried through round-trips.
- `next_state_idx` (which palette entry the next click places) is editor UI state, not part of the `Fab` schema, so it is naturally absent from saved files and resets to `0` on load.

## §2 — Save

Pure serialization, native-unit-testable:

```rust
impl Scene {
    /// Serialize this scene to fabs/*.toml format. Emits positions, states,
    /// and explicit bonds (sorted); no velocities.
    pub fn to_toml(&self) -> String;
}
```

- Builds a `Fab` from the scene the same way `Scene::to_sim` already does: `bonds: HashSet<(u32,u32)>` → sorted `Vec<[u32; 2]>`; `BeadSpec.vel` forced to `None`; `world_size`/`seed`/`chemistry_name` copied through.
- Serializes with `toml::to_string_pretty`.

Web flow:
- `Save` pill in `#editor-toolbar`, enabled only in Edit mode with `bead_count > 0` (driven by the existing `refreshToolbar` poll; same mechanism as the Revert disabled state).
- Click → `__jigglefabSave()` sets a `save` command. The app loop (which holds the `Scene`) calls `scene.to_toml()` and triggers a browser download via `web_sys`: build a `Blob`, `URL.createObjectURL`, create an `<a download=…>`, click it, remove it, `revokeObjectURL`.
- Filename: `jigglefab-<chemistry>-<YYYYMMDD-HHMMSS>.toml`.
- Save in Run mode is not offered (button disabled); this matches the Edit-only scope.

## §3 — Load

Strict validation against the static chemistry registry, native-unit-testable:

```rust
pub enum LoadError {
    Parse(String),              // malformed TOML
    UnknownChemistry(String),   // chemistry name not in the embedded registry
    UnknownState { bead: usize, state: String }, // state not in that chemistry's palette
}

impl Scene {
    pub fn from_toml(text: &str) -> Result<Scene, LoadError>;
}
```

Steps (any failure aborts before any scene is built):
1. `parse_fab(text)` → `Fab`, else `LoadError::Parse`.
2. Look up `fab.meta.chemistry` via the embedded registry (`crate::editor::load_chemistry_by_name`); unknown → `LoadError::UnknownChemistry`.
3. Validate every `bead.state` is in that chemistry's palette; first offender → `LoadError::UnknownState`.
4. `Scene::from_fab(&fab, chem, name)` (this already derives/uses bonds correctly: explicit `bonds` are honored; a legacy preset without `bonds` distance-derives).

Web flow:
- `Load` pill, always enabled.
- Click → JS confirms before replacing a non-empty scene (`window.confirm`, mirroring Clear/Revert), then opens a hidden `<input type="file" accept=".toml">`.
- `FileReader` reads the file as text → `__jigglefabLoadToml(text)` returns `""` on success or a human-readable error string (`LoadError` rendered to text). JS surfaces a non-empty return via `alert` (consistent with the existing confirm-dialog UX).
- On success the closure builds the full `Scene` synchronously (using the static registry — no live-`App` access needed) and queues a `load_scene: Option<Scene>` command.
- The app loop's `load_scene` handler: replaces `self.scene`, forces Edit mode, drops the running `Sim`, and clears `pre_run_snapshot` (Revert must not reach across a load).

## §4 — Bridge additions

Following the existing `__jigglefab*` window-global + `COMMANDS`/`SNAPSHOT` pattern in `src/app.rs`:

| Global | Mechanism |
| --- | --- |
| `__jigglefabSave()` | sets `COMMANDS.save = true`; loop serializes + downloads |
| `__jigglefabLoadToml(text) -> String` | validates+builds `Scene`; on success sets `COMMANDS.load_scene`; returns `""` or error text |

No new `SNAPSHOT` fields are required: `Save` availability reuses the existing `bead_count` and `mode` already in `SNAPSHOT`.

## §5 — UI surface

Two new pills in the `#editor-toolbar` action row, next to `Clear` / `Revert`:

```
[ Clear ] [ Revert ] [ Save ] [ Load ]
```

- `Save`: gets the `disabled` class when not (Edit mode AND bead_count > 0), via `refreshToolbar`.
- `Load`: always enabled.
- Confirm dialogs reuse the existing `window.confirm` style: Load confirms when it would discard a non-empty scene; Save never confirms.

## §6 — Files touched

- `src/fab.rs` — add `Serialize` derives + skip-if-none attributes.
- `src/editor.rs` — `Scene::to_toml`, `Scene::from_toml`, `LoadError`.
- `src/app.rs` — `save` + `load_scene` commands; `install_window_save`, `install_window_load`; `load_scene` handler (swap scene, Edit mode, drop sim, clear snapshot); `web_sys` download helper.
- `index.html` — `Save` / `Load` pills, file-input wiring, `refreshToolbar` Save-disabled logic.
- `scripts/verify-web.py` — extend the `--editor` block.

No new modules.

## §7 — Testing

**Native unit tests (the weight of coverage, TDD):**
- `to_toml` → `from_toml` round-trip preserves beads (state + pos), bonds, chemistry, world_size, seed.
- `to_toml` output parses as a valid `Fab`; `bonds` present and sorted; no `vel` keys emitted.
- `from_toml` rejects malformed TOML (`Parse`), unknown chemistry (`UnknownChemistry`), and an unknown bead state (`UnknownState` naming the bead index); accepts a valid file.
- Regression: every shipped `fabs/*.toml` preset still parses (legacy files without `bonds` derive correctly through `from_fab`).
- Empty-scene round-trip: `to_toml` of a 0-bead scene re-loads to a 0-bead scene.

**Browser smoke (`scripts/verify-web.py --editor`):**
- Feed a known TOML string via `__jigglefabLoadToml`; assert it returns `""`, that `__jigglefabBeadCount()` matches the file, and that mode is `edit`.
- Feed an invalid TOML (unknown chemistry); assert a non-empty error string and that bead_count is unchanged.
- Trigger `__jigglefabSave` and capture the download with Playwright (`expect_download`); optionally re-parse the downloaded bytes through `__jigglefabLoadToml` to confirm a full round-trip through the bridge.

## §8 — Open / deferred questions

- **Filename collisions / overwrite**: browser download handles naming; the timestamped default makes collisions unlikely. No in-app rename for v1.
- **Large generated scenes**: hand-authored scenes are KB-sized, so files are the right tool. If a future feature saves the live 30k-bead sim, revisit storage (a DB/IndexedDB path) then — out of scope here.
- **Load merging vs. replacing**: v1 replaces. Merge/import-into-current is a later idea.
- **Drag-and-drop onto canvas**: deferred; the `Load` button + file dialog is enough for v1.
