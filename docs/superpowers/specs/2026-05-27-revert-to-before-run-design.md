# Revert to before Run — design

Status: approved 2026-05-27. Implementation plan: TBD.

## Goal

Give the editor a safety net for running scenes. After hitting Run and
letting the sim mutate things, the user can click Revert to restore the
exact scene they had at the moment of Edit→Run. Today there's no way back:
`transition_mode`'s Edit arm calls `scene.snapshot_from_sim(sim)`, so
clicking Edit freezes the running sim into the scene rather than restoring
the original placement.

## Scope

**In:**
- One persistent snapshot of the scene captured at every Edit→Run.
- A Revert button in the editor toolbar mode row.
- Snapshot persists across reverts and re-edits, replaced on next Run.
- Snapshot invalidated by Clear and by chemistry switch.
- Web smoke coverage in `scripts/verify-web.py --editor`.

**Out:**
- Multi-step undo / history stack.
- Snapshots of any other state (sim velocities, selection, tool).
- Keyboard shortcut (button-only this round).
- Persistence across page reload.
- Native-build parity (web only — matches editor v1).

## UI surface

The editor toolbar mode row gains a fourth control:

```
mode    [ Edit ] [ Run ] [ Clear ] [ Revert ]
```

- `Revert` carries the same pill style as `Clear`.
- When no snapshot exists (initial load, post-Clear, post-chemistry-switch),
  the button takes a `disabled` class: `opacity: 0.4`, `cursor: default`,
  click is a no-op.
- If the current scene has beads, a `window.confirm()` prompts
  "Revert to pre-Run scene? This discards N bead(s)." before applying.
  Skipped when the scene is empty.

`refreshToolbar` toggles the `disabled` class each frame from
`__jigglefabCanRevert()`, mirroring how mode/tool buttons stay in sync.

## Data model

New struct in `src/editor.rs`:

```rust
pub struct ScenePayload {
    pub chemistry_name: String,
    pub world_size: f32,
    pub seed: u64,
    pub next_state_idx: u32,
    pub beads: Vec<BeadSpec>,
    pub bonds: HashSet<(u32, u32)>,
}
```

`Scene` gets two helpers:

```rust
impl Scene {
    pub fn capture_payload(&self) -> ScenePayload { ... }
    pub fn restore_payload(&mut self, payload: &ScenePayload) { ... }
}
```

`restore_payload` overwrites the listed fields and clears `selection`.
`chemistry` and `tool` are left as-is (chemistry is invariant for a valid
snapshot; tool is UI state, not scene data).

`App` gains:

```rust
pre_run_snapshot: Option<ScenePayload>,
```

## Behavior

**Capture.** `transition_mode` Run arm, before `rebuild_sim_from_scene`:

```rust
if let Some(scene) = self.scene.as_ref() {
    self.pre_run_snapshot = Some(scene.capture_payload());
}
```

**Restore.** New `App::revert_to_snapshot()`:

```rust
let Some(payload) = self.pre_run_snapshot.as_ref() else { return };
if let Some(scene) = self.scene.as_mut() { scene.restore_payload(payload); }
self.sim = None;
self.mode = Mode::Edit;
self.drag = DragState::None;
self.mouse_down = false;
if let (Some(renderer), Some(scene)) = (self.renderer.as_mut(), self.scene.as_ref()) {
    let palette: Vec<[f32; 3]> = scene.chemistry.colors.clone();
    renderer.update_camera(scene.world_size, &palette);
}
```

**Invalidation.** `pre_run_snapshot = None` in two places:
- The `clear_scene` arm of the command-dispatch block in app.rs.
- The `set_chemistry` arm of the command-dispatch block in app.rs,
  right where the scene is swapped.

## JS bridge

Two new bridges following the existing pattern in `src/app.rs`:

| Name | Returns | Notes |
|---|---|---|
| `__jigglefabRevert()` | — | Queues `revert = true` on `PendingCommands`. Processed each frame in the command-dispatch block, calls `App::revert_to_snapshot()`. |
| `__jigglefabCanRevert()` | `bool` | Reads `web_bridge::SNAPSHOT.can_revert` (new field). |

`web_bridge::Snapshot` gains `pub can_revert: bool`, populated each frame
from `self.pre_run_snapshot.is_some()`.

## Testing

Editor unit tests in `src/editor.rs`:

- `capture_payload_round_trips_through_restore` — beads, bonds, world_size,
  seed, next_state_idx all match.
- `restore_payload_clears_selection` — selection always empty after restore.

App-level isn't easily unit-testable, so the rest is covered by the web
smoke. New asserts in `scripts/verify-web.py --editor`:

1. Place a bead in Edit. Record bead count = N+1.
2. Switch to Run. Wait a few frames so the sim mutates.
3. Call `__jigglefabRevert()`.
4. Assert `__jigglefabGetMode() === 'edit'`.
5. Assert `__jigglefabBeadCount() === N+1` (the snapshot count, not Run mutations).
6. Assert `__jigglefabCanRevert() === true` still (snapshot persists).
7. Click Clear, assert `__jigglefabCanRevert() === false`.

## Open questions

None.
