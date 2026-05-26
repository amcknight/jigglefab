# Editor MVP Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a minimal click-to-place editor with Run / Stop / Edit, chemistry picker, and state picker to the web build.

**Architecture:** Hold a `Scene` (vec of `BeadSpec`s + a `Chemistry`) as the source of truth in Edit mode. On Run, build a fresh `Sim` from the Scene. On Stop, snapshot the Sim's positions/velocities/states back into the Scene. Clicks during Run snapshot-place-rebuild transparently (live edits cheap at 600 beads, hitchy at 30k — acceptable for MVP). Renderer already supports grow-on-demand and per-frame counts, so no renderer changes.

**Tech Stack:** Rust + wgpu + winit (existing), wasm-bindgen for JS bridge, HTML/JS for toolbar. Re-uses existing `window.__jigglefab*` pattern.

**Spec:** [docs/superpowers/specs/2026-05-25-editor-mvp-design.md](../specs/2026-05-25-editor-mvp-design.md)

---

## Pre-flight

- [ ] **Step 0: Confirm baseline builds**

Run:
```bash
cargo check --target wasm32-unknown-unknown
cargo test --lib
```
Expected: both pass. (If `wasm32-unknown-unknown` target isn't installed: `rustup target add wasm32-unknown-unknown`.)

---

## Task 1: Make `Chemistry` cloneable

**Files:**
- Modify: `src/chemistry/mod.rs:56-62`

A `Scene` will hold a `Chemistry` and clone-pass it to `Sim::from_fab` each time it builds a fresh `Sim`. `Action` is already `Copy`, so deriving `Clone` on `Chemistry` is trivial.

- [ ] **Step 1: Add Clone derive**

In `src/chemistry/mod.rs`, change:
```rust
#[derive(Debug)]
pub struct Chemistry {
```
to:
```rust
#[derive(Debug, Clone)]
pub struct Chemistry {
```

- [ ] **Step 2: Verify it builds**

Run:
```bash
cargo build --lib
```
Expected: success.

- [ ] **Step 3: Commit**

```bash
git add src/chemistry/mod.rs
git commit -m "chore(chem): derive Clone on Chemistry for editor reuse"
```

---

## Task 2: Editor module skeleton — `Mode`, `Scene`, chemistry registry

**Files:**
- Create: `src/editor.rs`
- Modify: `src/lib.rs:1-19`

We add the new module with the `Mode` enum, a chemistry-name-to-TOML registry (for web; chemistries are `include_str!`'d at compile time, same pattern as fabs in `app.rs:55`), and a stub `Scene` struct. Tests come in Task 3.

- [ ] **Step 1: Create `src/editor.rs`**

```rust
//! Scene editor: holds the source of truth for placed beads in Edit mode
//! and produces a fresh `Sim` on Run. See
//! docs/superpowers/specs/2026-05-25-editor-mvp-design.md.

use glam::Vec2;

use crate::chemistry::{parse_chemistry, Chemistry};
use crate::fab::{BeadSpec, Fab};
use crate::sim::Sim;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    Edit,
    Run,
}

/// Chemistries the editor can switch between. Tied to the files in
/// `chemistries/`, baked in at compile time on web (same pattern as fab
/// presets in `app.rs`).
pub const CHEMISTRY_REGISTRY: &[(&str, &str)] = &[
    ("wire", include_str!("../chemistries/wire.toml")),
    ("grey", include_str!("../chemistries/grey.toml")),
    ("sem_basic", include_str!("../chemistries/sem_basic.toml")),
];

pub fn chemistry_toml(name: &str) -> Option<&'static str> {
    CHEMISTRY_REGISTRY.iter().find(|(n, _)| *n == name).map(|(_, t)| *t)
}

pub fn chemistry_names() -> Vec<&'static str> {
    CHEMISTRY_REGISTRY.iter().map(|(n, _)| *n).collect()
}

/// The scene the user is editing. Holds chemistry, world size, the bead
/// list, and the state index that the next click will assign.
pub struct Scene {
    pub chemistry: Chemistry,
    pub chemistry_name: String,
    pub world_size: f32,
    pub beads: Vec<BeadSpec>,
    pub seed: u64,
    pub next_state_idx: u32,
}

impl Scene {
    /// Build a scene from a parsed fab (existing preset) + parsed chemistry.
    pub fn from_fab(fab: &Fab, chemistry: Chemistry, chemistry_name: String) -> Self {
        Self {
            chemistry,
            chemistry_name,
            world_size: fab.meta.world_size.unwrap_or(crate::sim::WORLD_SIZE),
            beads: fab.beads.iter().map(|b| BeadSpec {
                state: b.state.clone(),
                pos: b.pos,
                vel: b.vel,
            }).collect(),
            seed: fab.meta.seed,
            next_state_idx: 0,
        }
    }

    /// Construct a fresh `Sim` from the current scene state.
    pub fn to_sim(&self) -> Sim {
        let fab = Fab {
            meta: crate::fab::Meta {
                name: format!("editor-{}", self.chemistry_name),
                chemistry: self.chemistry_name.clone(),
                seed: self.seed,
                world_size: Some(self.world_size),
            },
            beads: self.beads.clone(),
        };
        Sim::from_fab(&fab, self.chemistry.clone())
    }

    /// Copy a Sim's bead state back into the scene. Velocities are stored
    /// as `Some(...)` so the next `to_sim()` preserves momentum.
    pub fn snapshot_from_sim(&mut self, sim: &Sim) {
        let n = sim.positions.len();
        self.beads.clear();
        self.beads.reserve(n);
        let state_names = &self.chemistry.states;
        for i in 0..n {
            let p = sim.positions[i];
            let v = sim.velocities[i];
            let s = sim.states[i] as usize;
            self.beads.push(BeadSpec {
                state: state_names[s].clone(),
                pos: [p.x, p.y],
                vel: Some([v.x, v.y]),
            });
        }
    }

    /// Append a new bead at `pos` with `self.next_state_idx`. Velocity is
    /// left `None`; `Sim::from_fab` will give it a seeded random unit
    /// direction (matches existing preset convention).
    pub fn place(&mut self, pos: Vec2) {
        let state_name = self.chemistry.states[self.next_state_idx as usize].clone();
        self.beads.push(BeadSpec {
            state: state_name,
            pos: [pos.x, pos.y],
            vel: None,
        });
    }

    /// Switch chemistry. Empties beads because state names from the old
    /// chemistry may not exist in the new one.
    pub fn switch_chemistry(&mut self, chemistry: Chemistry, name: String) {
        self.chemistry = chemistry;
        self.chemistry_name = name;
        self.beads.clear();
        self.next_state_idx = 0;
    }
}

/// Parse a chemistry from the registry by name. Convenience wrapper.
pub fn load_chemistry_by_name(name: &str) -> anyhow::Result<Chemistry> {
    let toml = chemistry_toml(name)
        .ok_or_else(|| anyhow::anyhow!("unknown chemistry: {name}"))?;
    parse_chemistry(toml)
}
```

- [ ] **Step 2: Add module to lib**

In `src/lib.rs`, add after `pub mod app;` (line 9):
```rust
pub mod editor;
```

- [ ] **Step 3: Verify it builds**

Run:
```bash
cargo build --lib
cargo build --lib --target wasm32-unknown-unknown
```
Expected: both succeed.

- [ ] **Step 4: Commit**

```bash
git add src/editor.rs src/lib.rs
git commit -m "feat(editor): module skeleton — Mode, Scene, chemistry registry"
```

---

## Task 3: Tests for `Scene` round-trip

**Files:**
- Modify: `src/editor.rs` (append tests module)
- Test: same file

Verifies the core data flow: from_fab → to_sim preserves count/positions/states; snapshot_from_sim → to_sim preserves count/positions/states and velocities; chemistry switch empties beads.

- [ ] **Step 1: Add tests at the end of `src/editor.rs`**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::fab::load_fab;

    fn small_wire_fab() -> Fab {
        // 30-bead wire chain, smallest preset we ship.
        load_fab("fabs/wire-30.toml").unwrap()
    }

    #[test]
    fn from_fab_preserves_bead_count() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let scene = Scene::from_fab(&fab, chem, "wire".into());
        assert_eq!(scene.beads.len(), fab.beads.len());
    }

    #[test]
    fn to_sim_preserves_count_and_positions() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let scene = Scene::from_fab(&fab, chem, "wire".into());
        let sim = scene.to_sim();
        assert_eq!(sim.positions.len(), fab.beads.len());
        for (i, b) in fab.beads.iter().enumerate() {
            assert!((sim.positions[i].x - b.pos[0]).abs() < 1e-5);
            assert!((sim.positions[i].y - b.pos[1]).abs() < 1e-5);
        }
    }

    #[test]
    fn snapshot_round_trip_preserves_positions_states_velocities() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        let sim_a = scene.to_sim();
        scene.snapshot_from_sim(&sim_a);
        let sim_b = scene.to_sim();
        assert_eq!(sim_a.positions.len(), sim_b.positions.len());
        for i in 0..sim_a.positions.len() {
            assert!((sim_a.positions[i] - sim_b.positions[i]).length() < 1e-5);
            assert!((sim_a.velocities[i] - sim_b.velocities[i]).length() < 1e-5);
            assert_eq!(sim_a.states[i], sim_b.states[i]);
        }
    }

    #[test]
    fn place_appends_with_chosen_state() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        let before = scene.beads.len();
        scene.next_state_idx = 1; // "on" for wire
        scene.place(Vec2::new(10.0, 10.0));
        assert_eq!(scene.beads.len(), before + 1);
        assert_eq!(scene.beads.last().unwrap().state, "on");
        assert_eq!(scene.beads.last().unwrap().pos, [10.0, 10.0]);
    }

    #[test]
    fn switch_chemistry_empties_beads() {
        let fab = small_wire_fab();
        let chem = load_chemistry_by_name("wire").unwrap();
        let mut scene = Scene::from_fab(&fab, chem, "wire".into());
        assert!(!scene.beads.is_empty());
        let grey = load_chemistry_by_name("grey").unwrap();
        scene.switch_chemistry(grey, "grey".into());
        assert!(scene.beads.is_empty());
        assert_eq!(scene.chemistry_name, "grey");
    }

    #[test]
    fn chemistry_registry_has_known_entries() {
        assert!(chemistry_toml("wire").is_some());
        assert!(chemistry_toml("grey").is_some());
        assert!(chemistry_toml("sem_basic").is_some());
        assert!(chemistry_toml("nonexistent").is_none());
    }
}
```

- [ ] **Step 2: Run the tests**

Run:
```bash
cargo test --lib editor::tests
```
Expected: all 6 pass.

- [ ] **Step 3: Commit**

```bash
git add src/editor.rs
git commit -m "test(editor): Scene round-trip and place/switch behaviour"
```

---

## Task 4: Wire `Mode` + `Scene` into `App` (no behaviour change)

**Files:**
- Modify: `src/app.rs:55-72` (chemistry registry replacement), `129-156` (App fields), `196-265` (`resumed` builds Scene first)

App keeps current visible behaviour: page loads in `Mode::Run`, sim runs immediately. The change is that `Sim` is now constructed *via* `Scene::to_sim()` instead of `Sim::from_fab` directly, so subsequent tasks can flip mode and rebuild.

- [ ] **Step 1: Replace the hardcoded chemistry constant and use the registry**

In `src/app.rs`, delete line 55 (`const CHEMISTRY_TOML: &str = include_str!("../chemistries/wire.toml");`). The registry in `editor.rs` is now the single source.

- [ ] **Step 2: Add `Mode` + `Scene` to `App`**

Change the `App` struct (around `src/app.rs:129-137`) from:
```rust
pub struct App {
    window: Option<Arc<Window>>,
    renderer: Option<Renderer>,
    sim: Option<Sim>,
    scheduler: Box<dyn Scheduler>,
    last_frame: Instant,
    #[cfg(target_arch = "wasm32")]
    proxy: Option<EventLoopProxy<UserEvent>>,
}
```
to:
```rust
pub struct App {
    window: Option<Arc<Window>>,
    renderer: Option<Renderer>,
    sim: Option<Sim>,
    scheduler: Box<dyn Scheduler>,
    last_frame: Instant,
    mode: crate::editor::Mode,
    scene: Option<crate::editor::Scene>,
    cursor: winit::dpi::PhysicalPosition<f64>,
    #[cfg(target_arch = "wasm32")]
    proxy: Option<EventLoopProxy<UserEvent>>,
}
```

And update `App::new` (around line 140-150) to:
```rust
pub fn new() -> Self {
    Self {
        window: None,
        renderer: None,
        sim: None,
        scheduler: Box::new(CpuSequential),
        last_frame: Instant::now(),
        mode: crate::editor::Mode::Run,
        scene: None,
        cursor: winit::dpi::PhysicalPosition::new(0.0, 0.0),
        #[cfg(target_arch = "wasm32")]
        proxy: None,
    }
}
```

- [ ] **Step 3: Build the Scene first, then derive Sim from it (web path)**

In `src/app.rs`, change the wasm Sim construction block (currently `src/app.rs:197-210`) from:
```rust
#[cfg(target_arch = "wasm32")]
let sim = {
    let (name, fab_toml) = pick_fab_from_url();
    log::info!("loading fab {name}");
    let hash = web_sys::window()
        .and_then(|w| w.location().hash().ok())
        .unwrap_or_default();
    let speed = crate::speed::parse_speed_from_hash(&hash);
    crate::speed::set_speed(speed);
    log::info!("initial speed = {speed}×");
    let fab = parse_fab(fab_toml).expect("parse fab");
    let chem = parse_chemistry(CHEMISTRY_TOML).expect("parse chem");
    Sim::from_fab(&fab, chem)
};
```
to:
```rust
#[cfg(target_arch = "wasm32")]
let sim = {
    let (name, fab_toml) = pick_fab_from_url();
    log::info!("loading fab {name}");
    let hash = web_sys::window()
        .and_then(|w| w.location().hash().ok())
        .unwrap_or_default();
    let speed = crate::speed::parse_speed_from_hash(&hash);
    crate::speed::set_speed(speed);
    log::info!("initial speed = {speed}×");
    let fab = parse_fab(fab_toml).expect("parse fab");
    let chemistry_name = fab.meta.chemistry.clone();
    let chem = crate::editor::load_chemistry_by_name(&chemistry_name)
        .expect("chemistry from fab not in registry");
    let scene = crate::editor::Scene::from_fab(&fab, chem, chemistry_name);
    let sim = scene.to_sim();
    self.scene = Some(scene);
    sim
};
```

Also remove the now-unused `use crate::chemistry::parse_chemistry;` near the top of `app.rs` (line 22). The native path doesn't use it either since it uses `bench::scenario`.

- [ ] **Step 4: Verify nothing broke**

Run:
```bash
cargo test --lib
cargo check --target wasm32-unknown-unknown
```
Expected: pass and clean. No new warnings about unused imports.

- [ ] **Step 5: Build and smoke-test the web bundle**

If `trunk` is set up:
```bash
trunk build
```
Otherwise, run the verification script. Either way, the page should still display the running default preset — no behavioural change yet.

- [ ] **Step 6: Commit**

```bash
git add src/app.rs
git commit -m "feat(editor): Scene owns chemistry, App holds Mode + Scene

Web init now goes fab → Scene → Sim via Scene::to_sim(). Mode defaults
to Run so behaviour is unchanged."
```

---

## Task 5: Branch render loop on `Mode`; add screen→world helper

**Files:**
- Modify: `src/app.rs:305-322` (RedrawRequested), `src/editor.rs` (add `screen_to_world` + tests)

In Run mode the loop is unchanged. In Edit mode we skip `scheduler.step` and render the Scene's bead positions/states instead. We also add a pure screen→world conversion function with tests, ready for Task 6's click handler.

- [ ] **Step 1: Add `screen_to_world` to `src/editor.rs`**

Append (before the tests module):
```rust
/// Convert a viewport pixel to world coordinates using the same camera
/// math as `Renderer::update_camera`. Inverse of:
///   ortho(0, w, 0, h) where (w, h) is the aspect-corrected world rect,
///   then translate by (offset_x, offset_y) to center the world inside.
/// Screen y is top-down; world y is bottom-up.
pub fn screen_to_world(
    cursor: (f64, f64),
    viewport: (u32, u32),
    world_size: f32,
) -> Vec2 {
    let (sx, sy) = cursor;
    let (vw, vh) = (viewport.0.max(1) as f32, viewport.1.max(1) as f32);
    let aspect = vw / vh;
    let (w, h) = if aspect >= 1.0 {
        (world_size * aspect, world_size)
    } else {
        (world_size, world_size / aspect)
    };
    let offset_x = (w - world_size) * 0.5;
    let offset_y = (h - world_size) * 0.5;
    let world_x = (sx as f32 / vw) * w - offset_x;
    let world_y = (1.0 - sy as f32 / vh) * h - offset_y;
    // Clamp to world bounds so a click outside the rendered square
    // still produces a placeable position (snapped to the edge).
    Vec2::new(
        world_x.clamp(0.0, world_size),
        world_y.clamp(0.0, world_size),
    )
}
```

- [ ] **Step 2: Add tests for `screen_to_world`**

Append inside the `tests` module:
```rust
#[test]
fn screen_to_world_square_viewport_center() {
    // 100×100 viewport, 30-unit world, cursor at exact center.
    let p = screen_to_world((50.0, 50.0), (100, 100), 30.0);
    assert!((p.x - 15.0).abs() < 1e-4);
    assert!((p.y - 15.0).abs() < 1e-4);
}

#[test]
fn screen_to_world_top_left_maps_to_world_top_left() {
    // Screen (0,0) is top-left; world (0, world_size) is top-left.
    let p = screen_to_world((0.0, 0.0), (100, 100), 30.0);
    assert!((p.x - 0.0).abs() < 1e-4);
    assert!((p.y - 30.0).abs() < 1e-4);
}

#[test]
fn screen_to_world_wide_viewport_clamps_outside_x() {
    // 200×100 viewport, world 30. Aspect=2 → camera-rect width=60, world
    // centered with 15 units of empty space on each side. Cursor at
    // far-left screen edge is at world_x = -15, which clamps to 0.
    let p = screen_to_world((0.0, 50.0), (200, 100), 30.0);
    assert!((p.x - 0.0).abs() < 1e-4);
    assert!((p.y - 15.0).abs() < 1e-4);
}
```

- [ ] **Step 3: Run the tests**

Run:
```bash
cargo test --lib editor::tests
```
Expected: 9 pass (6 from Task 3 + 3 new).

- [ ] **Step 4: Branch the render loop on Mode**

In `src/app.rs`, replace the entire `WindowEvent::RedrawRequested` arm (around `src/app.rs:305-322`):
```rust
WindowEvent::RedrawRequested => {
    let Some(renderer) = &mut self.renderer else { return };
    match self.mode {
        crate::editor::Mode::Run => {
            {
                let sim = self.sim.as_mut().unwrap();
                for _ in 0..crate::speed::current_substeps() {
                    self.scheduler.step(sim, FRAME_DT);
                }
            }
            let sim = self.sim.as_mut().unwrap();
            crate::telemetry::update_from_velocities(&sim.velocities);
            renderer.update_beads(&sim.positions, &sim.states);
            if let Err(e) = renderer.render(sim.positions.len()) {
                log::warn!("render error: {e:?}");
            }
        }
        crate::editor::Mode::Edit => {
            let scene = self.scene.as_ref().expect("scene missing in Edit mode");
            // Convert scene beads to (positions, states) slices for the renderer.
            let positions: Vec<glam::Vec2> = scene.beads.iter()
                .map(|b| glam::Vec2::new(b.pos[0], b.pos[1]))
                .collect();
            let states: Vec<u32> = scene.beads.iter()
                .map(|b| scene.chemistry.state_index(&b.state).unwrap_or(0) as u32)
                .collect();
            renderer.update_beads(&positions, &states);
            if let Err(e) = renderer.render(positions.len()) {
                log::warn!("render error: {e:?}");
            }
        }
    }
    FRAME_COUNT.fetch_add(1, Ordering::Relaxed);
    window.request_redraw();
    self.last_frame = Instant::now();
}
```

- [ ] **Step 5: Track cursor moves**

In the same `window_event` match, before `_ => {}`, add a `CursorMoved` arm:
```rust
WindowEvent::CursorMoved { position, .. } => {
    self.cursor = position;
}
```

- [ ] **Step 6: Verify builds**

Run:
```bash
cargo test --lib
cargo check --target wasm32-unknown-unknown
```
Expected: pass and clean.

- [ ] **Step 7: Commit**

```bash
git add src/app.rs src/editor.rs
git commit -m "feat(editor): render-loop branches on Mode; screen_to_world helper"
```

---

## Task 6: Click → place bead (with transparent Run rebuild)

**Files:**
- Modify: `src/app.rs` (add `MouseInput` arm, add `place_at_cursor` helper method)

Left-click anywhere on the canvas appends a bead at the cursor's world position. In Edit mode, the renderer picks it up next frame. In Run mode, we snapshot the live Sim into the Scene, append, and rebuild the Sim + scheduler. The scheduler rebuild matches the existing wasm setup in `app.rs:243-246`.

- [ ] **Step 1: Add `place_at_cursor` helper on `App`**

Add this method inside the `impl App` block in `src/app.rs` (after `pub fn new()`):
```rust
fn place_at_cursor(&mut self) {
    let Some(window) = &self.window else { return };
    let Some(scene) = self.scene.as_mut() else { return };
    let viewport = window.inner_size();
    let world_pos = crate::editor::screen_to_world(
        (self.cursor.x, self.cursor.y),
        (viewport.width, viewport.height),
        scene.world_size,
    );
    match self.mode {
        crate::editor::Mode::Edit => {
            scene.place(world_pos);
        }
        crate::editor::Mode::Run => {
            // Snapshot current sim into scene, append, rebuild sim + scheduler.
            if let Some(sim) = &self.sim {
                scene.snapshot_from_sim(sim);
            }
            scene.place(world_pos);
            let new_sim = scene.to_sim();
            #[cfg(target_arch = "wasm32")]
            {
                use crate::chemistry::compile_chemistry;
                use crate::parallel::CpuParallel;
                let compiled = compile_chemistry(new_sim.chemistry())
                    .expect("compile chemistry");
                self.scheduler = Box::new(CpuParallel::new(&new_sim, compiled));
            }
            #[cfg(not(target_arch = "wasm32"))]
            {
                // Native build keeps its existing scheduler; live edits there
                // are not in MVP scope and the GPU scheduler doesn't have a
                // public reseat path. Fall back to CpuSequential.
                self.scheduler = Box::new(CpuSequential);
            }
            self.sim = Some(new_sim);
        }
    }
}
```

- [ ] **Step 2: Wire up the click event**

In the `window_event` match, add a `MouseInput` arm next to `CursorMoved`:
```rust
WindowEvent::MouseInput { state, button, .. } => {
    use winit::event::{ElementState, MouseButton};
    if state == ElementState::Pressed && button == MouseButton::Left {
        self.place_at_cursor();
    }
}
```

- [ ] **Step 3: Verify builds**

Run:
```bash
cargo test --lib
cargo check --target wasm32-unknown-unknown
```
Expected: pass and clean.

- [ ] **Step 4: Commit**

```bash
git add src/app.rs
git commit -m "feat(editor): left-click places bead, with live rebuild during Run"
```

---

## Task 7: JS bridge — mode, palette, state, bead count

**Files:**
- Modify: `src/app.rs` (add four `install_window_*` functions, call them in `resumed`)

Extends the existing `expose_to_window!` macro pattern from `app.rs:79-123`. These let the HTML toolbar (Task 9) read and write App state without reloading the page.

- [ ] **Step 1: Add a thread-local for the App's mode/state knobs**

The closures need shared mutable access to App state. winit's `ApplicationHandler` owns `App`, so we route through a thread-local containing the bits the JS needs to twiddle. Add near the top of `app.rs` (after the `FRAME_COUNT` declaration around line 14):

```rust
#[cfg(target_arch = "wasm32")]
mod web_bridge {
    use std::cell::RefCell;

    /// Pending commands from the JS toolbar, drained by the App each frame.
    #[derive(Default)]
    pub struct PendingCommands {
        pub set_mode: Option<crate::editor::Mode>,
        pub set_edit_state: Option<u32>,
        pub set_chemistry: Option<String>,
    }

    thread_local! {
        pub static COMMANDS: RefCell<PendingCommands> = RefCell::new(PendingCommands::default());
        /// Latest snapshot the App writes after each frame. The toolbar
        /// reads these via the getter closures.
        pub static SNAPSHOT: RefCell<Snapshot> = RefCell::new(Snapshot::default());
    }

    #[derive(Default, Clone)]
    pub struct Snapshot {
        pub mode: &'static str,        // "edit" or "run"
        pub bead_count: u32,
        pub chemistry_name: String,
        // (state_name, [r,g,b]) for each state in current chemistry.
        pub palette: Vec<(String, [f32; 3])>,
    }
}
```

- [ ] **Step 2: Add bridge installers for mode/palette/state/count**

Append after `install_window_speed_stats` in `src/app.rs`:

```rust
#[cfg(target_arch = "wasm32")]
fn install_window_get_mode() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> String {
        web_bridge::SNAPSHOT.with(|s| s.borrow().mode.to_string())
    }) as Box<dyn Fn() -> String>);
    expose_to_window!("__jigglefabGetMode", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_set_mode() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|m: String| {
        let mode = match m.as_str() {
            "edit" => crate::editor::Mode::Edit,
            "run" => crate::editor::Mode::Run,
            _ => return,
        };
        web_bridge::COMMANDS.with(|c| c.borrow_mut().set_mode = Some(mode));
    }) as Box<dyn Fn(String)>);
    expose_to_window!("__jigglefabSetMode", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_get_palette() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> js_sys::Array {
        let outer = js_sys::Array::new();
        web_bridge::SNAPSHOT.with(|s| {
            for (name, color) in &s.borrow().palette {
                let entry = js_sys::Object::new();
                let _ = js_sys::Reflect::set(
                    &entry,
                    &"name".into(),
                    &wasm_bindgen::JsValue::from_str(name),
                );
                let color_arr = js_sys::Array::new();
                color_arr.push(&wasm_bindgen::JsValue::from_f64(color[0] as f64));
                color_arr.push(&wasm_bindgen::JsValue::from_f64(color[1] as f64));
                color_arr.push(&wasm_bindgen::JsValue::from_f64(color[2] as f64));
                let _ = js_sys::Reflect::set(&entry, &"color".into(), &color_arr);
                outer.push(&entry);
            }
        });
        outer
    }) as Box<dyn Fn() -> js_sys::Array>);
    expose_to_window!("__jigglefabGetPalette", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_set_edit_state() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|idx: u32| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().set_edit_state = Some(idx));
    }) as Box<dyn Fn(u32)>);
    expose_to_window!("__jigglefabSetEditState", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_bead_count() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> u32 {
        web_bridge::SNAPSHOT.with(|s| s.borrow().bead_count)
    }) as Box<dyn Fn() -> u32>);
    expose_to_window!("__jigglefabBeadCount", cb);
}
```

- [ ] **Step 3: Call the installers in `resumed`**

In `src/app.rs`, find the existing `install_window_*` calls inside the wasm branch of `resumed` (around `app.rs:247-249`) and extend:
```rust
install_window_speed_setter();
install_window_frame_counter();
install_window_speed_stats();
install_window_get_mode();
install_window_set_mode();
install_window_get_palette();
install_window_set_edit_state();
install_window_bead_count();
```

- [ ] **Step 4: Drain commands and publish snapshot each frame**

At the top of the `RedrawRequested` arm (the Run/Edit match from Task 5), drain pending commands first. Add immediately after `let Some(renderer) = &mut self.renderer else { return };`:

```rust
#[cfg(target_arch = "wasm32")]
{
    web_bridge::COMMANDS.with(|c| {
        let mut cmds = c.borrow_mut();
        if let Some(new_mode) = cmds.set_mode.take() {
            self.transition_mode(new_mode);
        }
        if let Some(idx) = cmds.set_edit_state.take() {
            if let Some(scene) = self.scene.as_mut() {
                if (idx as usize) < scene.chemistry.states.len() {
                    scene.next_state_idx = idx;
                }
            }
        }
    });
}
```

And at the end of the same arm (just before `FRAME_COUNT.fetch_add`), publish the snapshot:

```rust
#[cfg(target_arch = "wasm32")]
{
    let mode_str = match self.mode {
        crate::editor::Mode::Edit => "edit",
        crate::editor::Mode::Run => "run",
    };
    let bead_count = match self.mode {
        crate::editor::Mode::Edit => self.scene.as_ref().map(|s| s.beads.len() as u32).unwrap_or(0),
        crate::editor::Mode::Run => self.sim.as_ref().map(|s| s.positions.len() as u32).unwrap_or(0),
    };
    let (chem_name, palette) = match &self.scene {
        Some(s) => (
            s.chemistry_name.clone(),
            s.chemistry.states.iter().zip(s.chemistry.colors.iter())
                .map(|(n, c)| (n.clone(), *c)).collect::<Vec<_>>(),
        ),
        None => (String::new(), Vec::new()),
    };
    web_bridge::SNAPSHOT.with(|s| {
        *s.borrow_mut() = web_bridge::Snapshot {
            mode: mode_str,
            bead_count,
            chemistry_name: chem_name,
            palette,
        };
    });
}
```

- [ ] **Step 5: Add `transition_mode` method on `App`**

Add inside the `impl App` block (after `place_at_cursor`):

```rust
fn transition_mode(&mut self, new_mode: crate::editor::Mode) {
    if self.mode == new_mode { return; }
    match new_mode {
        crate::editor::Mode::Edit => {
            // Stop: snapshot current sim back into scene, drop sim.
            if let (Some(scene), Some(sim)) = (self.scene.as_mut(), self.sim.as_ref()) {
                scene.snapshot_from_sim(sim);
            }
            self.sim = None;
            self.mode = crate::editor::Mode::Edit;
        }
        crate::editor::Mode::Run => {
            // Run: build sim from scene, rebuild scheduler.
            if let Some(scene) = &self.scene {
                let new_sim = scene.to_sim();
                #[cfg(target_arch = "wasm32")]
                {
                    use crate::chemistry::compile_chemistry;
                    use crate::parallel::CpuParallel;
                    let compiled = compile_chemistry(new_sim.chemistry())
                        .expect("compile chemistry");
                    self.scheduler = Box::new(CpuParallel::new(&new_sim, compiled));
                }
                #[cfg(not(target_arch = "wasm32"))]
                {
                    self.scheduler = Box::new(CpuSequential);
                }
                self.sim = Some(new_sim);
                self.mode = crate::editor::Mode::Run;
            }
        }
    }
}
```

- [ ] **Step 6: Verify builds**

Run:
```bash
cargo test --lib
cargo check --target wasm32-unknown-unknown
```
Expected: pass and clean.

- [ ] **Step 7: Commit**

```bash
git add src/app.rs
git commit -m "feat(editor): JS bridge — mode, palette, state, bead count

Adds web_bridge thread-local for command intake + snapshot publish.
RedrawRequested drains commands at start, publishes snapshot at end."
```

---

## Task 8: JS bridge — chemistry switch

**Files:**
- Modify: `src/app.rs` (two more installers, command handler)

Lets the toolbar list chemistries and switch between them (emptying the scene).

- [ ] **Step 1: Add the command field**

In `web_bridge`, extend `PendingCommands`:
```rust
#[derive(Default)]
pub struct PendingCommands {
    pub set_mode: Option<crate::editor::Mode>,
    pub set_edit_state: Option<u32>,
    pub set_chemistry: Option<String>,   // already present from Task 7
}
```
(Already there from Task 7 — verify it is.)

- [ ] **Step 2: Add the two installers**

Append in `src/app.rs`:
```rust
#[cfg(target_arch = "wasm32")]
fn install_window_get_chemistries() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> js_sys::Array {
        let arr = js_sys::Array::new();
        for name in crate::editor::chemistry_names() {
            arr.push(&wasm_bindgen::JsValue::from_str(name));
        }
        arr
    }) as Box<dyn Fn() -> js_sys::Array>);
    expose_to_window!("__jigglefabGetChemistries", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_set_chemistry() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|name: String| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().set_chemistry = Some(name));
    }) as Box<dyn Fn(String)>);
    expose_to_window!("__jigglefabSetChemistry", cb);
}
```

- [ ] **Step 3: Call the installers in `resumed`**

Add to the existing list:
```rust
install_window_get_chemistries();
install_window_set_chemistry();
```

- [ ] **Step 4: Handle the command**

In the `COMMANDS.with(...)` block in `RedrawRequested` (from Task 7), add after the `set_edit_state` handler:
```rust
if let Some(name) = cmds.set_chemistry.take() {
    if let Ok(new_chem) = crate::editor::load_chemistry_by_name(&name) {
        // Switching chemistry forces Edit mode (no live sim makes sense
        // against an emptied scene) and rebuilds nothing — Scene clears
        // beads, next Run rebuilds Sim.
        if let Some(scene) = self.scene.as_mut() {
            scene.switch_chemistry(new_chem, name);
        }
        self.sim = None;
        self.mode = crate::editor::Mode::Edit;
        if let (Some(renderer), Some(scene)) = (self.renderer.as_mut(), self.scene.as_ref()) {
            // Camera palette needs to refresh since state colors changed.
            let palette: Vec<[f32; 3]> = scene.chemistry.colors.clone();
            renderer.update_camera(scene.world_size, &palette);
        }
    } else {
        log::warn!("set_chemistry: unknown chemistry {name:?}");
    }
}
```

- [ ] **Step 5: Verify builds**

Run:
```bash
cargo test --lib
cargo check --target wasm32-unknown-unknown
```
Expected: pass and clean.

- [ ] **Step 6: Commit**

```bash
git add src/app.rs
git commit -m "feat(editor): JS bridge — chemistry registry + switch

Switching chemistry empties scene, forces Edit mode, refreshes camera
palette."
```

---

## Task 9: HTML toolbar — Edit/Run, chemistry pills, state pills

**Files:**
- Modify: `index.html`

The editor toolbar sits above the existing preset picker. State pills are rebuilt from `__jigglefabGetPalette()` whenever chemistry changes or mode flips (since reading the palette requires a snapshot to have been published).

- [ ] **Step 1: Add the editor toolbar markup**

In `index.html`, just before `<nav id="speed-picker"></nav>` (around line 162), add:
```html
<nav id="editor-toolbar">
    <div class="row">
        <span class="group-label">mode</span>
        <a id="btn-edit" class="mode">Edit</a>
        <a id="btn-run" class="mode">Run</a>
    </div>
    <div class="row">
        <span class="group-label">chemistry</span>
        <span id="chemistry-pills"></span>
    </div>
    <div class="row">
        <span class="group-label">state</span>
        <span id="state-pills"></span>
    </div>
</nav>
```

- [ ] **Step 2: Add styles for the toolbar**

In `index.html`, append inside the `<style>` block:
```css
#editor-toolbar {
    position: absolute;
    top: 8px;
    right: 8px;
    display: flex;
    flex-direction: column;
    gap: 4px;
    padding: 6px;
    background: rgba(0, 0, 0, 0.45);
    border-radius: 10px;
    font: 12px/1.2 ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
    z-index: 10;
}
#editor-toolbar .row { display: flex; gap: 4px; align-items: center; }
#editor-toolbar .group-label { color: #888; padding: 4px 6px; width: 70px; }
#editor-toolbar a {
    color: #cfd;
    text-decoration: none;
    padding: 4px 10px;
    border-radius: 6px;
    background: rgba(255, 255, 255, 0.06);
    border: 1px solid transparent;
    cursor: pointer;
}
#editor-toolbar a:hover { background: rgba(255, 255, 255, 0.12); }
#editor-toolbar a.active { background: #2a4d3a; color: #fff; border-color: #4a8; }
#editor-toolbar a.state-pill .dot {
    display: inline-block; width: 10px; height: 10px;
    border-radius: 50%; margin-right: 6px; vertical-align: middle;
}
```

- [ ] **Step 3: Add toolbar JS**

In `index.html`, append inside the existing `<script>` block (after the FPS tick block, before the closing `</script>`):
```js
// Editor toolbar wiring. Reads/writes the wasm-side state via the
// __jigglefab* bridge installed in src/app.rs.
const btnEdit = document.getElementById("btn-edit");
const btnRun = document.getElementById("btn-run");
const chemPills = document.getElementById("chemistry-pills");
const statePills = document.getElementById("state-pills");

let editStateIdx = 0;

function setModeBtnActive(mode) {
    btnEdit.classList.toggle("active", mode === "edit");
    btnRun.classList.toggle("active", mode === "run");
}

function paintChemistryPills() {
    if (typeof window.__jigglefabGetChemistries !== "function") return;
    const current = (typeof window.__jigglefabGetPalette === "function")
        ? null  // we don't know name from palette alone; track via attribute on active pill
        : null;
    chemPills.innerHTML = "";
    for (const name of window.__jigglefabGetChemistries()) {
        const a = document.createElement("a");
        a.textContent = name;
        a.dataset.name = name;
        a.addEventListener("click", (e) => {
            e.preventDefault();
            const beads = (window.__jigglefabBeadCount && window.__jigglefabBeadCount()) || 0;
            if (beads > 0 && !window.confirm(
                `Switch chemistry to "${name}"? This empties ${beads} placed bead${beads === 1 ? "" : "s"}.`
            )) return;
            window.__jigglefabSetChemistry(name);
            editStateIdx = 0;
            // Repaint will happen on next refreshToolbar tick.
        });
        chemPills.appendChild(a);
    }
}

function paintStatePills() {
    if (typeof window.__jigglefabGetPalette !== "function") return;
    const palette = window.__jigglefabGetPalette();
    statePills.innerHTML = "";
    palette.forEach((entry, idx) => {
        const a = document.createElement("a");
        a.className = "state-pill" + (idx === editStateIdx ? " active" : "");
        const dot = document.createElement("span");
        dot.className = "dot";
        dot.style.background = `rgb(${Math.round(entry.color[0]*255)},${Math.round(entry.color[1]*255)},${Math.round(entry.color[2]*255)})`;
        a.appendChild(dot);
        a.appendChild(document.createTextNode(entry.name));
        a.addEventListener("click", (e) => {
            e.preventDefault();
            editStateIdx = idx;
            window.__jigglefabSetEditState(idx);
            paintStatePills();
        });
        statePills.appendChild(a);
    });
}

btnEdit.addEventListener("click", (e) => {
    e.preventDefault();
    window.__jigglefabSetMode && window.__jigglefabSetMode("edit");
});
btnRun.addEventListener("click", (e) => {
    e.preventDefault();
    window.__jigglefabSetMode && window.__jigglefabSetMode("run");
});

// Poll the wasm side once a tick to reflect mode, repaint state pills
// when the palette changes (i.e. after chemistry switch), and keep the
// HUD bead count in sync with edits. Cheap.
let lastChemName = "";
const hudBeadsEl = document.getElementById("hud-beads");
function refreshToolbar() {
    if (typeof window.__jigglefabGetMode === "function") {
        setModeBtnActive(window.__jigglefabGetMode());
    }
    if (typeof window.__jigglefabGetPalette === "function") {
        const palette = window.__jigglefabGetPalette();
        // Cheap signature: comma-joined names. Reflows pills only when changed.
        const sig = palette.map(e => e.name).join(",");
        if (sig !== lastChemName) {
            lastChemName = sig;
            paintStatePills();
        }
    }
    if (typeof window.__jigglefabBeadCount === "function" && hudBeadsEl) {
        // Overrides the static count set by paintFabInfo() so edits show up.
        hudBeadsEl.textContent = window.__jigglefabBeadCount().toLocaleString();
    }
    requestAnimationFrame(refreshToolbar);
}
// Bridge installers fire inside `resumed`, after window creation. Wait
// one frame so they exist before our first read.
requestAnimationFrame(() => {
    paintChemistryPills();
    paintStatePills();
    refreshToolbar();
});
```

- [ ] **Step 4: Build the web bundle**

Run:
```bash
trunk build
```
Expected: success. If `trunk` isn't installed, the engineer should install it (`cargo install trunk`).

- [ ] **Step 5: Manual smoke test in a browser**

Run:
```bash
trunk serve
```
Open `http://localhost:8080`. Verify:
1. Default preset (wire 20×30) loads and is simulating.
2. Top-right toolbar shows "mode", "chemistry", "state" rows.
3. "Run" pill is active.
4. Clicking "Edit" freezes the sim. "Edit" pill becomes active.
5. Clicking on the canvas while in Edit places a bead (small dot in the chemistry's chosen color).
6. Clicking "Run" resumes — placed bead participates in physics.
7. Clicking on canvas while in Run places another bead and the sim keeps running.
8. Clicking the "grey" chemistry pill prompts a confirm; accepting empties the scene and switches the state pills to show only "grey".
9. Placing a grey bead and pressing Run shows it jiggling.

- [ ] **Step 6: Commit**

```bash
git add index.html
git commit -m "feat(editor): HTML toolbar — Edit/Run, chemistry pills, state pills"
```

---

## Task 10: Browser smoke test in `verify-web.py`

**Files:**
- Modify: `scripts/verify-web.py` (add an editor smoke-test mode)

Adds a programmatic regression check so future deploys catch a broken editor.

- [ ] **Step 1: Add an `--editor` flag block**

Open `scripts/verify-web.py` and find the end of `main()` just before the WebGPU summary print. Add (rough placement — adapt to existing structure):

```python
if "--editor" in sys.argv:
    # Editor smoke test: Stop → place → Run → place during run → switch chem.
    await page.wait_for_function("typeof window.__jigglefabSetMode === 'function'", timeout=10000)
    await page.evaluate("window.__jigglefabSetMode('edit')")
    await page.wait_for_function("window.__jigglefabGetMode() === 'edit'")
    before = await page.evaluate("window.__jigglefabBeadCount()")
    # Click canvas center.
    box = await page.evaluate("(() => { const c = document.querySelector('canvas'); const r = c.getBoundingClientRect(); return {x: r.left + r.width/2, y: r.top + r.height/2}; })()")
    await page.mouse.click(box["x"], box["y"])
    # Mouse events are async-ish; poll briefly.
    await page.wait_for_function(f"window.__jigglefabBeadCount() === {before + 1}", timeout=2000)
    await page.evaluate("window.__jigglefabSetMode('run')")
    await page.wait_for_function("window.__jigglefabGetMode() === 'run'")
    # Place one more during Run.
    after_edit = await page.evaluate("window.__jigglefabBeadCount()")
    await page.mouse.click(box["x"] + 20, box["y"] + 20)
    await page.wait_for_function(f"window.__jigglefabBeadCount() === {after_edit + 1}", timeout=2000)
    # Switch chemistry — auto-dismiss confirm with handler.
    page.once("dialog", lambda d: asyncio.create_task(d.accept()))
    await page.evaluate("window.__jigglefabSetChemistry('grey')")
    await page.wait_for_function("window.__jigglefabBeadCount() === 0", timeout=2000)
    console_lines.append("[editor] smoke test passed")
```

- [ ] **Step 2: Run against a local trunk serve**

In one terminal:
```bash
trunk serve
```
In another:
```bash
python scripts/verify-web.py http://localhost:8080 --editor --headed
```
Expected: exits 0, console log includes `[editor] smoke test passed`.

- [ ] **Step 3: Commit**

```bash
git add scripts/verify-web.py
git commit -m "test(web): editor smoke path in verify-web.py --editor"
```

---

## Done

At this point the editor MVP is live: Edit/Run/Stop, chemistry switch, state pick, click-to-place, and live edits during Run. Deferred items per spec §"Out": save/load, drawing, selection.
