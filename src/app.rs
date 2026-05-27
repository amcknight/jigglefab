use winit::application::ApplicationHandler;
use winit::event::WindowEvent;
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop};
#[cfg(target_arch = "wasm32")]
use winit::event_loop::EventLoopProxy;
use winit::window::{Window, WindowId};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;
use web_time::Instant;

/// Monotonic count of completed `RedrawRequested` handlers. Read by the web
/// HUD (via `window.__jigglefabFrameCount`) to measure the real sim+render
/// frame rate, independent of the browser's vsync rAF tick rate.
pub static FRAME_COUNT: AtomicU32 = AtomicU32::new(0);

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
        // (state_name, [r,g,b]) for each state in current chemistry.
        pub palette: Vec<(String, [f32; 3])>,
    }
}

#[cfg(not(target_arch = "wasm32"))]
use crate::bench::chains::DisconnectedChains;
#[cfg(not(target_arch = "wasm32"))]
use crate::bench::scenario::Scenario;

#[cfg(target_arch = "wasm32")]
use crate::fab::parse_fab;

use crate::render::Renderer;
use crate::scheduler::{CpuSequential, Scheduler};
use crate::sim::Sim;

const FRAME_DT: f32 = 1.0 / 60.0;

// Web demo: parallel bonded wire chains, each with one "on" signal walking
// the chain. 30 beads per chain (not 100): with wire's outside=pass, long
// chains can self-fold tightly because there's nothing pushing non-adjacent
// beads apart, so we keep chains short and add more of them.
// Default 20×30 (600 beads); URL hash picks larger sizes for phone perf
// probing (#wire-40x30 .. #wire-100x30). World grows linearly with chain
// count, so beads get visually smaller at the top end — that's fine for
// "does motion stay smooth" testing.
#[cfg(target_arch = "wasm32")]
const FAB_20X30: &str = include_str!("../fabs/wire-20x30.toml");
#[cfg(target_arch = "wasm32")]
const FAB_40X30: &str = include_str!("../fabs/wire-40x30.toml");
#[cfg(target_arch = "wasm32")]
const FAB_100X30: &str = include_str!("../fabs/wire-100x30.toml");
#[cfg(target_arch = "wasm32")]
const FAB_20X20X20: &str = include_str!("../fabs/wire-20x20x20.toml");
#[cfg(target_arch = "wasm32")]
const FAB_30X30X10: &str = include_str!("../fabs/wire-30x30x10.toml");
#[cfg(target_arch = "wasm32")]
const FAB_50X50X4: &str = include_str!("../fabs/wire-50x50x4.toml");
#[cfg(target_arch = "wasm32")]
const FAB_100X30X10: &str = include_str!("../fabs/wire-100x30x10.toml");

#[cfg(target_arch = "wasm32")]
fn pick_fab_from_url() -> (&'static str, &'static str) {
    let hash = web_sys::window()
        .and_then(|w| w.location().hash().ok())
        .unwrap_or_default();
    let key = hash.trim_start_matches('#');
    match key {
        "wire-40x30" => ("wire-40x30", FAB_40X30),
        "wire-100x30" => ("wire-100x30", FAB_100X30),
        "wire-20x20x20" => ("wire-20x20x20", FAB_20X20X20),
        "wire-30x30x10" => ("wire-30x30x10", FAB_30X30X10),
        "wire-50x50x4" => ("wire-50x50x4", FAB_50X50X4),
        "wire-100x30x10" => ("wire-100x30x10", FAB_100X30X10),
        _ => ("wire-20x30", FAB_20X30),
    }
}

/// Stash a `Closure` on `window` under `name` and leak it for the page's
/// lifetime. The closure must be `'static` (the macro doesn't check, but
/// `forget()` would otherwise complain). Used by `install_window_*` below
/// to keep each bridge to ~5 lines.
#[cfg(target_arch = "wasm32")]
macro_rules! expose_to_window {
    ($name:expr, $cb:expr) => {{
        use wasm_bindgen::JsCast;
        let cb = $cb;
        if let Some(window) = web_sys::window() {
            let _ = js_sys::Reflect::set(
                &window,
                &wasm_bindgen::JsValue::from_str($name),
                cb.as_ref().unchecked_ref(),
            );
        }
        cb.forget();
    }};
}

#[cfg(target_arch = "wasm32")]
fn install_window_speed_setter() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|m: f32| {
        crate::speed::set_speed(m);
    }) as Box<dyn Fn(f32)>);
    expose_to_window!("__jigglefabSetSpeed", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_frame_counter() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> u32 {
        FRAME_COUNT.load(Ordering::Relaxed)
    }) as Box<dyn Fn() -> u32>);
    expose_to_window!("__jigglefabFrameCount", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_speed_stats() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> js_sys::Array {
        let arr = js_sys::Array::new();
        arr.push(&wasm_bindgen::JsValue::from_f64(crate::telemetry::min() as f64));
        arr.push(&wasm_bindgen::JsValue::from_f64(crate::telemetry::mean() as f64));
        arr.push(&wasm_bindgen::JsValue::from_f64(crate::telemetry::max() as f64));
        arr
    }) as Box<dyn Fn() -> js_sys::Array>);
    expose_to_window!("__jigglefabSpeedStats", cb);
}

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

pub enum UserEvent {
    RendererReady(Renderer),
}

pub struct App {
    window: Option<Arc<Window>>,
    renderer: Option<Renderer>,
    sim: Option<Sim>,
    scheduler: Box<dyn Scheduler>,
    last_frame: Instant,
    mode: crate::editor::Mode,
    scene: Option<crate::editor::Scene>,
    cursor: winit::dpi::PhysicalPosition<f64>,
    drag: crate::editor::DragState,
    /// True only while the left mouse button is held. mousemove uses this to
    /// know whether to extend the current `drag`.
    mouse_down: bool,
    #[cfg(target_arch = "wasm32")]
    proxy: Option<EventLoopProxy<UserEvent>>,
}

impl App {
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
            drag: crate::editor::DragState::None,
            mouse_down: false,
            #[cfg(target_arch = "wasm32")]
            proxy: None,
        }
    }

    #[cfg(target_arch = "wasm32")]
    pub fn set_proxy(&mut self, proxy: EventLoopProxy<UserEvent>) {
        self.proxy = Some(proxy);
    }

    fn cursor_world(&self) -> Option<glam::Vec2> {
        let window = self.window.as_ref()?;
        let scene = self.scene.as_ref()?;
        let viewport = window.inner_size();
        Some(crate::editor::screen_to_world(
            (self.cursor.x, self.cursor.y),
            (viewport.width, viewport.height),
            scene.world_size,
        ))
    }

    /// True if `world_pos` lies within RADIUS of any currently-selected bead.
    fn hit_selected(scene: &crate::editor::Scene, world_pos: glam::Vec2) -> bool {
        scene.selection.iter().any(|&idx| {
            let p = glam::Vec2::from(scene.beads[idx as usize].pos);
            (p - world_pos).length() <= crate::ccd::RADIUS
        })
    }

    fn rebuild_sim_from_scene(&mut self) {
        let scene = self.scene.as_ref().expect("scene present");
        let new_sim = scene.to_sim();
        #[cfg(target_arch = "wasm32")]
        {
            use crate::chemistry::compile_chemistry;
            use crate::parallel::CpuParallel;
            let compiled = compile_chemistry(new_sim.chemistry()).expect("compile chemistry");
            self.scheduler = Box::new(CpuParallel::new(&new_sim, compiled));
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            self.scheduler = Box::new(CpuSequential);
        }
        self.sim = Some(new_sim);
    }

    fn on_mouse_down(&mut self) {
        self.mouse_down = true;
        let Some(world_pos) = self.cursor_world() else { return };
        let Some(scene) = self.scene.as_mut() else { return };
        if Self::hit_selected(scene, world_pos) {
            self.drag = crate::editor::DragState::Move { last_cursor: world_pos };
            return;
        }
        match (self.mode, scene.tool) {
            (crate::editor::Mode::Run, _) => {
                if let Some(sim) = &self.sim { scene.snapshot_from_sim(sim); }
                scene.place(world_pos);
                self.rebuild_sim_from_scene();
                self.drag = crate::editor::DragState::None;
            }
            (crate::editor::Mode::Edit, crate::editor::Tool::Place) => {
                scene.place(world_pos);
                self.drag = crate::editor::DragState::None;
            }
            (crate::editor::Mode::Edit, crate::editor::Tool::Chain) => {
                let idx = scene.place(world_pos);
                self.drag = crate::editor::DragState::Chain { last_idx: idx };
            }
            (crate::editor::Mode::Edit, crate::editor::Tool::Rect) => {
                self.drag = crate::editor::DragState::Rect { anchor: world_pos, current: world_pos, moved: false };
            }
            (crate::editor::Mode::Edit, crate::editor::Tool::Lasso) => {
                self.drag = crate::editor::DragState::Lasso { points: vec![world_pos] };
            }
        }
    }

    fn on_mouse_move(&mut self) {
        if !self.mouse_down { return; }
        let Some(world_pos) = self.cursor_world() else { return };
        let Some(scene) = self.scene.as_mut() else { return };
        match &mut self.drag {
            crate::editor::DragState::Chain { last_idx } => {
                *last_idx = scene.chain_extend(*last_idx, world_pos);
            }
            crate::editor::DragState::Rect { current, moved, .. } => {
                *current = world_pos;
                *moved = true;
            }
            crate::editor::DragState::Lasso { points } => {
                if let Some(last) = points.last() {
                    if (*last - world_pos).length() >= 0.05 {
                        points.push(world_pos);
                    }
                }
            }
            crate::editor::DragState::Move { last_cursor } => {
                let delta = world_pos - *last_cursor;
                scene.translate_selection(delta);
                *last_cursor = world_pos;
            }
            crate::editor::DragState::None => {}
        }
    }

    fn on_mouse_up(&mut self) {
        self.mouse_down = false;
        let drag = std::mem::take(&mut self.drag);
        let Some(scene) = self.scene.as_mut() else { return };
        match drag {
            crate::editor::DragState::Rect { anchor, current, moved } => {
                if moved {
                    scene.select_rect(anchor, current);
                } else {
                    scene.selection.clear();
                }
            }
            crate::editor::DragState::Lasso { points } => {
                if points.len() >= 3 {
                    scene.select_lasso(&points);
                } else {
                    scene.selection.clear();
                }
            }
            crate::editor::DragState::Move { .. } => {}
            crate::editor::DragState::Chain { .. } | crate::editor::DragState::None => {}
        }
    }

    /// World-space line segments to draw as the rect/lasso overlay this frame.
    /// Returns an empty vec when no overlay is active. LineList topology: each
    /// pair of consecutive entries defines one segment.
    fn overlay_segments(&self) -> Vec<[f32; 2]> {
        match &self.drag {
            crate::editor::DragState::Rect { anchor, current, .. } => {
                let (a, b) = (*anchor, *current);
                let (xmin, xmax) = if a.x <= b.x { (a.x, b.x) } else { (b.x, a.x) };
                let (ymin, ymax) = if a.y <= b.y { (a.y, b.y) } else { (b.y, a.y) };
                vec![
                    [xmin, ymin], [xmax, ymin],
                    [xmax, ymin], [xmax, ymax],
                    [xmax, ymax], [xmin, ymax],
                    [xmin, ymax], [xmin, ymin],
                ]
            }
            crate::editor::DragState::Lasso { points } => {
                if points.len() < 2 { return Vec::new(); }
                let mut segs = Vec::with_capacity(points.len() * 2);
                for w in points.windows(2) {
                    segs.push([w[0].x, w[0].y]);
                    segs.push([w[1].x, w[1].y]);
                }
                segs
            }
            _ => Vec::new(),
        }
    }

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
                if let Some(scene) = self.scene.as_mut() {
                    scene.selection.clear();
                }
                self.drag = crate::editor::DragState::None;
                self.mouse_down = false;
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
}

impl Default for App {
    fn default() -> Self {
        Self::new()
    }
}

impl ApplicationHandler<UserEvent> for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        event_loop.set_control_flow(ControlFlow::Poll);

        #[cfg(not(target_arch = "wasm32"))]
        let attrs = Window::default_attributes()
            .with_title("JiggleFab P1")
            .with_inner_size(winit::dpi::PhysicalSize::new(800u32, 800u32))
            .with_position(winit::dpi::PhysicalPosition::new(50i32, 50i32));

        #[cfg(target_arch = "wasm32")]
        let attrs = {
            use winit::platform::web::WindowAttributesExtWebSys;
            Window::default_attributes()
                .with_title("JiggleFab P1")
                .with_append(true)
        };

        let window = Arc::new(event_loop.create_window(attrs).expect("create window"));

        #[cfg(not(target_arch = "wasm32"))]
        let sim = {
            // Native build: GPU CCD demo — 30 × 30 = 900 beads, the bead count
            // where the CPU scheduler craters (2.6 fps in the baseline bench).
            let scenario = DisconnectedChains {
                chain_count: 30,
                chain_len: 30,
                world_size: 128.0,
            };
            let (sim, _invariants) = scenario.build();
            sim
        };

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
        let world_size = sim.world_size();
        let palette = sim.palette();

        #[cfg(not(target_arch = "wasm32"))]
        {
            let mut renderer = pollster::block_on(Renderer::new(window.clone(), sim.positions.len()))
                .expect("create renderer");
            renderer.update_camera(world_size, &palette);

            // Upgrade to GPU scheduler now that we have the wgpu device/queue
            // from the renderer. GpuEventLoop shares the same device as the
            // renderer, so there is no duplicate GPU context.
            {
                use crate::gpu::context::GpuContext;
                use crate::gpu::scheduler::GpuEventLoop;
                let ctx = GpuContext::from_renderer(
                    renderer.device.clone(),
                    renderer.queue.clone(),
                );
                self.scheduler = Box::new(GpuEventLoop::new(ctx, &sim));
            }

            self.renderer = Some(renderer);
        }

        #[cfg(target_arch = "wasm32")]
        {
            // Upgrade to CpuParallel before the async renderer spawns —
            // it doesn't need the wgpu device, just the sim's bead state.
            // CpuSequential at 300 beads × 10 substeps/frame was already
            // close to budget; CpuParallel handles the same load in a
            // fraction of the time and leaves room to grow the demo.
            use crate::chemistry::compile_chemistry;
            use crate::parallel::CpuParallel;
            let compiled = compile_chemistry(sim.chemistry()).expect("compile chemistry");
            self.scheduler = Box::new(CpuParallel::new(&sim, compiled));
            install_window_speed_setter();
            install_window_frame_counter();
            install_window_speed_stats();
            install_window_get_mode();
            install_window_set_mode();
            install_window_get_palette();
            install_window_set_edit_state();
            install_window_bead_count();
            install_window_get_chemistries();
            install_window_set_chemistry();

            let proxy = self.proxy.clone().expect("proxy not set before resumed()");
            let window_clone = window.clone();
            let n = sim.positions.len();
            wasm_bindgen_futures::spawn_local(async move {
                let mut renderer = Renderer::new(window_clone, n).await
                    .expect("create renderer");
                renderer.update_camera(world_size, &palette);
                let _ = proxy.send_event(UserEvent::RendererReady(renderer));
            });
        }

        self.window = Some(window);
        self.sim = Some(sim);
        self.last_frame = Instant::now();
    }

    fn user_event(&mut self, _event_loop: &ActiveEventLoop, event: UserEvent) {
        match event {
            UserEvent::RendererReady(mut renderer) => {
                // On web the async wgpu init started before winit's
                // ResizeObserver had populated the canvas size, so the surface
                // was configured at 1×1. Reconfigure to the actual viewport
                // now that we have a renderer to talk to.
                if let (Some(w), Some(sim)) = (&self.window, &self.sim) {
                    let size = w.inner_size();
                    if size.width > 0 && size.height > 0 {
                        renderer.resize(size);
                        renderer.update_camera(sim.world_size(), &sim.palette());
                    }
                    w.request_redraw();
                }
                self.renderer = Some(renderer);
            }
        }
    }

    fn about_to_wait(&mut self, _event_loop: &ActiveEventLoop) {
        // In Poll mode, drive the redraw cycle ourselves so the sim keeps
        // stepping at vsync rate regardless of focus or window-event activity.
        if let Some(w) = &self.window {
            w.request_redraw();
        }
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, _id: WindowId, event: WindowEvent) {
        let Some(_window) = &self.window else { return };
        match event {
            WindowEvent::CloseRequested => event_loop.exit(),
            WindowEvent::Resized(size) => {
                let Some(renderer) = &mut self.renderer else { return };
                let Some(sim) = &mut self.sim else { return };
                renderer.resize(size);
                renderer.update_camera(sim.world_size(), &sim.palette());
            }
            WindowEvent::RedrawRequested => {
                // Clone the Arc so we can call request_redraw() at the end
                // without holding a borrow of self.window across the whole arm.
                let Some(window_arc) = self.window.clone() else { return };
                #[cfg(target_arch = "wasm32")]
                {
                    let (new_mode, edit_state, new_chemistry) = web_bridge::COMMANDS.with(|c| {
                        let mut cmds = c.borrow_mut();
                        (cmds.set_mode.take(), cmds.set_edit_state.take(), cmds.set_chemistry.take())
                    });
                    if let Some(new_mode) = new_mode { self.transition_mode(new_mode); }
                    if let Some(idx) = edit_state {
                        if let Some(scene) = self.scene.as_mut() {
                            if (idx as usize) < scene.chemistry.states.len() {
                                scene.next_state_idx = idx;
                            }
                        }
                    }
                    if let Some(name) = new_chemistry {
                        if let Ok(new_chem) = crate::editor::load_chemistry_by_name(&name) {
                            if let Some(scene) = self.scene.as_mut() {
                                scene.switch_chemistry(new_chem, name);
                            }
                            self.sim = None;
                            self.mode = crate::editor::Mode::Edit;
                            self.drag = crate::editor::DragState::None;
                            self.mouse_down = false;
                            if let (Some(renderer), Some(scene)) = (self.renderer.as_mut(), self.scene.as_ref()) {
                                let palette: Vec<[f32; 3]> = scene.chemistry.colors.clone();
                                renderer.update_camera(scene.world_size, &palette);
                            }
                        } else {
                            log::warn!("set_chemistry: unknown chemistry {:?}", name);
                        }
                    }
                }
                let overlay = self.overlay_segments();
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
                        let selected: Vec<u32> = match &self.scene {
                            Some(s) => (0..sim.positions.len()).map(|i| if s.selection.contains(&(i as u32)) { 1 } else { 0 }).collect(),
                            None => vec![0; sim.positions.len()],
                        };
                        renderer.update_beads(&sim.positions, &sim.states, &selected);
                        renderer.update_overlay(&overlay);
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
                        let selected: Vec<u32> = (0..positions.len())
                            .map(|i| if scene.selection.contains(&(i as u32)) { 1 } else { 0 })
                            .collect();
                        renderer.update_beads(&positions, &states, &selected);
                        renderer.update_overlay(&overlay);
                        if let Err(e) = renderer.render(positions.len()) {
                            log::warn!("render error: {e:?}");
                        }
                    }
                }
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
                    let palette: Vec<(String, [f32; 3])> = match &self.scene {
                        Some(s) => s.chemistry.states.iter().zip(s.chemistry.colors.iter())
                            .map(|(n, c)| (n.clone(), *c)).collect(),
                        None => Vec::new(),
                    };
                    web_bridge::SNAPSHOT.with(|s| {
                        *s.borrow_mut() = web_bridge::Snapshot {
                            mode: mode_str,
                            bead_count,
                            palette,
                        };
                    });
                }
                FRAME_COUNT.fetch_add(1, Ordering::Relaxed);
                window_arc.request_redraw();
                self.last_frame = Instant::now();
            }
            WindowEvent::CursorMoved { position, .. } => {
                self.cursor = position;
                self.on_mouse_move();
            }
            WindowEvent::MouseInput { state, button, .. } => {
                use winit::event::{ElementState, MouseButton};
                if button == MouseButton::Left {
                    match state {
                        ElementState::Pressed => self.on_mouse_down(),
                        ElementState::Released => self.on_mouse_up(),
                    }
                }
            }
            WindowEvent::KeyboardInput { event: key_event, .. } => {
                use winit::event::ElementState;
                use winit::keyboard::{Key, NamedKey};
                if key_event.state == ElementState::Pressed {
                    let is_delete = matches!(
                        key_event.logical_key,
                        Key::Named(NamedKey::Delete) | Key::Named(NamedKey::Backspace)
                    );
                    if is_delete {
                        if self.mode == crate::editor::Mode::Edit {
                            if let Some(scene) = self.scene.as_mut() {
                                scene.delete_selection();
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

pub fn run() -> anyhow::Result<()> {
    let event_loop = EventLoop::<UserEvent>::with_user_event().build()?;
    let mut app = App::new();
    #[cfg(target_arch = "wasm32")]
    app.set_proxy(event_loop.create_proxy());
    event_loop.run_app(&mut app)?;
    Ok(())
}
