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
            #[cfg(target_arch = "wasm32")]
            proxy: None,
        }
    }

    #[cfg(target_arch = "wasm32")]
    pub fn set_proxy(&mut self, proxy: EventLoopProxy<UserEvent>) {
        self.proxy = Some(proxy);
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
        let Some(window) = &self.window else { return };
        match event {
            WindowEvent::CloseRequested => event_loop.exit(),
            WindowEvent::Resized(size) => {
                let Some(renderer) = &mut self.renderer else { return };
                let Some(sim) = &mut self.sim else { return };
                renderer.resize(size);
                renderer.update_camera(sim.world_size(), &sim.palette());
            }
            WindowEvent::RedrawRequested => {
                let Some(renderer) = &mut self.renderer else { return };
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
                FRAME_COUNT.fetch_add(1, Ordering::Relaxed);
                window.request_redraw();
                self.last_frame = Instant::now();
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
