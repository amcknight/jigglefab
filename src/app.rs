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
        pub set_tool: Option<crate::editor::Tool>,
        pub clear: bool,
        pub revert: bool,
        pub load_library: Option<String>,
        pub save_to_dock: Option<String>,
        pub rename_device: Option<(u32, String)>,
        pub remove_device: Option<u32>,
        pub arm_device: Option<u32>,
        pub disarm: bool,
        pub save_suite: Option<String>,
        pub load_suite: Option<String>,
        pub import_suite: Option<String>,
        pub set_render_mode: Option<String>,
    }

    thread_local! {
        pub static COMMANDS: RefCell<PendingCommands> = RefCell::new(PendingCommands::default());
        /// Latest snapshot the App writes after each frame. The toolbar
        /// reads these via the getter closures.
        pub static SNAPSHOT: RefCell<Snapshot> = RefCell::new(Snapshot::default());
    }

    /// One dock device, projected for the JS thumbnail renderer. `beads` is a
    /// list of (relative position, rgb color) pairs; `compatible` is computed
    /// against the scene's current chemistry.
    #[derive(Clone)]
    pub struct DockEntry {
        pub id: u32,
        pub name: String,
        pub chemistry: String,
        pub beads: Vec<([f32; 2], [f32; 3])>,
        pub compatible: bool,
    }

    #[derive(Default, Clone)]
    pub struct Snapshot {
        pub mode: &'static str,        // "edit" or "run"
        pub bead_count: u32,
        // (state_name, [r,g,b]) for each state in current chemistry.
        pub palette: Vec<(String, [f32; 3])>,
        pub tool: &'static str,
        pub selection_count: u32,
        pub chemistry_name: String,
        pub can_revert: bool,
        pub zoom: f32,
        pub center_x: f32,
        pub center_y: f32,
        pub grid_alpha: f32,
        pub library_json: String,
        pub library_rev: u32,
        pub armed_id: i32,
        pub dock: Vec<DockEntry>,
        pub suite_names: Vec<String>,
        pub render_mode: &'static str,
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

/// Seconds the grid stays fully in after the last camera move before fading.
const GRID_HOLD_S: f32 = 0.4;
const GRID_FADE_IN_S: f32 = 0.12;
const GRID_FADE_OUT_S: f32 = 0.40;

/// Advance the grid fade one frame (frame-rate independent exponential ease).
/// `idle_since` = seconds since the last zoom/pan. Returns the new alpha in [0,1].
fn grid_fade_step(alpha: f32, idle_since: f32, dt: f32) -> f32 {
    let target = if idle_since < GRID_HOLD_S { 1.0 } else { 0.0 };
    let tau = if target > alpha { GRID_FADE_IN_S } else { GRID_FADE_OUT_S };
    let k = 1.0 - (-dt / tau.max(1e-6)).exp();
    alpha + (target - alpha) * k
}

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
fn install_window_get_chemistry_name() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> String {
        web_bridge::SNAPSHOT.with(|s| s.borrow().chemistry_name.clone())
    }) as Box<dyn Fn() -> String>);
    expose_to_window!("__jigglefabGetChemistryName", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_set_chemistry() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|name: String| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().set_chemistry = Some(name));
    }) as Box<dyn Fn(String)>);
    expose_to_window!("__jigglefabSetChemistry", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_get_tool() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> String {
        web_bridge::SNAPSHOT.with(|s| s.borrow().tool.to_string())
    }) as Box<dyn Fn() -> String>);
    expose_to_window!("__jigglefabGetTool", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_set_tool() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|s: String| {
        if let Some(t) = crate::editor::Tool::from_str(&s) {
            web_bridge::COMMANDS.with(|c| c.borrow_mut().set_tool = Some(t));
        }
    }) as Box<dyn Fn(String)>);
    expose_to_window!("__jigglefabSetTool", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_selection_count() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> u32 {
        web_bridge::SNAPSHOT.with(|s| s.borrow().selection_count)
    }) as Box<dyn Fn() -> u32>);
    expose_to_window!("__jigglefabSelectionCount", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_clear() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().clear = true);
    }) as Box<dyn Fn()>);
    expose_to_window!("__jigglefabClear", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_revert() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().revert = true);
    }) as Box<dyn Fn()>);
    expose_to_window!("__jigglefabRevert", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_can_revert() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> bool {
        web_bridge::SNAPSHOT.with(|s| s.borrow().can_revert)
    }) as Box<dyn Fn() -> bool>);
    expose_to_window!("__jigglefabCanRevert", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_get_zoom() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> f32 {
        web_bridge::SNAPSHOT.with(|s| s.borrow().zoom)
    }) as Box<dyn Fn() -> f32>);
    expose_to_window!("__jigglefabGetZoom", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_get_center_x() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> f32 {
        web_bridge::SNAPSHOT.with(|s| s.borrow().center_x)
    }) as Box<dyn Fn() -> f32>);
    expose_to_window!("__jigglefabGetCenterX", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_get_center_y() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> f32 {
        web_bridge::SNAPSHOT.with(|s| s.borrow().center_y)
    }) as Box<dyn Fn() -> f32>);
    expose_to_window!("__jigglefabGetCenterY", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_grid_alpha() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> f32 {
        web_bridge::SNAPSHOT.with(|s| s.borrow().grid_alpha)
    }) as Box<dyn Fn() -> f32>);
    expose_to_window!("__jigglefabGridAlpha", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_load_library() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|json: String| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().load_library = Some(json));
    }) as Box<dyn Fn(String)>);
    expose_to_window!("__jigglefabLoadLibrary", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_get_library_json() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> String {
        web_bridge::SNAPSHOT.with(|s| s.borrow().library_json.clone())
    }) as Box<dyn Fn() -> String>);
    expose_to_window!("__jigglefabGetLibraryJson", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_get_library_rev() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> u32 {
        web_bridge::SNAPSHOT.with(|s| s.borrow().library_rev)
    }) as Box<dyn Fn() -> u32>);
    expose_to_window!("__jigglefabGetLibraryRev", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_armed_id() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> i32 {
        web_bridge::SNAPSHOT.with(|s| s.borrow().armed_id)
    }) as Box<dyn Fn() -> i32>);
    expose_to_window!("__jigglefabArmedId", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_get_suite_names() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> js_sys::Array {
        let arr = js_sys::Array::new();
        web_bridge::SNAPSHOT.with(|s| {
            for name in &s.borrow().suite_names {
                arr.push(&wasm_bindgen::JsValue::from_str(name));
            }
        });
        arr
    }) as Box<dyn Fn() -> js_sys::Array>);
    expose_to_window!("__jigglefabGetSuiteNames", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_get_dock() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> js_sys::Array {
        let outer = js_sys::Array::new();
        web_bridge::SNAPSHOT.with(|s| {
            for d in &s.borrow().dock {
                let entry = js_sys::Object::new();
                let _ = js_sys::Reflect::set(&entry, &"id".into(), &wasm_bindgen::JsValue::from_f64(d.id as f64));
                let _ = js_sys::Reflect::set(&entry, &"name".into(), &wasm_bindgen::JsValue::from_str(&d.name));
                let _ = js_sys::Reflect::set(&entry, &"chemistry".into(), &wasm_bindgen::JsValue::from_str(&d.chemistry));
                let _ = js_sys::Reflect::set(&entry, &"compatible".into(), &wasm_bindgen::JsValue::from_bool(d.compatible));
                let beads = js_sys::Array::new();
                for (pos, color) in &d.beads {
                    let b = js_sys::Object::new();
                    let p = js_sys::Array::new();
                    p.push(&wasm_bindgen::JsValue::from_f64(pos[0] as f64));
                    p.push(&wasm_bindgen::JsValue::from_f64(pos[1] as f64));
                    let _ = js_sys::Reflect::set(&b, &"pos".into(), &p);
                    let col = js_sys::Array::new();
                    col.push(&wasm_bindgen::JsValue::from_f64(color[0] as f64));
                    col.push(&wasm_bindgen::JsValue::from_f64(color[1] as f64));
                    col.push(&wasm_bindgen::JsValue::from_f64(color[2] as f64));
                    let _ = js_sys::Reflect::set(&b, &"color".into(), &col);
                    beads.push(&b);
                }
                let _ = js_sys::Reflect::set(&entry, &"beads".into(), &beads);
                outer.push(&entry);
            }
        });
        outer
    }) as Box<dyn Fn() -> js_sys::Array>);
    expose_to_window!("__jigglefabGetDock", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_save_to_dock() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|name: String| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().save_to_dock = Some(name));
    }) as Box<dyn Fn(String)>);
    expose_to_window!("__jigglefabSaveToDock", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_rename_device() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|id: u32, name: String| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().rename_device = Some((id, name)));
    }) as Box<dyn Fn(u32, String)>);
    expose_to_window!("__jigglefabRenameDevice", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_remove_device() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|id: u32| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().remove_device = Some(id));
    }) as Box<dyn Fn(u32)>);
    expose_to_window!("__jigglefabRemoveDevice", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_arm_device() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|id: u32| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().arm_device = Some(id));
    }) as Box<dyn Fn(u32)>);
    expose_to_window!("__jigglefabArmDevice", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_disarm() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().disarm = true);
    }) as Box<dyn Fn()>);
    expose_to_window!("__jigglefabDisarm", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_save_suite() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|name: String| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().save_suite = Some(name));
    }) as Box<dyn Fn(String)>);
    expose_to_window!("__jigglefabSaveSuite", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_load_suite() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|name: String| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().load_suite = Some(name));
    }) as Box<dyn Fn(String)>);
    expose_to_window!("__jigglefabLoadSuite", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_import_suite() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|json: String| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().import_suite = Some(json));
    }) as Box<dyn Fn(String)>);
    expose_to_window!("__jigglefabImportSuite", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_get_render_mode() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> String {
        web_bridge::SNAPSHOT.with(|s| s.borrow().render_mode.to_string())
    }) as Box<dyn Fn() -> String>);
    expose_to_window!("__jigglefabGetRenderMode", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_set_render_mode() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|name: String| {
        web_bridge::COMMANDS.with(|c| c.borrow_mut().set_render_mode = Some(name));
    }) as Box<dyn Fn(String)>);
    expose_to_window!("__jigglefabSetRenderMode", cb);
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
    camera: crate::camera::Camera,
    /// True while Space is held — turns a left-drag into a pan.
    space_held: bool,
    /// `Some` while a pan drag is active; the value is the button that owns the
    /// pan (Middle, or Left for a space+left pan). Tracking the owner keeps pan
    /// and edit gestures mutually exclusive: a left press/release can't disturb a
    /// middle-button pan, and vice versa.
    pan_button: Option<winit::event::MouseButton>,
    /// Adaptive grid fade: current alpha and seconds since the last camera move.
    grid_alpha: f32,
    idle_since: f32,
    drag: crate::editor::DragState,
    /// True only while the left mouse button is held. mousemove uses this to
    /// know whether to extend the current `drag`.
    mouse_down: bool,
    /// Scene payload captured at the most recent Edit→Run transition.
    /// `Some` means Revert is available. Cleared on chemistry switch or Clear.
    pre_run_snapshot: Option<crate::editor::ScenePayload>,
    /// In-app device library. Persisted to localStorage by JS; mutated here.
    library: crate::library::Library,
    /// The device currently armed for stamping (Edit mode), if any.
    armed_device: Option<crate::library::Device>,
    /// Accumulated ghost rotation in radians (reset to 0 on each fresh arm).
    ghost_angle: f32,
    /// True while Shift is held — turns scroll into rotate-the-selection.
    shift_held: bool,
    /// Bumped on every library mutation so JS re-persists + re-renders.
    library_rev: u32,
    /// Current render mode (Disc, Voronoi, etc.). Uploaded to the shader each frame.
    render_mode: crate::render_mode::RenderMode,
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
            camera: crate::camera::Camera::fit(crate::sim::WORLD_SIZE),
            space_held: false,
            pan_button: None,
            grid_alpha: 0.0,
            idle_since: 1.0e9, // start fully idle (grid hidden)
            drag: crate::editor::DragState::None,
            mouse_down: false,
            pre_run_snapshot: None,
            library: crate::library::Library::default(),
            armed_device: None,
            ghost_angle: 0.0,
            shift_held: false,
            library_rev: 0,
            render_mode: crate::render_mode::RenderMode::Disc,
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
        Some(self.camera.screen_to_world(
            (self.cursor.x, self.cursor.y),
            (viewport.width, viewport.height),
            scene.world_size,
        ))
    }

    fn viewport(&self) -> Option<(u32, u32)> {
        let s = self.window.as_ref()?.inner_size();
        Some((s.width, s.height))
    }

    fn world_size(&self) -> f32 {
        self.scene.as_ref().map(|s| s.world_size)
            .or_else(|| self.sim.as_ref().map(|s| s.world_size()))
            .unwrap_or(crate::sim::WORLD_SIZE)
    }

    fn palette_for_camera(&self) -> Vec<[f32; 3]> {
        if let Some(scene) = &self.scene {
            scene.chemistry.colors.clone()
        } else if let Some(sim) = &self.sim {
            sim.palette()
        } else {
            Vec::new()
        }
    }

    /// Mark a camera move (zoom/pan) so the scale grid fades in this frame.
    fn note_camera_activity(&mut self) {
        self.idle_since = 0.0;
    }

    /// Rotate the current rotation target by `delta` radians: the armed ghost
    /// if one is armed, otherwise the current selection in the scene.
    fn apply_rotation(&mut self, delta: f32) {
        if self.armed_device.is_some() {
            self.ghost_angle += delta;
        } else if let Some(scene) = self.scene.as_mut() {
            scene.rotate_selection(delta);
        }
    }

    /// Re-upload the current camera to the renderer. Locals are pulled out first
    /// so the `&mut self.renderer` borrow doesn't overlap the `&self` reads
    /// (`palette_for_camera`/`world_size` borrow self whole; `Camera` is `Copy`).
    fn refresh_camera(&mut self) {
        let ws = self.world_size();
        let palette = self.palette_for_camera();
        let camera = self.camera;
        let bead_count = self.sim.as_ref().map(|s| s.positions.len() as u32)
            .or_else(|| self.scene.as_ref().map(|s| s.beads.len() as u32))
            .unwrap_or(0);
        let render_mode = self.render_mode;
        if let Some(r) = &mut self.renderer {
            r.update_camera(&camera, ws, &palette, bead_count, render_mode);
        }
    }

    /// True if `world_pos` lies within RADIUS of any currently-selected bead.
    fn hit_selected(scene: &crate::editor::Scene, world_pos: glam::Vec2) -> bool {
        scene.selection.iter().any(|&idx| {
            let p = glam::Vec2::from(scene.beads[idx as usize].pos);
            crate::grid::min_image(world_pos, p, scene.world_size).length() <= crate::ccd::RADIUS
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

    fn revert_to_snapshot(&mut self) {
        let Some(payload) = self.pre_run_snapshot.as_ref() else { return };
        if let Some(scene) = self.scene.as_mut() {
            scene.restore_payload(payload);
        }
        self.sim = None;
        self.mode = crate::editor::Mode::Edit;
        self.drag = crate::editor::DragState::None;
        self.mouse_down = false;
    }

    fn on_mouse_down(&mut self) {
        self.mouse_down = true;
        let Some(world_pos) = self.cursor_world() else { return };
        // Armed device takes priority: stamp a copy and stay armed (Edit only).
        if self.mode == crate::editor::Mode::Edit {
            if let Some(dev) = self.armed_device.clone() {
                if let Some(scene) = self.scene.as_mut() {
                    scene.instantiate_device(&dev, world_pos, self.ghost_angle);
                }
                self.drag = crate::editor::DragState::None;
                return;
            }
        }
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
                scene.selection.clear();
                scene.place(world_pos);
                self.drag = crate::editor::DragState::None;
            }
            (crate::editor::Mode::Edit, crate::editor::Tool::Chain) => {
                scene.selection.clear();
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

    /// Peak shade of the fully-faded-in scale grid (boundary lines); interior
    /// subdivision lines get half. The overlay fragment also multiplies by 0.7.
    const GRID_SHADE: f32 = 0.35;

    /// One rotation notch: 15°, in radians.
    const ROTATE_SNAP_RAD: f32 = std::f32::consts::PI / 12.0;

    fn overlay_segments(&self) -> Vec<crate::render::OverlayVertex> {
        use crate::render::OverlayVertex;
        let mut out: Vec<OverlayVertex> = Vec::new();
        // Adaptive scale grid, faded by camera activity. Skipped entirely at rest.
        if self.grid_alpha > 0.001 {
            if let Some(viewport) = self.viewport() {
                let ws = self.world_size();
                let (min, max) = self.camera.visible_world_rect(viewport, ws);
                let wpp = self.camera.world_per_px(viewport, ws);
                for (pos, weight) in crate::camera::grid_segments(min, max, ws, wpp) {
                    out.push(OverlayVertex { pos, shade: self.grid_alpha * weight * Self::GRID_SHADE });
                }
            }
        }
        // Drag overlay (bright).
        let bright = |p: [f32; 2]| OverlayVertex { pos: p, shade: 1.0 };
        match &self.drag {
            crate::editor::DragState::Rect { anchor, current, .. } => {
                let (a, b) = (*anchor, *current);
                let (xmin, xmax) = if a.x <= b.x { (a.x, b.x) } else { (b.x, a.x) };
                let (ymin, ymax) = if a.y <= b.y { (a.y, b.y) } else { (b.y, a.y) };
                for p in [
                    [xmin, ymin], [xmax, ymin],
                    [xmax, ymin], [xmax, ymax],
                    [xmax, ymax], [xmin, ymax],
                    [xmin, ymax], [xmin, ymin],
                ] {
                    out.push(bright(p));
                }
            }
            crate::editor::DragState::Lasso { points } => {
                if points.len() >= 2 {
                    for w in points.windows(2) {
                        out.push(bright([w[0].x, w[0].y]));
                        out.push(bright([w[1].x, w[1].y]));
                    }
                    let last = points[points.len() - 1];
                    let first = points[0];
                    out.push(bright([last.x, last.y]));
                    out.push(bright([first.x, first.y]));
                }
            }
            _ => {}
        }
        // Armed-device ghost: a small cross per device bead at the cursor,
        // turned by the accumulated ghost angle. Beads-only (matches thumbnails).
        if let (Some(dev), Some(cursor)) = (self.armed_device.as_ref(), self.cursor_world()) {
            let (s, c) = self.ghost_angle.sin_cos();
            let r = 0.15; // half-length of each cross arm, world units
            for b in &dev.beads {
                let gx = b.pos[0] * c - b.pos[1] * s + cursor.x;
                let gy = b.pos[0] * s + b.pos[1] * c + cursor.y;
                out.push(OverlayVertex { pos: [gx - r, gy], shade: 0.8 });
                out.push(OverlayVertex { pos: [gx + r, gy], shade: 0.8 });
                out.push(OverlayVertex { pos: [gx, gy - r], shade: 0.8 });
                out.push(OverlayVertex { pos: [gx, gy + r], shade: 0.8 });
            }
        }
        out
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
                self.armed_device = None;
                if let Some(scene) = self.scene.as_ref() {
                    self.pre_run_snapshot = Some(scene.capture_payload());
                }
                if self.scene.is_some() {
                    self.rebuild_sim_from_scene();
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
            self.camera = crate::camera::Camera::fit(world_size);
            renderer.update_camera(&self.camera, world_size, &palette, sim.positions.len() as u32, self.render_mode);

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
            install_window_get_chemistry_name();
            install_window_set_chemistry();
            install_window_get_tool();
            install_window_set_tool();
            install_window_selection_count();
            install_window_clear();
            install_window_revert();
            install_window_can_revert();
            install_window_get_zoom();
            install_window_get_center_x();
            install_window_get_center_y();
            install_window_grid_alpha();
            install_window_load_library();
            install_window_get_library_json();
            install_window_get_library_rev();
            install_window_armed_id();
            install_window_get_suite_names();
            install_window_get_dock();
            install_window_save_to_dock();
            install_window_rename_device();
            install_window_remove_device();
            install_window_arm_device();
            install_window_disarm();
            install_window_save_suite();
            install_window_load_suite();
            install_window_import_suite();
            install_window_get_render_mode();
            install_window_set_render_mode();

            self.camera = crate::camera::Camera::fit(world_size);
            let camera = self.camera;
            let proxy = self.proxy.clone().expect("proxy not set before resumed()");
            let window_clone = window.clone();
            let n = sim.positions.len();
            let render_mode = self.render_mode;
            wasm_bindgen_futures::spawn_local(async move {
                let mut renderer = Renderer::new(window_clone, n).await
                    .expect("create renderer");
                renderer.update_camera(&camera, world_size, &palette, n as u32, render_mode);
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
                        renderer.update_camera(&self.camera, sim.world_size(), &sim.palette(), sim.positions.len() as u32, self.render_mode);
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
            WindowEvent::Focused(false) => {
                // Focus loss swallows key/button releases, so clear held-modifier
                // and in-progress pan state to avoid a stuck pan mode.
                self.space_held = false;
                self.shift_held = false;
                self.pan_button = None;
            }
            WindowEvent::Resized(size) => {
                if let Some(renderer) = self.renderer.as_mut() {
                    renderer.resize(size);
                }
                if let (Some(renderer), Some(sim)) = (self.renderer.as_mut(), self.sim.as_mut()) {
                    let bead_count = sim.positions.len() as u32;
                    let render_mode = self.render_mode;
                    renderer.update_camera(&self.camera, sim.world_size(), &sim.palette(), bead_count, render_mode);
                } else if let (Some(renderer), Some(scene)) = (self.renderer.as_mut(), self.scene.as_ref()) {
                    let palette: Vec<[f32; 3]> = scene.chemistry.colors.clone();
                    let bead_count = scene.beads.len() as u32;
                    let render_mode = self.render_mode;
                    renderer.update_camera(&self.camera, scene.world_size, &palette, bead_count, render_mode);
                }
            }
            WindowEvent::RedrawRequested => {
                // Clone the Arc so we can call request_redraw() at the end
                // without holding a borrow of self.window across the whole arm.
                let Some(window_arc) = self.window.clone() else { return };
                // Advance the adaptive-grid fade from this frame's dt (last_frame
                // is reset at the end of this arm). Runs in Run and Edit.
                let grid_dt = self.last_frame.elapsed().as_secs_f32();
                self.idle_since += grid_dt;
                self.grid_alpha = grid_fade_step(self.grid_alpha, self.idle_since, grid_dt);
                #[cfg(target_arch = "wasm32")]
                {
                    let (new_mode, edit_state, new_chemistry, new_tool, clear_scene, revert) = web_bridge::COMMANDS.with(|c| {
                        let mut cmds = c.borrow_mut();
                        let clr = std::mem::replace(&mut cmds.clear, false);
                        let rev = std::mem::replace(&mut cmds.revert, false);
                        (cmds.set_mode.take(), cmds.set_edit_state.take(), cmds.set_chemistry.take(), cmds.set_tool.take(), clr, rev)
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
                            self.pre_run_snapshot = None;
                            self.mode = crate::editor::Mode::Edit;
                            self.drag = crate::editor::DragState::None;
                            self.mouse_down = false;
                            self.armed_device = None;
                            if let Some(scene) = self.scene.as_ref() {
                                self.camera = crate::camera::Camera::fit(scene.world_size);
                            }
                            if let (Some(renderer), Some(scene)) = (self.renderer.as_mut(), self.scene.as_ref()) {
                                let palette: Vec<[f32; 3]> = scene.chemistry.colors.clone();
                                let bead_count = scene.beads.len() as u32;
                                let render_mode = self.render_mode;
                                renderer.update_camera(&self.camera, scene.world_size, &palette, bead_count, render_mode);
                            }
                        } else {
                            log::warn!("set_chemistry: unknown chemistry {:?}", name);
                        }
                    }
                    if let Some(tool) = new_tool {
                        if let Some(scene) = self.scene.as_mut() {
                            scene.tool = tool;
                        }
                    }
                    if clear_scene {
                        if let Some(scene) = self.scene.as_mut() {
                            scene.clear();
                        }
                        self.sim = None;
                        self.pre_run_snapshot = None;
                        self.mode = crate::editor::Mode::Edit;
                        self.drag = crate::editor::DragState::None;
                        self.mouse_down = false;
                        self.armed_device = None;
                    }
                    if revert {
                        self.revert_to_snapshot();
                    }
                    let (load_library, save_to_dock, rename_device, remove_device,
                         arm_device, disarm, save_suite, load_suite, import_suite,
                         set_render_mode) =
                        web_bridge::COMMANDS.with(|c| {
                            let mut cmds = c.borrow_mut();
                            let dis = std::mem::replace(&mut cmds.disarm, false);
                            (cmds.load_library.take(), cmds.save_to_dock.take(),
                             cmds.rename_device.take(), cmds.remove_device.take(),
                             cmds.arm_device.take(), dis, cmds.save_suite.take(),
                             cmds.load_suite.take(), cmds.import_suite.take(),
                             cmds.set_render_mode.take())
                        });
                    let mut lib_changed = false;
                    if let Some(json) = load_library {
                        self.library = crate::library::Library::load_or_default(&json);
                        lib_changed = true;
                    }
                    if let Some(name) = save_to_dock {
                        if let Some(scene) = self.scene.as_ref() {
                            if let Some(dev) = scene.extract_device(name) {
                                self.library.add_to_dock(dev);
                                lib_changed = true;
                            }
                        }
                    }
                    if let Some((id, name)) = rename_device {
                        self.library.rename_device(id, name);
                        lib_changed = true;
                    }
                    if let Some(id) = remove_device {
                        self.library.remove_device(id);
                        lib_changed = true;
                    }
                    if let Some(name) = save_suite {
                        let chem = self.scene.as_ref().map(|s| s.chemistry_name.clone()).unwrap_or_default();
                        self.library.save_suite(name, &chem);
                        lib_changed = true;
                    }
                    if let Some(name) = load_suite {
                        self.library.load_suite(&name);
                        lib_changed = true;
                    }
                    if let Some(json) = import_suite {
                        match serde_json::from_str::<crate::library::Suite>(&json) {
                            Ok(suite) => { self.library.import_suite(suite); lib_changed = true; }
                            Err(e) => log::warn!("importSuite: parse error: {e}"),
                        }
                    }
                    if let Some(id) = arm_device {
                        let dev = self.scene.as_ref().and_then(|scene| {
                            self.library.dock.iter()
                                .find(|d| d.id == id)
                                .filter(|d| d.is_compatible_with(&scene.chemistry))
                                .cloned()
                        });
                        if let Some(dev) = dev {
                            self.armed_device = Some(dev);
                            self.ghost_angle = 0.0;
                        }
                    }
                    if disarm { self.armed_device = None; }
                    if let Some(name) = set_render_mode {
                        if let Ok(mode) = serde_json::from_str::<crate::render_mode::RenderMode>(
                            &format!("\"{}\"", name)
                        ) {
                            self.render_mode = mode;
                            if let Some(renderer) = self.renderer.as_mut() {
                                renderer.set_mode(mode);
                            }
                        } else {
                            log::warn!("set_render_mode: unknown mode {:?}", name);
                        }
                    }
                    if lib_changed { self.library_rev = self.library_rev.wrapping_add(1); }
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
                        let bonds_vec: Vec<crate::bond::BondPair> = sim.bonds().iter().cloned().collect();
                        let comp_ids = crate::component::compute_component_ids(sim.positions.len(), &bonds_vec);
                        renderer.update_beads(&sim.positions, &sim.velocities, &sim.states, &selected, &comp_ids);
                        let world_size = sim.world_size();
                        let palette = sim.palette();
                        let bead_count = sim.positions.len() as u32;
                        let camera = self.camera;
                        let render_mode = self.render_mode;
                        renderer.set_mode(render_mode);
                        renderer.update_camera(&camera, world_size, &palette, bead_count, render_mode);
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
                        let velocities: Vec<glam::Vec2> = vec![glam::Vec2::ZERO; positions.len()];
                        let bonds_vec: Vec<crate::bond::BondPair> = scene.bonds.iter().cloned().collect();
                        let comp_ids = crate::component::compute_component_ids(positions.len(), &bonds_vec);
                        let world_size = scene.world_size;
                        let palette: Vec<[f32; 3]> = scene.chemistry.colors.clone();
                        let bead_count = positions.len() as u32;
                        renderer.update_beads(&positions, &velocities, &states, &selected, &comp_ids);
                        let camera = self.camera;
                        let render_mode = self.render_mode;
                        renderer.set_mode(render_mode);
                        renderer.update_camera(&camera, world_size, &palette, bead_count, render_mode);
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
                    let tool_str = self.scene.as_ref().map(|s| s.tool.as_str()).unwrap_or("place");
                    let selection_count = self.scene.as_ref().map(|s| s.selection.len() as u32).unwrap_or(0);
                    let chemistry_name = self.scene.as_ref().map(|s| s.chemistry_name.clone()).unwrap_or_default();
                    let can_revert = self.pre_run_snapshot.is_some();
                    let library_json = self.library.to_json();
                    let library_rev = self.library_rev;
                    let armed_id = self.armed_device.as_ref().map(|d| d.id as i32).unwrap_or(-1);
                    let dock: Vec<web_bridge::DockEntry> = match self.scene.as_ref() {
                        Some(scene) => self.library.dock.iter().map(|d| {
                            let beads = d.beads.iter().map(|b| {
                                let color = scene.chemistry.state_index(&b.state)
                                    .map(|si| scene.chemistry.colors[si])
                                    .unwrap_or([0.5, 0.5, 0.5]);
                                (b.pos, color)
                            }).collect();
                            web_bridge::DockEntry {
                                id: d.id,
                                name: d.name.clone(),
                                chemistry: d.chemistry.clone(),
                                beads,
                                compatible: d.is_compatible_with(&scene.chemistry),
                            }
                        }).collect(),
                        None => Vec::new(),
                    };
                    let suite_names: Vec<String> = match self.scene.as_ref() {
                        Some(scene) => self.library.suites.iter()
                            .filter(|s| s.chemistry == scene.chemistry_name)
                            .map(|s| s.name.clone())
                            .collect(),
                        None => Vec::new(),
                    };
                    let render_mode_kebab = self.render_mode.label_kebab();
                    web_bridge::SNAPSHOT.with(|s| {
                        *s.borrow_mut() = web_bridge::Snapshot {
                            mode: mode_str,
                            bead_count,
                            palette,
                            tool: tool_str,
                            selection_count,
                            chemistry_name,
                            can_revert,
                            zoom: self.camera.zoom,
                            center_x: self.camera.center.x,
                            center_y: self.camera.center.y,
                            grid_alpha: self.grid_alpha,
                            library_json,
                            library_rev,
                            armed_id,
                            dock,
                            suite_names,
                            render_mode: render_mode_kebab,
                        };
                    });
                }
                FRAME_COUNT.fetch_add(1, Ordering::Relaxed);
                window_arc.request_redraw();
                self.last_frame = Instant::now();
            }
            WindowEvent::CursorMoved { position, .. } => {
                let prev = self.cursor;
                self.cursor = position;
                if self.pan_button.is_some() {
                    let dx = (position.x - prev.x) as f32;
                    let dy = (position.y - prev.y) as f32;
                    if let Some(viewport) = self.viewport() {
                        let ws = self.world_size();
                        self.camera.pan_by((dx, dy), viewport, ws);
                        self.refresh_camera();
                        self.note_camera_activity(); // pan also fades the grid in; drop this line to make it zoom-only
                    }
                } else {
                    self.on_mouse_move();
                }
            }
            WindowEvent::MouseInput { state, button, .. } => {
                use winit::event::{ElementState, MouseButton};
                match (button, state) {
                    (MouseButton::Middle, ElementState::Pressed) => {
                        // Cancel any in-progress left-drag before switching to pan,
                        // so it doesn't resume (and corrupt the scene) after the pan ends.
                        self.drag = crate::editor::DragState::None;
                        self.mouse_down = false;
                        self.pan_button = Some(MouseButton::Middle);
                    }
                    (MouseButton::Middle, ElementState::Released) => {
                        if self.pan_button == Some(MouseButton::Middle) {
                            self.pan_button = None;
                        }
                    }
                    (MouseButton::Left, ElementState::Pressed) => {
                        if self.pan_button.is_some() {
                            // A pan (e.g. middle held) already owns the gesture —
                            // ignore the left press so it can't start a latent edit.
                        } else if self.space_held {
                            self.pan_button = Some(MouseButton::Left);
                        } else {
                            self.on_mouse_down();
                        }
                    }
                    (MouseButton::Left, ElementState::Released) => {
                        if self.pan_button == Some(MouseButton::Left) {
                            self.pan_button = None; // end the space+left pan
                        } else if self.pan_button.is_some() {
                            // A middle-button pan owns the gesture; a stray left
                            // release must not end it, and no edit drag is pending.
                        } else {
                            self.on_mouse_up();
                        }
                    }
                    _ => {}
                }
            }
            WindowEvent::MouseWheel { delta, .. } => {
                use winit::event::MouseScrollDelta;
                let scroll = match delta {
                    MouseScrollDelta::LineDelta(_, y) => y,
                    MouseScrollDelta::PixelDelta(p) => (p.y as f32) / 50.0,
                };
                if scroll != 0.0 {
                    if self.shift_held {
                        // Rotate the ghost/selection; plain scroll stays zoom so
                        // you can zoom for precise placement while a ghost is up.
                        self.apply_rotation(scroll.signum() * Self::ROTATE_SNAP_RAD);
                    } else if let Some(viewport) = self.viewport() {
                        let ws = self.world_size();
                        let factor = crate::camera::ZOOM_STEP.powf(scroll);
                        self.camera.zoom_at((self.cursor.x, self.cursor.y), factor, viewport, ws);
                        self.refresh_camera();
                        self.note_camera_activity();
                    }
                }
            }
            WindowEvent::KeyboardInput { event: key_event, .. } => {
                use winit::event::ElementState;
                use winit::keyboard::{Key, NamedKey};
                let pressed = key_event.state == ElementState::Pressed;
                if matches!(key_event.logical_key, Key::Named(NamedKey::Space)) {
                    self.space_held = pressed;
                }
                if matches!(key_event.logical_key, Key::Named(NamedKey::Shift)) {
                    self.shift_held = pressed;
                }
                if pressed {
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
                    if matches!(&key_event.logical_key, Key::Character(c) if c.as_str() == "0") {
                        self.camera.reset(self.world_size());
                        self.refresh_camera();
                    }
                    if matches!(key_event.logical_key, Key::Named(NamedKey::Escape)) {
                        self.armed_device = None;
                    }
                    if let Key::Character(ch) = &key_event.logical_key {
                        match ch.as_str() {
                            "[" => self.apply_rotation(-Self::ROTATE_SNAP_RAD),
                            "]" => self.apply_rotation(Self::ROTATE_SNAP_RAD),
                            s if s.eq_ignore_ascii_case("r") => {
                                let forward = !self.shift_held;
                                let new_mode = self.render_mode.cycle(forward);
                                self.render_mode = new_mode;
                                if let Some(renderer) = self.renderer.as_mut() {
                                    renderer.set_mode(new_mode);
                                }

                                #[cfg(target_arch = "wasm32")]
                                {
                                    use wasm_bindgen::JsValue;
                                    if let Some(w) = web_sys::window() {
                                        let detail = JsValue::from_str(new_mode.label_kebab());
                                        let init = web_sys::CustomEventInit::new();
                                        init.set_detail(&detail);
                                        if let Ok(ev) = web_sys::CustomEvent::new_with_event_init_dict(
                                            "jigglefab:render-mode-changed",
                                            &init,
                                        ) {
                                            let _ = w.dispatch_event(&ev);
                                        }
                                    }
                                }
                            }
                            _ => {}
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

#[cfg(test)]
mod fade_tests {
    use super::{grid_fade_step, GRID_FADE_IN_S, GRID_HOLD_S};

    #[test]
    fn fades_in_while_active() {
        let a = grid_fade_step(0.0, 0.0, GRID_FADE_IN_S);
        assert!((a - (1.0 - (-1.0f32).exp())).abs() < 1e-3, "got {a}");
        let mut alpha = a;
        for _ in 0..30 { alpha = grid_fade_step(alpha, 0.0, 0.016); }
        assert!(alpha > 0.95, "should be nearly full: {alpha}");
    }

    #[test]
    fn fades_out_when_idle() {
        let mut alpha = 1.0;
        for _ in 0..120 { alpha = grid_fade_step(alpha, GRID_HOLD_S + 1.0, 0.016); }
        assert!(alpha < 0.05, "should fade out: {alpha}");
    }

    #[test]
    fn large_dt_does_not_overshoot() {
        let a = grid_fade_step(0.0, 0.0, 100.0);
        assert!(a <= 1.0 + 1e-6 && a > 0.99, "no overshoot, near target: {a}");
        let b = grid_fade_step(1.0, GRID_HOLD_S + 1.0, 100.0);
        assert!(b >= -1e-6 && b < 0.01, "no overshoot below 0: {b}");
    }
}
