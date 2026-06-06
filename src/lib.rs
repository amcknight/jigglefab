pub mod camera;
pub mod fab;
pub mod bond;
pub mod chemistry;
pub mod rng;
pub mod ccd;
pub mod collide;
pub mod grid;
pub mod library;
pub mod sim;
pub mod render;
pub mod app;
pub mod editor;
// bench pulls in load_chemistry (fs-backed, non-wasm) via chains.rs and ships
// its own bin — neither builds for the web target.
#[cfg(not(target_arch = "wasm32"))]
pub mod bench;
pub mod scheduler;
pub mod gpu;
pub mod parallel;
pub mod scheduler_selector;
pub mod speed;
pub mod telemetry;

#[cfg(not(target_arch = "wasm32"))]
pub fn run() -> anyhow::Result<()> {
    env_logger::init();
    app::run()
}

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::wasm_bindgen;

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen(start)]
pub fn web_start() {
    console_error_panic_hook::set_once();
    let _ = console_log::init_with_level(log::Level::Info);
    if let Err(e) = app::run() {
        log::error!("app error: {e:?}");
    }
}
