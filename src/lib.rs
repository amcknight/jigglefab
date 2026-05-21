pub mod fab;
pub mod chemistry;
pub mod rng;
pub mod ccd;
pub mod collide;
pub mod grid;
pub mod sim;
pub mod render;
pub mod app;

pub fn run() -> anyhow::Result<()> {
    env_logger::init();
    app::run()
}
