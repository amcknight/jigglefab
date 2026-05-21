use winit::application::ApplicationHandler;
use winit::event::WindowEvent;
use winit::event_loop::{ActiveEventLoop, EventLoop};
use winit::window::{Window, WindowId};
use std::sync::Arc;
use std::time::Instant;

use crate::chemistry::load_chemistry;
use crate::fab::load_fab;
use crate::render::Renderer;
use crate::sim::{Sim, WORLD_SIZE};

const FRAME_DT: f32 = 1.0 / 60.0;

pub struct App {
    window: Option<Arc<Window>>,
    renderer: Option<Renderer>,
    sim: Option<Sim>,
    last_frame: Instant,
}

impl App {
    pub fn new() -> Self {
        Self { window: None, renderer: None, sim: None, last_frame: Instant::now() }
    }
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        let attrs = Window::default_attributes().with_title("JiggleFab P1");
        let window = Arc::new(event_loop.create_window(attrs).expect("create window"));

        let fab = load_fab("fabs/grey-30.toml").expect("load fab");
        let chem = load_chemistry("chemistries/grey.toml").expect("load chem");
        let sim = Sim::from_fab(&fab, chem);

        let mut renderer = pollster::block_on(Renderer::new(window.clone(), sim.positions.len()))
            .expect("create renderer");
        renderer.update_camera(WORLD_SIZE);

        self.window = Some(window);
        self.renderer = Some(renderer);
        self.sim = Some(sim);
        self.last_frame = Instant::now();
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, _id: WindowId, event: WindowEvent) {
        let Some(window) = &self.window else { return };
        let Some(renderer) = &mut self.renderer else { return };
        let Some(sim) = &mut self.sim else { return };
        match event {
            WindowEvent::CloseRequested => event_loop.exit(),
            WindowEvent::Resized(size) => {
                renderer.resize(size);
                renderer.update_camera(WORLD_SIZE);
            }
            WindowEvent::RedrawRequested => {
                // Use a fixed dt for deterministic stepping.
                sim.step(FRAME_DT);
                renderer.update_beads(&sim.positions);
                if let Err(e) = renderer.render(sim.positions.len()) {
                    log::warn!("render error: {e:?}");
                }
                window.request_redraw();
                self.last_frame = Instant::now();
            }
            _ => {}
        }
    }
}

pub fn run() -> anyhow::Result<()> {
    let event_loop = EventLoop::new()?;
    let mut app = App::new();
    event_loop.run_app(&mut app)?;
    Ok(())
}
