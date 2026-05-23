use crate::sim::{Sim, StepMetrics};

pub trait Scheduler {
    fn step(&mut self, sim: &mut Sim, dt: f32) -> StepMetrics;
}

pub struct CpuSequential;

impl Scheduler for CpuSequential {
    fn step(&mut self, sim: &mut Sim, dt: f32) -> StepMetrics {
        sim.step(dt);
        sim.last_step_metrics()
    }
}
