use std::collections::HashSet;

use crate::chemistry::{CompiledChemistry, Op, Tag};
use crate::grid::Grid;
use crate::parallel::substep::do_substep;
use crate::parallel::{Bead, BeadPool, STACK_CAP};
use crate::scheduler::Scheduler;
use crate::sim::{Sim, StepMetrics};

pub const DEFAULT_DT_SUB: f32 = 1.0 / 240.0;

pub struct CpuParallel {
    pool: BeadPool,
    bonds: HashSet<(u32, u32)>,
    grid: Grid,
    chem: CompiledChemistry,
    dt_sub: f32,
    // Map from Sim's bead index → pool slot. Initially identity; will diverge
    // after births/deaths, but prototype chems (grey, wire) never trigger
    // those so identity holds for Phase 1's test coverage.
    sim_to_slot: Vec<u32>,
}

impl CpuParallel {
    pub fn new(sim: &Sim, chem: CompiledChemistry) -> Self {
        let n = sim.positions.len();
        let pool_cap = (n.max(512) * 2) as u32;
        let mut pool = BeadPool::with_capacity(pool_cap);
        let mut sim_to_slot = Vec::with_capacity(n);
        for i in 0..n {
            let mut stack = [Op::nop(); STACK_CAP];
            stack[0] = Op::sig_legacy(sim.states[i]);
            let slot = pool.alloc(Bead {
                pos: sim.positions[i],
                vel: sim.velocities[i],
                tag: Tag::Wire,
                payload: sim.states[i],
                alive: true,
                born_this_substep: false,
                stack_len: 1,
                stack,
            });
            sim_to_slot.push(slot);
        }
        let bonds = sim_bonds_clone(sim);
        let grid = Grid::new(sim.world_size());
        Self {
            pool,
            bonds,
            grid,
            chem,
            dt_sub: DEFAULT_DT_SUB,
            sim_to_slot,
        }
    }
}

// Tiny helper: sim.bonds is pub(crate); within the crate this just clones it.
fn sim_bonds_clone(sim: &Sim) -> HashSet<(u32, u32)> {
    sim.bonds.clone()
}

impl Scheduler for CpuParallel {
    fn step(&mut self, sim: &mut Sim, frame_dt: f32) -> StepMetrics {
        let metrics = StepMetrics::default();
        let n_substeps = (frame_dt / self.dt_sub).ceil() as u32;
        for _ in 0..n_substeps {
            do_substep(
                &mut self.pool,
                &mut self.grid,
                &self.chem,
                &mut self.bonds,
                self.dt_sub,
            );
        }
        // Phase 1: prototype chems don't birth/die, so sim_to_slot stays
        // identity. Once Sem chems run through here we'll grow the sim vectors
        // (or rework the boundary) — flagged in the Phase 1 retro.
        for (i, &slot) in self.sim_to_slot.iter().enumerate() {
            let b = self.pool.get(slot);
            sim.positions[i] = b.pos;
            sim.velocities[i] = b.vel;
            // Decode the sig payload back to a legacy state index.
            sim.states[i] = u32::from(b.top_op()) & 0x0FFF_FFFF;
        }
        sim.bonds = self.bonds.clone();
        metrics
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chemistry::{compile_chemistry, load_chemistry};
    use crate::fab::load_fab;

    #[test]
    fn grey_30_steps_without_crash() {
        let fab = load_fab("fabs/grey-30.toml").unwrap();
        let chem = load_chemistry("chemistries/grey.toml").unwrap();
        let mut sim = Sim::from_fab(&fab, chem);
        let compiled = compile_chemistry(sim.chemistry()).unwrap();
        let mut sched = CpuParallel::new(&sim, compiled);
        for _ in 0..30 {
            sched.step(&mut sim, 1.0 / 60.0);
        }
        for p in &sim.positions {
            assert!(p.x >= 0.0 && p.x <= sim.world_size());
            assert!(p.y >= 0.0 && p.y <= sim.world_size());
        }
    }
}
