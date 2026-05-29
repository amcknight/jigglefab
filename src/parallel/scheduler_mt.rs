use std::collections::HashSet;

use crate::bond::BondPair;
use crate::chemistry::{CompiledChemistry, Op, Tag};
use crate::grid::Grid;
use crate::parallel::substep::do_substep_mt;
use crate::parallel::{Bead, BeadPool, STACK_CAP};
use crate::scheduler::Scheduler;
use crate::sim::{Sim, StepMetrics};

pub const DEFAULT_DT_SUB: f32 = 1.0 / 240.0;

// Rayon-parallel cousin of CpuParallel. Identical state shape; differs
// only in that step() routes substeps through do_substep_mt instead of
// do_substep. Bit-identical to CpuParallel on grey/wire workloads — see
// tests/parallel_self_determinism.rs.
pub struct CpuParallelMt {
    pool: BeadPool,
    bonds: HashSet<BondPair>,
    grid: Grid,
    chem: CompiledChemistry,
    dt_sub: f32,
    sim_to_slot: Vec<u32>,
}

impl CpuParallelMt {
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
        let bonds = sim.bonds().clone();
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

impl Scheduler for CpuParallelMt {
    fn step(&mut self, sim: &mut Sim, frame_dt: f32) -> StepMetrics {
        let metrics = StepMetrics::default();
        let n_substeps = (frame_dt / self.dt_sub).ceil() as u32;
        for _ in 0..n_substeps {
            do_substep_mt(
                &mut self.pool,
                &mut self.grid,
                &self.chem,
                &mut self.bonds,
                self.dt_sub,
            );
        }
        for (i, &slot) in self.sim_to_slot.iter().enumerate() {
            let b = self.pool.get(slot);
            sim.positions[i] = b.pos;
            sim.velocities[i] = b.vel;
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
        let mut sched = CpuParallelMt::new(&sim, compiled);
        for _ in 0..30 {
            sched.step(&mut sim, 1.0 / 60.0);
        }
        for p in &sim.positions {
            assert!(p.x >= 0.0 && p.x <= sim.world_size());
            assert!(p.y >= 0.0 && p.y <= sim.world_size());
        }
    }
}
