// Central registry of scheduler backends. Bench, the native app, and
// (eventually) the web URL-hash all parse a backend name through this
// module so adding a new backend only touches one file.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SchedulerKind {
    CpuSequential,
    CpuParallel,
    CpuParallelMt,
    #[cfg(not(target_arch = "wasm32"))]
    GpuEventLoop,
}

impl SchedulerKind {
    pub fn parse(s: &str) -> Option<Self> {
        Some(match s {
            "cpu" | "cpu-sequential" => SchedulerKind::CpuSequential,
            "cpu-parallel" => SchedulerKind::CpuParallel,
            "cpu-parallel-mt" | "cpu-mt" => SchedulerKind::CpuParallelMt,
            #[cfg(not(target_arch = "wasm32"))]
            "gpu" | "gpu-event-loop" => SchedulerKind::GpuEventLoop,
            _ => return None,
        })
    }

    pub fn name(self) -> &'static str {
        match self {
            SchedulerKind::CpuSequential => "cpu",
            SchedulerKind::CpuParallel => "cpu-parallel",
            SchedulerKind::CpuParallelMt => "cpu-parallel-mt",
            #[cfg(not(target_arch = "wasm32"))]
            SchedulerKind::GpuEventLoop => "gpu",
        }
    }
}

use crate::scheduler::{CpuSequential, Scheduler};
use crate::sim::Sim;

#[derive(Debug)]
pub enum BuildError {
    /// Variant requested isn't usable in the current cfg / runtime context.
    Unsupported(&'static str),
    /// Chemistry compilation failed.
    ChemistryCompile(anyhow::Error),
}

impl std::fmt::Display for BuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuildError::Unsupported(why) => write!(f, "scheduler unsupported here: {why}"),
            BuildError::ChemistryCompile(e) => write!(f, "compile_chemistry failed: {e}"),
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub fn build(
    kind: SchedulerKind,
    sim: &Sim,
    gpu_ctx: Option<crate::gpu::context::GpuContext>,
) -> Result<Box<dyn Scheduler>, BuildError> {
    match kind {
        SchedulerKind::CpuSequential => Ok(Box::new(CpuSequential)),
        SchedulerKind::CpuParallel => {
            let chem = crate::chemistry::compile_chemistry(sim.chemistry())
                .map_err(BuildError::ChemistryCompile)?;
            Ok(Box::new(crate::parallel::CpuParallel::new(sim, chem)))
        }
        SchedulerKind::CpuParallelMt => {
            let chem = crate::chemistry::compile_chemistry(sim.chemistry())
                .map_err(BuildError::ChemistryCompile)?;
            Ok(Box::new(crate::parallel::CpuParallelMt::new(sim, chem)))
        }
        SchedulerKind::GpuEventLoop => {
            let ctx = gpu_ctx.ok_or(BuildError::Unsupported("no GpuContext supplied"))?;
            Ok(Box::new(crate::gpu::scheduler::GpuEventLoop::new(ctx, sim)))
        }
    }
}

#[cfg(target_arch = "wasm32")]
pub fn build(kind: SchedulerKind, sim: &Sim) -> Result<Box<dyn Scheduler>, BuildError> {
    match kind {
        SchedulerKind::CpuSequential => Ok(Box::new(CpuSequential)),
        SchedulerKind::CpuParallel => {
            let chem = crate::chemistry::compile_chemistry(sim.chemistry())
                .map_err(BuildError::ChemistryCompile)?;
            Ok(Box::new(crate::parallel::CpuParallel::new(sim, chem)))
        }
        SchedulerKind::CpuParallelMt => Err(BuildError::Unsupported(
            "CpuParallelMt requires native threads; wasm is single-threaded",
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_known_names() {
        assert_eq!(SchedulerKind::parse("cpu"), Some(SchedulerKind::CpuSequential));
        assert_eq!(SchedulerKind::parse("cpu-parallel"), Some(SchedulerKind::CpuParallel));
        assert_eq!(
            SchedulerKind::parse("cpu-parallel-mt"),
            Some(SchedulerKind::CpuParallelMt)
        );
        assert_eq!(SchedulerKind::parse("cpu-mt"), Some(SchedulerKind::CpuParallelMt));
    }

    #[test]
    fn parses_unknown_returns_none() {
        assert_eq!(SchedulerKind::parse("nope"), None);
        assert_eq!(SchedulerKind::parse(""), None);
    }

    #[test]
    fn name_round_trip() {
        for k in [
            SchedulerKind::CpuSequential,
            SchedulerKind::CpuParallel,
            SchedulerKind::CpuParallelMt,
        ] {
            assert_eq!(SchedulerKind::parse(k.name()), Some(k));
        }
    }
}
