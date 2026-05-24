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
