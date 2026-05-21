#[derive(Clone, Debug)]
pub struct BenchArgs {
    pub substeps: u32,
    pub frames: u32,
    pub warmup_frames: u32,
    pub max_wall_seconds: f64,
    pub verify_determinism: bool,
}

impl Default for BenchArgs {
    fn default() -> Self {
        Self {
            substeps: 10,
            frames: 3000,
            warmup_frames: 60,
            max_wall_seconds: 300.0,
            verify_determinism: false,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Percentiles {
    pub mean: f64,
    pub p50: f64,
    pub p99: f64,
    pub max: f64,
}

impl Percentiles {
    /// Compute percentiles from a vector of samples. Sorts in-place.
    /// Panics on empty input — callers should guard.
    pub fn from_samples(samples: &mut [f64]) -> Self {
        assert!(!samples.is_empty(), "Percentiles::from_samples on empty");
        samples.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        let sum: f64 = samples.iter().sum();
        let mean = sum / samples.len() as f64;
        let p50 = samples[samples.len() / 2];
        let p99 = samples[((samples.len() as f64) * 0.99) as usize];
        let max = *samples.last().unwrap();
        Self { mean, p50, p99, max }
    }
}

#[derive(Clone, Debug)]
pub struct ScenarioResult {
    pub name: String,
    pub bead_count: u32,
    pub frames_completed: u32,
    pub frames_requested: u32,
    pub truncated: bool,
    pub frame_time_ms: Percentiles,
    pub substep_time_us: Percentiles,
    pub contacts_per_substep: Percentiles,
    pub candidate_pairs_per_substep_mean: f64,
    pub iter_cap_saturation_rate: f64,
    pub effective_fps: f64,
    pub substeps_per_16ms_budget: u32,
    pub bonds_preserved: bool,
    pub bonds_lost: usize,
    pub bonds_added: usize,
    pub determinism_verified: Option<bool>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn percentiles_basic() {
        let mut s = vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0];
        let p = Percentiles::from_samples(&mut s);
        assert!((p.mean - 5.5).abs() < 1e-6);
        assert_eq!(p.p50, 6.0); // samples[5]
        assert_eq!(p.p99, 10.0); // samples[9]
        assert_eq!(p.max, 10.0);
    }

    #[test]
    fn percentiles_single_sample() {
        let mut s = vec![42.0];
        let p = Percentiles::from_samples(&mut s);
        assert_eq!(p.mean, 42.0);
        assert_eq!(p.p50, 42.0);
        assert_eq!(p.p99, 42.0);
        assert_eq!(p.max, 42.0);
    }

    #[test]
    fn bench_args_default() {
        let a = BenchArgs::default();
        assert_eq!(a.substeps, 10);
        assert_eq!(a.frames, 3000);
        assert_eq!(a.warmup_frames, 60);
        assert_eq!(a.max_wall_seconds, 300.0);
        assert!(!a.verify_determinism);
    }
}
