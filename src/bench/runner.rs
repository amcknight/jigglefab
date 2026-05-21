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

use web_time::Instant;

use super::scenario::{Scenario, geometric_bonds};

pub fn run_scenario(scenario: &dyn Scenario, args: &BenchArgs) -> ScenarioResult {
    let (mut sim, invariants) = scenario.build();
    let bead_count = sim.positions.len() as u32;
    let frame_dt = 1.0 / 60.0;

    // Warmup — discard timings.
    for _ in 0..args.warmup_frames {
        for _ in 0..args.substeps {
            sim.step(frame_dt);
        }
    }

    let run_start = Instant::now();
    let total_substeps_planned = (args.frames as usize) * (args.substeps as usize);
    let mut frame_times_ms = Vec::with_capacity(args.frames as usize);
    let mut substep_times_us = Vec::with_capacity(total_substeps_planned);
    let mut contacts_per_substep = Vec::with_capacity(total_substeps_planned);
    let mut candidate_pairs_total: u64 = 0;
    let mut iter_cap_hits: u64 = 0;
    let mut frames_completed: u32 = 0;
    let mut truncated = false;

    for _ in 0..args.frames {
        if run_start.elapsed().as_secs_f64() > args.max_wall_seconds {
            truncated = true;
            break;
        }
        let frame_start = Instant::now();
        for _ in 0..args.substeps {
            let substep_start = Instant::now();
            sim.step(frame_dt);
            let elapsed_us = substep_start.elapsed().as_micros() as f64;
            substep_times_us.push(elapsed_us);
            let m = sim.last_step_metrics();
            contacts_per_substep.push(m.contacts_resolved as f64);
            candidate_pairs_total = candidate_pairs_total.saturating_add(m.candidate_pairs as u64);
            if m.iter_cap_hit { iter_cap_hits += 1; }
        }
        frame_times_ms.push(frame_start.elapsed().as_secs_f64() * 1000.0);
        frames_completed += 1;
    }

    // Guard empty samples (truncated immediately).
    if frame_times_ms.is_empty() {
        frame_times_ms.push(0.0);
    }
    if substep_times_us.is_empty() {
        substep_times_us.push(0.0);
        contacts_per_substep.push(0.0);
    }

    let frame_time = Percentiles::from_samples(&mut frame_times_ms.clone());
    let substep_time = Percentiles::from_samples(&mut substep_times_us.clone());
    let contacts = Percentiles::from_samples(&mut contacts_per_substep.clone());
    let total_substeps = substep_times_us.len().max(1);
    let candidate_pairs_mean = candidate_pairs_total as f64 / total_substeps as f64;
    let iter_cap_saturation_rate = iter_cap_hits as f64 / total_substeps as f64;
    let effective_fps = if frame_time.mean > 0.0 { 1000.0 / frame_time.mean } else { 0.0 };
    let substeps_per_16ms_budget = if substep_time.mean > 0.0 {
        (16_600.0 / substep_time.mean).floor() as u32
    } else { 0 };

    let final_bonds = geometric_bonds(&sim.positions, sim.world_size());
    let bonds_preserved = final_bonds == invariants.initial_bond_set;
    let bonds_lost = invariants.initial_bond_set.difference(&final_bonds).count();
    let bonds_added = final_bonds.difference(&invariants.initial_bond_set).count();

    ScenarioResult {
        name: scenario.name(),
        bead_count,
        frames_completed,
        frames_requested: args.frames,
        truncated,
        frame_time_ms: frame_time,
        substep_time_us: substep_time,
        contacts_per_substep: contacts,
        candidate_pairs_per_substep_mean: candidate_pairs_mean,
        iter_cap_saturation_rate,
        effective_fps,
        substeps_per_16ms_budget,
        bonds_preserved,
        bonds_lost,
        bonds_added,
        determinism_verified: None,
    }
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

    use crate::bench::chains::DisconnectedChains;

    #[test]
    fn run_scenario_produces_result_with_correct_shape() {
        let scenario = DisconnectedChains { chain_count: 2, chain_len: 5, world_size: 30.0 };
        let args = BenchArgs {
            substeps: 2,
            frames: 10,
            warmup_frames: 1,
            max_wall_seconds: 60.0,
            verify_determinism: false,
        };
        let r = run_scenario(&scenario, &args);
        assert_eq!(r.name, "chains_2x5");
        assert_eq!(r.bead_count, 10);
        assert_eq!(r.frames_completed, 10);
        assert!(!r.truncated);
        assert!(r.bonds_preserved, "tiny chain scenario should preserve bonds");
        assert_eq!(r.bonds_lost, 0);
        assert_eq!(r.bonds_added, 0);
    }
}
