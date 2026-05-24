use jigglefab::ccd::RADIUS;
use jigglefab::chemistry::{compile_chemistry, load_chemistry};
use jigglefab::fab::load_fab;
use jigglefab::parallel::CpuParallel;
use jigglefab::scheduler::Scheduler;
use jigglefab::sim::Sim;

// Minimum distance across all *non-bonded* pairs. Bonded pairs sit inside R
// by design, so excluding them isolates the "free-pair overlap" invariant.
fn min_free_pair_distance(sim: &Sim) -> f32 {
    let mut min_d = f32::INFINITY;
    let bonds = sim.bonds();
    let n = sim.positions.len();
    for i in 0..n {
        for j in (i + 1)..n {
            let key = (i as u32, j as u32);
            if bonds.contains(&key) {
                continue;
            }
            let d = (sim.positions[i] - sim.positions[j]).length();
            if d < min_d {
                min_d = d;
            }
        }
    }
    min_d
}

#[test]
fn no_free_pair_overlap_grey_30() {
    let fab = load_fab("fabs/grey-30.toml").unwrap();
    let chem = load_chemistry("chemistries/grey.toml").unwrap();
    let mut sim = Sim::from_fab(&fab, chem);
    let compiled = compile_chemistry(sim.chemistry()).unwrap();
    let mut sched = CpuParallel::new(&sim, compiled);
    let eps = 0.05;
    for f in 0..600 {
        sched.step(&mut sim, 1.0 / 60.0);
        let min_d = min_free_pair_distance(&sim);
        assert!(
            min_d > RADIUS - eps,
            "frame {}: min free-pair distance {} < R - eps ({})",
            f,
            min_d,
            RADIUS - eps,
        );
    }
}

#[test]
fn wire_signal_count_conserved() {
    let fab = load_fab("fabs/wire-30.toml").unwrap();
    let chem = load_chemistry("chemistries/wire.toml").unwrap();
    let mut sim = Sim::from_fab(&fab, chem);
    let initial_on_count = sim.states.iter().filter(|&&s| s == 1).count();
    let compiled = compile_chemistry(sim.chemistry()).unwrap();
    let mut sched = CpuParallel::new(&sim, compiled);
    for f in 0..600 {
        sched.step(&mut sim, 1.0 / 60.0);
        let on_count = sim.states.iter().filter(|&&s| s == 1).count();
        assert_eq!(
            on_count, initial_on_count,
            "frame {}: wire signal count drifted ({} → {})",
            f, initial_on_count, on_count
        );
    }
}

#[test]
fn bonds_stay_within_radius_plus_eps_grey_30() {
    let fab = load_fab("fabs/grey-30.toml").unwrap();
    let chem = load_chemistry("chemistries/grey.toml").unwrap();
    let mut sim = Sim::from_fab(&fab, chem);
    let compiled = compile_chemistry(sim.chemistry()).unwrap();
    let mut sched = CpuParallel::new(&sim, compiled);
    let eps = 0.01;
    for f in 0..600 {
        sched.step(&mut sim, 1.0 / 60.0);
        for &(a, b) in sim.bonds() {
            let d = (sim.positions[a as usize] - sim.positions[b as usize]).length();
            assert!(
                d <= RADIUS + eps,
                "frame {}: bond ({},{}) length {} > R + eps",
                f,
                a,
                b,
                d
            );
        }
    }
}
