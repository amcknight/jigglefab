use jigglefab::chemistry::{compile_chemistry, load_chemistry};
use jigglefab::fab::load_fab;
use jigglefab::parallel::CpuParallel;
use jigglefab::scheduler::Scheduler;
use jigglefab::sim::Sim;

#[test]
#[ignore = "slow — run with `cargo test --release chains_30x300_smoke -- --ignored`"]
fn chains_30x300_runs_30_frames_without_panic() {
    let fab = load_fab("fabs/chains_30x300.toml").unwrap();
    let chem = load_chemistry("chemistries/grey.toml").unwrap();
    let mut sim = Sim::from_fab(&fab, chem);
    let compiled = compile_chemistry(sim.chemistry()).unwrap();
    let mut sched = CpuParallel::new(&sim, compiled);
    for _ in 0..30 {
        sched.step(&mut sim, 1.0 / 60.0);
    }
    for (i, p) in sim.positions.iter().enumerate() {
        assert!(p.is_finite(), "bead {} position is non-finite: {:?}", i, p);
    }
}
