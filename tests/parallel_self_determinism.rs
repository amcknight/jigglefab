use jigglefab::chemistry::{compile_chemistry, load_chemistry};
use jigglefab::fab::load_fab;
use jigglefab::parallel::CpuParallel;
use jigglefab::scheduler::Scheduler;
use jigglefab::sim::Sim;

fn run_to_frame_60(fab_path: &str, chem_path: &str) -> Vec<f32> {
    let fab = load_fab(fab_path).unwrap();
    let chem = load_chemistry(chem_path).unwrap();
    let mut sim = Sim::from_fab(&fab, chem);
    let compiled = compile_chemistry(sim.chemistry()).unwrap();
    let mut sched = CpuParallel::new(&sim, compiled);
    for _ in 0..60 {
        sched.step(&mut sim, 1.0 / 60.0);
    }
    let mut out = Vec::with_capacity(sim.positions.len() * 4);
    for p in &sim.positions {
        out.push(p.x);
        out.push(p.y);
    }
    for v in &sim.velocities {
        out.push(v.x);
        out.push(v.y);
    }
    out
}

#[test]
fn cpu_parallel_is_self_deterministic_grey_30() {
    let a = run_to_frame_60("fabs/grey-30.toml", "chemistries/grey.toml");
    let b = run_to_frame_60("fabs/grey-30.toml", "chemistries/grey.toml");
    assert_eq!(
        a, b,
        "two runs of the same scenario must produce identical bit patterns"
    );
}

#[test]
fn cpu_parallel_is_self_deterministic_wire_30() {
    let a = run_to_frame_60("fabs/wire-30.toml", "chemistries/wire.toml");
    let b = run_to_frame_60("fabs/wire-30.toml", "chemistries/wire.toml");
    assert_eq!(a, b);
}
