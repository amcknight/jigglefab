//! GPU self-determinism test: run chains_10x30 twice with GpuEventLoop,
//! verify bit-identical positions and velocities.
//! Run with: cargo test --test gpu_determinism -- --ignored

#[cfg(not(target_arch = "wasm32"))]
mod tests {
    use jigglefab::{
        bench::{DisconnectedChains, Scenario},
        gpu::{context::GpuContext, scheduler::GpuEventLoop},
        scheduler::Scheduler,
    };

    fn run_frames(n_frames: u32) -> (Vec<glam::Vec2>, Vec<glam::Vec2>) {
        let scenario = DisconnectedChains { chain_count: 10, chain_len: 30, world_size: 50.0 };
        let (mut sim, _) = scenario.build();
        let ctx = GpuContext::new_headless().expect("GPU context");
        let mut sched = GpuEventLoop::new(ctx, &sim);
        let dt = 1.0f32 / 60.0;
        for _ in 0..n_frames {
            sched.step(&mut sim, dt);
        }
        (sim.positions.clone(), sim.velocities.clone())
    }

    #[test]
    #[ignore = "requires GPU"]
    fn gpu_self_deterministic_chains_10x30() {
        let (pos1, vel1) = run_frames(10);
        let (pos2, vel2) = run_frames(10);
        for (i, (a, b)) in pos1.iter().zip(pos2.iter()).enumerate() {
            assert_eq!(
                a.to_array(), b.to_array(),
                "position mismatch at bead {i}: {a:?} vs {b:?}"
            );
        }
        for (i, (a, b)) in vel1.iter().zip(vel2.iter()).enumerate() {
            assert_eq!(
                a.to_array(), b.to_array(),
                "velocity mismatch at bead {i}: {a:?} vs {b:?}"
            );
        }
    }
}
