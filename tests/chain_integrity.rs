use jigglefab::ccd::RADIUS;
use jigglefab::chemistry::load_chemistry;
use jigglefab::fab::load_fab;
use jigglefab::sim::{Sim, WORLD_SIZE};

// The bug this guards against: bonded pairs drift outward through the |d|=R
// boundary due to float precision, and the chain visibly disintegrates within
// a few seconds. With always-reflect grey chemistry, the set of bonded pairs
// must be invariant in time — the chain stays a chain.
#[test]
fn grey_30_chain_keeps_all_initial_bonds_for_30s() {
    run_chain_test(1.0, 1800);
}

#[test]
fn grey_30_chain_keeps_all_initial_bonds_at_3x_speed_for_30s() {
    run_chain_test(3.0, 1800);
}

#[test]
fn grey_30_chain_keeps_all_initial_bonds_at_3x_speed_for_100s() {
    run_chain_test(3.0, 6000);
}

fn run_chain_test(speed_scale: f32, frames: usize) {
    let fab = load_fab("fabs/grey-30.toml").unwrap();
    let chem = load_chemistry("chemistries/grey.toml").unwrap();
    let mut sim = Sim::from_fab(&fab, chem);
    let n = sim.positions.len();

    // Normalize then set to absolute target speed — independent of the SPEED
    // const so we can dial 1x and 3x explicitly.
    for v in &mut sim.velocities {
        *v = v.normalize() * speed_scale;
    }

    // Confirm the assumed initial topology: only adjacent (i, i+1) pairs are bonded.
    for i in 0..n {
        for j in (i + 1)..n {
            let d = min_image_dist(sim.positions[i], sim.positions[j]);
            if j == i + 1 {
                assert!(d < RADIUS, "expected initial bond between {i} and {j}, |d|={d}");
            } else {
                assert!(d >= RADIUS, "unexpected initial bond between {i} and {j}, |d|={d}");
            }
        }
    }

    let dt = 1.0 / 60.0;
    for f in 0..frames {
        sim.step(dt);
        for i in 0..(n - 1) {
            let d = min_image_dist(sim.positions[i], sim.positions[i + 1]);
            assert!(
                d <= RADIUS + 1e-3,
                "speed_scale={speed_scale}: bond between {i} and {} broke at frame {f}: |d|={d}",
                i + 1,
            );
        }
    }
}

fn min_image_dist(a: glam::Vec2, b: glam::Vec2) -> f32 {
    let half = WORLD_SIZE * 0.5;
    let mut d = b - a;
    if d.x > half { d.x -= WORLD_SIZE; }
    if d.x < -half { d.x += WORLD_SIZE; }
    if d.y > half { d.y -= WORLD_SIZE; }
    if d.y < -half { d.y += WORLD_SIZE; }
    d.length()
}
