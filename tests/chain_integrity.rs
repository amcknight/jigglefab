use jigglefab::ccd::RADIUS;
use jigglefab::chemistry::load_chemistry;
use jigglefab::fab::load_fab;
use jigglefab::sim::Sim;

// The bug this guards against: bonded pairs drift outward through the |d|=R
// boundary due to float precision, and the chain visibly disintegrates within
// a few seconds. With always-reflect grey chemistry, the set of bonded pairs
// must be invariant in time — the chain stays a chain.
#[test]
fn grey_30_chain_keeps_all_initial_bonds_for_30s() {
    run_chain_test("fabs/grey-30.toml", "chemistries/grey.toml", 1.0, 1800);
}

#[test]
fn grey_30_chain_keeps_all_initial_bonds_at_3x_speed_for_30s() {
    run_chain_test("fabs/grey-30.toml", "chemistries/grey.toml", 3.0, 1800);
}

#[test]
fn grey_30_chain_keeps_all_initial_bonds_at_3x_speed_for_100s() {
    run_chain_test("fabs/grey-30.toml", "chemistries/grey.toml", 3.0, 6000);
}

// Wire shares the bond-preservation invariant — every contact reflects, just
// like grey — so the chain should hold equally well under it. This catches
// any regression in the ReflectSwap path that breaks the velocity reflect.
#[test]
fn wire_30_chain_keeps_all_initial_bonds_for_30s() {
    run_chain_test("fabs/wire-30.toml", "chemistries/wire.toml", 1.0, 1800);
}

#[test]
fn wire_100_chain_keeps_all_initial_bonds_for_30s() {
    run_chain_test("fabs/wire-100.toml", "chemistries/wire.toml", 1.0, 1800);
}

fn run_chain_test(fab_path: &str, chem_path: &str, speed_scale: f32, frames: usize) {
    let fab = load_fab(fab_path).unwrap();
    let chem = load_chemistry(chem_path).unwrap();
    let mut sim = Sim::from_fab(&fab, chem);
    let n = sim.positions.len();
    let world_size = sim.world_size();

    // Normalize then set to absolute target speed — independent of the SPEED
    // const so we can dial 1x and 3x explicitly.
    for v in &mut sim.velocities {
        *v = v.normalize() * speed_scale;
    }

    // Confirm the assumed initial topology: only adjacent (i, i+1) pairs are bonded.
    for i in 0..n {
        for j in (i + 1)..n {
            let d = min_image_dist(sim.positions[i], sim.positions[j], world_size);
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
            let d = min_image_dist(sim.positions[i], sim.positions[i + 1], world_size);
            assert!(
                d <= RADIUS + 1e-3,
                "speed_scale={speed_scale}: bond between {i} and {} broke at frame {f}: |d|={d}",
                i + 1,
            );
        }
    }
}

fn min_image_dist(a: glam::Vec2, b: glam::Vec2, world_size: f32) -> f32 {
    let half = world_size * 0.5;
    let mut d = b - a;
    if d.x > half { d.x -= world_size; }
    if d.x < -half { d.x += world_size; }
    if d.y > half { d.y -= world_size; }
    if d.y < -half { d.y += world_size; }
    d.length()
}
