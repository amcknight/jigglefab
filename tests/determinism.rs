use jigglefab::chemistry::load_chemistry;
use jigglefab::fab::load_fab;
use jigglefab::sim::Sim;

#[test]
fn same_seed_produces_same_state_after_n_frames() {
    let fab = load_fab("fabs/grey-30.toml").unwrap();
    let chem_a = load_chemistry("chemistries/grey.toml").unwrap();
    let chem_b = load_chemistry("chemistries/grey.toml").unwrap();

    let mut a = Sim::from_fab(&fab, chem_a);
    let mut b = Sim::from_fab(&fab, chem_b);

    let dt = 1.0 / 60.0;
    for _ in 0..600 { // 10 seconds of sim time
        a.step(dt);
        b.step(dt);
    }

    for i in 0..a.positions.len() {
        assert_eq!(a.positions[i].to_array(), b.positions[i].to_array(),
                   "position mismatch at bead {}", i);
        assert_eq!(a.velocities[i].to_array(), b.velocities[i].to_array(),
                   "velocity mismatch at bead {}", i);
    }
}
