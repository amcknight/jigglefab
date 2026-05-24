// Writes fabs/chains_30x300.toml: 30 parallel vertical chains, 300 beads each,
// for the parallel-CCD smoke test. Bead spacing 0.95 (just inside RADIUS=1.0)
// so consecutive beads form bonds. Run via:
//   cargo run --release --bin gen_chains
use std::fmt::Write;

fn main() {
    let n_chains = 30usize;
    let beads_per_chain = 300usize;
    let chain_spacing = 4.0f32;
    let bead_spacing = 0.95f32;
    let world_x = n_chains as f32 * chain_spacing + 4.0;
    let world_y = beads_per_chain as f32 * bead_spacing + 4.0;
    let world_size = world_x.max(world_y);
    let mut out = String::new();
    writeln!(
        &mut out,
        "[meta]\nname = \"chains_30x300\"\nchemistry = \"grey\"\nseed = 12345\nworld_size = {}\n",
        world_size
    )
    .unwrap();
    for c in 0..n_chains {
        let x = 2.0 + c as f32 * chain_spacing;
        for i in 0..beads_per_chain {
            let y = 2.0 + i as f32 * bead_spacing;
            writeln!(
                &mut out,
                "[[bead]]\nstate = \"grey\"\npos = [{:.4}, {:.4}]\n",
                x, y
            )
            .unwrap();
        }
    }
    std::fs::write("fabs/chains_30x300.toml", out).unwrap();
    println!(
        "wrote fabs/chains_30x300.toml ({} chains × {} beads, world {:.1})",
        n_chains, beads_per_chain, world_size
    );
}
