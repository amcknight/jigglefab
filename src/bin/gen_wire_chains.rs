// Writes fabs/wire-NxM.toml: N parallel vertical wire chains, M beads each,
// one "on" signal at the top of each chain. Used to scale up the web demo
// now that CpuParallel can handle several thousand beads.
//
// Run via: cargo run --release --bin gen_wire_chains -- <chains> <beads_per_chain>
use std::env;
use std::fmt::Write;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    let n_chains: usize = args.get(0).and_then(|s| s.parse().ok()).unwrap_or(20);
    let beads_per_chain: usize = args.get(1).and_then(|s| s.parse().ok()).unwrap_or(30);

    // 3.0 chain spacing keeps chains visually distinct (RADIUS = 1 → chains
    // still have ~1 unit gap when laid out) while packing tightly enough that
    // the world stays compact on small (phone) screens. Wire's outside=pass
    // means chains passing through each other is fine.
    let bead_spacing: f32 = 0.667;
    let chain_x_spacing: f32 = 3.0;
    let margin: f32 = 3.0;
    let world_w = n_chains as f32 * chain_x_spacing + 2.0 * margin;
    let world_h = (beads_per_chain as f32 - 1.0) * bead_spacing + 2.0 * 5.0;
    let world_size = world_w.max(world_h);

    let mut out = String::new();
    writeln!(
        &mut out,
        "[meta]\nname = \"{} parallel {}-bead wire chains\"\nchemistry = \"wire\"\nseed = 42\nworld_size = {:.1}\n",
        n_chains, beads_per_chain, world_size,
    )
    .unwrap();
    for c in 0..n_chains {
        let x = margin + c as f32 * chain_x_spacing;
        for i in 0..beads_per_chain {
            let y = 5.0 + i as f32 * bead_spacing;
            let state = if i == 0 { "on" } else { "off" };
            writeln!(
                &mut out,
                "[[bead]]\nstate = \"{}\"\npos = [{:.2}, {:.3}]\n",
                state, x, y,
            )
            .unwrap();
        }
    }
    let path = format!("fabs/wire-{}x{}.toml", n_chains, beads_per_chain);
    std::fs::write(&path, out).unwrap();
    println!(
        "wrote {} ({} chains × {} beads, world {:.1})",
        path, n_chains, beads_per_chain, world_size
    );
}
