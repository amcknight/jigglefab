// Writes fabs/wire-CxBxR.toml: a grid of R rows × C columns of vertical
// wire chains, B beads each, with one "on" signal at the top of every
// chain. When R==1 the filename and meta name drop the row dimension to
// match the older 1-D naming (wire-CxB).
//
// Run via: cargo run --release --bin gen_wire_chains -- <cols> <beads> [rows]
use std::env;
use std::fmt::Write;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    let cols: usize = args.get(0).and_then(|s| s.parse().ok()).unwrap_or(20);
    let beads_per_chain: usize = args.get(1).and_then(|s| s.parse().ok()).unwrap_or(30);
    let rows: usize = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(1);

    // 3.0 chain spacing keeps chains visually distinct (RADIUS = 1 → chains
    // still have ~1 unit gap when laid out) while packing tightly enough that
    // the world stays compact on small screens. Wire's outside=pass means
    // chains passing through each other is fine.
    let bead_spacing: f32 = 0.667;
    let chain_x_spacing: f32 = 3.0;
    let chain_height = (beads_per_chain as f32 - 1.0) * bead_spacing;
    // Vertical gap between rows must clear the bond constraint (~R=1) so
    // chains in successive rows don't interpenetrate at start of sim.
    let row_gap: f32 = 3.0;
    let row_pitch = chain_height + row_gap;
    let margin: f32 = 3.0;
    let world_w = cols as f32 * chain_x_spacing + 2.0 * margin;
    let world_h = rows as f32 * chain_height + (rows.saturating_sub(1)) as f32 * row_gap + 2.0 * margin;
    let world_size = world_w.max(world_h);

    let n_chains = cols * rows;
    let total_beads = n_chains * beads_per_chain;
    let mut out = String::new();
    writeln!(
        &mut out,
        "[meta]\nname = \"{} wire chains ({} rows × {} cols × {} beads)\"\nchemistry = \"wire\"\nseed = 42\nworld_size = {:.1}\n",
        n_chains, rows, cols, beads_per_chain, world_size,
    )
    .unwrap();
    for r in 0..rows {
        let y0 = margin + r as f32 * row_pitch;
        for c in 0..cols {
            let x = margin + c as f32 * chain_x_spacing;
            for i in 0..beads_per_chain {
                let y = y0 + i as f32 * bead_spacing;
                let state = if i == 0 { "on" } else { "off" };
                writeln!(
                    &mut out,
                    "[[bead]]\nstate = \"{}\"\npos = [{:.2}, {:.3}]\n",
                    state, x, y,
                )
                .unwrap();
            }
        }
    }
    let path = if rows == 1 {
        format!("fabs/wire-{}x{}.toml", cols, beads_per_chain)
    } else {
        format!("fabs/wire-{}x{}x{}.toml", cols, beads_per_chain, rows)
    };
    std::fs::write(&path, out).unwrap();
    println!(
        "wrote {} ({} rows × {} cols × {} beads = {} total, world {:.1})",
        path, rows, cols, beads_per_chain, total_beads, world_size
    );
}
