//! Renders a fixed bead scene through the offscreen adapter and writes a PNG.
//! Used to (re-)generate golden images.
//!
//! Usage: cargo run --bin render_golden -- <mode-kebab> <out.png>

use anyhow::{Context, Result};
use glam::Vec2;
use jigglefab::bond::BondPair;
use jigglefab::component::compute_component_ids;
use jigglefab::render_mode::RenderMode;
use jigglefab::render_offscreen::render_scene;

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 3 {
        anyhow::bail!("usage: render_golden <mode-kebab> <out.png>");
    }
    let mode: RenderMode = serde_json::from_str(&format!("\"{}\"", args[1]))
        .context("parse mode")?;
    let out_path = &args[2];

    let positions = vec![
        Vec2::new(-0.45,  0.0),
        Vec2::new(-0.12,  0.15),
        Vec2::new( 0.20,  0.05),
        Vec2::new( 0.55,  0.30),
        Vec2::new(-0.05, -0.45),
    ];
    let velocities = vec![Vec2::ZERO; positions.len()];
    let states = vec![0u32, 0, 1, 1, 0];
    let selected = vec![0u32; positions.len()];
    let bonds = vec![
        BondPair::new(0, 1),
        BondPair::new(1, 2),
        BondPair::new(3, 4),
    ];
    let component_ids = compute_component_ids(positions.len(), &bonds);

    let pixels = render_scene(
        256, 256, mode,
        &positions, &velocities, &states, &selected, &component_ids,
    )?;

    image::save_buffer(out_path, &pixels, 256, 256, image::ExtendedColorType::Rgba8)
        .context("write PNG")?;
    Ok(())
}
