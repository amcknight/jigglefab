//! Renders a fixed bead scene through the offscreen adapter and writes a PNG.
//! Used to (re-)generate golden images.
//!
//! Usage: cargo run --bin render_golden -- <mode-kebab> <out.png>

use anyhow::{Context, Result};
use jigglefab::render_mode::RenderMode;
use jigglefab::render_offscreen::{canonical_scene, render_scene};

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 3 {
        anyhow::bail!("usage: render_golden <mode-kebab> <out.png>");
    }
    let mode: RenderMode = serde_json::from_str(&format!("\"{}\"", args[1]))
        .context("parse mode")?;
    let out_path = &args[2];

    let (positions, velocities, states, selected, component_ids) = canonical_scene();

    let pixels = render_scene(
        256, 256, mode,
        &positions, &velocities, &states, &selected, &component_ids,
    )?;

    image::save_buffer(out_path, &pixels, 256, 256, image::ExtendedColorType::Rgba8)
        .context("write PNG")?;
    Ok(())
}
