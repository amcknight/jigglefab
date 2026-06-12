use std::path::PathBuf;

const MODES: &[&str] = &[
    "voronoi", "soft-voronoi", "worley", "metaball-blend", "metaball-argmax",
];
const TOLERANCE: u8 = 8;
const GOLDEN_DIR: &str = "tests/golden/render-modes";

fn render(mode: &str) -> image::RgbaImage {
    use jigglefab::render_mode::RenderMode;
    use jigglefab::render_offscreen::{canonical_scene, render_scene};

    let (positions, velocities, states, selected, component_ids) = canonical_scene();
    let mode_enum: RenderMode = serde_json::from_str(&format!("\"{}\"", mode)).unwrap();
    let pixels = render_scene(
        256, 256, mode_enum,
        &positions, &velocities, &states, &selected, &component_ids,
    ).unwrap();
    image::RgbaImage::from_vec(256, 256, pixels).unwrap()
}

fn compare(a: &image::RgbaImage, b: &image::RgbaImage, tol: u8) -> bool {
    if a.dimensions() != b.dimensions() { return false; }
    let mut bad = 0usize;
    for (pa, pb) in a.pixels().zip(b.pixels()) {
        for c in 0..3 {
            if pa.0[c].abs_diff(pb.0[c]) > tol { bad += 1; break; }
        }
    }
    let total = (a.width() * a.height()) as usize;
    (bad as f32) / (total as f32) < 0.005
}

#[test]
fn render_modes_match_goldens() {
    for mode in MODES {
        let golden_path = PathBuf::from(GOLDEN_DIR).join(format!("{}.png", mode));
        let current = render(mode);
        if !golden_path.exists() {
            panic!(
                "missing golden for {mode}: rerun with `cargo test -- --ignored regenerate_goldens`"
            );
        }
        let golden = image::open(&golden_path).unwrap().to_rgba8();
        assert!(compare(&current, &golden, TOLERANCE),
            "render-mode {mode} mismatches golden at {}", golden_path.display());
    }
}

#[test]
#[ignore]
fn regenerate_goldens() {
    std::fs::create_dir_all(GOLDEN_DIR).unwrap();
    for mode in MODES {
        let current = render(mode);
        let golden_path = PathBuf::from(GOLDEN_DIR).join(format!("{}.png", mode));
        current.save(&golden_path).unwrap();
    }
    println!("regenerated {} goldens", MODES.len());
}
