use std::path::PathBuf;

const MODES: &[&str] = &[
    "voronoi", "soft-voronoi", "worley", "metaball-blend", "metaball-argmax",
];
const TOLERANCE: u8 = 8;
const GOLDEN_DIR: &str = "tests/golden/render-modes";

fn render(mode: &str) -> image::RgbaImage {
    use glam::Vec2;
    use jigglefab::bond::BondPair;
    use jigglefab::component::compute_component_ids;
    use jigglefab::render_mode::RenderMode;
    use jigglefab::render_offscreen::render_scene;

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
        BondPair::new(0, 1), BondPair::new(1, 2), BondPair::new(3, 4),
    ];
    let component_ids = compute_component_ids(positions.len(), &bonds);
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
