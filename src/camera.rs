use glam::{Mat4, Vec2};

pub const MIN_ZOOM: f32 = 1.0;
pub const MAX_ZOOM: f32 = 16.0;
/// Multiplicative zoom factor applied per unit of scroll.
pub const ZOOM_STEP: f32 = 1.1;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Camera {
    /// 1.0 = fit-world: the whole world fits within the viewport (letterboxed on
    /// the long axis) — today's behavior. Larger = zoomed in.
    pub zoom: f32,
    /// World point shown at the centre of the viewport.
    pub center: Vec2,
}

impl Camera {
    pub fn fit(world_size: f32) -> Self {
        Self { zoom: 1.0, center: Vec2::new(world_size * 0.5, world_size * 0.5) }
    }

    /// Width/height of the world rect currently visible, in world units.
    fn visible_extent(&self, viewport: (u32, u32), world_size: f32) -> Vec2 {
        debug_assert!(self.zoom > 0.0, "camera zoom must be positive, got {}", self.zoom);
        let vw = viewport.0.max(1) as f32;
        let vh = viewport.1.max(1) as f32;
        let a = vw / vh;
        let base_w = world_size * a.max(1.0);
        let base_h = world_size * (1.0 / a).max(1.0);
        Vec2::new(base_w / self.zoom, base_h / self.zoom)
    }

    /// Convert a screen pixel to a continuous world point. On the torus the
    /// result may lie outside `[0, world_size]`; callers wrap when committing a
    /// position to the scene.
    pub fn screen_to_world(&self, cursor: (f64, f64), viewport: (u32, u32), world_size: f32) -> Vec2 {
        let vw = viewport.0.max(1) as f32;
        let vh = viewport.1.max(1) as f32;
        let vis = self.visible_extent(viewport, world_size);
        let fx = cursor.0 as f32 / vw;
        let fy = cursor.1 as f32 / vh;
        Vec2::new(
            self.center.x - vis.x * 0.5 + fx * vis.x,
            self.center.y + vis.y * 0.5 - fy * vis.y,
        )
    }

    /// Inverse of `screen_to_world`, for tests/overlay math.
    pub fn world_to_screen(&self, world: Vec2, viewport: (u32, u32), world_size: f32) -> (f32, f32) {
        let vw = viewport.0.max(1) as f32;
        let vh = viewport.1.max(1) as f32;
        let vis = self.visible_extent(viewport, world_size);
        let fx = (world.x - self.center.x + vis.x * 0.5) / vis.x;
        let fy = (self.center.y + vis.y * 0.5 - world.y) / vis.y;
        (fx * vw, fy * vh)
    }

    /// Multiply zoom by `factor` (clamped to [MIN_ZOOM, MAX_ZOOM]) while keeping the
    /// world point under `cursor` fixed on screen.
    pub fn zoom_at(&mut self, cursor: (f64, f64), factor: f32, viewport: (u32, u32), world_size: f32) {
        let new_zoom = (self.zoom * factor).clamp(MIN_ZOOM, MAX_ZOOM);
        if (new_zoom - self.zoom).abs() < f32::EPSILON {
            return; // at a clamp: no zoom change ⇒ no centre shift (anchor stays exact)
        }
        let anchor = self.screen_to_world(cursor, viewport, world_size);
        self.zoom = new_zoom;
        // Solve for the centre that puts `anchor` back under `cursor` at the new zoom.
        let vw = viewport.0.max(1) as f32;
        let vh = viewport.1.max(1) as f32;
        let vis = self.visible_extent(viewport, world_size);
        let fx = cursor.0 as f32 / vw;
        let fy = cursor.1 as f32 / vh;
        self.center = Vec2::new(
            anchor.x - (fx - 0.5) * vis.x,
            anchor.y + (fy - 0.5) * vis.y,
        );
        self.center.x = self.center.x.rem_euclid(world_size);
        self.center.y = self.center.y.rem_euclid(world_size);
    }

    /// Pan by a cursor screen-delta in physical pixels (content follows the
    /// cursor). The center wraps mod `world_size` — the world is a torus, so
    /// panning never hits a wall; the renderer's ghost ring keeps a full
    /// neighborhood visible.
    pub fn pan_by(&mut self, screen_delta: (f32, f32), viewport: (u32, u32), world_size: f32) {
        let vw = viewport.0.max(1) as f32;
        let vh = viewport.1.max(1) as f32;
        let vis = self.visible_extent(viewport, world_size);
        let cx = self.center.x - screen_delta.0 * vis.x / vw;
        let cy = self.center.y + screen_delta.1 * vis.y / vh;
        self.center = Vec2::new(cx.rem_euclid(world_size), cy.rem_euclid(world_size));
    }

    /// Reset to the default fit-world view (zoom 1, centered).
    pub fn reset(&mut self, world_size: f32) {
        *self = Camera::fit(world_size);
    }

    /// World→clip orthographic projection for the current view.
    pub fn view_proj(&self, viewport: (u32, u32), world_size: f32) -> Mat4 {
        let vis = self.visible_extent(viewport, world_size);
        let left = self.center.x - vis.x * 0.5;
        let right = self.center.x + vis.x * 0.5;
        let bottom = self.center.y - vis.y * 0.5;
        let top = self.center.y + vis.y * 0.5;
        Mat4::orthographic_rh(left, right, bottom, top, -1.0, 1.0)
    }

    /// The raw world-space rectangle currently visible: (min corner, max corner).
    /// Corners may lie outside `[0, world_size]` (the view can straddle the seam).
    pub fn visible_world_rect(&self, viewport: (u32, u32), world_size: f32) -> (Vec2, Vec2) {
        let vis = self.visible_extent(viewport, world_size);
        (self.center - vis * 0.5, self.center + vis * 0.5)
    }
}

/// World-boundary grid lines (at integer multiples of `world_size`) that
/// intersect the visible rect `[min, max]`. Returns LineList vertex pairs in
/// world space — purely cosmetic seam markers for orientation on the torus.
pub fn seam_segments(min: Vec2, max: Vec2, world_size: f32) -> Vec<[f32; 2]> {
    let mut segs = Vec::new();
    let first = (min.x / world_size).ceil() as i32;
    let last = (max.x / world_size).floor() as i32;
    for m in first..=last {
        let x = m as f32 * world_size;
        segs.push([x, min.y]);
        segs.push([x, max.y]);
    }
    let first = (min.y / world_size).ceil() as i32;
    let last = (max.y / world_size).floor() as i32;
    for m in first..=last {
        let y = m as f32 * world_size;
        segs.push([min.x, y]);
        segs.push([max.x, y]);
    }
    segs
}

#[cfg(test)]
mod tests {
    use super::*;

    const WS: f32 = 128.0;

    fn approx(a: Vec2, b: Vec2) -> bool {
        (a - b).length() < 1e-3
    }

    /// True if `a` equals `b` per-axis modulo `ws` (torus equivalence).
    fn approx_mod(a: Vec2, b: Vec2, ws: f32) -> bool {
        let dx = (a.x - b.x).rem_euclid(ws);
        let dy = (a.y - b.y).rem_euclid(ws);
        let near = |v: f32| v < 1e-2 || (ws - v) < 1e-2;
        near(dx) && near(dy)
    }

    #[test]
    fn fit_centers_world() {
        let cam = Camera::fit(WS);
        assert_eq!(cam.zoom, 1.0);
        assert!(approx(cam.center, Vec2::new(WS / 2.0, WS / 2.0)));
    }

    #[test]
    fn fit_center_pixel_maps_to_world_center() {
        // Square viewport: the centre pixel is the world centre.
        let cam = Camera::fit(WS);
        let w = cam.screen_to_world((400.0, 400.0), (800, 800), WS);
        assert!(approx(w, Vec2::new(WS / 2.0, WS / 2.0)), "got {w:?}");
    }

    #[test]
    fn fit_top_left_pixel_is_world_top_left() {
        // Screen (0,0) is top-left; world (0, WS) is top-left on a square view.
        let cam = Camera::fit(WS);
        let w = cam.screen_to_world((0.0, 0.0), (800, 800), WS);
        assert!(approx(w, Vec2::new(0.0, WS)), "got {w:?}");
    }

    #[test]
    fn screen_world_round_trip_when_zoomed() {
        // world -> screen -> world is identity away from the edge clamp.
        let cam = Camera { zoom: 4.0, center: Vec2::new(40.0, 90.0) };
        let viewport = (1024, 768);
        let world_in = Vec2::new(42.0, 88.0);
        let screen = cam.world_to_screen(world_in, viewport, WS);
        let world_out = cam.screen_to_world((screen.0 as f64, screen.1 as f64), viewport, WS);
        assert!(approx(world_in, world_out), "in {world_in:?} out {world_out:?}");
    }

    #[test]
    fn view_proj_is_inverse_of_screen_to_world() {
        // A world point projected to clip, then mapped clip->screen->world,
        // returns the original. Guards render/input consistency.
        let cam = Camera { zoom: 2.5, center: Vec2::new(70.0, 60.0) };
        let viewport = (800, 600);
        let world_in = Vec2::new(72.0, 55.0);
        let clip = cam.view_proj(viewport, WS) * world_in.extend(0.0).extend(1.0);
        // clip.xy in [-1,1]; convert to screen (y flips).
        let sx = (clip.x * 0.5 + 0.5) * viewport.0 as f32;
        let sy = (1.0 - (clip.y * 0.5 + 0.5)) * viewport.1 as f32;
        let world_out = cam.screen_to_world((sx as f64, sy as f64), viewport, WS);
        assert!(approx(world_in, world_out), "in {world_in:?} out {world_out:?}");
    }

    #[test]
    fn zoom_at_keeps_cursor_world_point_fixed() {
        let mut cam = Camera::fit(WS);
        let viewport = (1024, 768);
        let cursor = (300.0, 500.0);
        let before = cam.screen_to_world(cursor, viewport, WS);
        cam.zoom_at(cursor, 2.0, viewport, WS);
        let after = cam.screen_to_world(cursor, viewport, WS);
        assert!((cam.zoom - 2.0).abs() < 1e-4, "zoom {}", cam.zoom);
        assert!(approx_mod(before, after, WS), "anchor moved: {before:?} -> {after:?}");
    }

    #[test]
    fn zoom_at_clamps_to_max() {
        let mut cam = Camera::fit(WS);
        let viewport = (800, 800);
        for _ in 0..100 {
            cam.zoom_at((400.0, 400.0), 2.0, viewport, WS);
        }
        assert!((cam.zoom - MAX_ZOOM).abs() < 1e-4, "zoom {}", cam.zoom);
    }

    #[test]
    fn zoom_at_clamps_to_min_and_anchor_exact_at_clamp() {
        // Already at min; zooming out further is a no-op, so the centre cannot drift.
        let mut cam = Camera::fit(WS);
        let viewport = (800, 800);
        cam.zoom_at((100.0, 700.0), 0.5, viewport, WS);
        assert!((cam.zoom - MIN_ZOOM).abs() < 1e-4, "zoom {}", cam.zoom);
        assert!(approx(cam.center, Vec2::new(WS / 2.0, WS / 2.0)), "center {:?}", cam.center);
    }

    #[test]
    fn pan_moves_center_opposite_to_cursor_x() {
        let mut cam = Camera { zoom: 4.0, center: Vec2::new(WS / 2.0, WS / 2.0) };
        let viewport = (800, 800);
        let before = cam.center.x;
        cam.pan_by((20.0, 0.0), viewport, WS); // drag cursor right
        assert!(cam.center.x < before, "center.x should decrease: {before} -> {}", cam.center.x);
    }

    #[test]
    fn reset_equals_fit() {
        let mut cam = Camera { zoom: 7.0, center: Vec2::new(10.0, 10.0) };
        cam.reset(WS);
        assert_eq!(cam, Camera::fit(WS));
    }

    #[test]
    fn pan_wraps_center_into_domain() {
        let mut cam = Camera::fit(WS); // center (64,64), zoom 1
        let viewport = (800, 800);
        cam.pan_by((100_000.0, 0.0), viewport, WS);
        assert!(cam.center.x >= 0.0 && cam.center.x < WS, "center.x not wrapped: {}", cam.center.x);
    }

    #[test]
    fn pan_full_world_returns_to_start() {
        let mut cam = Camera { zoom: 2.0, center: Vec2::new(30.0, 40.0) };
        let viewport = (800, 800); // square ⇒ vis = WS/zoom = 64; world-per-px = 64/800
        let start = cam.center;
        // dx that moves center by exactly -WS on x: dx = WS * vw / vis.x = 128*800/64 = 1600.
        cam.pan_by((1600.0, 0.0), viewport, WS);
        assert!(approx_mod(cam.center, start, WS), "not seamless: {:?} vs {:?}", cam.center, start);
    }

    #[test]
    fn screen_to_world_is_unclamped() {
        let cam = Camera::fit(WS);
        let w = cam.screen_to_world((0.0, 400.0), (1600, 800), WS); // aspect 2 ⇒ vis.x = 2*WS
        assert!(w.x < 0.0, "expected raw negative x, got {}", w.x);
    }

    #[test]
    fn visible_world_rect_matches_extent() {
        let cam = Camera { zoom: 2.0, center: Vec2::new(70.0, 60.0) };
        let viewport = (800, 800);
        let (min, max) = cam.visible_world_rect(viewport, WS);
        let half = WS / (2.0 * 2.0); // square, vis = WS/zoom = 64 ⇒ half = 32
        assert!(approx(min, Vec2::new(70.0 - half, 60.0 - half)), "min {min:?}");
        assert!(approx(max, Vec2::new(70.0 + half, 60.0 + half)), "max {max:?}");
    }

    #[test]
    fn seam_segments_covers_boundaries_in_view() {
        let segs = seam_segments(Vec2::new(-10.0, -10.0), Vec2::new(138.0, 138.0), WS);
        assert_eq!(segs.len(), 8, "got {:?}", segs); // 2 vertical + 2 horizontal lines × 2 verts
        assert!(segs.windows(2).any(|w| w[0][0] == 0.0 && w[1][0] == 0.0));    // x=0 line
        assert!(segs.windows(2).any(|w| w[0][0] == 128.0 && w[1][0] == 128.0)); // x=128 line
    }

    #[test]
    fn seam_segments_fit_view_shows_outer_box() {
        let segs = seam_segments(Vec2::new(0.0, 0.0), Vec2::new(WS, WS), WS);
        assert_eq!(segs.len(), 8);
    }
}
