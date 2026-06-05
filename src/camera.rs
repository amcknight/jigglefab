use glam::{Mat4, Vec2};

pub const MIN_ZOOM: f32 = 1.0;
pub const MAX_ZOOM: f32 = 16.0;
/// Multiplicative zoom factor applied per unit of scroll.
pub const ZOOM_STEP: f32 = 1.1;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Camera {
    /// 1.0 = fit-world (whole world fills the viewport — today's behavior).
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
        let vw = viewport.0.max(1) as f32;
        let vh = viewport.1.max(1) as f32;
        let a = vw / vh;
        let base_w = world_size * a.max(1.0);
        let base_h = world_size * (1.0 / a).max(1.0);
        Vec2::new(base_w / self.zoom, base_h / self.zoom)
    }

    /// Convert a screen pixel to a world point. Result is NOT clamped — callers
    /// that want edge-snapping clamp the return value. (Internal anchor math
    /// needs the raw value.)
    fn screen_to_world_raw(&self, cursor: (f64, f64), viewport: (u32, u32), world_size: f32) -> Vec2 {
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

    /// Public screen->world, clamped to `[0, world_size]` per axis so a click
    /// outside the rendered world still yields a placeable (edge-snapped) point.
    pub fn screen_to_world(&self, cursor: (f64, f64), viewport: (u32, u32), world_size: f32) -> Vec2 {
        let w = self.screen_to_world_raw(cursor, viewport, world_size);
        Vec2::new(w.x.clamp(0.0, world_size), w.y.clamp(0.0, world_size))
    }

    /// Inverse of `screen_to_world_raw`, for tests/overlay math.
    pub fn world_to_screen(&self, world: Vec2, viewport: (u32, u32), world_size: f32) -> (f32, f32) {
        let vw = viewport.0.max(1) as f32;
        let vh = viewport.1.max(1) as f32;
        let vis = self.visible_extent(viewport, world_size);
        let fx = (world.x - self.center.x + vis.x * 0.5) / vis.x;
        let fy = (self.center.y + vis.y * 0.5 - world.y) / vis.y;
        (fx * vw, fy * vh)
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
}

#[cfg(test)]
mod tests {
    use super::*;

    const WS: f32 = 128.0;

    fn approx(a: Vec2, b: Vec2) -> bool {
        (a - b).length() < 1e-3
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
}
