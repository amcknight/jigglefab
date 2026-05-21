use glam::Vec2;

pub const RADIUS: f32 = 1.0;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Contact {
    pub t: f32,        // time of contact in [0, dt]
    pub inside: bool,  // true if pair is currently bonded (|d| < r)
}

/// Returns the next contact in [0, dt] between two beads, or None if none.
/// Pair positions/velocities can be passed in any order; the result is symmetric.
pub fn next_contact(p1: Vec2, v1: Vec2, p2: Vec2, v2: Vec2, dt: f32) -> Option<Contact> {
    let d = p2 - p1;
    let dv = v2 - v1;
    let r = RADIUS;

    // Solve |d + dv * t|^2 = r^2
    //   => (dv·dv) t^2 + 2 (d·dv) t + (d·d - r^2) = 0
    let a = dv.dot(dv);
    let b = 2.0 * d.dot(dv);
    let c = d.dot(d) - r * r;

    // If beads have zero relative velocity, no contact will be formed.
    if a < 1e-12 {
        return None;
    }

    let disc = b * b - 4.0 * a * c;
    if disc < 0.0 {
        return None;
    }
    let sqrt_disc = disc.sqrt();
    let t_early = (-b - sqrt_disc) / (2.0 * a);
    let t_late = (-b + sqrt_disc) / (2.0 * a);

    let currently_inside = c < 0.0; // |d|^2 < r^2

    // Which root is "the next boundary crossing from where we are"?
    // If currently outside (c > 0): we want t_early (the entry).
    // If currently inside  (c < 0): t_early is negative-or-already-past;
    //   t_late is the exit. We want t_late.
    let t = if currently_inside { t_late } else { t_early };

    if t < 0.0 || t > dt {
        return None;
    }

    Some(Contact { t, inside: currently_inside })
}

#[cfg(test)]
mod tests {
    use super::*;
    use glam::Vec2;

    #[test]
    fn head_on_outside_collision() {
        // Two beads on x-axis at x = -2 and +2, moving toward each other at speed 1.
        // Relative position is (4, 0), relative velocity is (-2, 0).
        // They touch when |d + dv*t| = 1, i.e. |4 - 2t| = 1 => t = 1.5 (first).
        let p1 = Vec2::new(-2.0, 0.0);
        let p2 = Vec2::new( 2.0, 0.0);
        let v1 = Vec2::new( 1.0, 0.0);
        let v2 = Vec2::new(-1.0, 0.0);
        let c = next_contact(p1, v1, p2, v2, 2.0).unwrap();
        assert!((c.t - 1.5).abs() < 1e-5);
        assert!(!c.inside);
    }

    #[test]
    fn parallel_motion_no_contact() {
        let p1 = Vec2::new(0.0, 0.0);
        let p2 = Vec2::new(5.0, 0.0);
        let v1 = Vec2::new(1.0, 0.0);
        let v2 = Vec2::new(1.0, 0.0);
        assert!(next_contact(p1, v1, p2, v2, 10.0).is_none());
    }

    #[test]
    fn diverging_no_contact() {
        // Two beads separating; no contact possible.
        let p1 = Vec2::new(0.0, 0.0);
        let p2 = Vec2::new(2.0, 0.0);
        let v1 = Vec2::new(-1.0, 0.0);
        let v2 = Vec2::new( 1.0, 0.0);
        assert!(next_contact(p1, v1, p2, v2, 10.0).is_none());
    }

    #[test]
    fn inside_pair_exits() {
        // Two beads bonded (|d| = 0.5), moving apart at relative speed 1.
        // They reach the boundary |d| = 1 at t = 0.5.
        let p1 = Vec2::new(-0.25, 0.0);
        let p2 = Vec2::new( 0.25, 0.0);
        let v1 = Vec2::new(-0.5, 0.0);
        let v2 = Vec2::new( 0.5, 0.0);
        let c = next_contact(p1, v1, p2, v2, 1.0).unwrap();
        assert!((c.t - 0.5).abs() < 1e-5);
        assert!(c.inside);
    }

    #[test]
    fn contact_outside_dt_window() {
        // Contact would be at t = 1.5 but dt = 1.0.
        let p1 = Vec2::new(-2.0, 0.0);
        let p2 = Vec2::new( 2.0, 0.0);
        let v1 = Vec2::new( 1.0, 0.0);
        let v2 = Vec2::new(-1.0, 0.0);
        assert!(next_contact(p1, v1, p2, v2, 1.0).is_none());
    }
}
