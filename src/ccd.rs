use glam::Vec2;

pub const RADIUS: f32 = 1.0;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Contact {
    pub t: f32,         // time of contact in [0, dt]
    pub exiting: bool,  // true if |d| is increasing through R (inside → outside)
}

/// Returns the next boundary crossing in [0, dt] for a pair, or None.
///
/// The returned `exiting` flag is the geometric direction at the crossing (sign
/// of d|d|²/dt at t), not a "side the pair is on" assumption. The caller pairs
/// this with the pair's topological bond state to pick a chemistry action:
///
/// - bonded + exiting  → reflect (the bond pulls them back inside)
/// - free + entering   → reflect (hard-sphere collision)
/// - bonded + entering → pass (drift correction: re-enter the bonded region)
/// - free + exiting    → pass (drift correction: leave the inside region)
///
/// This decouples physics decisions from float-noisy `|d| vs R` comparisons,
/// which used to flip a bonded pair to "free" on a single bit of arithmetic
/// noise and let the chain disintegrate.
pub fn next_contact(p1: Vec2, v1: Vec2, p2: Vec2, v2: Vec2, dt: f32) -> Option<Contact> {
    let d = p2 - p1;
    let r = RADIUS;

    // Cheap distance-based early-out. A pair travelling toward each other
    // at relative speed ≤ MAX_REL_SPEED_FOR_EARLY_OUT moves at most
    // MAX·dt units closer in the window. If |d| is already larger than
    // R + MAX·dt, the pair literally cannot reach contact within dt.
    //
    // The constant is the *assumed* relative-speed cap, picked with a
    // generous margin over what we've observed via the speed-stats HUD
    // (typical max individual ≤ 3.5, so max relative ≤ 7; we use 100 to
    // stay safe for ~25× headroom). Verify max via the HUD before
    // lowering. dt-aware so dt=1/240 production and dt~1 tests both work.
    const MAX_REL_SPEED_FOR_EARLY_OUT: f32 = 100.0;
    let reach = r + MAX_REL_SPEED_FOR_EARLY_OUT * dt;
    if d.dot(d) > reach * reach {
        return None;
    }

    let dv = v2 - v1;

    // Solve |d + dv * t|^2 = r^2
    //   => (dv·dv) t^2 + 2 (d·dv) t + (d·d - r^2) = 0
    let a = dv.dot(dv);
    let b = 2.0 * d.dot(dv);
    let c = d.dot(d) - r * r;

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

    // Smallest non-negative root within the dt window.
    let mut best: Option<f32> = None;
    for t in [t_early, t_late] {
        if t >= 0.0 && t <= dt {
            best = Some(match best {
                None => t,
                Some(prev) => prev.min(t),
            });
        }
    }
    let t = best?;

    // Direction at the crossing: sign of d|d|²/dt = 2 (d + dv*t) · dv.
    let d_at_t = d + dv * t;
    let exiting = d_at_t.dot(dv) > 0.0;

    Some(Contact { t, exiting })
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
        assert!(!c.exiting, "approaching from outside is an entry, not an exit");
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
        assert!(c.exiting, "moving apart from inside is an exit");
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
