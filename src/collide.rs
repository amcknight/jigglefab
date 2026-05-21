use glam::Vec2;

/// Reflect two beads' velocities given their positions at the moment of contact.
/// Both inside and outside collisions use the same formula.
pub fn reflect(p1: Vec2, v1: Vec2, p2: Vec2, v2: Vec2) -> (Vec2, Vec2) {
    let n = (p2 - p1).normalize_or_zero();
    if n.length_squared() < 1e-12 {
        // Coincident centers (degenerate). Leave velocities unchanged.
        return (v1, v2);
    }
    let v1n = v1.dot(n);
    let v2n = v2.dot(n);
    // Equal-mass elastic: swap normal components.
    let delta = (v2n - v1n) * n;
    (v1 + delta, v2 - delta)
}

#[cfg(test)]
mod tests {
    use super::*;
    use glam::Vec2;

    #[test]
    fn head_on_equal_speed_swaps() {
        // Both beads moving toward each other at unit speed along x; should bounce back.
        let p1 = Vec2::new(-0.5, 0.0);
        let p2 = Vec2::new( 0.5, 0.0);
        let v1 = Vec2::new( 1.0, 0.0);
        let v2 = Vec2::new(-1.0, 0.0);
        let (v1p, v2p) = reflect(p1, v1, p2, v2);
        assert!((v1p - Vec2::new(-1.0, 0.0)).length() < 1e-5);
        assert!((v2p - Vec2::new( 1.0, 0.0)).length() < 1e-5);
    }

    #[test]
    fn perpendicular_unchanged() {
        // One bead has only normal-direction velocity, other only tangential.
        // After collision: normal swaps, tangential unchanged.
        let p1 = Vec2::new(-0.5, 0.0);
        let p2 = Vec2::new( 0.5, 0.0);
        let v1 = Vec2::new( 1.0, 0.0); // pure normal toward p2
        let v2 = Vec2::new( 0.0, 2.0); // pure tangential
        let (v1p, v2p) = reflect(p1, v1, p2, v2);
        assert!((v1p - Vec2::new(0.0, 0.0)).length() < 1e-5);  // gave up its normal
        assert!((v2p - Vec2::new(1.0, 2.0)).length() < 1e-5);  // gained the normal
    }

    #[test]
    fn momentum_and_energy_conserved() {
        let p1 = Vec2::new(0.0, 0.0);
        let p2 = Vec2::new(0.7, 0.5).normalize();
        let v1 = Vec2::new( 0.3, 0.9);
        let v2 = Vec2::new(-0.6, 0.2);
        let (v1p, v2p) = reflect(p1, v1, p2, v2);
        let p_before = v1 + v2;
        let p_after = v1p + v2p;
        assert!((p_before - p_after).length() < 1e-5);
        let e_before = v1.length_squared() + v2.length_squared();
        let e_after = v1p.length_squared() + v2p.length_squared();
        assert!((e_before - e_after).abs() < 1e-5);
    }
}
