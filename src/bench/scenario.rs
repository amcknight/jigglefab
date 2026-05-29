use std::collections::HashSet;
use glam::Vec2;

use crate::bond::BondPair;
use crate::ccd::RADIUS;
use crate::sim::Sim;

/// Captures the initial configuration of a Sim for end-of-run invariant checks.
pub struct Invariants {
    pub initial_bond_set: HashSet<BondPair>,
    pub initial_state_histogram: Vec<usize>,
}

pub trait Scenario {
    /// Stable identifier used in CSV / report rows. snake_case, no spaces.
    fn name(&self) -> String;
    /// Construct a fresh Sim and snapshot its initial invariants.
    fn build(&self) -> (Sim, Invariants);
}

/// Set of all unordered pairs (a, b) with a < b whose torus-min-image distance
/// is strictly less than RADIUS (the bond threshold). Used to capture
/// initial bonds and check final bonds. O(N²) — only run at scenario setup
/// and at end-of-run, never per-step.
pub fn geometric_bonds(positions: &[Vec2], world_size: f32) -> HashSet<BondPair> {
    let mut bonds = HashSet::new();
    let half = world_size * 0.5;
    for i in 0..positions.len() {
        for j in (i + 1)..positions.len() {
            let mut d = positions[j] - positions[i];
            if d.x >  half { d.x -= world_size; }
            if d.x < -half { d.x += world_size; }
            if d.y >  half { d.y -= world_size; }
            if d.y < -half { d.y += world_size; }
            if d.length() < RADIUS {
                bonds.insert(BondPair::new(i as u32, j as u32));
            }
        }
    }
    bonds
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn geometric_bonds_finds_close_pair() {
        let positions = vec![
            Vec2::new(5.0, 5.0),
            Vec2::new(5.0, 5.5),  // 0.5 apart — bonded
            Vec2::new(5.0, 10.0), // far — not bonded
        ];
        let bonds = geometric_bonds(&positions, 50.0);
        assert_eq!(bonds.len(), 1);
        assert!(bonds.contains(&BondPair::new(0, 1)));
    }

    #[test]
    fn geometric_bonds_respects_torus_wrap() {
        // Beads at opposite edges of a 10-wide world should bond (short way is 0.4).
        let positions = vec![
            Vec2::new(0.2, 5.0),
            Vec2::new(9.8, 5.0),
        ];
        let bonds = geometric_bonds(&positions, 10.0);
        assert_eq!(bonds.len(), 1);
        assert!(bonds.contains(&BondPair::new(0, 1)));
    }

    #[test]
    fn geometric_bonds_excludes_pairs_at_radius() {
        // Exactly at R — not bonded (strict inequality |d| < R).
        let positions = vec![
            Vec2::new(5.0, 5.0),
            Vec2::new(5.0, 6.0),  // exactly 1.0 apart
        ];
        let bonds = geometric_bonds(&positions, 50.0);
        assert_eq!(bonds.len(), 0);
    }
}
