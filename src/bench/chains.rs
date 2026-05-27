use glam::Vec2;

use crate::chemistry::load_chemistry;
use crate::sim::Sim;
use crate::fab::{Fab, Meta, BeadSpec};

use super::scenario::{Scenario, Invariants, geometric_bonds};

const BEAD_SPACING: f32 = 0.667;
const CHAIN_SPACING_X: f32 = 5.0;
const CHAIN_ROW_GAP_Y: f32 = 2.0;
const SEED: u64 = 42;

/// Vertical bonded chains laid out in a 2D grid across the world. Beads
/// within a chain are at 0.667 spacing (just inside the bond threshold).
/// Chains within a row are at 5.0 horizontal spacing. Rows wrap when the
/// world width is exhausted, with vertical gap `chain_len * 0.667 + 2.0`.
///
/// Each chain must fit vertically in `world_size`; longer chains will panic
/// at `build()`. The default sweep is configured to never trigger this.
///
/// One `on` bead at index 0 of each chain; rest `off`. Uses the wire
/// chemistry — bonds are invariant by topology.
pub struct DisconnectedChains {
    pub chain_count: u32,
    pub chain_len: u32,
    pub world_size: f32,
}

impl DisconnectedChains {
    fn layout(&self) -> Vec<Vec2> {
        let chain_extent_y = (self.chain_len as f32 - 1.0) * BEAD_SPACING;
        assert!(
            chain_extent_y < self.world_size,
            "chain too long for world: chain_extent_y = {} >= world_size = {}",
            chain_extent_y, self.world_size,
        );
        self.grid_layout()
    }

    fn grid_layout(&self) -> Vec<Vec2> {
        let chains_per_row = (self.world_size / CHAIN_SPACING_X).floor() as u32;
        assert!(chains_per_row >= 1, "world too narrow to fit even one chain");
        let row_height = (self.chain_len as f32 - 1.0) * BEAD_SPACING + CHAIN_ROW_GAP_Y;
        let mut positions = Vec::with_capacity((self.chain_count * self.chain_len) as usize);
        for c in 0..self.chain_count {
            let row = c / chains_per_row;
            let col = c % chains_per_row;
            let x = CHAIN_SPACING_X * (col as f32) + (CHAIN_SPACING_X / 2.0);
            let y0 = row_height * (row as f32) + (CHAIN_ROW_GAP_Y / 2.0);
            for b in 0..self.chain_len {
                positions.push(Vec2::new(x, y0 + (b as f32) * BEAD_SPACING));
            }
        }
        positions
    }
}

impl Scenario for DisconnectedChains {
    fn name(&self) -> String {
        format!("chains_{}x{}", self.chain_count, self.chain_len)
    }

    fn build(&self) -> (Sim, Invariants) {
        let positions = self.layout();
        let beads: Vec<BeadSpec> = positions.iter().enumerate().map(|(i, p)| {
            let state = if i as u32 % self.chain_len == 0 { "on" } else { "off" };
            BeadSpec {
                state: state.to_string(),
                pos: [p.x, p.y],
                vel: None,
            }
        }).collect();
        let fab = Fab {
            meta: Meta {
                name: self.name(),
                chemistry: "wire".to_string(),
                seed: SEED,
                world_size: Some(self.world_size),
                bonds: None,
            },
            beads,
        };
        let chemistry = load_chemistry("chemistries/wire.toml").expect("load wire chemistry");
        let sim = Sim::from_fab(&fab, chemistry);

        let initial_bond_set = geometric_bonds(&sim.positions, sim.world_size());
        let mut histogram = vec![0usize; 2]; // wire has "off", "on"
        for &s in &sim.states {
            histogram[s as usize] += 1;
        }
        (sim, Invariants { initial_bond_set, initial_state_histogram: histogram })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ten_by_thirty_single_row_layout() {
        let s = DisconnectedChains { chain_count: 10, chain_len: 30, world_size: 50.0 };
        let (sim, inv) = s.build();
        assert_eq!(sim.positions.len(), 300);
        // 10 chains × 29 intra-chain bonds = 290 bonds, no cross-chain bonds.
        assert_eq!(inv.initial_bond_set.len(), 290);
        // First bead of each chain is "on", rest are "off". State indices:
        // wire chemistry has states ["off", "on"], so on=1, off=0.
        assert_eq!(inv.initial_state_histogram[1], 10);
        assert_eq!(inv.initial_state_histogram[0], 290);
    }

    #[test]
    fn multi_row_layout_no_cross_chain_bonds() {
        let s = DisconnectedChains { chain_count: 30, chain_len: 30, world_size: 128.0 };
        let (sim, inv) = s.build();
        assert_eq!(sim.positions.len(), 900);
        // 30 chains × 29 = 870 intra-chain bonds expected.
        assert_eq!(inv.initial_bond_set.len(), 870, "no cross-chain bonds should form at this spacing");
    }

    #[test]
    #[should_panic(expected = "chain too long for world")]
    fn build_panics_when_chain_too_long_for_world() {
        let s = DisconnectedChains { chain_count: 1, chain_len: 300, world_size: 64.0 };
        let _ = s.build();
    }

    #[test]
    fn name_is_snake_case_with_dims() {
        let s = DisconnectedChains { chain_count: 50, chain_len: 30, world_size: 256.0 };
        assert_eq!(s.name(), "chains_50x30");
    }
}
