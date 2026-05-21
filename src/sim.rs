use glam::Vec2;
use std::f32::consts::TAU;

use crate::ccd::next_contact;
use crate::chemistry::{Action, Chemistry};
use crate::collide::reflect;
use crate::fab::Fab;
use crate::grid::Grid;
use crate::rng::prng_f32;

pub const WORLD_SIZE: f32 = 30.0;
pub const SPEED: f32 = 1.0;

pub struct Sim {
    pub positions: Vec<Vec2>,
    pub velocities: Vec<Vec2>,
    pub states: Vec<u32>,
    chemistry: Chemistry,
    grid: Grid,
    tick: u32,
}

impl Sim {
    pub fn from_fab(fab: &Fab, chemistry: Chemistry) -> Self {
        let n = fab.beads.len();
        let mut positions = Vec::with_capacity(n);
        let mut velocities = Vec::with_capacity(n);
        let mut states = Vec::with_capacity(n);
        for (i, bs) in fab.beads.iter().enumerate() {
            positions.push(bs.pos());
            // If vel not specified, derive from seed.
            let v = if let Some([vx, vy]) = bs.vel {
                Vec2::new(vx, vy)
            } else {
                let angle = prng_f32(fab.meta.seed, i as u32, 0) * TAU;
                Vec2::new(angle.cos(), angle.sin()) * SPEED
            };
            velocities.push(v);
            let state_idx = chemistry.state_index(&bs.state)
                .expect("bead state not in chemistry") as u32;
            states.push(state_idx);
        }
        let grid = Grid::new(WORLD_SIZE);
        Self { positions, velocities, states, chemistry, grid, tick: 0 }
    }

    pub fn step(&mut self, frame_dt: f32) {
        let mut dt_remaining = frame_dt;
        // Cap iterations to avoid pathological infinite loops (paranoia, shouldn't fire).
        let mut iter_cap = self.positions.len() * 64;
        while dt_remaining > 0.0 && iter_cap > 0 {
            iter_cap -= 1;
            // 1) Bin into grid.
            self.grid.clear();
            for (i, &p) in self.positions.iter().enumerate() {
                self.grid.insert(i as u32, p);
            }

            // 2) Find earliest contact across candidate pairs.
            let mut earliest: Option<(f32, u32, u32, bool)> = None;
            for (a, b) in self.grid.candidate_pairs() {
                let pa = self.positions[a as usize];
                let pb_raw = self.positions[b as usize];
                // Use min-image so pairs across the wrap see the short distance.
                let pb = pa + self.grid.min_image(pa, pb_raw);
                let va = self.velocities[a as usize];
                let vb = self.velocities[b as usize];
                if let Some(c) = next_contact(pa, va, pb, vb, dt_remaining) {
                    // Deterministic tiebreak: earlier t wins; for ties, lower (a,b) wins.
                    let key = (c.t, a, b);
                    let new_best = match earliest {
                        None => true,
                        Some((t0, a0, b0, _)) => key < (t0, a0, b0),
                    };
                    if new_best {
                        earliest = Some((c.t, a, b, c.inside));
                    }
                }
            }

            // 3) Advance everyone to the earliest contact (or full frame_dt if none).
            let advance_dt = match earliest {
                Some((t, _, _, _)) => t,
                None => dt_remaining,
            };
            for (p, v) in self.positions.iter_mut().zip(self.velocities.iter()) {
                *p += *v * advance_dt;
                *p = self.grid.wrap_pos(*p);
            }
            dt_remaining -= advance_dt;

            // 4) Resolve the contact (if any) per chemistry.
            if let Some((_t, a, b, inside)) = earliest {
                let sa = self.states[a as usize] as usize;
                let sb = self.states[b as usize] as usize;
                let action = self.chemistry.lookup(sa, sb, inside);
                if action == Action::Reflect {
                    let pa = self.positions[a as usize];
                    let pb_raw = self.positions[b as usize];
                    let pb = pa + self.grid.min_image(pa, pb_raw);
                    let va = self.velocities[a as usize];
                    let vb = self.velocities[b as usize];
                    let (va_new, vb_new) = reflect(pa, va, pb, vb);
                    self.velocities[a as usize] = va_new;
                    self.velocities[b as usize] = vb_new;
                }
                // Action::Pass: no state change in P1's grey chemistry.
                // (State-change logic lands in P2.)
            } else {
                break; // no contact this frame
            }
        }
        self.tick += 1;
    }

    pub fn tick(&self) -> u32 { self.tick }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chemistry::load_chemistry;
    use crate::fab::load_fab;

    #[test]
    fn two_beads_head_on_swap_velocities() {
        // Build a tiny custom Sim by hand to test step() in isolation.
        let chem = load_chemistry("chemistries/grey.toml").unwrap();
        let g = chem.state_index("grey").unwrap() as u32;
        let mut sim = Sim {
            positions: vec![Vec2::new(5.0, 5.0), Vec2::new(6.5, 5.0)],
            velocities: vec![Vec2::new(1.0, 0.0), Vec2::new(-1.0, 0.0)],
            states: vec![g, g],
            chemistry: chem,
            grid: Grid::new(WORLD_SIZE),
            tick: 0,
        };
        // Step a frame long enough to cover the collision (t = 0.5).
        sim.step(1.0);
        // After collision, velocities should be reversed.
        assert!((sim.velocities[0] - Vec2::new(-1.0, 0.0)).length() < 1e-3);
        assert!((sim.velocities[1] - Vec2::new( 1.0, 0.0)).length() < 1e-3);
    }

    #[test]
    fn from_fab_loads_grey_30_with_unit_speed() {
        let fab = load_fab("fabs/grey-30.toml").unwrap();
        let chem = load_chemistry("chemistries/grey.toml").unwrap();
        let sim = Sim::from_fab(&fab, chem);
        assert_eq!(sim.positions.len(), 30);
        for v in &sim.velocities {
            assert!((v.length() - SPEED).abs() < 1e-5);
        }
    }
}
