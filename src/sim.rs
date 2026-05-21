use glam::Vec2;
use std::collections::HashSet;
use std::f32::consts::TAU;

use crate::ccd::{next_contact, RADIUS};
use crate::chemistry::{Action, Chemistry};
use crate::collide::reflect;
use crate::fab::Fab;
use crate::grid::Grid;
use crate::rng::prng_f32;

// Default world size used when a fab doesn't specify one. Tests and the older
// grey-30 fab still rely on this.
pub const WORLD_SIZE: f32 = 30.0;
pub const SPEED: f32 = 1.0;

// After a resolution the pair is at |d| ≈ R but a few ULPs off. We pin it to
// R ± BOUNDARY_EPS on the topology-correct side so the next CCD iteration sees
// a clean sign on c, and so a drifted bonded pair gets snapped back inside
// (rather than slowly escaping the bond). Small enough to perturb other
// neighbour distances by < ½ ULP per resolution.
const BOUNDARY_EPS: f32 = 1e-5;

pub struct Sim {
    pub positions: Vec<Vec2>,
    pub velocities: Vec<Vec2>,
    pub states: Vec<u32>,
    chemistry: Chemistry,
    grid: Grid,
    // Set of currently-bonded pairs, keyed by (min(a,b), max(a,b)). Authoritative
    // source of truth — initialised from initial geometry, then carried through
    // sim time independent of float drift in |d|. For grey chemistry the set is
    // invariant; future chemistries that form/break bonds will mutate it.
    bonds: HashSet<(u32, u32)>,
    tick: u32,
}

impl Sim {
    pub fn world_size(&self) -> f32 { self.grid.world_size() }

    /// Per-state colors for rendering, defined by the chemistry. The renderer
    /// uploads this once at startup and indexes into it per-bead.
    pub fn palette(&self) -> Vec<[f32; 3]> { self.chemistry.colors.clone() }
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
        let world_size = fab.meta.world_size.unwrap_or(WORLD_SIZE);
        let grid = Grid::new(world_size);
        let mut bonds = HashSet::new();
        for i in 0..n {
            for j in (i + 1)..n {
                let pa = positions[i];
                let pb = pa + grid.min_image(pa, positions[j]);
                if (pb - pa).length() < RADIUS {
                    bonds.insert((i as u32, j as u32));
                }
            }
        }
        Self { positions, velocities, states, chemistry, grid, bonds, tick: 0 }
    }

    fn is_bonded(&self, a: u32, b: u32) -> bool {
        let key = if a < b { (a, b) } else { (b, a) };
        self.bonds.contains(&key)
    }

    /// Walk the bond set and pull any pair that has drifted to |d| ≥ R back
    /// inside, also flipping their normal velocity if it was still outward.
    /// Without this, a bonded pair that gets nudged across R by a sibling
    /// pair's snap (or any rare float drift) is invisible to the CCD: it's
    /// already past the boundary and diverging, so `next_contact` returns
    /// `None`. The pair would drift apart forever. Calling this once per
    /// step bounds total drift to one frame's worth.
    fn enforce_bonds(&mut self) {
        let pairs: Vec<(u32, u32)> = self.bonds.iter().copied().collect();
        for (a, b) in pairs {
            let pa = self.positions[a as usize];
            let pb_raw = self.positions[b as usize];
            let pb = pa + self.grid.min_image(pa, pb_raw);
            let d = pb - pa;
            let dist = d.length();
            if dist < RADIUS || dist < 1e-12 {
                continue;
            }
            let n = d / dist;
            // Snap back inside by enough margin that subsequent sibling snaps
            // (each up to BOUNDARY_EPS / 2) can't immediately push us back out.
            let target = RADIUS - BOUNDARY_EPS;
            let correction = (target - dist) * 0.5;
            self.positions[a as usize] = self.grid.wrap_pos(pa - n * correction);
            let new_b = self.positions[b as usize] + n * correction;
            self.positions[b as usize] = self.grid.wrap_pos(new_b);

            // If their relative velocity is still outward, they would just
            // drift back out — that means we missed a reflect when they
            // originally crossed R. Apply the missed reflect now.
            let va = self.velocities[a as usize];
            let vb = self.velocities[b as usize];
            let dv = vb - va;
            if dv.dot(n) > 0.0 {
                let (va_new, vb_new) = reflect(pa, va, pb, vb);
                self.velocities[a as usize] = va_new;
                self.velocities[b as usize] = vb_new;
            }
        }
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

            // 2) Find earliest boundary crossing across candidate pairs.
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
                        earliest = Some((c.t, a, b, c.exiting));
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

            // 4) Resolve the contact (if any) per chemistry + topology + direction.
            //
            // Four cases:
            //   bonded + exiting   → consult chemistry's inside-rule (reflect for grey)
            //   free   + entering  → consult chemistry's outside-rule (reflect for grey)
            //   bonded + entering  → Pass (drift correction: pair was outside, re-entering)
            //   free   + exiting   → Pass (drift correction: pair was inside, leaving)
            //
            // The two drift-correction cases never fire in clean geometry; they only
            // trigger when float noise (e.g. a neighbouring pair's snap perturbing one
            // shared bead) has pushed this pair to the wrong side of R. The pass-through
            // restores the geometry-topology alignment without spuriously swapping
            // velocities.
            if let Some((_t, a, b, exiting)) = earliest {
                let bonded = self.is_bonded(a, b);
                let action = if bonded == exiting {
                    let sa = self.states[a as usize] as usize;
                    let sb = self.states[b as usize] as usize;
                    self.chemistry.lookup(sa, sb, bonded)
                } else {
                    Action::Pass
                };

                let pa = self.positions[a as usize];
                let pb_raw = self.positions[b as usize];
                let pb = pa + self.grid.min_image(pa, pb_raw);

                if matches!(action, Action::Reflect | Action::ReflectSwap) {
                    let va = self.velocities[a as usize];
                    let vb = self.velocities[b as usize];
                    let (va_new, vb_new) = reflect(pa, va, pb, vb);
                    self.velocities[a as usize] = va_new;
                    self.velocities[b as usize] = vb_new;
                }

                // Wire-style swap: signal hops between the two beads when they
                // bump. Same-state swaps are a no-op; different-state swaps
                // move the signal along the chain by one step per collision.
                if action == Action::ReflectSwap {
                    self.states.swap(a as usize, b as usize);
                }

                // Snap onto the side of |d|=R the pair is heading toward
                // *after* this action, so the next CCD iteration's `c` has
                // a clean sign and we don't immediately re-trigger.
                //
                //   Reflect: pair bounces back to the side it came from.
                //   Pass:    pair continues to the opposite side.
                //
                // Picking the post-action side from `bonded` alone (the old
                // logic) was correct only as long as bonded ↔ inside, i.e.
                // when free pairs always reflected and bonded pairs always
                // stayed. The wire chemistry's outside=pass rule breaks
                // that — a free pair can now legitimately end up inside R
                // (passing through each other), and snapping it back to
                // R+EPS while it's still moving inward ping-pongs against
                // the boundary every CCD iteration.
                let post_state_inside = match action {
                    Action::Reflect | Action::ReflectSwap => exiting,
                    Action::Pass => !exiting,
                };
                let d = pb - pa;
                let dist = d.length();
                if dist > 1e-12 {
                    let target = if post_state_inside { RADIUS - BOUNDARY_EPS } else { RADIUS + BOUNDARY_EPS };
                    let correction = (target - dist) * 0.5;
                    let n = d / dist;
                    self.positions[a as usize] = self.grid.wrap_pos(pa - n * correction);
                    let new_b = self.positions[b as usize] + n * correction;
                    self.positions[b as usize] = self.grid.wrap_pos(new_b);
                }
                // `bonded` is now only used inside the contact-resolution
                // arm above; silence the unused-binding warning that
                // appears when neither Reflect arm touches it.
                let _ = bonded;
            } else {
                break; // no contact this frame
            }
        }

        // Repair any bond that drifted past R during the frame's CCD pass.
        // Pairs whose exit was missed (e.g. consistently outpriced by other
        // pairs across iterations and never reached) end up at |d| > R; we
        // pull them back inside and apply the missed reflect. Running this
        // at end-of-step (rather than start) means external observers
        // reading positions after `step` always see bonds within R.
        self.enforce_bonds();

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
            positions: vec![Vec2::new(5.0, 5.0), Vec2::new(7.0, 5.0)],
            velocities: vec![Vec2::new(1.0, 0.0), Vec2::new(-1.0, 0.0)],
            states: vec![g, g],
            chemistry: chem,
            grid: Grid::new(WORLD_SIZE),
            bonds: HashSet::new(),
            tick: 0,
        };
        // Step a frame long enough to cover the collision (t = 0.5).
        sim.step(1.0);
        // After collision, velocities should be reversed.
        assert!((sim.velocities[0] - Vec2::new(-1.0, 0.0)).length() < 1e-3);
        assert!((sim.velocities[1] - Vec2::new( 1.0, 0.0)).length() < 1e-3);
    }

    #[test]
    fn two_bonded_beads_stay_bonded_over_time() {
        // Two beads start bonded at |d| = 0.5, moving apart at relative speed 2
        // along the y-axis. With R = 1 and Action::Reflect on both inside and
        // outside contacts, the bond should hold forever: they exit at |d|=R,
        // reflect (swap normal v), pass through each other, exit on the other
        // side, reflect, repeat. Period = 1.0. Over 20 sim seconds (1200 frames
        // at dt = 1/60) the pair must never drift past |d| = R + tolerance.
        let chem = load_chemistry("chemistries/grey.toml").unwrap();
        let g = chem.state_index("grey").unwrap() as u32;
        let mut bonds = HashSet::new();
        bonds.insert((0u32, 1u32));
        let mut sim = Sim {
            positions: vec![Vec2::new(15.0, 14.75), Vec2::new(15.0, 15.25)],
            velocities: vec![Vec2::new(0.0, -1.0), Vec2::new(0.0, 1.0)],
            states: vec![g, g],
            chemistry: chem,
            grid: Grid::new(WORLD_SIZE),
            bonds,
            tick: 0,
        };
        let dt = 1.0 / 60.0;
        let mut max_dist = 0.0f32;
        let mut max_at_frame = 0usize;
        for i in 0..1200 {
            sim.step(dt);
            // World is 30 wide and beads stay near y=15, so no torus wrap.
            let d = (sim.positions[0] - sim.positions[1]).length();
            if d > max_dist {
                max_dist = d;
                max_at_frame = i;
            }
        }
        assert!(
            max_dist <= crate::ccd::RADIUS + 1e-3,
            "bond broke: max |d| = {} at frame {} (R = {})",
            max_dist,
            max_at_frame,
            crate::ccd::RADIUS,
        );
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
        // No world_size in grey-30.toml → falls back to the default.
        assert_eq!(sim.world_size(), WORLD_SIZE);
    }

    #[test]
    fn fab_meta_world_size_propagates_to_sim() {
        let fab = load_fab("fabs/wire-100.toml").unwrap();
        let chem = load_chemistry("chemistries/wire.toml").unwrap();
        let sim = Sim::from_fab(&fab, chem);
        assert_eq!(sim.positions.len(), 100);
        assert_eq!(sim.world_size(), 80.0);
    }

    #[test]
    fn wire_bonded_pair_swaps_states_on_contact() {
        // A bonded pair: signal must hop between them when they reach
        // |d|=R (the "inside" rule of the wire chemistry).
        let chem = load_chemistry("chemistries/wire.toml").unwrap();
        let off = chem.state_index("off").unwrap() as u32;
        let on = chem.state_index("on").unwrap() as u32;
        let mut bonds = HashSet::new();
        bonds.insert((0u32, 1u32));
        let mut sim = Sim {
            // Start close, moving apart, so they hit |d|=R from the inside.
            positions: vec![Vec2::new(14.75, 5.0), Vec2::new(15.25, 5.0)],
            velocities: vec![Vec2::new(-1.0, 0.0), Vec2::new(1.0, 0.0)],
            states: vec![on, off],
            chemistry: chem,
            grid: Grid::new(WORLD_SIZE),
            bonds,
            tick: 0,
        };
        sim.step(1.0);
        assert_eq!(sim.states[0], off, "signal hopped to the other bead");
        assert_eq!(sim.states[1], on, "signal hopped to the other bead");
        // And the bond holds: pair stays within R after the reflect.
        let d = (sim.positions[0] - sim.positions[1]).length();
        assert!(d <= crate::ccd::RADIUS + 1e-3, "bond should hold, |d|={d}");
    }

    #[test]
    fn wire_free_pair_reflects_without_swap() {
        // A free pair (no bond): wire's outside rule is "reflect" — the
        // beads bounce off each other (so chains have shape and bounce
        // off neighbours) but the signal does NOT transfer, since the
        // user's clarification is "only swap between bonded circles."
        let chem = load_chemistry("chemistries/wire.toml").unwrap();
        let off = chem.state_index("off").unwrap() as u32;
        let on = chem.state_index("on").unwrap() as u32;
        let mut sim = Sim {
            positions: vec![Vec2::new(5.0, 5.0), Vec2::new(7.0, 5.0)],
            velocities: vec![Vec2::new(1.0, 0.0), Vec2::new(-1.0, 0.0)],
            states: vec![on, off],
            chemistry: chem,
            grid: Grid::new(WORLD_SIZE),
            bonds: HashSet::new(),
            tick: 0,
        };
        sim.step(1.0);
        // States preserved — free-pair contacts do not propagate signal.
        assert_eq!(sim.states[0], on, "free-pair contact does not swap");
        assert_eq!(sim.states[1], off, "free-pair contact does not swap");
        // Velocities reflected — chains have physical extent.
        assert!((sim.velocities[0] - Vec2::new(-1.0, 0.0)).length() < 1e-3);
        assert!((sim.velocities[1] - Vec2::new( 1.0, 0.0)).length() < 1e-3);
    }
}
