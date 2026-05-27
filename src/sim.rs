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

/// Per-step counters populated inside `Sim::step`. Returned by value via
/// `last_step_metrics()`. The cost of populating them is a few `+=`s per
/// CCD iteration — negligible vs the work being measured, so no cfg gating.
#[derive(Clone, Copy, Debug, Default)]
pub struct StepMetrics {
    /// Number of contacts the step resolved before `dt_remaining` ran out.
    pub contacts_resolved: u32,
    /// Sum of `grid.candidate_pairs()` lengths across all iterations of the
    /// step (not the per-iteration mean — divide by `contacts_resolved + 1`
    /// for that).
    pub candidate_pairs: u32,
    /// True if the step terminated by exhausting `iter_cap`. Indicates the
    /// scheduler ran out of budget before resolving all contacts.
    pub iter_cap_hit: bool,
}

pub struct Sim {
    pub positions: Vec<Vec2>,
    pub velocities: Vec<Vec2>,
    pub states: Vec<u32>,
    pub(crate) chemistry: Chemistry,
    grid: Grid,
    // Set of currently-bonded pairs, keyed by (min(a,b), max(a,b)). Authoritative
    // source of truth — initialised from initial geometry, then carried through
    // sim time independent of float drift in |d|. For grey chemistry the set is
    // invariant; future chemistries that form/break bonds will mutate it.
    pub(crate) bonds: HashSet<(u32, u32)>,
    last_metrics: StepMetrics,
    tick: u32,
}

/// Distance-derive bonds for legacy presets (no explicit `bonds` field). A
/// pair bonds when their min-image separation is < RADIUS at preset time.
/// Mirrors the Haskell `bbSides` build at `haskell/src/Motion/Point.hs:40-41`.
pub(crate) fn derive_bonds_by_distance(positions: &[Vec2], grid: &Grid) -> HashSet<(u32, u32)> {
    let n = positions.len();
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
    bonds
}

impl Sim {
    pub fn world_size(&self) -> f32 { self.grid.world_size() }

    pub fn chemistry(&self) -> &Chemistry { &self.chemistry }

    pub fn bonds(&self) -> &HashSet<(u32, u32)> { &self.bonds }

    /// Per-state colors for rendering, defined by the chemistry. The renderer
    /// uploads this once at startup and indexes into it per-bead.
    pub fn palette(&self) -> Vec<[f32; 3]> { self.chemistry.colors.clone() }

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
        let bonds = match fab.bonds() {
            Some(explicit) => explicit.iter().map(|p| (p[0].min(p[1]), p[0].max(p[1]))).collect(),
            None => derive_bonds_by_distance(&positions, &grid),
        };
        Self { positions, velocities, states, chemistry, grid, bonds, last_metrics: StepMetrics::default(), tick: 0 }
    }

    pub fn tick(&self) -> u32 { self.tick }

    pub fn last_step_metrics(&self) -> StepMetrics { self.last_metrics }

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
    pub(crate) fn enforce_bonds(&mut self) {
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
        let mut metrics = StepMetrics::default();
        while dt_remaining > 0.0 && iter_cap > 0 {
            iter_cap -= 1;
            // 1) Bin into grid.
            self.grid.clear();
            for (i, &p) in self.positions.iter().enumerate() {
                self.grid.insert(i as u32, p);
            }

            // 2) Find earliest boundary crossing across candidate pairs.
            let mut earliest: Option<(f32, u32, u32, bool)> = None;
            let pairs = self.grid.candidate_pairs();
            metrics.candidate_pairs = metrics.candidate_pairs.saturating_add(pairs.len() as u32);
            for (a, b) in pairs {
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
            // The contact case is (bonded?, exiting?). When they agree the
            // chemistry rule fires (inside=bonded). When they disagree it's a
            // drift correction — geometry is on the wrong side of R for the
            // topology, so we Pass to restore alignment without swapping
            // velocities. Drift corrections only happen under float noise
            // (e.g. a sibling pair's snap perturbing a shared bead).
            if let Some((_t, a, b, exiting)) = earliest {
                metrics.contacts_resolved = metrics.contacts_resolved.saturating_add(1);
                let action = {
                    let bonded = self.is_bonded(a, b);
                    if bonded == exiting {
                        let sa = self.states[a as usize] as usize;
                        let sb = self.states[b as usize] as usize;
                        self.chemistry.lookup(sa, sb, bonded)
                    } else {
                        // Drift correction: the pair's geometric side disagrees
                        // with its topology. Pass-through pulls them back into
                        // alignment without spuriously changing velocities.
                        Action::Pass
                    }
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
                // *after* this action, so the next CCD iteration sees a clean
                // sign on `c` and we don't immediately re-trigger.
                //   Reflect: pair bounces back to the side it came from.
                //   Pass:    pair continues to the opposite side.
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
            } else {
                break; // no contact this frame
            }
        }

        if iter_cap == 0 {
            metrics.iter_cap_hit = true;
        }

        // Repair any bond that drifted past R during the frame's CCD pass.
        // Pairs whose exit was missed (e.g. consistently outpriced by other
        // pairs across iterations and never reached) end up at |d| > R; we
        // pull them back inside and apply the missed reflect. Running this
        // at end-of-step (rather than start) means external observers
        // reading positions after `step` always see bonds within R.
        self.enforce_bonds();

        self.tick += 1;
        self.last_metrics = metrics;
    }

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
            last_metrics: StepMetrics::default(),
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
            last_metrics: StepMetrics::default(),
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
            last_metrics: StepMetrics::default(),
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
            last_metrics: StepMetrics::default(),
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

    #[test]
    fn from_fab_uses_explicit_bonds_when_present() {
        // Three beads in a row at 0.5 spacing — distance derivation would bond
        // (0,1), (1,2), AND (0,2) (|0-2|=1.0 = RADIUS, borderline; flip to 0.9
        // to keep it strictly inside).
        let chem = load_chemistry("chemistries/grey.toml").unwrap();
        let toml_text = r#"
[meta]
name = "explicit"
chemistry = "grey"
seed = 1
bonds = [[0, 1], [1, 2]]

[[bead]]
state = "grey"
pos = [5.0, 5.0]

[[bead]]
state = "grey"
pos = [5.45, 5.0]

[[bead]]
state = "grey"
pos = [5.90, 5.0]
"#;
        let fab = crate::fab::parse_fab(toml_text).unwrap();
        let sim = Sim::from_fab(&fab, chem);
        assert_eq!(sim.bonds().len(), 2);
        assert!(sim.bonds().contains(&(0, 1)));
        assert!(sim.bonds().contains(&(1, 2)));
        assert!(!sim.bonds().contains(&(0, 2)), "explicit bonds must not be widened");
    }

    #[test]
    fn from_fab_without_bonds_matches_distance_derivation() {
        // Regression guard: the no-bonds path must produce the same set as the
        // pre-promotion code on a known preset.
        let chem = load_chemistry("chemistries/wire.toml").unwrap();
        let fab = load_fab("fabs/wire-20x30.toml").unwrap();
        assert!(fab.bonds().is_none());
        let sim = Sim::from_fab(&fab, chem);
        // wire-20x30 = 20 chains of 30, each chain bonds 29 pairs = 580 bonds.
        assert_eq!(sim.bonds().len(), 580);
    }

    #[test]
    fn step_metrics_reports_contacts_and_pairs() {
        // Two beads on collision course at speed 1; one head-on contact
        // resolved in this step.
        let chem = load_chemistry("chemistries/grey.toml").unwrap();
        let g = chem.state_index("grey").unwrap() as u32;
        let mut sim = Sim {
            positions: vec![Vec2::new(5.0, 5.0), Vec2::new(7.0, 5.0)],
            velocities: vec![Vec2::new(1.0, 0.0), Vec2::new(-1.0, 0.0)],
            states: vec![g, g],
            chemistry: chem,
            grid: Grid::new(WORLD_SIZE),
            bonds: HashSet::new(),
            last_metrics: StepMetrics::default(),
            tick: 0,
        };
        sim.step(1.0);
        let m = sim.last_step_metrics();
        assert_eq!(m.contacts_resolved, 1, "exactly one head-on contact in this step");
        assert!(m.candidate_pairs >= 1, "at least one candidate pair scanned");
        assert!(!m.iter_cap_hit, "iter cap should not be near for two beads");
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    #[cfg(not(debug_assertions))]
    fn from_fab_with_explicit_bonds_30k_under_5ms() {
        use std::time::Instant;
        let chem = load_chemistry("chemistries/wire.toml").unwrap();
        // Cold path: derive once so we have a bond list to seed the explicit path with.
        let mut fab = load_fab("fabs/wire-100x30x10.toml").unwrap();
        let sim_warm = Sim::from_fab(&fab, chem.clone());
        let bonds_vec: Vec<[u32; 2]> = sim_warm.bonds().iter().map(|&(a, b)| [a, b]).collect();
        fab.meta.bonds = Some(bonds_vec);
        // Warm-up build (don't measure first iteration — allocator hot path).
        let _ = Sim::from_fab(&fab, chem.clone());
        // Measured build.
        let start = Instant::now();
        let _ = Sim::from_fab(&fab, chem);
        let elapsed = start.elapsed();
        assert!(elapsed.as_millis() < 5,
                "explicit-bonds build on 30k beads took {} ms (budget 5)", elapsed.as_millis());
    }
}
