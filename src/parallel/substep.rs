use std::collections::HashSet;

use crate::ccd::{next_contact, RADIUS};
use crate::chemistry::CompiledChemistry;
use crate::collide::reflect;
use crate::grid::Grid;
use crate::parallel::{coloring, resolve, BeadPool, Pair};

const BOND_EPS: f32 = 1e-5;

// Pull any bonded pair that drifted past R back inside, and flip their
// normal velocity if it was still outward. Matches Sim::enforce_bonds:
// without this, a bonded pair nudged across R by a sibling pair's snap
// is invisible to the next substep's CCD and would drift apart forever.
pub fn enforce_bonds(pool: &mut BeadPool, grid: &Grid, bonds: &HashSet<(u32, u32)>) {
    let pairs: Vec<(u32, u32)> = bonds.iter().copied().collect();
    for (a, b) in pairs {
        if !pool.get(a).alive || !pool.get(b).alive {
            continue;
        }
        let pa = pool.get(a).pos;
        let pb_raw = pool.get(b).pos;
        let pb = pa + grid.min_image(pa, pb_raw);
        let d = pb - pa;
        let dist = d.length();
        if dist < RADIUS || dist < 1e-12 {
            continue;
        }
        let n = d / dist;
        let target = RADIUS - BOND_EPS;
        let correction = (target - dist) * 0.5;
        let new_a = grid.wrap_pos(pa - n * correction);
        let new_b = grid.wrap_pos(pool.get(b).pos + n * correction);
        pool.get_mut(a).pos = new_a;
        pool.get_mut(b).pos = new_b;
        let va = pool.get(a).vel;
        let vb = pool.get(b).vel;
        if (vb - va).dot(n) > 0.0 {
            let (va_new, vb_new) = reflect(pa, va, pb, vb);
            pool.get_mut(a).vel = va_new;
            pool.get_mut(b).vel = vb_new;
        }
    }
}

pub fn do_substep(
    pool: &mut BeadPool,
    grid: &mut Grid,
    chem: &CompiledChemistry,
    bonds: &mut HashSet<(u32, u32)>,
    dt_sub: f32,
) {
    let contacts = compute_active_contacts(pool, grid, dt_sub);
    if contacts.is_empty() {
        advance_all(pool, grid, dt_sub);
        clear_substep_flags(pool);
        return;
    }
    let colors = coloring::color_pairs(&contacts);
    let max_color = colors.iter().copied().max().unwrap_or(0);
    let mut pending_bonds: Vec<(u32, u32)> = Vec::new();
    let mut pending_deaths: Vec<u32> = Vec::new();
    for c in 0..=max_color {
        let mut pairs_in_color: Vec<&Pair> = contacts
            .iter()
            .enumerate()
            .filter(|(i, _)| colors[*i] == c)
            .map(|(_, p)| p)
            .collect();
        // contacts is already sorted by (t, a, b); preserve order within color.
        pairs_in_color.sort_by(|p, q| (p.t, p.a, p.b).partial_cmp(&(q.t, q.a, q.b)).unwrap());
        for pair in pairs_in_color {
            let mut ctx = resolve::ResolveContext {
                pool,
                chem,
                grid,
                bonds,
                pending_bonds: &mut pending_bonds,
                pending_deaths: &mut pending_deaths,
            };
            resolve::resolve_pair(pair, &mut ctx);
        }
    }
    advance_all(pool, grid, dt_sub);
    pending_bonds.sort_unstable();
    pending_bonds.dedup();
    for pair in pending_bonds {
        bonds.insert(pair);
    }
    for slot in pending_deaths {
        pool.free(slot);
        bonds.retain(|&(a, b)| a != slot && b != slot);
    }
    enforce_bonds(pool, grid, bonds);
    clear_substep_flags(pool);
}

fn advance_all(pool: &mut BeadPool, grid: &Grid, dt_sub: f32) {
    let slots: Vec<u32> = pool.alive_slots().collect();
    for slot in slots {
        let b = pool.get_mut(slot);
        if b.born_this_substep {
            continue;
        }
        let new_pos = b.pos + b.vel * dt_sub;
        b.pos = grid.wrap_pos(new_pos);
    }
}

fn clear_substep_flags(pool: &mut BeadPool) {
    for b in pool.beads_mut() {
        b.born_this_substep = false;
    }
}

pub fn compute_active_contacts(pool: &BeadPool, grid: &mut Grid, dt_sub: f32) -> Vec<Pair> {
    grid.clear();
    for slot in pool.alive_slots() {
        if pool.get(slot).born_this_substep {
            continue;
        }
        grid.insert(slot, pool.get(slot).pos);
    }
    let candidates = grid.candidate_pairs();
    let mut out = Vec::with_capacity(candidates.len());
    for (a, b) in candidates {
        let ba = pool.get(a);
        let bb = pool.get(b);
        if !ba.alive || !bb.alive {
            continue;
        }
        if ba.born_this_substep || bb.born_this_substep {
            continue;
        }
        let pb = ba.pos + grid.min_image(ba.pos, bb.pos);
        if let Some(c) = next_contact(ba.pos, ba.vel, pb, bb.vel, dt_sub) {
            out.push(Pair { a, b, t: c.t });
        }
    }
    // Stable ordering by (t, a, b) so coloring + resolve are deterministic.
    out.sort_by(|p, q| (p.t, p.a, p.b).partial_cmp(&(q.t, q.a, q.b)).unwrap());
    out
}

/// Rayon-parallel version of `compute_active_contacts`. Bit-identical to
/// the sequential form: `par_iter().filter_map().collect()` preserves
/// source order, and `next_contact` is a pure function. The grid build
/// stays sequential — it's O(N) bin-insertion, cheap relative to the TOI
/// loop, and the grid's mutable shape makes it the awkward piece to
/// parallelize.
#[cfg(not(target_arch = "wasm32"))]
pub fn compute_active_contacts_par(pool: &BeadPool, grid: &mut Grid, dt_sub: f32) -> Vec<Pair> {
    use rayon::prelude::*;
    grid.clear();
    for slot in pool.alive_slots() {
        if pool.get(slot).born_this_substep {
            continue;
        }
        grid.insert(slot, pool.get(slot).pos);
    }
    let candidates = grid.candidate_pairs();
    let mut out: Vec<Pair> = candidates
        .par_iter()
        .filter_map(|&(a, b)| {
            let ba = pool.get(a);
            let bb = pool.get(b);
            if !ba.alive || !bb.alive {
                return None;
            }
            if ba.born_this_substep || bb.born_this_substep {
                return None;
            }
            let pb = ba.pos + grid.min_image(ba.pos, bb.pos);
            next_contact(ba.pos, ba.vel, pb, bb.vel, dt_sub).map(|c| Pair { a, b, t: c.t })
        })
        .collect();
    out.sort_by(|p, q| (p.t, p.a, p.b).partial_cmp(&(q.t, q.a, q.b)).unwrap());
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chemistry::{Op, Tag};
    use crate::parallel::{Bead, BeadPool, STACK_CAP};
    use glam::Vec2;

    fn place(pool: &mut BeadPool, pos: Vec2, vel: Vec2) -> u32 {
        pool.alloc(Bead {
            pos,
            vel,
            tag: Tag::Wire,
            payload: 0,
            alive: true,
            born_this_substep: false,
            stack_len: 0,
            stack: [Op::nop(); STACK_CAP],
        })
    }

    // Note: substep contract assumes dt_sub is small enough that beads travel
    // less than one grid cell (CELL_SIZE = 2*RADIUS) per substep. These tests
    // place beads in adjacent cells with a short dt to honour that invariant.

    #[test]
    fn two_beads_on_collision_course_produce_one_contact() {
        let mut pool = BeadPool::with_capacity(4);
        place(&mut pool, Vec2::new(15.0, 15.0), Vec2::new(1.0, 0.0));
        place(&mut pool, Vec2::new(16.5, 15.0), Vec2::new(-1.0, 0.0));
        let mut grid = Grid::new(30.0);
        let contacts = compute_active_contacts(&pool, &mut grid, 1.0);
        assert_eq!(contacts.len(), 1);
        // Contact when |d| = 1 (RADIUS): start 1.5 apart, closing at 2/s → t = 0.25.
        assert!((contacts[0].t - 0.25).abs() < 1e-5);
    }

    #[test]
    fn bonded_pair_stays_within_radius_after_substeps() {
        let mut pool = BeadPool::with_capacity(4);
        let mut stack = [Op::nop(); STACK_CAP];
        stack[0] = Op::sig_legacy(0);
        let a = pool.alloc(Bead {
            pos: Vec2::new(15.0, 14.75),
            vel: Vec2::new(0.0, -1.0),
            tag: Tag::Wire,
            payload: 0,
            alive: true,
            born_this_substep: false,
            stack_len: 1,
            stack,
        });
        let b = pool.alloc(Bead {
            pos: Vec2::new(15.0, 15.25),
            vel: Vec2::new(0.0, 1.0),
            tag: Tag::Wire,
            payload: 0,
            alive: true,
            born_this_substep: false,
            stack_len: 1,
            stack,
        });
        let mut grid = Grid::new(30.0);
        let mut bonds: std::collections::HashSet<(u32, u32)> = Default::default();
        bonds.insert((a.min(b), a.max(b)));
        let chem = {
            let mut c = crate::chemistry::CompiledChemistry::empty();
            let key = crate::chemistry::BeadKey {
                tag: Tag::Wire,
                top_op: Op::sig_legacy(0),
            };
            c.insert_rule(
                key,
                key,
                crate::chemistry::Side::Out,
                crate::chemistry::Rule {
                    kind: crate::chemistry::ReactionKind::Exchange,
                    new_state_a: crate::chemistry::NewState::keep_with(Op::sig_legacy(0)),
                    new_state_b: crate::chemistry::NewState::keep_with(Op::sig_legacy(0)),
                    birth_state: None,
                },
            );
            c.insert_rule(
                key,
                key,
                crate::chemistry::Side::In,
                crate::chemistry::Rule {
                    kind: crate::chemistry::ReactionKind::Exchange,
                    new_state_a: crate::chemistry::NewState::keep_with(Op::sig_legacy(0)),
                    new_state_b: crate::chemistry::NewState::keep_with(Op::sig_legacy(0)),
                    birth_state: None,
                },
            );
            c
        };
        let dt = 1.0 / 60.0;
        let mut max_dist = 0f32;
        for _ in 0..1200 {
            for _ in 0..4 {
                do_substep(&mut pool, &mut grid, &chem, &mut bonds, dt / 4.0);
            }
            let d = (pool.get(a).pos - pool.get(b).pos).length();
            if d > max_dist {
                max_dist = d;
            }
        }
        assert!(
            max_dist <= RADIUS + 1e-3,
            "bond stayed within R + eps; max = {}",
            max_dist
        );
    }

    #[test]
    fn head_on_collision_resolves_in_one_substep() {
        let mut pool = BeadPool::with_capacity(4);
        let mut stack = [Op::nop(); STACK_CAP];
        stack[0] = Op::sig_legacy(0);
        let a = pool.alloc(Bead {
            pos: Vec2::new(15.0, 15.0),
            vel: Vec2::new(1.0, 0.0),
            tag: Tag::Wire,
            payload: 0,
            alive: true,
            born_this_substep: false,
            stack_len: 1,
            stack,
        });
        let b = pool.alloc(Bead {
            pos: Vec2::new(16.5, 15.0),
            vel: Vec2::new(-1.0, 0.0),
            tag: Tag::Wire,
            payload: 0,
            alive: true,
            born_this_substep: false,
            stack_len: 1,
            stack,
        });
        let mut grid = Grid::new(30.0);
        let mut bonds: std::collections::HashSet<(u32, u32)> = Default::default();
        let chem = {
            let mut c = crate::chemistry::CompiledChemistry::empty();
            let key = crate::chemistry::BeadKey {
                tag: Tag::Wire,
                top_op: Op::sig_legacy(0),
            };
            c.insert_rule(
                key,
                key,
                crate::chemistry::Side::Out,
                crate::chemistry::Rule {
                    kind: crate::chemistry::ReactionKind::Exchange,
                    new_state_a: crate::chemistry::NewState::keep_with(Op::sig_legacy(0)),
                    new_state_b: crate::chemistry::NewState::keep_with(Op::sig_legacy(0)),
                    birth_state: None,
                },
            );
            c
        };
        do_substep(&mut pool, &mut grid, &chem, &mut bonds, 1.0);
        assert!((pool.get(a).vel.x - (-1.0)).abs() < 1e-3);
        assert!((pool.get(b).vel.x - 1.0).abs() < 1e-3);
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn parallel_contacts_bit_match_sequential_chain() {
        let mut pool = BeadPool::with_capacity(32);
        let mut stack = [Op::nop(); STACK_CAP];
        stack[0] = Op::sig_legacy(0);
        for i in 0..30 {
            pool.alloc(Bead {
                pos: Vec2::new(15.0, 5.0 + i as f32 * 0.667),
                vel: Vec2::new(0.0, if i % 2 == 0 { 0.5 } else { -0.5 }),
                tag: Tag::Wire,
                payload: 0,
                alive: true,
                born_this_substep: false,
                stack_len: 1,
                stack,
            });
        }
        let mut grid_a = Grid::new(30.0);
        let mut grid_b = Grid::new(30.0);
        let seq = compute_active_contacts(&pool, &mut grid_a, 1.0 / 240.0);
        let par = compute_active_contacts_par(&pool, &mut grid_b, 1.0 / 240.0);
        assert_eq!(par.len(), seq.len());
        for (a, b) in par.iter().zip(seq.iter()) {
            assert_eq!(a.a, b.a);
            assert_eq!(a.b, b.b);
            assert_eq!(a.t.to_bits(), b.t.to_bits(), "TOI must bit-match");
        }
    }

    #[test]
    fn newborn_excluded_from_contacts() {
        let mut pool = BeadPool::with_capacity(4);
        let _ = place(&mut pool, Vec2::new(15.0, 15.0), Vec2::new(1.0, 0.0));
        let i1 = place(&mut pool, Vec2::new(16.5, 15.0), Vec2::new(-1.0, 0.0));
        pool.get_mut(i1).born_this_substep = true;
        let mut grid = Grid::new(30.0);
        let contacts = compute_active_contacts(&pool, &mut grid, 1.0);
        assert!(
            contacts.is_empty(),
            "born-this-substep bead does not produce contacts"
        );
    }
}
