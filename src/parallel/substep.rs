use std::collections::HashSet;

use crate::ccd::next_contact;
use crate::chemistry::CompiledChemistry;
use crate::grid::Grid;
use crate::parallel::{coloring, resolve, BeadPool, Pair};

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
