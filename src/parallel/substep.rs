use crate::ccd::next_contact;
use crate::grid::Grid;
use crate::parallel::{BeadPool, Pair};

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
