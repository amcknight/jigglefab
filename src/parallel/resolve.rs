use glam::Vec2;
use std::collections::HashSet;

use crate::bond::BondPair;
use crate::ccd::RADIUS;
use crate::chemistry::{BeadKey, CompiledChemistry, NewState, Op, ReactionKind, Rule, Side};
use crate::collide::reflect;
use crate::grid::Grid;
use crate::parallel::{Bead, BeadPool, Pair, STACK_CAP};

const BOUNDARY_EPS: f32 = 1e-5;

pub struct ResolveContext<'a> {
    pub pool: &'a mut BeadPool,
    pub chem: &'a CompiledChemistry,
    pub grid: &'a Grid,
    pub bonds: &'a HashSet<BondPair>,
    pub pending_bonds: &'a mut Vec<BondPair>,
    pub pending_deaths: &'a mut Vec<u32>,
}

pub fn resolve_pair(pair: &Pair, ctx: &mut ResolveContext) {
    let (a, b) = (pair.a, pair.b);
    let ba = *ctx.pool.get(a);
    let bb = *ctx.pool.get(b);
    let pa = ba.pos;
    let pb = ba.pos + ctx.grid.min_image(ba.pos, bb.pos);
    let bonded = is_bonded(ctx.bonds, a, b);

    // Geometric direction: sign of d|d|²/dt = (pb-pa)·(vb-va). When positive
    // the pair is moving apart at the contact = exiting; when negative they
    // are approaching = entering.
    let exiting = (pb - pa).dot(bb.vel - ba.vel) > 0.0;

    let side = if bonded { Side::In } else { Side::Out };
    // Drift correction: if (bonded, exiting) disagree (e.g. bonded pair has
    // drifted past R and is now exiting from "outside"), force Passthrough to
    // restore topology without spuriously changing velocities.
    let effective_side = if bonded == exiting { side } else { Side::Out };

    let key_a = BeadKey {
        tag: ba.tag,
        top_op: ba.top_op(),
    };
    let key_b = BeadKey {
        tag: bb.tag,
        top_op: bb.top_op(),
    };
    let rule = if bonded == exiting {
        ctx.chem.lookup(key_a, key_b, side)
    } else {
        Rule {
            kind: ReactionKind::Passthrough,
            new_state_a: NewState::keep_with(ba.top_op()),
            new_state_b: NewState::keep_with(bb.top_op()),
            birth_state: None,
        }
    };

    match rule.kind {
        ReactionKind::Exchange => {
            let (va_new, vb_new) = reflect(pa, ba.vel, pb, bb.vel);
            ctx.pool.get_mut(a).vel = va_new;
            ctx.pool.get_mut(b).vel = vb_new;
            apply_new_state(ctx.pool.get_mut(a), &rule.new_state_a, ctx.chem);
            apply_new_state(ctx.pool.get_mut(b), &rule.new_state_b, ctx.chem);
            // Exchange = bounce: pair stays on the side it came from.
            snap_back(ctx.pool, a, b, ctx.grid, exiting);
        }
        ReactionKind::Passthrough => {
            apply_new_state(ctx.pool.get_mut(a), &rule.new_state_a, ctx.chem);
            apply_new_state(ctx.pool.get_mut(b), &rule.new_state_b, ctx.chem);
            // Passthrough = continue: pair ends on the opposite side.
            snap_back(ctx.pool, a, b, ctx.grid, !exiting);
        }
        ReactionKind::LeftOnly => {
            // Right bead dies; left inherits combined velocity (momentum).
            let combined_vel = ba.vel + bb.vel;
            ctx.pool.get_mut(a).vel = combined_vel;
            apply_new_state(ctx.pool.get_mut(a), &rule.new_state_a, ctx.chem);
            ctx.pending_deaths.push(b);
        }
        ReactionKind::RightOnly => {
            let combined_vel = ba.vel + bb.vel;
            ctx.pool.get_mut(b).pos = pa;
            ctx.pool.get_mut(b).vel = combined_vel;
            apply_new_state(ctx.pool.get_mut(b), &rule.new_state_b, ctx.chem);
            ctx.pending_deaths.push(a);
        }
        ReactionKind::Birth => {
            // Existing pair bounces (when effectively inside); new bead at midpoint.
            if effective_side == Side::In {
                let (va_new, vb_new) = reflect(pa, ba.vel, pb, bb.vel);
                ctx.pool.get_mut(a).vel = va_new;
                ctx.pool.get_mut(b).vel = vb_new;
            }
            apply_new_state(ctx.pool.get_mut(a), &rule.new_state_a, ctx.chem);
            apply_new_state(ctx.pool.get_mut(b), &rule.new_state_b, ctx.chem);
            let mid = 0.5 * (pa + pb);
            let new_vel = -0.5 * (ba.vel + bb.vel);
            let birth_state = rule
                .birth_state
                .as_ref()
                .expect("Birth reaction must have birth_state");
            let mut new_bead = Bead {
                pos: ctx.grid.wrap_pos(mid),
                vel: new_vel,
                tag: birth_state.tag,
                payload: 0,
                alive: true,
                born_this_substep: true,
                stack_len: 0,
                stack: [Op::nop(); STACK_CAP],
            };
            if let Some(prog_idx) = birth_state.program {
                new_bead.load_program(ctx.chem.program(prog_idx));
            }
            if let Ok(new_slot) = ctx.pool.try_alloc(new_bead) {
                ctx.pending_bonds.push(BondPair::new(a, new_slot));
                ctx.pending_bonds.push(BondPair::new(b, new_slot));
            }
            // Snap parents to the same side they came from so the next substep
            // sees a clean pair.
            snap_back(ctx.pool, a, b, ctx.grid, exiting);
        }
    }
}

fn apply_new_state(bead: &mut Bead, ns: &NewState, chem: &CompiledChemistry) {
    match *ns {
        NewState::KeepWith { top } => {
            if bead.stack_len == 0 {
                bead.stack[0] = top;
                bead.stack_len = 1;
            } else {
                bead.stack[(bead.stack_len - 1) as usize] = top;
            }
        }
        NewState::KeepPopTop => bead.pop_top(),
        NewState::LoadProgram(idx) => bead.load_program(chem.program(idx)),
        NewState::Dead => { /* caller queues death */ }
    }
}

// Snap the pair to |d| = R ± eps on the requested topological side. Matches
// sim.rs's post-resolve snap: pins the pair off the boundary so the next
// CCD pass sees a clean sign on `c`.
fn snap_back(pool: &mut BeadPool, a: u32, b: u32, grid: &Grid, post_state_inside: bool) {
    let pa = pool.get(a).pos;
    let pb_raw = pool.get(b).pos;
    let pb = pa + grid.min_image(pa, pb_raw);
    let d: Vec2 = pb - pa;
    let dist = d.length();
    if dist <= 1e-12 {
        return;
    }
    let target = if post_state_inside {
        RADIUS - BOUNDARY_EPS
    } else {
        RADIUS + BOUNDARY_EPS
    };
    let correction = (target - dist) * 0.5;
    let n = d / dist;
    let new_a = grid.wrap_pos(pa - n * correction);
    let new_b = grid.wrap_pos(pool.get(b).pos + n * correction);
    pool.get_mut(a).pos = new_a;
    pool.get_mut(b).pos = new_b;
}

fn is_bonded(bonds: &HashSet<BondPair>, a: u32, b: u32) -> bool {
    bonds.contains(&BondPair::new(a, b))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chemistry::{BeadKey, CompiledChemistry, Op, ReactionKind, Rule, Tag};
    use crate::parallel::{Bead, BeadPool, Pair, STACK_CAP};
    use glam::Vec2;

    fn make_grey_chem() -> CompiledChemistry {
        let mut chem = CompiledChemistry::empty();
        let key = BeadKey {
            tag: Tag::Wire,
            top_op: Op::sig_legacy(0),
        };
        let rule = Rule {
            kind: ReactionKind::Exchange,
            new_state_a: NewState::keep_with(Op::sig_legacy(0)),
            new_state_b: NewState::keep_with(Op::sig_legacy(0)),
            birth_state: None,
        };
        chem.insert_rule(key, key, Side::Out, rule.clone());
        chem.insert_rule(key, key, Side::In, rule);
        chem
    }

    fn place(pool: &mut BeadPool, pos: Vec2, vel: Vec2) -> u32 {
        let mut stack = [Op::nop(); STACK_CAP];
        stack[0] = Op::sig_legacy(0);
        pool.alloc(Bead {
            pos,
            vel,
            tag: Tag::Wire,
            payload: 0,
            alive: true,
            born_this_substep: false,
            stack_len: 1,
            stack,
        })
    }

    #[test]
    fn exchange_reflects_velocities() {
        let mut pool = BeadPool::with_capacity(4);
        let a = place(&mut pool, Vec2::new(15.0, 15.0), Vec2::new(1.0, 0.0));
        let b = place(&mut pool, Vec2::new(17.0, 15.0), Vec2::new(-1.0, 0.0));
        let chem = make_grey_chem();
        let grid = Grid::new(30.0);
        let bonds: HashSet<BondPair> = Default::default();
        let pair = Pair { a, b, t: 1.0 };
        let mut pending_bonds = Vec::new();
        let mut pending_deaths = Vec::new();
        let mut ctx = ResolveContext {
            pool: &mut pool,
            chem: &chem,
            grid: &grid,
            bonds: &bonds,
            pending_bonds: &mut pending_bonds,
            pending_deaths: &mut pending_deaths,
        };
        resolve_pair(&pair, &mut ctx);
        assert!((ctx.pool.get(a).vel.x - (-1.0)).abs() < 1e-3);
        assert!((ctx.pool.get(b).vel.x - 1.0).abs() < 1e-3);
    }
}
