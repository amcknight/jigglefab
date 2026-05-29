use std::collections::HashSet;
use std::sync::atomic::Ordering;

use crate::bond::BondPair;
use crate::ccd::{next_contact, RADIUS};
use crate::chemistry::CompiledChemistry;
use crate::collide::reflect;
use crate::grid::Grid;
use crate::parallel::profile;
use crate::parallel::{coloring, resolve, BeadPool, Pair};
use crate::time_phase;

const BOND_EPS: f32 = 1e-5;

// Pull any bonded pair that drifted past R back inside, and flip their
// normal velocity if it was still outward. Matches Sim::enforce_bonds:
// without this, a bonded pair nudged across R by a sibling pair's snap
// is invisible to the next substep's CCD and would drift apart forever.
pub fn enforce_bonds(pool: &mut BeadPool, grid: &Grid, bonds: &HashSet<BondPair>) {
    let pairs: Vec<BondPair> = bonds.iter().copied().collect();
    for bond in pairs {
        let (a, b) = (bond.lo(), bond.hi());
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

/// Rayon-parallel version of `do_substep`. Calls the `_par` helpers for
/// compute_active_contacts, resolve_color, and advance_all. Sort + greedy
/// coloring stay sequential (coloring is inherently order-dependent).
/// enforce_bonds stays sequential — bonds can share beads, and the cost
/// is O(bonds) which is fine.
///
/// Bit-identical to do_substep on grey/wire/chain workloads (no Birth) —
/// verified across 30 substeps by do_substep_mt_bit_matches_do_substep.
#[cfg(not(target_arch = "wasm32"))]
pub fn do_substep_mt(
    pool: &mut BeadPool,
    grid: &mut Grid,
    chem: &CompiledChemistry,
    bonds: &mut HashSet<BondPair>,
    dt_sub: f32,
) {
    let contacts = compute_active_contacts_par(pool, grid, dt_sub);
    if contacts.is_empty() {
        advance_all_par(pool, grid, dt_sub);
        clear_substep_flags(pool);
        return;
    }
    let colors = coloring::color_pairs(&contacts);
    let max_color = colors.iter().copied().max().unwrap_or(0);
    let mut pending_bonds: Vec<BondPair> = Vec::new();
    let mut pending_deaths: Vec<u32> = Vec::new();
    for c in 0..=max_color {
        let mut pairs_in_color: Vec<Pair> = contacts
            .iter()
            .enumerate()
            .filter_map(|(i, p)| if colors[i] == c { Some(*p) } else { None })
            .collect();
        pairs_in_color.sort_by(|p, q| (p.t, p.a, p.b).partial_cmp(&(q.t, q.a, q.b)).unwrap());
        resolve_color_par(
            &pairs_in_color, pool, chem, grid, bonds,
            &mut pending_bonds, &mut pending_deaths,
        );
    }
    advance_all_par(pool, grid, dt_sub);
    pending_bonds.sort_unstable();
    pending_bonds.dedup();
    for pair in pending_bonds {
        bonds.insert(pair);
    }
    for slot in pending_deaths {
        pool.free(slot);
        bonds.retain(|bp| !bp.contains(slot));
    }
    enforce_bonds(pool, grid, bonds);
    clear_substep_flags(pool);
}

pub fn do_substep(
    pool: &mut BeadPool,
    grid: &mut Grid,
    chem: &CompiledChemistry,
    bonds: &mut HashSet<BondPair>,
    dt_sub: f32,
) {
    let contacts = time_phase!(
        profile::CONTACTS_NS,
        compute_active_contacts(pool, grid, dt_sub)
    );
    if contacts.is_empty() {
        time_phase!(profile::ADVANCE_NS, advance_all(pool, grid, dt_sub));
        clear_substep_flags(pool);
        profile::SUBSTEPS.fetch_add(1, Ordering::Relaxed);
        return;
    }
    let colors = time_phase!(profile::COLOR_NS, coloring::color_pairs(&contacts));
    let max_color = colors.iter().copied().max().unwrap_or(0);
    let mut pending_bonds: Vec<BondPair> = Vec::new();
    let mut pending_deaths: Vec<u32> = Vec::new();
    time_phase!(profile::RESOLVE_NS, {
        for c in 0..=max_color {
            let mut pairs_in_color: Vec<&Pair> = contacts
                .iter()
                .enumerate()
                .filter(|(i, _)| colors[*i] == c)
                .map(|(_, p)| p)
                .collect();
            // contacts is already sorted by (t, a, b); preserve order within color.
            pairs_in_color
                .sort_by(|p, q| (p.t, p.a, p.b).partial_cmp(&(q.t, q.a, q.b)).unwrap());
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
    });
    time_phase!(profile::ADVANCE_NS, advance_all(pool, grid, dt_sub));
    pending_bonds.sort_unstable();
    pending_bonds.dedup();
    for pair in pending_bonds {
        bonds.insert(pair);
    }
    for slot in pending_deaths {
        pool.free(slot);
        grid.remove_bead(slot);
        bonds.retain(|bp| !bp.contains(slot));
    }
    time_phase!(profile::BONDS_NS, enforce_bonds(pool, grid, bonds));
    clear_substep_flags(pool);
    profile::SUBSTEPS.fetch_add(1, Ordering::Relaxed);
}

pub(crate) fn advance_all(pool: &mut BeadPool, grid: &Grid, dt_sub: f32) {
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

/// Rayon-parallel advance: each bead is independent (no cross-bead reads),
/// so par_iter_mut over the pool's bead slice bit-matches the sequential
/// version. Both alive_slots ordering and the wrap_pos function are
/// deterministic, so output is identical regardless of thread schedule.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn advance_all_par(pool: &mut BeadPool, grid: &Grid, dt_sub: f32) {
    use rayon::prelude::*;
    pool.beads_mut().par_iter_mut().for_each(|b| {
        if !b.alive || b.born_this_substep {
            return;
        }
        let new_pos = b.pos + b.vel * dt_sub;
        b.pos = grid.wrap_pos(new_pos);
    });
}

pub(crate) fn clear_substep_flags(pool: &mut BeadPool) {
    for b in pool.beads_mut() {
        b.born_this_substep = false;
    }
}

pub fn compute_active_contacts(pool: &BeadPool, grid: &mut Grid, dt_sub: f32) -> Vec<Pair> {
    time_phase!(profile::CT_BIN_NS, {
        // Incremental binning: most beads stay in their cell across a substep
        // (at dt_sub=1/240 with unit speed, a bead moves 0.004 units in a cell
        // 2 units wide). update_position short-circuits when the cell hasn't
        // changed, so this loop is dominated by the alive_slots iteration
        // itself rather than per-bead insert work.
        for slot in pool.alive_slots() {
            if pool.get(slot).born_this_substep {
                continue;
            }
            grid.update_position(slot, pool.get(slot).pos);
        }
    });
    let candidates = time_phase!(profile::CT_CANDIDATES_NS, grid.candidate_pairs());
    let mut out = time_phase!(profile::CT_CCD_NS, {
        let mut pairs = Vec::with_capacity(candidates.len());
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
                pairs.push(Pair { a, b, t: c.t });
            }
        }
        pairs
    });
    // Stable ordering by (t, a, b) so coloring + resolve are deterministic.
    time_phase!(
        profile::CT_SORT_NS,
        out.sort_by(|p, q| (p.t, p.a, p.b).partial_cmp(&(q.t, q.a, q.b)).unwrap())
    );
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

/// Resolve every pair in a single color in parallel. Caller must guarantee
/// no two pairs in `pairs_in_color` share a bead slot — this is the
/// graph-coloring invariant from `coloring::color_pairs`. Pending bonds
/// and deaths are gathered per pair (rayon collect preserves source order),
/// then flattened in that deterministic order.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn resolve_color_par(
    pairs_in_color: &[Pair],
    pool: &mut BeadPool,
    chem: &CompiledChemistry,
    grid: &Grid,
    bonds: &HashSet<BondPair>,
    pending_bonds: &mut Vec<BondPair>,
    pending_deaths: &mut Vec<u32>,
) {
    use rayon::prelude::*;
    use sync_beads_impl::SyncBeads;
    use std::cell::UnsafeCell;

    let beads_slice = pool.beads_mut();
    // Reinterpret &mut [Bead] as &[UnsafeCell<Bead>]. SAFETY: layout-
    // compatible because UnsafeCell<T> is #[repr(transparent)] over T,
    // and we relinquish exclusive access by going through UnsafeCell.
    let beads_cell: &[UnsafeCell<crate::parallel::Bead>] = unsafe {
        std::slice::from_raw_parts(
            beads_slice.as_ptr() as *const UnsafeCell<crate::parallel::Bead>,
            beads_slice.len(),
        )
    };
    let sync = SyncBeads(beads_cell);

    let per_pair: Vec<(Vec<BondPair>, Vec<u32>)> = pairs_in_color
        .par_iter()
        .map(|pair| {
            let mut pb: Vec<BondPair> = Vec::new();
            let mut pd: Vec<u32> = Vec::new();
            resolve_pair_disjoint(pair, &sync, chem, grid, bonds, &mut pb, &mut pd);
            (pb, pd)
        })
        .collect();

    for (pb, pd) in per_pair {
        pending_bonds.extend(pb);
        pending_deaths.extend(pd);
    }
}

/// Disjoint-access version of resolve_pair: takes two slot indices into a
/// SyncBeads slice and uses UnsafeCell for interior mutation. SAFETY is
/// delegated to the caller, who must ensure no other thread touches these
/// two slots concurrently (true within a color by construction).
#[cfg(not(target_arch = "wasm32"))]
fn resolve_pair_disjoint(
    pair: &Pair,
    beads: &sync_beads_impl::SyncBeads<'_>,
    chem: &CompiledChemistry,
    grid: &Grid,
    bonds: &HashSet<BondPair>,
    pending_bonds: &mut Vec<BondPair>,
    pending_deaths: &mut Vec<u32>,
) {
    use crate::chemistry::{BeadKey, NewState, ReactionKind, Rule, Side};

    let (a, b) = (pair.a, pair.b);
    // SAFETY: caller guarantees disjoint access; pair.a != pair.b (pairs
    // come from grid.candidate_pairs which only emits unordered pairs).
    let ba = unsafe { beads.read(a) };
    let bb = unsafe { beads.read(b) };
    let pa = ba.pos;
    let pb = ba.pos + grid.min_image(ba.pos, bb.pos);
    let bonded = bonds.contains(&BondPair::new(a, b));
    let exiting = (pb - pa).dot(bb.vel - ba.vel) > 0.0;
    let side = if bonded { Side::In } else { Side::Out };
    let effective_side = if bonded == exiting { side } else { Side::Out };
    let key_a = BeadKey { tag: ba.tag, top_op: ba.top_op() };
    let key_b = BeadKey { tag: bb.tag, top_op: bb.top_op() };
    let rule = if bonded == exiting {
        chem.lookup(key_a, key_b, side)
    } else {
        Rule {
            kind: ReactionKind::Passthrough,
            new_state_a: NewState::keep_with(ba.top_op()),
            new_state_b: NewState::keep_with(bb.top_op()),
            birth_state: None,
        }
    };

    const BOUNDARY_EPS: f32 = 1e-5;

    // Inline snap: mutates both beads through the SyncBeads. Order
    // matches resolve.rs::snap_back (compute target, correct both ends).
    let snap = |beads: &sync_beads_impl::SyncBeads<'_>, a: u32, b: u32, post_state_inside: bool| {
        unsafe {
            let pa = beads.read(a).pos;
            let pb_raw = beads.read(b).pos;
            let pb = pa + grid.min_image(pa, pb_raw);
            let d = pb - pa;
            let dist = d.length();
            if dist <= 1e-12 {
                return;
            }
            let target = if post_state_inside { RADIUS - BOUNDARY_EPS } else { RADIUS + BOUNDARY_EPS };
            let correction = (target - dist) * 0.5;
            let n = d / dist;
            beads.write_with(a, |ba| ba.pos = grid.wrap_pos(pa - n * correction));
            beads.write_with(b, |bb| bb.pos = grid.wrap_pos(bb.pos + n * correction));
        }
    };

    match rule.kind {
        ReactionKind::Exchange => {
            let (va_new, vb_new) = reflect(pa, ba.vel, pb, bb.vel);
            unsafe {
                beads.write_with(a, |ba| ba.vel = va_new);
                beads.write_with(b, |bb| bb.vel = vb_new);
                beads.write_with(a, |ba| apply_new_state_inline(ba, &rule.new_state_a, chem));
                beads.write_with(b, |bb| apply_new_state_inline(bb, &rule.new_state_b, chem));
            }
            snap(beads, a, b, exiting);
        }
        ReactionKind::Passthrough => {
            unsafe {
                beads.write_with(a, |ba| apply_new_state_inline(ba, &rule.new_state_a, chem));
                beads.write_with(b, |bb| apply_new_state_inline(bb, &rule.new_state_b, chem));
            }
            snap(beads, a, b, !exiting);
        }
        ReactionKind::LeftOnly => {
            let combined_vel = ba.vel + bb.vel;
            unsafe {
                beads.write_with(a, |ba| ba.vel = combined_vel);
                beads.write_with(a, |ba| apply_new_state_inline(ba, &rule.new_state_a, chem));
            }
            pending_deaths.push(b);
        }
        ReactionKind::RightOnly => {
            let combined_vel = ba.vel + bb.vel;
            unsafe {
                beads.write_with(b, |bb| bb.pos = pa);
                beads.write_with(b, |bb| bb.vel = combined_vel);
                beads.write_with(b, |bb| apply_new_state_inline(bb, &rule.new_state_b, chem));
            }
            pending_deaths.push(a);
        }
        ReactionKind::Birth => {
            let _ = effective_side;
            // Birth needs pool allocation, which isn't safe from a rayon
            // worker. Sem-style births still work end-to-end through
            // CpuParallel (single-threaded). Until pool alloc is mutex-
            // wrapped, the MT path panics rather than silently dropping
            // the birth.
            panic!(
                "ReactionKind::Birth in resolve_pair_disjoint is not supported; \
                 use CpuParallel (single-threaded) for sem chemistries until \
                 pool allocation is mutex-wrapped on the MT path"
            );
        }
    }

    let _ = pending_bonds; // unused unless Birth fires
}

#[cfg(not(target_arch = "wasm32"))]
mod sync_beads_impl {
    use std::cell::UnsafeCell;

    /// Wrapper around a slice of UnsafeCells. The `Sync` marker is the
    /// caller's safety contract (see resolve_color_par): no two threads
    /// may read or write the same slot concurrently.
    pub(super) struct SyncBeads<'a>(pub &'a [UnsafeCell<crate::parallel::Bead>]);
    unsafe impl<'a> Sync for SyncBeads<'a> {}

    impl<'a> SyncBeads<'a> {
        /// SAFETY: caller guarantees no concurrent access to `slot`.
        pub(super) unsafe fn read(&self, slot: u32) -> crate::parallel::Bead {
            *self.0[slot as usize].get()
        }
        /// SAFETY: caller guarantees no concurrent access to `slot`.
        pub(super) unsafe fn write_with<F: FnOnce(&mut crate::parallel::Bead)>(&self, slot: u32, f: F) {
            f(&mut *self.0[slot as usize].get());
        }
    }
}

fn apply_new_state_inline(
    bead: &mut crate::parallel::Bead,
    ns: &crate::chemistry::NewState,
    chem: &CompiledChemistry,
) {
    use crate::chemistry::NewState;
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
        let mut bonds: std::collections::HashSet<BondPair> = Default::default();
        bonds.insert(BondPair::new(a, b));
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
        let mut bonds: std::collections::HashSet<BondPair> = Default::default();
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
    fn do_substep_mt_bit_matches_do_substep() {
        use std::collections::HashSet;
        fn build_chain() -> (BeadPool, HashSet<BondPair>) {
            let mut pool = BeadPool::with_capacity(64);
            let mut stack = [Op::nop(); STACK_CAP];
            stack[0] = Op::sig_legacy(0);
            for i in 0..30u32 {
                pool.alloc(Bead {
                    pos: Vec2::new(15.0, 5.0 + i as f32 * 0.667),
                    vel: Vec2::new(0.0, if i % 2 == 0 { 0.4 } else { -0.4 }),
                    tag: Tag::Wire,
                    payload: 0,
                    alive: true,
                    born_this_substep: false,
                    stack_len: 1,
                    stack,
                });
            }
            let mut bonds = HashSet::new();
            for i in 0..29u32 {
                bonds.insert(BondPair::new(i, i + 1));
            }
            (pool, bonds)
        }
        let (mut pool_seq, mut bonds_seq) = build_chain();
        let (mut pool_par, mut bonds_par) = build_chain();
        let chem = {
            let mut c = crate::chemistry::CompiledChemistry::empty();
            let key = crate::chemistry::BeadKey {
                tag: Tag::Wire,
                top_op: Op::sig_legacy(0),
            };
            let rule = crate::chemistry::Rule {
                kind: crate::chemistry::ReactionKind::Exchange,
                new_state_a: crate::chemistry::NewState::keep_with(Op::sig_legacy(0)),
                new_state_b: crate::chemistry::NewState::keep_with(Op::sig_legacy(0)),
                birth_state: None,
            };
            c.insert_rule(key, key, crate::chemistry::Side::Out, rule.clone());
            c.insert_rule(key, key, crate::chemistry::Side::In, rule);
            c
        };
        let mut grid_seq = Grid::new(30.0);
        let mut grid_par = Grid::new(30.0);
        let dt = 1.0 / 240.0;
        for _ in 0..30 {
            do_substep(&mut pool_seq, &mut grid_seq, &chem, &mut bonds_seq, dt);
            do_substep_mt(&mut pool_par, &mut grid_par, &chem, &mut bonds_par, dt);
        }
        for slot in 0..30u32 {
            let a = pool_seq.get(slot);
            let b = pool_par.get(slot);
            assert_eq!(a.pos.x.to_bits(), b.pos.x.to_bits(), "slot {slot} pos.x");
            assert_eq!(a.pos.y.to_bits(), b.pos.y.to_bits(), "slot {slot} pos.y");
            assert_eq!(a.vel.x.to_bits(), b.vel.x.to_bits(), "slot {slot} vel.x");
            assert_eq!(a.vel.y.to_bits(), b.vel.y.to_bits(), "slot {slot} vel.y");
        }
        assert_eq!(bonds_seq, bonds_par);
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn parallel_resolve_color_bit_matches_sequential() {
        use std::collections::HashSet;
        fn build_chain() -> (BeadPool, HashSet<BondPair>) {
            let mut pool = BeadPool::with_capacity(4);
            let mut stack = [Op::nop(); STACK_CAP];
            stack[0] = Op::sig_legacy(0);
            for i in 0..4u32 {
                pool.alloc(Bead {
                    pos: Vec2::new(15.0, 14.0 + i as f32 * 0.95),
                    vel: Vec2::new(0.0, if i % 2 == 0 { 0.4 } else { -0.4 }),
                    tag: Tag::Wire,
                    payload: 0,
                    alive: true,
                    born_this_substep: false,
                    stack_len: 1,
                    stack,
                });
            }
            let mut bonds = HashSet::new();
            bonds.insert(BondPair::new(0, 1));
            bonds.insert(BondPair::new(2, 3));
            (pool, bonds)
        }
        let (mut pool_seq, bonds_seq) = build_chain();
        let (mut pool_par, bonds_par) = build_chain();
        let chem = {
            let mut c = crate::chemistry::CompiledChemistry::empty();
            let key = crate::chemistry::BeadKey {
                tag: Tag::Wire,
                top_op: Op::sig_legacy(0),
            };
            let rule = crate::chemistry::Rule {
                kind: crate::chemistry::ReactionKind::Exchange,
                new_state_a: crate::chemistry::NewState::keep_with(Op::sig_legacy(0)),
                new_state_b: crate::chemistry::NewState::keep_with(Op::sig_legacy(0)),
                birth_state: None,
            };
            c.insert_rule(key, key, crate::chemistry::Side::Out, rule.clone());
            c.insert_rule(key, key, crate::chemistry::Side::In, rule);
            c
        };
        let grid = Grid::new(30.0);
        let pairs = vec![
            Pair { a: 0, b: 1, t: 0.0 },
            Pair { a: 2, b: 3, t: 0.0 },
        ];

        // Sequential: existing resolve_pair loop
        let mut pb_seq: Vec<BondPair> = Vec::new();
        let mut pd_seq: Vec<u32> = Vec::new();
        for p in &pairs {
            let mut ctx = crate::parallel::resolve::ResolveContext {
                pool: &mut pool_seq,
                chem: &chem,
                grid: &grid,
                bonds: &bonds_seq,
                pending_bonds: &mut pb_seq,
                pending_deaths: &mut pd_seq,
            };
            crate::parallel::resolve::resolve_pair(p, &mut ctx);
        }

        // Parallel: resolve_color_par
        let mut pb_par: Vec<BondPair> = Vec::new();
        let mut pd_par: Vec<u32> = Vec::new();
        resolve_color_par(
            &pairs, &mut pool_par, &chem, &grid, &bonds_par,
            &mut pb_par, &mut pd_par,
        );

        for slot in 0..4u32 {
            let a = pool_seq.get(slot);
            let b = pool_par.get(slot);
            assert_eq!(a.pos.x.to_bits(), b.pos.x.to_bits(), "slot {slot} pos.x");
            assert_eq!(a.pos.y.to_bits(), b.pos.y.to_bits(), "slot {slot} pos.y");
            assert_eq!(a.vel.x.to_bits(), b.vel.x.to_bits(), "slot {slot} vel.x");
            assert_eq!(a.vel.y.to_bits(), b.vel.y.to_bits(), "slot {slot} vel.y");
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn parallel_advance_bit_matches_sequential() {
        let mut pool_a = BeadPool::with_capacity(32);
        let mut pool_b = BeadPool::with_capacity(32);
        let mut stack = [Op::nop(); STACK_CAP];
        stack[0] = Op::sig_legacy(0);
        for i in 0..30 {
            let bead = Bead {
                pos: Vec2::new(15.0 + (i % 5) as f32, 5.0 + (i / 5) as f32 * 0.667),
                vel: Vec2::new(0.3, -0.7),
                tag: Tag::Wire,
                payload: 0,
                alive: true,
                born_this_substep: false,
                stack_len: 1,
                stack,
            };
            pool_a.alloc(bead);
            pool_b.alloc(bead);
        }
        pool_a.get_mut(5).born_this_substep = true;
        pool_b.get_mut(5).born_this_substep = true;
        let grid = Grid::new(30.0);
        let dt = 1.0 / 240.0;
        advance_all(&mut pool_a, &grid, dt);
        advance_all_par(&mut pool_b, &grid, dt);
        for slot in 0..30u32 {
            let a = pool_a.get(slot);
            let b = pool_b.get(slot);
            assert_eq!(a.pos.x.to_bits(), b.pos.x.to_bits(), "slot {slot} pos.x");
            assert_eq!(a.pos.y.to_bits(), b.pos.y.to_bits(), "slot {slot} pos.y");
        }
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
