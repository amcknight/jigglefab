// End-to-end exercise of the compiled sem_basic chemistry under CpuParallel's
// substep loop. Phase 1 shipped the parser, the CompiledChemistry, the rule
// table, and the Birth/LeftOnly branches of resolve_pair — but no scenario
// actually drove a Birth or Die through the pipeline. These tests close that
// loop: a bonded pair carrying Apply+Die or Apply+Spawn on their stacks is
// run through do_substep until the bond's natural rebound at |d|=R fires the
// rule. We assert on the pool's population, bond set, and per-bead stack.
//
// The harness operates below the Sim/CpuParallel boundary on purpose: that
// boundary (sim_to_slot growing/shrinking on births/deaths) is genuinely
// Phase 2 work per the retro. The pool-level pipeline IS Phase 1's, and
// that's what we want to prove out before building anything on top.

use std::collections::HashSet;

use glam::Vec2;
use jigglefab::chemistry::{parse_sem_chemistry, CompiledChemistry, Op, OpKind, Tag};
use jigglefab::grid::Grid;
use jigglefab::parallel::substep::do_substep;
use jigglefab::parallel::{Bead, BeadPool, STACK_CAP};

const DT_SUB: f32 = 1.0 / 240.0;
const WORLD: f32 = 30.0;

fn load_sem_basic() -> CompiledChemistry {
    let text = std::fs::read_to_string("chemistries/sem_basic.toml")
        .expect("read chemistries/sem_basic.toml");
    parse_sem_chemistry(&text).expect("compile sem_basic")
}

fn place(pool: &mut BeadPool, pos: Vec2, vel: Vec2, top: Op) -> u32 {
    let mut stack = [Op::nop(); STACK_CAP];
    stack[0] = top;
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

// Bonded pair, |d|=0.5 inside R=1, pulling apart at combined 1.0/sec → bond
// boundary reached at t≈0.5s. 240 substeps × 1/240 = 1.0s gives the rule two
// chances to fire (initial outward swing + one bounce back).
fn run_to_first_reaction(
    chem: &CompiledChemistry,
    a_top: Op,
    b_top: Op,
) -> (BeadPool, HashSet<(u32, u32)>, u32, u32) {
    let mut pool = BeadPool::with_capacity(8);
    let a = place(&mut pool, Vec2::new(15.0, 15.0), Vec2::new(-0.5, 0.0), a_top);
    let b = place(&mut pool, Vec2::new(15.5, 15.0), Vec2::new(0.5, 0.0), b_top);
    let mut bonds: HashSet<(u32, u32)> = HashSet::new();
    bonds.insert((a.min(b), a.max(b)));
    let mut grid = Grid::new(WORLD);
    for _ in 0..240 {
        do_substep(&mut pool, &mut grid, chem, &mut bonds, DT_SUB);
    }
    (pool, bonds, a, b)
}

#[test]
fn apply_die_kills_the_die_bead() {
    let chem = load_sem_basic();
    let (pool, bonds, a, b) = run_to_first_reaction(
        &chem,
        Op::simple(OpKind::Apply),
        Op::simple(OpKind::Die),
    );

    let alive: Vec<u32> = pool.alive_slots().collect();
    assert_eq!(
        alive,
        vec![a],
        "exactly the apply-bead should remain alive"
    );
    assert!(!pool.get(b).alive, "die-bead must be dead");
    assert_eq!(
        pool.get(a).stack_len,
        0,
        "apply-bead's stack must be popped (was [Apply], now empty)"
    );
    assert!(
        !bonds.contains(&(a.min(b), a.max(b))),
        "bond touching the dead slot must be removed"
    );
}

#[test]
fn apply_spawn_births_a_new_wire_bonded_to_both_parents() {
    let chem = load_sem_basic();
    let (pool, bonds, a, b) = run_to_first_reaction(
        &chem,
        Op::simple(OpKind::Apply),
        Op::simple(OpKind::Spawn),
    );

    let alive: Vec<u32> = pool.alive_slots().collect();
    assert_eq!(alive.len(), 3, "Birth must add exactly one new alive slot");

    // Parents still alive, both popped their top op.
    assert!(pool.get(a).alive && pool.get(b).alive, "parents survive");
    assert_eq!(pool.get(a).stack_len, 0, "apply-bead popped to empty");
    assert_eq!(pool.get(b).stack_len, 0, "spawn-bead popped to empty");

    // Newborn is whichever alive slot isn't a parent.
    let new_slot = *alive
        .iter()
        .find(|&&s| s != a && s != b)
        .expect("a third slot must exist");
    let newborn = pool.get(new_slot);
    assert_eq!(newborn.tag, Tag::Wire, "birth_tag was wire");
    assert_eq!(
        newborn.stack_len, 0,
        "birth_program 'empty' has zero ops"
    );

    // Triangle of bonds: parents stay bonded, newborn bonded to each parent.
    let key = |x: u32, y: u32| (x.min(y), x.max(y));
    assert!(bonds.contains(&key(a, b)), "parents' bond is preserved");
    assert!(bonds.contains(&key(a, new_slot)), "newborn bonded to a");
    assert!(bonds.contains(&key(b, new_slot)), "newborn bonded to b");
}

#[test]
fn no_rule_no_population_change() {
    // Same geometry, but both beads carry Apply (no sem_basic rule). The
    // contact must fall through to the default Exchange rule (bounce) and
    // leave the pair intact — no spurious births or deaths.
    let chem = load_sem_basic();
    let (pool, bonds, a, b) = run_to_first_reaction(
        &chem,
        Op::simple(OpKind::Apply),
        Op::simple(OpKind::Apply),
    );

    let alive_count = pool.alive_slots().count();
    assert_eq!(alive_count, 2, "no rule → no population change");
    assert!(pool.get(a).alive && pool.get(b).alive);
    assert!(bonds.contains(&(a.min(b), a.max(b))), "bond intact");
}
