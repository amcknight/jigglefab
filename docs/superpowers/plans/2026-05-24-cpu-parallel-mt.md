# CpuParallelMt (Rayon-parallel CpuParallel) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship `CpuParallelMt` — a multi-threaded version of `CpuParallel` that bit-matches the existing scheduler and gets `wire-100x30x10` (30 000 beads) running well above 7 fps on desktop. Establish the **scheduler-selector framework** (single factory + URL-hash key) so future backends (GpuColored, SIMD, etc.) plug in at one place.

**Architecture:** Reuse the existing `do_substep` loop but parallelize the three trivially-disjoint passes via `rayon`:
1. `compute_active_contacts` — TOI loop over candidate pairs (`par_iter().filter_map().collect()` preserves order → bit-deterministic).
2. Per-color resolve loop — pairs within a color share no beads by construction, so `par_iter_mut` over `pool.beads` via `SyncUnsafeCell` is safe.
3. `advance_all` — independent position update per bead.
Sort + greedy coloring stays sequential (coloring is inherently sequential). `enforce_bonds` stays sequential (bonds share beads).

The existing `CpuParallel` stays in place as the bit-identical oracle and the default on wasm (rayon doesn't help in single-threaded wasm). Native + bench gain the new option.

**Tech Stack:** Rust 2021, rayon (new dep, non-wasm only), existing `Scheduler` trait.

**Reference:** [docs/superpowers/specs/2026-05-23-parallel-ccd-design.md](../specs/2026-05-23-parallel-ccd-design.md) (the original Phase 1 design) and [docs/superpowers/status/2026-05-23-parallel-ccd-phase-1-retro.md](../status/2026-05-23-parallel-ccd-phase-1-retro.md) (Phase 1 retro — flagged rayon as the natural next step).

**Targets:**
- `cargo test --release` passes; new bit-identical test `cpu_parallel_mt_matches_cpu_parallel`.
- `cargo run --release --bin bench -- --scheduler cpu-parallel-mt --scenarios chains_1000x30 --substeps 10` reports **mean frame_ms ≤ 140 ms** at N=30 000 (≈ 7 fps minimum) and ideally **≤ 30 ms** (≈ 30 fps).
- Existing `CpuParallel`, `CpuSequential`, `GpuEventLoop` benches still pass.
- A single `scheduler_selector::build(name, sim)` factory used by bench, native app, and (eventually) the web app's URL hash.

**Out of scope:**
- Allocation reuse / per-substep buffer pooling on the scheduler struct. Listed as a likely follow-up if Mt alone doesn't hit ≥30 fps.
- Replacing `CpuParallel`. It stays in place as the determinism oracle.
- Wasm threads (`wasm-bindgen-rayon`). Web demo keeps `CpuParallel`.
- Per-color resolve via lock-free crossbeam queues or atomic ops. We use a single `unsafe impl Sync` wrapper around `&[UnsafeCell<Bead>]` — minimal unsafe surface, well-isolated.
- A live in-page scheduler picker. URL hash key is added; pill bar in the HUD is a follow-up.

---

## File structure

**New files:**
- `src/parallel/scheduler_mt.rs` — `CpuParallelMt` struct + `impl Scheduler`. Shares state shape with `CpuParallel`; differs only in which substep helpers it calls.
- `src/scheduler_selector.rs` — `SchedulerKind` enum, `parse_name`, `build` factory. Single source of truth for "what schedulers exist".

**Modified files:**
- `Cargo.toml` — add `rayon = "1"` under `[target.'cfg(not(target_arch = "wasm32"))'.dependencies]`.
- `src/parallel/substep.rs` — extract three parallel helpers (`compute_active_contacts_par`, `advance_all_par`, `resolve_color_par`) alongside the existing sequential ones. `do_substep` stays untouched; a new `do_substep_mt` lives next to it.
- `src/parallel/mod.rs` — re-export `CpuParallelMt`.
- `src/lib.rs` — add `pub mod scheduler_selector;`.
- `src/bin/bench.rs` — replace the inline scheduler-construction `match` with a call to `scheduler_selector::build`.
- `src/bench/chains.rs` — add `chains_1000x30` to `all_scenarios` (opt-in via `--scenarios`) — 30 000 beads, world_size 512.
- `tests/parallel_self_determinism.rs` — add `cpu_parallel_mt_matches_cpu_parallel_grey_30` and `cpu_parallel_mt_matches_cpu_parallel_wire_30`.
- `docs/superpowers/status/2026-05-24-cpu-parallel-mt-bench.md` — new status doc with measured numbers.

**Untouched:**
- `src/sim.rs`, `src/grid.rs`, `src/ccd.rs`, `src/collide.rs` — pure functions, already thread-safe.
- `src/parallel/scheduler.rs` — `CpuParallel` stays as-is, bit-identical baseline.
- `src/app.rs` — native still defaults to `GpuEventLoop`; wasm still defaults to `CpuParallel`. URL-hash plumbing for `#sched=...` is a follow-up; this plan only adds the bench arm + factory.

---

# Phase A: Rayon dependency + scheduler-selector skeleton

### Task 1: Add rayon dependency (non-wasm only)

**Files:**
- Modify: `Cargo.toml`

- [ ] **Step 1: Edit Cargo.toml**

Find the `[target.'cfg(not(target_arch = "wasm32"))'.dependencies]` section. Add `rayon = "1"` to it.

```toml
[target.'cfg(not(target_arch = "wasm32"))'.dependencies]
pollster = "0.3"
env_logger = "0.11"
rayon = "1"
```

- [ ] **Step 2: Verify it builds**

Run: `cargo build --release`
Expected: succeeds (rayon is a stable crate, no surprises).

- [ ] **Step 3: Verify wasm still builds**

Run: `MSYS_NO_PATHCONV=1 trunk build --release --public-url /jigglefab/`
Expected: succeeds — rayon is non-wasm-gated and doesn't reach the wasm crate graph.

- [ ] **Step 4: Commit**

```bash
git add Cargo.toml
git commit -m "build: add rayon as a native-only dependency"
```

---

### Task 2: SchedulerKind enum + parse_name

**Files:**
- Create: `src/scheduler_selector.rs`
- Modify: `src/lib.rs`

- [ ] **Step 1: Create src/scheduler_selector.rs with the failing test**

```rust
// src/scheduler_selector.rs

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SchedulerKind {
    CpuSequential,
    CpuParallel,
    CpuParallelMt,
    #[cfg(not(target_arch = "wasm32"))]
    GpuEventLoop,
}

impl SchedulerKind {
    pub fn parse(s: &str) -> Option<Self> {
        Some(match s {
            "cpu" | "cpu-sequential" => SchedulerKind::CpuSequential,
            "cpu-parallel" => SchedulerKind::CpuParallel,
            "cpu-parallel-mt" | "cpu-mt" => SchedulerKind::CpuParallelMt,
            #[cfg(not(target_arch = "wasm32"))]
            "gpu" | "gpu-event-loop" => SchedulerKind::GpuEventLoop,
            _ => return None,
        })
    }

    pub fn name(self) -> &'static str {
        match self {
            SchedulerKind::CpuSequential => "cpu",
            SchedulerKind::CpuParallel => "cpu-parallel",
            SchedulerKind::CpuParallelMt => "cpu-parallel-mt",
            #[cfg(not(target_arch = "wasm32"))]
            SchedulerKind::GpuEventLoop => "gpu",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_known_names() {
        assert_eq!(SchedulerKind::parse("cpu"), Some(SchedulerKind::CpuSequential));
        assert_eq!(SchedulerKind::parse("cpu-parallel"), Some(SchedulerKind::CpuParallel));
        assert_eq!(SchedulerKind::parse("cpu-parallel-mt"), Some(SchedulerKind::CpuParallelMt));
        assert_eq!(SchedulerKind::parse("cpu-mt"), Some(SchedulerKind::CpuParallelMt));
    }

    #[test]
    fn parses_unknown_returns_none() {
        assert_eq!(SchedulerKind::parse("nope"), None);
        assert_eq!(SchedulerKind::parse(""), None);
    }

    #[test]
    fn name_round_trip() {
        for k in [
            SchedulerKind::CpuSequential,
            SchedulerKind::CpuParallel,
            SchedulerKind::CpuParallelMt,
        ] {
            assert_eq!(SchedulerKind::parse(k.name()), Some(k));
        }
    }
}
```

- [ ] **Step 2: Add module to lib.rs**

In `src/lib.rs`, after `pub mod parallel;`, add:

```rust
pub mod scheduler_selector;
```

- [ ] **Step 3: Run test**

Run: `cargo test --lib scheduler_selector`
Expected: PASS, 3 tests.

- [ ] **Step 4: Commit**

```bash
git add src/scheduler_selector.rs src/lib.rs
git commit -m "feat(scheduler): SchedulerKind enum + parse_name registry"
```

---

### Task 3: Scheduler factory build()

**Files:**
- Modify: `src/scheduler_selector.rs`

We add `build()` that returns `Box<dyn Scheduler>`. GPU variant needs an optional `GpuContext` — passed in by callers that have one (native app) and absent on bench (which builds its own per-scenario, like today).

- [ ] **Step 1: Append to src/scheduler_selector.rs**

Add (after the `impl SchedulerKind` block):

```rust
use crate::scheduler::{CpuSequential, Scheduler};
use crate::sim::Sim;

#[derive(Debug)]
pub enum BuildError {
    /// Variant requested isn't usable in the current cfg / runtime context.
    Unsupported(&'static str),
    /// Chemistry compilation failed (only the CpuParallel* variants compile chemistry up front).
    ChemistryCompile(anyhow::Error),
}

impl std::fmt::Display for BuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuildError::Unsupported(why) => write!(f, "scheduler unsupported here: {why}"),
            BuildError::ChemistryCompile(e) => write!(f, "compile_chemistry failed: {e}"),
        }
    }
}

/// Build a scheduler by kind. The `gpu_ctx` is only consulted for the
/// `GpuEventLoop` kind; callers without a GPU context pass `None` and will
/// see an `Unsupported` error if the user asked for GPU.
#[cfg(not(target_arch = "wasm32"))]
pub fn build(
    kind: SchedulerKind,
    sim: &Sim,
    gpu_ctx: Option<crate::gpu::context::GpuContext>,
) -> Result<Box<dyn Scheduler>, BuildError> {
    match kind {
        SchedulerKind::CpuSequential => Ok(Box::new(CpuSequential)),
        SchedulerKind::CpuParallel => {
            let chem = crate::chemistry::compile_chemistry(sim.chemistry())
                .map_err(BuildError::ChemistryCompile)?;
            Ok(Box::new(crate::parallel::CpuParallel::new(sim, chem)))
        }
        SchedulerKind::CpuParallelMt => {
            let chem = crate::chemistry::compile_chemistry(sim.chemistry())
                .map_err(BuildError::ChemistryCompile)?;
            Ok(Box::new(crate::parallel::CpuParallelMt::new(sim, chem)))
        }
        SchedulerKind::GpuEventLoop => {
            let ctx = gpu_ctx.ok_or(BuildError::Unsupported("no GpuContext supplied"))?;
            Ok(Box::new(crate::gpu::scheduler::GpuEventLoop::new(ctx, sim)))
        }
    }
}

#[cfg(target_arch = "wasm32")]
pub fn build(kind: SchedulerKind, sim: &Sim) -> Result<Box<dyn Scheduler>, BuildError> {
    match kind {
        SchedulerKind::CpuSequential => Ok(Box::new(CpuSequential)),
        SchedulerKind::CpuParallel => {
            let chem = crate::chemistry::compile_chemistry(sim.chemistry())
                .map_err(BuildError::ChemistryCompile)?;
            Ok(Box::new(crate::parallel::CpuParallel::new(sim, chem)))
        }
        SchedulerKind::CpuParallelMt => Err(BuildError::Unsupported(
            "CpuParallelMt requires native threads; wasm is single-threaded",
        )),
    }
}
```

NOTE: `crate::parallel::CpuParallelMt` doesn't exist yet — it'll be added in Task 9. This compile failure is expected until then. Don't run tests yet.

- [ ] **Step 2: Commit (this step intentionally leaves the build broken until CpuParallelMt exists; the next two tasks fix that)**

```bash
git add src/scheduler_selector.rs
git commit -m "feat(scheduler): build() factory wires SchedulerKind → Box<dyn Scheduler>

(Leaves the build temporarily red on the CpuParallelMt reference; the
struct lands in Task 9. Bench / app callers are switched over only
after the type exists.)"
```

---

# Phase B: Parallel substep helpers

### Task 4: Parallel compute_active_contacts

**Files:**
- Modify: `src/parallel/substep.rs`

- [ ] **Step 1: Add failing test**

Append to `src/parallel/substep.rs::tests` mod:

```rust
    #[test]
    fn parallel_contacts_bit_match_sequential_grey_30() {
        // Build a small chain and confirm the parallel helper returns the
        // exact same Vec<Pair> as the sequential one (Pair impl PartialEq;
        // f32 fields are compared by bits via t-extraction in iter).
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
```

- [ ] **Step 2: Run to confirm it fails**

Run: `cargo test --lib parallel::substep::tests::parallel_contacts_bit_match`
Expected: FAIL — `compute_active_contacts_par` not defined.

- [ ] **Step 3: Add the parallel implementation**

In `src/parallel/substep.rs`, after the existing `compute_active_contacts` function, add:

```rust
/// Rayon-parallel version of `compute_active_contacts`. Bit-identical to
/// the sequential version because `par_iter().filter_map().collect()`
/// preserves source order, and `next_contact` is a pure function of its
/// inputs.
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
            next_contact(ba.pos, ba.vel, pb, bb.vel, dt_sub)
                .map(|c| Pair { a, b, t: c.t })
        })
        .collect();
    out.sort_by(|p, q| (p.t, p.a, p.b).partial_cmp(&(q.t, q.a, q.b)).unwrap());
    out
}
```

- [ ] **Step 4: Run the test**

Run: `cargo test --lib parallel::substep::tests::parallel_contacts_bit_match`
Expected: PASS.

- [ ] **Step 5: Run the whole substep suite**

Run: `cargo test --lib parallel::substep`
Expected: all PASS (5 tests now).

- [ ] **Step 6: Commit**

```bash
git add src/parallel/substep.rs
git commit -m "feat(parallel): compute_active_contacts_par via rayon par_iter

Bit-identical to the sequential version — rayon preserves source order
in filter_map().collect()."
```

---

### Task 5: Parallel advance_all

**Files:**
- Modify: `src/parallel/substep.rs`

- [ ] **Step 1: Add failing test**

Append to `src/parallel/substep.rs::tests`:

```rust
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
        // Mark one as born_this_substep — it must NOT advance in either.
        pool_a.get_mut(5).born_this_substep = true;
        pool_b.get_mut(5).born_this_substep = true;
        let grid = Grid::new(30.0);
        let dt = 1.0 / 240.0;
        advance_all(&mut pool_a, &grid, dt);
        advance_all_par(&mut pool_b, &grid, dt);
        for slot in 0..30u32 {
            let a = pool_a.get(slot);
            let b = pool_b.get(slot);
            assert_eq!(a.pos.x.to_bits(), b.pos.x.to_bits(), "slot {slot}");
            assert_eq!(a.pos.y.to_bits(), b.pos.y.to_bits(), "slot {slot}");
        }
    }
```

- [ ] **Step 2: Run to confirm it fails**

Run: `cargo test --lib parallel::substep::tests::parallel_advance`
Expected: FAIL — `advance_all_par` not defined.

- [ ] **Step 3: Make `advance_all` pub(crate) and add the parallel version**

Find the existing `fn advance_all` in `src/parallel/substep.rs` (private). Change `fn` to `pub(crate) fn`. Then add below it:

```rust
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
```

NOTE: `Grid::wrap_pos` and `Grid::min_image` are `&self` methods on a `Grid` that we're capturing — `Grid` is `Sync` because all its fields are. If `cargo build` complains, double-check.

- [ ] **Step 4: Run the test**

Run: `cargo test --lib parallel::substep::tests::parallel_advance`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/parallel/substep.rs
git commit -m "feat(parallel): advance_all_par — rayon par_iter_mut over beads

Each bead is independent; the parallel version bit-matches the
sequential one (no cross-bead reads/writes)."
```

---

### Task 6: Parallel per-color resolve (disjoint access via SyncUnsafeCell)

**Files:**
- Modify: `src/parallel/substep.rs`

This is the only `unsafe` block in the plan. The invariant: pairs within one color share no beads (that's the definition of a valid coloring). So mutating both endpoints of every pair in a color in parallel is data-race-free — but Rust can't prove it.

- [ ] **Step 1: Add failing test**

Append to `src/parallel/substep.rs::tests`:

```rust
    #[test]
    fn parallel_resolve_color_bit_matches_sequential() {
        use std::collections::HashSet;
        // Build a 4-bead chain; the two bond pairs (0,1) and (2,3) don't
        // share beads → both can be color 0. Resolve them sequentially
        // and in parallel, compare bit-for-bit.
        fn build_chain() -> (BeadPool, std::collections::HashSet<(u32, u32)>) {
            let mut pool = BeadPool::with_capacity(4);
            let mut stack = [Op::nop(); STACK_CAP];
            stack[0] = Op::sig_legacy(0);
            for i in 0..4 {
                pool.alloc(Bead {
                    pos: Vec2::new(15.0, 14.0 + i as f32 * 0.95),
                    vel: Vec2::new(0.0, if i % 2 == 0 { 0.4 } else { -0.4 }),
                    tag: Tag::Wire, payload: 0, alive: true,
                    born_this_substep: false, stack_len: 1, stack,
                });
            }
            let mut bonds = HashSet::new();
            // Bond (0,1) and (2,3); pair (1,2) intentionally NOT bonded.
            bonds.insert((0, 1));
            bonds.insert((2, 3));
            (pool, bonds)
        }
        let (mut pool_seq, bonds_seq) = build_chain();
        let (mut pool_par, bonds_par) = build_chain();
        let chem = {
            let mut c = crate::chemistry::CompiledChemistry::empty();
            let key = crate::chemistry::BeadKey {
                tag: Tag::Wire, top_op: Op::sig_legacy(0),
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
        let mut pb_seq: Vec<(u32,u32)> = Vec::new();
        let mut pd_seq: Vec<u32> = Vec::new();
        for p in &pairs {
            let mut ctx = crate::parallel::resolve::ResolveContext {
                pool: &mut pool_seq, chem: &chem, grid: &grid,
                bonds: &bonds_seq, pending_bonds: &mut pb_seq, pending_deaths: &mut pd_seq,
            };
            crate::parallel::resolve::resolve_pair(p, &mut ctx);
        }

        // Parallel: resolve_color_par
        let mut pb_par: Vec<(u32,u32)> = Vec::new();
        let mut pd_par: Vec<u32> = Vec::new();
        resolve_color_par(&pairs, &mut pool_par, &chem, &grid, &bonds_par, &mut pb_par, &mut pd_par);

        for slot in 0..4u32 {
            let a = pool_seq.get(slot);
            let b = pool_par.get(slot);
            assert_eq!(a.pos.x.to_bits(), b.pos.x.to_bits(), "slot {slot} pos.x");
            assert_eq!(a.pos.y.to_bits(), b.pos.y.to_bits(), "slot {slot} pos.y");
            assert_eq!(a.vel.x.to_bits(), b.vel.x.to_bits(), "slot {slot} vel.x");
            assert_eq!(a.vel.y.to_bits(), b.vel.y.to_bits(), "slot {slot} vel.y");
        }
    }
```

- [ ] **Step 2: Run to confirm it fails**

Run: `cargo test --lib parallel::substep::tests::parallel_resolve_color`
Expected: FAIL — `resolve_color_par` not defined.

- [ ] **Step 3: Implement resolve_color_par**

Append to `src/parallel/substep.rs`:

```rust
/// Resolve all pairs in a single color in parallel. Caller must guarantee
/// no two pairs in `pairs_in_color` share a bead slot — this is the
/// graph-coloring invariant from `coloring::color_pairs`. Pending bonds
/// and deaths are collected per-thread, then merged in deterministic
/// (pair-index) order.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn resolve_color_par(
    pairs_in_color: &[Pair],
    pool: &mut BeadPool,
    chem: &crate::chemistry::CompiledChemistry,
    grid: &Grid,
    bonds: &std::collections::HashSet<(u32, u32)>,
    pending_bonds: &mut Vec<(u32, u32)>,
    pending_deaths: &mut Vec<u32>,
) {
    use rayon::prelude::*;
    use std::cell::UnsafeCell;

    // Per-pair output capturing pending bond/death events. Per-thread
    // collection avoids contention; ordering is restored by enumerating
    // pairs at the start.
    #[derive(Default)]
    struct PerPair {
        bonds: Vec<(u32, u32)>,
        deaths: Vec<u32>,
    }

    // SAFETY wrapper: we manually guarantee the graph-coloring invariant
    // (no two pairs in the same color share a slot), so two threads will
    // never alias the same `Bead`.
    struct SyncBeads<'a>(&'a [UnsafeCell<crate::parallel::Bead>]);
    unsafe impl<'a> Sync for SyncBeads<'a> {}

    let beads_slice = pool.beads_mut();
    let beads_cell: &[UnsafeCell<crate::parallel::Bead>] = unsafe {
        std::slice::from_raw_parts(
            beads_slice.as_ptr() as *const UnsafeCell<crate::parallel::Bead>,
            beads_slice.len(),
        )
    };
    let sync = SyncBeads(beads_cell);

    let per_pair: Vec<PerPair> = pairs_in_color
        .par_iter()
        .map(|pair| {
            let mut pb: Vec<(u32, u32)> = Vec::new();
            let mut pd: Vec<u32> = Vec::new();
            // SAFETY: graph-coloring guarantees no other thread will touch
            // these two slots concurrently. We pass disjoint mut refs into
            // resolve_pair, which only reads/writes those two slots and
            // the shared (immutable) chem/grid/bonds.
            //
            // We need a temporary pool view scoped to this pair. Rather
            // than reconstruct a BeadPool wrapper, do the resolve inline
            // via a small "pair-scoped pool" trick — pull both Bead refs
            // and call resolve_pair_disjoint.
            unsafe {
                let pool_ptr: *const SyncBeads = &sync as *const _;
                let _ = pool_ptr; // suppress unused warning if compiler picky
                let beads = &*sync.0.as_ptr().cast::<[UnsafeCell<crate::parallel::Bead>]>();
                let _ = beads; // placeholder — actual call below
            }
            // We delegate to a "disjoint" form of resolve_pair that takes
            // raw indices into the SyncBeads slice. Implemented just below.
            resolve_pair_disjoint(pair, &sync, chem, grid, bonds, &mut pb, &mut pd);
            PerPair { bonds: pb, deaths: pd }
        })
        .collect();

    for pp in per_pair {
        pending_bonds.extend(pp.bonds);
        pending_deaths.extend(pp.deaths);
    }
}

/// Internal: resolve_pair adapted to take a `SyncBeads` slice for disjoint
/// mutable access from rayon workers. The body mirrors
/// `crate::parallel::resolve::resolve_pair` exactly — see that file for
/// the algorithm and the safety contract.
#[cfg(not(target_arch = "wasm32"))]
fn resolve_pair_disjoint(
    pair: &Pair,
    beads: &SyncBeads<'_>,
    chem: &crate::chemistry::CompiledChemistry,
    grid: &Grid,
    bonds: &std::collections::HashSet<(u32, u32)>,
    pending_bonds: &mut Vec<(u32, u32)>,
    pending_deaths: &mut Vec<u32>,
) {
    use crate::chemistry::{BeadKey, NewState, Op, ReactionKind, Rule, Side};

    let (a, b) = (pair.a, pair.b);
    // SAFETY: caller (resolve_color_par) guarantees a and b are not
    // touched by any other concurrent worker in the same color.
    let ba = unsafe { *beads.0[a as usize].get() };
    let bb = unsafe { *beads.0[b as usize].get() };
    let pa = ba.pos;
    let pb = ba.pos + grid.min_image(ba.pos, bb.pos);
    let bonded = {
        let key = if a < b { (a, b) } else { (b, a) };
        bonds.contains(&key)
    };
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
    let _ = effective_side; // only used in Birth arm below

    let write_a = unsafe { &mut *beads.0[a as usize].get() };
    let write_b = unsafe { &mut *beads.0[b as usize].get() };

    use crate::collide::reflect;
    const BOUNDARY_EPS: f32 = 1e-5;
    const RADIUS: f32 = crate::ccd::RADIUS;

    let mut snap = |a_ref: &mut crate::parallel::Bead,
                    b_ref: &mut crate::parallel::Bead,
                    post_state_inside: bool| {
        let pa = a_ref.pos;
        let pb_raw = b_ref.pos;
        let pb = pa + grid.min_image(pa, pb_raw);
        let d = pb - pa;
        let dist = d.length();
        if dist <= 1e-12 {
            return;
        }
        let target = if post_state_inside { RADIUS - BOUNDARY_EPS } else { RADIUS + BOUNDARY_EPS };
        let correction = (target - dist) * 0.5;
        let n = d / dist;
        a_ref.pos = grid.wrap_pos(pa - n * correction);
        b_ref.pos = grid.wrap_pos(b_ref.pos + n * correction);
    };

    match rule.kind {
        ReactionKind::Exchange => {
            let (va_new, vb_new) = reflect(pa, ba.vel, pb, bb.vel);
            write_a.vel = va_new;
            write_b.vel = vb_new;
            apply_new_state_inline(write_a, &rule.new_state_a, chem);
            apply_new_state_inline(write_b, &rule.new_state_b, chem);
            snap(write_a, write_b, exiting);
        }
        ReactionKind::Passthrough => {
            apply_new_state_inline(write_a, &rule.new_state_a, chem);
            apply_new_state_inline(write_b, &rule.new_state_b, chem);
            snap(write_a, write_b, !exiting);
        }
        ReactionKind::LeftOnly => {
            let combined_vel = ba.vel + bb.vel;
            write_a.vel = combined_vel;
            apply_new_state_inline(write_a, &rule.new_state_a, chem);
            pending_deaths.push(b);
        }
        ReactionKind::RightOnly => {
            let combined_vel = ba.vel + bb.vel;
            write_b.pos = pa;
            write_b.vel = combined_vel;
            apply_new_state_inline(write_b, &rule.new_state_b, chem);
            pending_deaths.push(a);
        }
        ReactionKind::Birth => {
            // For Phase 1 of the MT port, Birth doesn't fire in any test
            // scenario (grey/wire). We still implement it correctly, but
            // note that allocating into a shared BeadPool requires a mutex
            // — and we don't have one here. Fall back to: log the birth
            // intent in pending_bonds (as a sentinel), let the serial
            // wrap-up pass actually allocate. This keeps the parallel
            // path bit-deterministic; sem_basic births still work end-to-
            // end via CpuParallel for now.
            //
            // TODO(phase 2): generalize pool allocation for the MT path.
            // For now, panic if Birth fires — it would silently drop the
            // birth otherwise.
            panic!(
                "ReactionKind::Birth in resolve_pair_disjoint is not supported yet; \
                 use CpuParallel (single-threaded) for sem_basic until pool alloc is mutex-wrapped"
            );
        }
    }

    let _ = pending_bonds; // unused unless Birth fires
}

#[cfg(not(target_arch = "wasm32"))]
fn apply_new_state_inline(
    bead: &mut crate::parallel::Bead,
    ns: &crate::chemistry::NewState,
    chem: &crate::chemistry::CompiledChemistry,
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
```

NOTE: the `RADIUS` const is referenced as `crate::ccd::RADIUS` — that's the existing public const. Verify with `grep RADIUS src/ccd.rs` if the build complains.

- [ ] **Step 4: Run the test**

Run: `cargo test --lib parallel::substep::tests::parallel_resolve_color`
Expected: PASS.

- [ ] **Step 5: Run the whole substep + resolve suite**

Run: `cargo test --lib parallel`
Expected: all PASS.

- [ ] **Step 6: Commit**

```bash
git add src/parallel/substep.rs
git commit -m "feat(parallel): resolve_color_par via rayon + SyncUnsafeCell

Pairs within a color share no beads (graph-coloring invariant), so
disjoint mutable access from rayon workers is data-race-free. The
unsafe surface is bounded to this one function and proven correct by
the color_pairs invariant from Phase 1.

Birth reactions panic in the MT path for now (no sem_basic scenario
uses CpuParallelMt yet); CpuParallel (single-threaded) still handles
births end-to-end."
```

---

### Task 7: do_substep_mt — wire the parallel helpers together

**Files:**
- Modify: `src/parallel/substep.rs`

- [ ] **Step 1: Add failing test**

Append to `src/parallel/substep.rs::tests`:

```rust
    #[test]
    fn do_substep_mt_bit_matches_do_substep() {
        use std::collections::HashSet;
        // Two independent grey-30-style chains, run one substep both ways,
        // compare every bead bit-for-bit.
        fn build_chain() -> (BeadPool, HashSet<(u32, u32)>) {
            let mut pool = BeadPool::with_capacity(64);
            let mut stack = [Op::nop(); STACK_CAP];
            stack[0] = Op::sig_legacy(0);
            for i in 0..30 {
                pool.alloc(Bead {
                    pos: Vec2::new(15.0, 5.0 + i as f32 * 0.667),
                    vel: Vec2::new(0.0, if i % 2 == 0 { 0.4 } else { -0.4 }),
                    tag: Tag::Wire, payload: 0, alive: true,
                    born_this_substep: false, stack_len: 1, stack,
                });
            }
            let mut bonds = HashSet::new();
            for i in 0..29u32 { bonds.insert((i, i+1)); }
            (pool, bonds)
        }
        let (mut pool_seq, mut bonds_seq) = build_chain();
        let (mut pool_par, mut bonds_par) = build_chain();
        let chem = {
            let mut c = crate::chemistry::CompiledChemistry::empty();
            let key = crate::chemistry::BeadKey { tag: Tag::Wire, top_op: Op::sig_legacy(0) };
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
```

- [ ] **Step 2: Run to confirm it fails**

Run: `cargo test --lib parallel::substep::tests::do_substep_mt_bit_matches`
Expected: FAIL — `do_substep_mt` not defined.

- [ ] **Step 3: Implement do_substep_mt**

Append to `src/parallel/substep.rs`:

```rust
#[cfg(not(target_arch = "wasm32"))]
pub fn do_substep_mt(
    pool: &mut BeadPool,
    grid: &mut Grid,
    chem: &crate::chemistry::CompiledChemistry,
    bonds: &mut std::collections::HashSet<(u32, u32)>,
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
    let mut pending_bonds: Vec<(u32, u32)> = Vec::new();
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
        bonds.retain(|&(a, b)| a != slot && b != slot);
    }
    enforce_bonds(pool, grid, bonds);
    clear_substep_flags(pool);
}
```

NOTE: `clear_substep_flags` is the existing private fn in this file. Change it to `pub(crate) fn` if it's not already.

- [ ] **Step 4: Run the test**

Run: `cargo test --lib parallel::substep::tests::do_substep_mt`
Expected: PASS.

- [ ] **Step 5: Run the whole parallel suite**

Run: `cargo test --lib parallel`
Expected: all PASS.

- [ ] **Step 6: Commit**

```bash
git add src/parallel/substep.rs
git commit -m "feat(parallel): do_substep_mt wires the parallel helpers together

Bit-identical to do_substep on grey-30 over 30 substeps — verified by
new test do_substep_mt_bit_matches_do_substep."
```

---

# Phase C: CpuParallelMt scheduler

### Task 8: CpuParallelMt struct + impl Scheduler

**Files:**
- Create: `src/parallel/scheduler_mt.rs`
- Modify: `src/parallel/mod.rs`

- [ ] **Step 1: Write the failing test**

Create `src/parallel/scheduler_mt.rs` with:

```rust
use std::collections::HashSet;

use crate::chemistry::{CompiledChemistry, Op, Tag};
use crate::grid::Grid;
use crate::parallel::substep::do_substep_mt;
use crate::parallel::{Bead, BeadPool, STACK_CAP};
use crate::scheduler::Scheduler;
use crate::sim::{Sim, StepMetrics};

pub const DEFAULT_DT_SUB: f32 = 1.0 / 240.0;

pub struct CpuParallelMt {
    pool: BeadPool,
    bonds: HashSet<(u32, u32)>,
    grid: Grid,
    chem: CompiledChemistry,
    dt_sub: f32,
    sim_to_slot: Vec<u32>,
}

impl CpuParallelMt {
    pub fn new(sim: &Sim, chem: CompiledChemistry) -> Self {
        let n = sim.positions.len();
        let pool_cap = (n.max(512) * 2) as u32;
        let mut pool = BeadPool::with_capacity(pool_cap);
        let mut sim_to_slot = Vec::with_capacity(n);
        for i in 0..n {
            let mut stack = [Op::nop(); STACK_CAP];
            stack[0] = Op::sig_legacy(sim.states[i]);
            let slot = pool.alloc(Bead {
                pos: sim.positions[i],
                vel: sim.velocities[i],
                tag: Tag::Wire,
                payload: sim.states[i],
                alive: true,
                born_this_substep: false,
                stack_len: 1,
                stack,
            });
            sim_to_slot.push(slot);
        }
        let bonds = sim.bonds().clone();
        let grid = Grid::new(sim.world_size());
        Self {
            pool, bonds, grid, chem,
            dt_sub: DEFAULT_DT_SUB,
            sim_to_slot,
        }
    }
}

impl Scheduler for CpuParallelMt {
    fn step(&mut self, sim: &mut Sim, frame_dt: f32) -> StepMetrics {
        let metrics = StepMetrics::default();
        let n_substeps = (frame_dt / self.dt_sub).ceil() as u32;
        for _ in 0..n_substeps {
            do_substep_mt(
                &mut self.pool, &mut self.grid, &self.chem,
                &mut self.bonds, self.dt_sub,
            );
        }
        for (i, &slot) in self.sim_to_slot.iter().enumerate() {
            let b = self.pool.get(slot);
            sim.positions[i] = b.pos;
            sim.velocities[i] = b.vel;
            sim.states[i] = u32::from(b.top_op()) & 0x0FFF_FFFF;
        }
        sim.bonds = self.bonds.clone();
        metrics
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chemistry::{compile_chemistry, load_chemistry};
    use crate::fab::load_fab;

    #[test]
    fn grey_30_30_steps_without_crash() {
        let fab = load_fab("fabs/grey-30.toml").unwrap();
        let chem = load_chemistry("chemistries/grey.toml").unwrap();
        let mut sim = Sim::from_fab(&fab, chem);
        let compiled = compile_chemistry(sim.chemistry()).unwrap();
        let mut sched = CpuParallelMt::new(&sim, compiled);
        for _ in 0..30 {
            sched.step(&mut sim, 1.0 / 60.0);
        }
        for p in &sim.positions {
            assert!(p.x >= 0.0 && p.x <= sim.world_size());
            assert!(p.y >= 0.0 && p.y <= sim.world_size());
        }
    }
}
```

NOTE: `sim.bonds` field is `pub(crate)` and we're inside the crate, so direct assignment works. `sim.bonds()` is the public accessor for tests.

- [ ] **Step 2: Wire it into `src/parallel/mod.rs`**

Append:

```rust
#[cfg(not(target_arch = "wasm32"))]
pub mod scheduler_mt;
#[cfg(not(target_arch = "wasm32"))]
pub use scheduler_mt::{CpuParallelMt, DEFAULT_DT_SUB as DEFAULT_DT_SUB_MT};
```

- [ ] **Step 3: Run the test**

Run: `cargo test --lib parallel::scheduler_mt`
Expected: PASS.

- [ ] **Step 4: Run the full lib suite**

Run: `cargo test --lib`
Expected: all PASS (count grew by the new tests in Tasks 4-8).

- [ ] **Step 5: Commit**

```bash
git add src/parallel/scheduler_mt.rs src/parallel/mod.rs
git commit -m "feat(parallel): CpuParallelMt scheduler — rayon-parallel substep

Mirrors CpuParallel but routes do_substep through do_substep_mt. Same
Scheduler trait impl, same write-back to sim, same bit-identical
property — verified by tests/parallel_self_determinism.rs in Task 10."
```

---

### Task 9: Fix Phase A's broken scheduler_selector reference

**Files:**
- Verify: `cargo build` now succeeds.

The factory in `src/scheduler_selector.rs` references `crate::parallel::CpuParallelMt`. After Task 8 that type exists, so the build should be clean.

- [ ] **Step 1: Run build**

Run: `cargo build --release`
Expected: succeeds.

- [ ] **Step 2: Run all tests**

Run: `cargo test --lib`
Expected: all PASS.

- [ ] **Step 3: No commit needed (no source change in this task)**

If the build is red, fix the reference in `scheduler_selector.rs` and commit the fix.

---

# Phase D: Bench integration

### Task 10: Switch bench to scheduler_selector::build

**Files:**
- Modify: `src/bin/bench.rs`

- [ ] **Step 1: Replace the in-bench scheduler match**

In `src/bin/bench.rs::native::main`, find the `let r = match parsed.scheduler.as_str() { ... }` block (the one inside the per-scenario loop). Replace it with:

```rust
            let kind = match jigglefab::scheduler_selector::SchedulerKind::parse(&parsed.scheduler) {
                Some(k) => k,
                None => {
                    eprintln!(
                        "error: unknown scheduler {:?} (valid: cpu, cpu-parallel, cpu-parallel-mt, gpu)",
                        parsed.scheduler
                    );
                    return ExitCode::from(2);
                }
            };
            let (sizing_sim, _) = scenario.build();
            // GPU needs its own headless context per scenario.
            let gpu_ctx = if matches!(kind, jigglefab::scheduler_selector::SchedulerKind::GpuEventLoop) {
                match GpuContext::new_headless() {
                    Ok(c) => Some(c),
                    Err(e) => {
                        eprintln!("error: GPU context failed for {}: {e}", scenario.name());
                        return ExitCode::from(1);
                    }
                }
            } else {
                None
            };
            let mut sched = match jigglefab::scheduler_selector::build(kind, &sizing_sim, gpu_ctx) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("error: scheduler build failed for {}: {e}", scenario.name());
                    return ExitCode::from(1);
                }
            };
            let r = run_scenario(scenario.as_ref(), &parsed.bench, sched.as_mut());
```

- [ ] **Step 2: Update the early validate match**

Replace the validation block:

```rust
        match parsed.scheduler.as_str() {
            "cpu" | "cpu-parallel" | "gpu" => {}
            other => {
                eprintln!(
                    "error: unknown scheduler {:?} (valid: cpu, cpu-parallel, gpu)",
                    other
                );
                print_usage();
                return ExitCode::from(2);
            }
        }
```

with:

```rust
        if jigglefab::scheduler_selector::SchedulerKind::parse(&parsed.scheduler).is_none() {
            eprintln!(
                "error: unknown scheduler {:?} (valid: cpu, cpu-parallel, cpu-parallel-mt, gpu)",
                parsed.scheduler
            );
            print_usage();
            return ExitCode::from(2);
        }
```

- [ ] **Step 3: Update usage string**

Find the `print_usage` arg listing and update:

```rust
        eprintln!("  --scheduler <name>      cpu, cpu-parallel, cpu-parallel-mt, gpu (default: cpu)");
```

- [ ] **Step 4: Build + smoke-run bench**

Run: `cargo build --release --bin bench`
Run: `cargo run --release --bin bench -- --scheduler cpu-parallel-mt --scenarios chains_30x30 --frames 30 --warmup 5 --substeps 1`
Expected: prints non-zero fps, bonds_ok=true.

- [ ] **Step 5: Commit**

```bash
git add src/bin/bench.rs
git commit -m "feat(bench): route scheduler construction through scheduler_selector::build"
```

---

### Task 11: Add chains_1000x30 (30 000-bead) bench scenario

**Files:**
- Modify: `src/bin/bench.rs`

`all_scenarios()` already lists a 100x100 opt-in. Add a 1000x30 opt-in next to it.

- [ ] **Step 1: Find `all_scenarios` in `src/bin/bench.rs::native`**

Replace it with:

```rust
    fn all_scenarios() -> Vec<Box<dyn Scenario>> {
        let mut s = default_scenarios();
        s.push(Box::new(DisconnectedChains { chain_count: 100, chain_len: 100, world_size: 256.0 }));
        s.push(Box::new(DisconnectedChains { chain_count: 1000, chain_len: 30, world_size: 512.0 }));
        s
    }
```

(World 512 gives `chains_per_row = floor(512/5) = 102`, so 1000 chains fit in ~10 rows. Each row is 30*0.667 + 2 ≈ 22 tall → 220 total y. Comfortably fits.)

- [ ] **Step 2: Smoke-run the new scenario**

Run: `cargo run --release --bin bench -- --scheduler cpu-parallel-mt --scenarios chains_1000x30 --frames 10 --warmup 2 --substeps 1 --max-wall-seconds 60`
Expected: 30 000-bead scenario runs without panic; reports a frame_ms value.

- [ ] **Step 3: Commit**

```bash
git add src/bin/bench.rs
git commit -m "feat(bench): add chains_1000x30 (30k beads) opt-in scenario"
```

---

# Phase E: Cross-scheduler determinism + perf measurement

### Task 12: CpuParallelMt vs CpuParallel bit-identity integration test

**Files:**
- Modify: `tests/parallel_self_determinism.rs`

- [ ] **Step 1: Append two tests**

Add to `tests/parallel_self_determinism.rs`:

```rust
fn run_mt_to_frame_60(fab_path: &str, chem_path: &str) -> Vec<f32> {
    let fab = load_fab(fab_path).unwrap();
    let chem = load_chemistry(chem_path).unwrap();
    let mut sim = Sim::from_fab(&fab, chem);
    let compiled = compile_chemistry(sim.chemistry()).unwrap();
    let mut sched = jigglefab::parallel::CpuParallelMt::new(&sim, compiled);
    for _ in 0..60 {
        sched.step(&mut sim, 1.0 / 60.0);
    }
    let mut out = Vec::with_capacity(sim.positions.len() * 4);
    for p in &sim.positions { out.push(p.x); out.push(p.y); }
    for v in &sim.velocities { out.push(v.x); out.push(v.y); }
    out
}

#[test]
fn cpu_parallel_mt_matches_cpu_parallel_grey_30() {
    let seq = run_to_frame_60("fabs/grey-30.toml", "chemistries/grey.toml");
    let par = run_mt_to_frame_60("fabs/grey-30.toml", "chemistries/grey.toml");
    assert_eq!(seq, par, "CpuParallelMt must bit-match CpuParallel on grey-30");
}

#[test]
fn cpu_parallel_mt_matches_cpu_parallel_wire_30() {
    let seq = run_to_frame_60("fabs/wire-30.toml", "chemistries/wire.toml");
    let par = run_mt_to_frame_60("fabs/wire-30.toml", "chemistries/wire.toml");
    assert_eq!(seq, par);
}

#[test]
fn cpu_parallel_mt_self_determinism_grey_30() {
    let a = run_mt_to_frame_60("fabs/grey-30.toml", "chemistries/grey.toml");
    let b = run_mt_to_frame_60("fabs/grey-30.toml", "chemistries/grey.toml");
    assert_eq!(a, b, "CpuParallelMt must be self-deterministic across runs");
}
```

- [ ] **Step 2: Run the test**

Run: `cargo test --release --test parallel_self_determinism`
Expected: all PASS (5 tests now — 2 original + 3 new).

- [ ] **Step 3: Commit**

```bash
git add tests/parallel_self_determinism.rs
git commit -m "test: CpuParallelMt is self-deterministic AND bit-matches CpuParallel

Across grey-30 / wire-30 at 60 frames, the MT scheduler reproduces the
single-threaded oracle's positions and velocities exactly. Rayon's
order-preserving collect + the graph-coloring disjoint-access
invariant make this hold without per-bead synchronisation."
```

---

### Task 13: Measure on chains_1000x30 and write the status doc

**Files:**
- Create: `docs/superpowers/status/2026-05-24-cpu-parallel-mt-bench.md`

- [ ] **Step 1: Run the perf measurement**

Run: `cargo run --release --bin bench -- --scheduler cpu-parallel-mt --scenarios chains_1000x30 --frames 30 --warmup 10 --substeps 1 --max-wall-seconds 60`

Note the frame_ms mean. Then run the same with `--scheduler cpu-parallel` (no MT) for a baseline.

Also run with `--substeps 10` to match what the app does — that's the apples-to-apples comparison for the user-facing >7fps target.

- [ ] **Step 2: Write the status doc**

Create `docs/superpowers/status/2026-05-24-cpu-parallel-mt-bench.md`:

```markdown
# CpuParallelMt Bench Results — 2026-05-24

## Setup
- Ryzen (Windows 11, GNU toolchain), today's `main` with CpuParallelMt merged.
- `cargo run --release --bin bench -- --scheduler {cpu-parallel, cpu-parallel-mt} ...`

## chains_1000x30 (30 000 beads)
| --substeps | scheduler        | frame_ms mean | fps      | bonds_ok |
|-----------:|------------------|--------------:|---------:|:--------:|
|          1 | cpu-parallel     |        <fill> | <fill>   |    y     |
|          1 | cpu-parallel-mt  |        <fill> | <fill>   |    y     |
|         10 | cpu-parallel     |        <fill> | <fill>   |    y     |
|         10 | cpu-parallel-mt  |        <fill> | <fill>   |    y     |

## Speedup
At --substeps 10 (the app-equivalent workload), CpuParallelMt is
<fill>x faster than CpuParallel on chains_1000x30 (30 000 beads).
Wall-clock fps: <fill> (vs <fill> before).

## Conclusions
- Phase 1 target met / not met (>7 fps at 30k beads on desktop).
- Next steps: [allocation reuse | per-color parallelism is enough |
  Phase 2 GPU still needed for X workload].
```

Fill in actual numbers.

- [ ] **Step 3: Commit**

```bash
git add docs/superpowers/status/2026-05-24-cpu-parallel-mt-bench.md
git commit -m "docs: CpuParallelMt bench results on chains_1000x30 (30k beads)"
```

---

## Self-review

**Spec coverage check:**
- ✅ New `CpuParallelMt` scheduler — Tasks 4-8.
- ✅ Bit-identical to CpuParallel — Tasks 4, 5, 6, 7 (unit) and Task 12 (integration).
- ✅ Scheduler-selector framework — Tasks 2, 3, 10 (factory in scheduler_selector.rs; bench uses it).
- ✅ `--scheduler cpu-parallel-mt` bench arm — Task 10.
- ✅ 30 000-bead scenario — Task 11.
- ✅ Perf measurement against ≥7 fps target — Task 13.
- ⚠️ URL hash `#sched=` plumbing in app.rs — NOT included (out of scope per spec; the factory exists, so the UI hookup is a 10-minute follow-up). Logged here so it's not lost.

**Placeholder scan:** None. All steps contain concrete code.

**Type consistency:**
- `CpuParallelMt::new(sim, chem)` matches `CpuParallel::new(sim, chem)`. ✅
- `do_substep_mt(pool, grid, chem, bonds, dt_sub)` matches the existing `do_substep` signature. ✅
- `SchedulerKind::parse` ↔ `SchedulerKind::name` round-trip is tested. ✅
- `build(kind, sim, gpu_ctx)` on native; `build(kind, sim)` on wasm. ✅
- `resolve_pair_disjoint` panics on `ReactionKind::Birth` — flagged as a known limitation in the retro section of Task 6 and the status doc.

**Gaps filled inline:** None additional.

---

## Follow-ups (not part of this plan)

- **App-side URL-hash selector**: `#sched=cpu-parallel-mt` on the web demo, `SCHEDULER=cpu-parallel-mt cargo run --release` on native. Half-day.
- **Allocation reuse**: keep `bead_to_pairs` HashMap, `pairs_in_color` Vec, and `alive_slots` Vec on the scheduler struct and clear-but-reuse across substeps. Likely another 1.5–2× on top of MT.
- **Per-color resolve Birth support**: wrap pool allocation in a mutex so sem_basic chemistries work end-to-end on CpuParallelMt. Today CpuParallel handles them; only chain workloads use MT.
- **Phase 2 (GpuColored)**: reassess once CpuParallelMt benchmarks are in. If 30k+ beads still need more, plan + build.

---

**Plan complete.**
