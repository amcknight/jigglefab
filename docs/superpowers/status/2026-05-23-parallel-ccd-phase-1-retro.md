# Parallel-CCD Phase 1 — Retrospective

## What shipped
- **CpuParallel scheduler** (graph-colored fixed substep CCD) — passes
  Scheduler trait, default sub-dt 1/240, sub-steps internally per frame.
- **Chemistry compiler**: legacy `Chemistry` TOML → `CompiledChemistry`
  (via `Op::sig_legacy` stop-gap) *and* native sem-style TOML compiler
  (`parse_sem_chemistry` for `[[sem_rule]]` / `[[program]]`).
- **sem_basic chemistry**: minimal subset of `haskell/src/Chem/Sem.hs`
  covering Apply+Die → LeftOnly and Apply+Spawn → Birth.
- **chains_30x300 fab** (9 000 beads, world 289) via `gen_chains` bin.
- **Tests**: unit (opcodes, compiled, compiler, pool, coloring, substep,
  resolve, scheduler) + integration (parallel_self_determinism,
  parallel_invariants, sem_basic_loads, chains_30x300_smoke).
- **Bench arm**: `--scheduler cpu-parallel` plumbed through `run_scenario`.

## Numbers (Ryzen, GNU toolchain, release)
- 10 000-bead chain world: **242 fps, 4 ms/frame, bonds preserved**.
- 5–6× faster than CpuSequential at 900–1000 beads (clean comparison,
  both terminate without iter_cap saturation).
- Larger N: CpuSequential breaks down (130 s per frame at 10k, bonds lost).
  See [`2026-05-23-parallel-ccd-phase-1-bench.md`](2026-05-23-parallel-ccd-phase-1-bench.md).

## What didn't ship
- **GPU implementation** — deferred to Phase 2.
- **Sem chemistry full opcode coverage** — only Apply/Die/Spawn for now.
  Hold, Take/Drop, Send, Wait, Done all stubbed out as `OpKind` variants
  but no rules exercise them yet.
- **Speed-adaptive substep** — `dt_sub` is fixed at 1/240. A chemistry
  with collision-amplified speeds (one of the user's "max bead speed
  drifts up after collisions" cases) will eventually need an
  `dt_sub = min(DEFAULT, R / v_max)` clamp. Not visible at grey/wire.
- **Per-frame rayon parallelism** — staying sequential per the plan.
  Algorithm is now correct; parallelism is a Phase-1.5 / Phase 2
  optimisation.
- **CpuParallel StepMetrics** — `step()` returns `StepMetrics::default()`
  (zeros). Bench `contacts/ss` and `iter_cap_sat` columns are
  meaningless for cpu-parallel runs. Trivial follow-up to wire
  per-substep counts back into the metrics struct.
- **sem_basic end-to-end run** — chemistry compiles and loads, but no
  scenario actually exercises Birth/Die through CpuParallel. Needs a
  small fab with apply-stacked wires + spawners to drive it. Deferred
  to Phase 2 prep.
- **Rock / wall reflection** — `Tag::Rock` exists; no fab or test
  exercises it.

## Surprises / lessons
- **Grid binning bounds the per-substep dt**. The plan's first test put
  beads 3 units apart with dt=2, but `Grid::candidate_pairs` only
  considers same-or-adjacent cells (CELL_SIZE = 2·R = 2), so beads
  starting in non-adjacent cells aren't paired. Production dt_sub of
  1/240 with unit speed ≈ 0.004 unit traversal stays well inside one
  cell, but the test setup had to be redone for adjacent-cell geometry
  (commit `e402d77`). Documented this as a substep contract in the file.
- **The plan's no-overlap invariant test was too strict** — asserted
  min global pair distance > R−ε, but a 30-bead chain's bonded pairs
  sit *inside* R by design (it's how the chain holds together). Fixed
  by checking only non-bonded pairs (commit `4938535`).
- **Algorithm correctness > GPU port for chain workloads**. The Phase 1
  CPU implementation already exceeds the spec's perf goal at 10k beads
  on a single thread. The motivation for Phase 2 is now contact-dense
  scenarios (births, dense gases) where per-color counts blow past CPU
  cache — not chain throughput.
- **Plan task ordering was infeasible**. Tasks 1 and 2 (add file under
  `src/chemistry/` vs. move `src/chemistry.rs` → `src/chemistry/mod.rs`)
  can't compile independently — Rust treats both as ambiguous module
  paths. Merged into a single commit. Plan structure should note when
  two tasks must commit together.
- **Windows GNU debug-mode link failure** is real and undocumented in
  the plan. All integration tests need `--release` (cdylib export
  ordinal > 65535 in debug mode). Already known per the build-env
  memory file but worth surfacing for the Phase 2 plan.

## Hand-off to Phase 2
- **CpuParallel is the bit-identical oracle.** Phase 2's `GpuColored`
  must match it exactly on the scenarios in
  `tests/parallel_self_determinism.rs`. The fixed-substep design + sort
  by `(t, a, b)` + deterministic greedy coloring all exist to make this
  testable end-to-end without per-bit f32 magic.
- **Substep loop is at `src/parallel/substep.rs::do_substep`** —
  Phase 2 ports this to WGSL. The pieces map cleanly:
  - `compute_active_contacts` → grid binning + pair list compute pass.
    **This pass is ~87 % of substep cost at 30 k beads** (measured
    2026-05-25 via `src/parallel/profile.rs`); GPU parallelism of the
    CCD inner loop is where the Phase 2 payoff lands.
  - `color_pairs` → graph-colouring compute pass (or CPU pass with
    upload, depending on size).
  - per-color resolve loop → one dispatch per color with a workgroup
    per pair.
  - `advance_all` + `enforce_bonds` → two more compute passes.
- **CompiledChemistry maps cleanly to a flat GPU buffer**: rule table
  becomes a hashmap-on-GPU (perfect-hash or linear scan, depending on
  rule count); `program_pool` is already a flat `Vec<Op>` →
  `Buffer<u32>`.
- **Delete `src/gpu/` and `shaders/*.wgsl`** at the start of Phase 2 —
  the sequential GpuEventLoop has been superseded by the
  fixed-substep design and is not Phase 2's starting point.
- **Don't bother with Phase 2 for chain workloads alone.** Phase 1
  CpuParallel already hits 10k @ 242 fps. Phase 2's earn-its-keep
  scenarios are contact-dense (births, dense gases, large rock fields).
