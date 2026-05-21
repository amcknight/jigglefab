# Bench harness — design

**Status:** Approved 2026-05-21. Precedes the Phase 2 GPU-CCD work; produces the baseline numbers we'll measure that work against.

## Why

The deployed sim hits a wall around 300–500 beads on the current sequential scheduler. We're about to redesign the scheduler (Phase 2: GPU-parallel CCD), but we don't actually have numbers on *which* part of the work is dominant, where the cliff is, or how the failure looks. Without a baseline, we can't tell whether Phase 2 worked or how much.

Goal: a repeatable, extensible benchmark that produces a small table of per-substep / per-frame timings + invariant checks across a sweep of bead counts and chain shapes. Re-runnable after Phase 2 lands to compare deltas.

## Non-goals

- Rendering benchmarks (the renderer is not the bottleneck and adds wgpu init noise). A `--with-renderer` mode is a future toggle, not v1.
- Statistical rigor beyond p50/p99/max. We're looking for order-of-magnitude effects, not micro-regressions.
- Cross-machine portability. Numbers are valid relative to the dev machine; treat absolutes with skepticism.
- Tuning `CELL_SIZE` or other scheduler knobs. The harness *measures*; tuning is a separate change driven by what we measure.

## Binary

`src/bin/bench.rs`, invoked as `cargo run --release --bin bench -- [args]`.

Args (all optional):

- `--scenarios <comma-separated>` — subset of scenarios to run. Default: all in the default sweep (which excludes `chains_100x100`, opt-in only).
- `--substeps <N>` — substeps per frame. Default: 10 (matches `app.rs::SUBSTEPS`).
- `--frames <N>` — total frames per scenario, post-warmup. Default: 3000 (50 sim-seconds).
- `--warmup <N>` — warmup frames discarded. Default: 60.
- `--max-wall-seconds <S>` — per-scenario wall-clock cap. Default: 300 (5 minutes). If exceeded, the scenario reports metrics for frames completed so far and is flagged `truncated`. Keeps the harness usable even when a scenario is hours-deep in the wall.
- `--csv <path>` — emit machine-readable CSV alongside the stdout markdown table.
- `--verify-determinism` — run each scenario twice, assert final positions bit-identical. Doubles runtime, off by default.

## Scenario trait

```rust
pub trait Scenario {
    fn name(&self) -> String;            // scenario_id used in CSV / report row
    fn build(&self) -> (Sim, Invariants);// constructs Sim + captures initial invariants
}

pub struct Invariants {
    pub initial_bond_set: HashSet<(u32, u32)>,
    pub initial_state_histogram: Vec<usize>,
    // Future: total energy, cluster-size distribution, max bead speed, ...
}
```

A new scenario (knots, free gas, mixed density, anomaly replay) is a new struct implementing `Scenario`. No changes to the harness loop.

## First scenarios

All use the wire chemistry — bond-count invariant is meaningful (wire's `inside=swap, outside=reflect` preserves topology exactly; no bonds form or break).

`DisconnectedChains { chain_count, chain_len, world_size }` lays vertical chains across the world. Beads within a chain spaced 0.667 apart (just inside the bond threshold of 1.0). Chains laid out in a 2D grid pattern: as many chains per row as fit at horizontal spacing 5.0, then wrap to the next row with vertical row-spacing `chain_len × 0.667 + 2.0`. Chains never overlap initially; no cross-chain bonds at t=0. Each chain starts with one `on` bead at index 0, rest `off`.

If a single chain's vertical extent (`chain_len × 0.667`) exceeds `world_size`, the chain serpentines (alternating columns, snake-style). For the sweep below, only `chains_5x300` triggers that path; others fit a single chain in one column.

Default sweep (run by `cargo run --bin bench` with no `--scenarios` arg):

| name | chain_count × chain_len | world_size | total beads | density (beads/unit²) | layout | notes |
|------|------------------------|------------|-------------|----------------------|--------|-------|
| chains_10x30 | 10 × 30 | 50 | 300 | 0.12 | 1 row × 10 | matches the pre-bump deployed sim — our floor reference |
| chains_30x30 | 30 × 30 | 128 | 900 | 0.055 | 2 rows × 15 | more chains, multi-row layout |
| chains_50x30 | 50 × 30 | 256 | 1500 | 0.023 | 1 row × 50 | the deployed bump target |
| chains_10x100 | 10 × 100 | 128 | 1000 | 0.061 | 1 row × 10 | longer chains; the handoff's crash case |
| chains_5x300 | 5 × 300 | 256 | 1500 | 0.023 | 1 row × 5 (no serpentine needed at world 256) | very long chains |
| chains_100x30 | 100 × 30 | 256 | 3000 | 0.046 | 2 rows × 50 | past the wall |

Opt-in only (run with `--scenarios chains_100x100`):

| chains_100x100 | 100 × 100 | 256 | 10000 | 0.15 | 2 rows × 50 | aspirational; likely hits the `--max-wall-seconds` cap |

Density is *not* held constant across the sweep — we want axes for "more chains," "longer chains," and "bigger world same shape" all covered.

## Metrics, per scenario

Headline:

- `frame_time_ms` — mean, p50, p99, max
- `substep_time_us` — mean, p50, p99, max
- `effective_fps` — `1000 / mean_frame_time_ms`, unclipped (so values above 60 show real headroom and ratios across scenarios stay meaningful)

Iteration-cost diagnostics:

- `contacts_per_substep` — mean, p50, p99, max
- `candidate_pairs_per_substep` — mean only (sanity check on grid sizing)
- `iter_cap_saturation_rate` — fraction of substeps that exhausted `iter_cap = N × 64`. Should be 0 in healthy regimes. The canary for "bonds breaking from incomplete contact resolution" failure mode.

Derived budget metric (option (b)'s "saturate the budget" answer, computed from (a)'s numbers):

- `substeps_per_16ms_budget` — `16.6ms / mean_substep_time` rounded down. Reads as "the physics quality ceiling this scheduler can deliver at 60fps."

Invariants:

- `bonds_preserved` — `final_bond_set == initial_bond_set`? bool.
- `bonds_lost`, `bonds_added` — sizes of the symmetric difference, only emitted when `bonds_preserved == false`.

## Instrumenting `Sim`

Add a `StepMetrics` struct populated inside `Sim::step`. `Copy + Clone`, returned by value:

```rust
#[derive(Clone, Copy, Debug, Default)]
pub struct StepMetrics {
    pub contacts_resolved: u32,
    pub candidate_pairs: u32, // sum over all iterations within the step
    pub iter_cap_hit: bool,
}

impl Sim {
    pub fn last_step_metrics(&self) -> StepMetrics { self.last_metrics }
}
```

`candidate_pairs` is the total across all iterations within the step (not the per-iteration mean), so the harness can divide by `contacts_resolved + 1` if it wants a per-iteration average. Two `+=`s per iteration, one save of the `iter_cap` exhaustion flag. No `cfg(feature = ...)` gating — the cost is negligible and gating costs more in code clutter than it saves at runtime. Documented in the field comment as "may add a few ns to step()."

## Output

Stdout: markdown table, paste-friendly. One row per scenario. Truncated scenarios get a `*` next to their name and a footnote with frames-actually-completed.

```
| scenario        | N    | frame_ms (mean/p99) | substep_us (mean/p99) | contacts/ss (mean/p99) | fps   | sub/16ms | bonds OK |
|-----------------|------|---------------------|-----------------------|------------------------|-------|----------|----------|
| chains_10x30    | 300  | 0.8 / 1.2           | 60 / 90               | 4 / 8                  | 1250  | 277      | y        |
| chains_50x30 *  | 1500 | ...                 | ...                   | ...                    | ...   | ...      | y/n      |
```

CSV (`--csv`): one row per scenario, one column per metric, plus a header row. Machine-readable for plotting later. No commitment to a stable schema yet.

## Failure modes the harness must surface

Cross-referencing the failure-mode table from the brainstorming session:

1. **Framerate drop** — captured by `frame_time_ms` and `effective_fps`. Expected to be the dominant failure as N grows.
2. **iter_cap saturation** — captured by `iter_cap_saturation_rate`. Canary for bond breakage from incomplete contact resolution.
3. **Bond drift / breakage** — captured by `bonds_preserved` invariant.
4. **Determinism breakage** — captured (only) by `--verify-determinism`.

Memory ceilings, CCD numerical garbage at huge world coordinates, and tunneling at coarse substeps are *not* exercised by the default sweep — they're far outside our currently planned operating range. Future scenarios can target them deliberately.

## Open knobs and future work (flagged, not in v1)

- **`CELL_SIZE` tuning**: currently 2.0 (conservative), theoretically optimal is ~1.1–1.2. Pair-scan isn't the dominant cost, so this is small fish until Approach A lands.
- **Powers-of-2 `cells_per_axis`**: the wrap-around modulo in `grid.rs:38-39` collapses to a bitmask. Free win. Recommend `world_size ∈ {64, 128, 256, ...}` going forward.
- **`--with-renderer` mode**: measure total load including the rendering path.
- **Knot scenarios**: dense bundles of bonded beads; stress-tests contact-rate ceilings.
- **Free-gas scenarios**: no initial bonds, beads bond on contact. Requires a chemistry change first.
- **Density sweeps**: parameter sweep on `density` for a fixed shape, plotted to find the cliff.
- **Cross-chemistry comparison**: same fabs under different chemistries.
- **Anomaly menagerie integration**: regression-test bench mode that replays archived (seed, scenario, frame) tuples and asserts invariants — design-doc P2 anomaly-menagerie work.

## Out of scope (this spec)

- Phase 2 GPU-parallel CCD itself (Approach A). Gets its own spec after we have baseline numbers.
- Phase 2 invariants and anomaly menagerie *beyond* the bond-count check the harness needs.
- Any modification to existing tests, chemistries, or sim behavior beyond adding `last_step_metrics`.

## Deliverables

1. `src/bin/bench.rs` — the binary.
2. `src/sim.rs` — add `StepMetrics` and `last_step_metrics`.
3. `docs/bench-results/2026-05-21-baseline.md` — captured first-run table, committed as the baseline reference.

Separate from this spec (committed alongside but not part of the harness work):

4. Rename `fabs/wire-10x30.toml` → `fabs/wire-50x30.toml`, bump `world_size = 50` → `256`, expand to 50 chains × 30 beads.
5. Update `app.rs:36` and `app.rs:98` to point to the new path.
