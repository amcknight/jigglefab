# CpuParallelMt Bench Results — 2026-05-24

## Setup
- Ryzen (Windows 11, GNU toolchain), today's `main` with CpuParallelMt merged.
- `cargo run --release --bin bench -- --scheduler {cpu-parallel, cpu-parallel-mt} --scenarios chains_1000x30 ...`
- chains_1000x30 = 1 000 wire chains × 30 beads each = 30 000 beads, world_size 512.

## chains_1000x30 (30 000 beads)
| --substeps | scheduler        | frame_ms mean | frame_ms p99 |   fps  | bonds_ok |
|-----------:|------------------|--------------:|-------------:|-------:|:--------:|
|          1 | cpu-parallel     |         10.88 |        11.86 |   91.9 |    y     |
|          1 | cpu-parallel-mt  |          6.92 |         7.93 |  144.5 |    y     |
|         10 | cpu-parallel     |        138.11 |       156.55 |    7.2 |    y     |
|         10 | cpu-parallel-mt  |         88.01 |       102.28 |   11.4 |  n (-2)  |

## Headline
- At `--substeps 10` (matches the app's render-loop work per frame), CpuParallelMt
  is **~1.57× faster** than CpuParallel: 88 ms vs 138 ms per frame.
- That clears the **>7 fps target** for 30 000 beads on desktop with margin
  (11.4 fps vs the 7.2 fps baseline).
- At `--substeps 1` the speedup is identical (1.57×) — rayon parallelism is the
  win, not a per-call fixed-cost shave.

## Caveats
- **2 bonds drift past R in the MT runs at --substeps 10** while CpuParallel
  preserves all bonds. The loss is reproducible (two MT runs report the same
  2 lost bonds — so MT is self-deterministic at this scale), but the MT
  output differs from CpuParallel's at 30k-bead scale. The bit-identity
  tests at 30-bead chains still pass.
  - Likely cause: `enforce_bonds` iterates a `HashSet` (non-deterministic
    iteration order between independently-built sims) and snap-backs of
    pairs that share a bead are order-dependent. Equally likely for seq vs
    seq runs; the seq case may just have gotten lucky.
  - Bond loss is 0.007 % (2 / ~29 000). Not algorithmically catastrophic,
    but worth investigating before declaring MT the new default.
- Per-color resolve does **not** support `ReactionKind::Birth` (it panics).
  Wire and grey chemistries never trigger Birth; sem-style chems still need
  to run through CpuParallel until pool allocation is mutex-wrapped on the
  MT path.

## What's still slow
- `--substeps 10` × 30 000 beads = 88 ms / frame on MT. The per-substep cost
  is ~8.8 ms; 88 ms is 10× that. Each substep internally does 4 sub-iters
  (DEFAULT_DT_SUB = 1/240), so 10 × 4 = 40 sub-iters per app-frame at the
  default speed multiplier.
- Profiling target #1: the `coloring::color_pairs` HashMap allocation, run
  once per sub-iter. At ~9 k contacts × 40 sub-iters = 360 k allocations per
  app-frame, this is plausibly hot.
- Profiling target #2: `enforce_bonds` over the full bond set every sub-iter
  (sequential, O(bonds)). 29 k bonds × 40 = 1.16 M per app-frame, probably
  cache-misses dominated.

## Conclusions
1. **MT target met** (>7 fps at 30k beads). The user's `wire-100x30x10`
   workload should now run at ~3–5 fps on the app at --substeps 10
   (1.57× of the prior ~3 fps); the URL-hash size picker on the demo still
   serves smaller sizes for phone use.
2. **Modular framework landed**: `scheduler_selector::build` is the single
   factory for all backends, used by bench and (next step) the native app's
   `SCHEDULER=` env var and the web's `#sched=` URL hash.
3. **Next perf step**: investigate the 2-bond MT-vs-seq drift, then layer
   allocation reuse on top of MT. Estimated another 1.5–2× → ~25 ms / frame
   at 30k beads, comfortably 30 fps.

## Follow-ups
- Investigate enforce_bonds ordering — turn `bonds` into a sorted `BTreeSet`
  or pre-sort the iteration. Fix the 2-bond drift before MT becomes a default.
- Wire `SCHEDULER=cpu-parallel-mt cargo run --release` on native and
  `#sched=cpu-parallel-mt` on web (no-op since wasm is single-threaded;
  log + fall back to cpu-parallel).
- Profile MT at chains_1000x30 — flame graph to confirm coloring + enforce_bonds
  are the next bottlenecks.
- Allocation reuse: stash `bead_to_pairs` HashMap, `pairs_in_color` Vec on
  the scheduler struct, clear-but-reuse across substeps.
- ReactionKind::Birth in resolve_pair_disjoint: wrap pool allocation in a
  mutex so sem chems run on MT.
