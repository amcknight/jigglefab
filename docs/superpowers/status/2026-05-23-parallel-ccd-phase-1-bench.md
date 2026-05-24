# Parallel-CCD Phase 1 Bench Results — 2026-05-23

## Setup
- Ryzen (Windows 11, GNU toolchain), today's `main` with Phase 1 merged.
- `cargo run --release --bin bench -- --scheduler {cpu, cpu-parallel} --substeps 1 ...`
- 200-frame runs at small N (200×1=200 substeps) and 60-frame runs at large N to keep the wall budget in check.
- DEFAULT_DT_SUB inside CpuParallel = 1/240, so `--substeps 1` here means one frame per call; CpuParallel itself sub-steps 4× internally per frame_dt = 1/60.

## CpuParallel measured
| scenario         |   N    | mean ms |  p99 ms |  fps  | bonds_ok |
|------------------|-------:|--------:|--------:|------:|:--------:|
| chains_30x30     |    900 |    0.32 |    0.45 |  3130 |    y     |
| chains_10x100    |  1 000 |    0.33 |    0.39 |  3040 |    y     |
| chains_5x300     |  1 500 |    0.78 |    1.47 |  1276 |    y     |
| chains_100x30    |  3 000 |    1.13 |    1.30 |   888 |    y     |
| chains_100x100   | 10 000 |    4.13 |    4.97 |   242 |    y     |

## Comparison to CpuSequential (same scenarios)
| scenario         |   N    | seq ms  | par ms | speed-up | seq bonds_ok |
|------------------|-------:|--------:|-------:|---------:|:------------:|
| chains_30x30     |    900 |    1.58 |   0.32 |     5.0× |      y       |
| chains_10x100    |  1 000 |    2.08 |   0.33 |     6.3× |      y       |
| chains_5x300     |  1 500 |   1465  |   0.78 |  ~1900×  |      n       |
| chains_100x30    |  3 000 |   1094  |   1.13 |   ~970×  |      n       |
| chains_100x100   | 10 000 | 130 204 |   4.13 | ~31 500× |      n       |

> CpuSequential past ~1k beads is dominated by the iter_cap pathology that
> the parallel-CCD design was built to fix — pairs near the start of the
> bead list keep firing while pairs near the end never get scheduled in
> a single frame, so the iterative one-contact-at-a-time loop saturates
> its budget. The seq numbers above include that saturation plus
> truncation, so the "speed-up" multipliers are dominated by the
> bonds_lost regime, not pure throughput. Comparison at small N (where
> seq still terminates cleanly) is the honest measure: ~5–6× faster.

## Conclusions
- CpuParallel is **5–6× faster** than CpuSequential at small N (clean termination).
- At 10 000 beads CpuParallel runs at **242 fps** (4 ms / frame) and preserves
  bonds — comfortably above the 60-fps goal the spec set as Phase 1 target.
  No need for Phase 2 (GPU) on chain workloads alone; Phase 2 still earns
  its keep on contact-dense scenarios (births, dense gases) where the
  per-color count blows past CPU cache budget.
- iter_cap saturation = 0 across all CpuParallel runs — the fixed-substep
  loop has no iter cap to saturate, by design.
- Next: write the Phase 2 (GpuColored) plan; it inherits CpuParallel as
  the bit-identical oracle.
