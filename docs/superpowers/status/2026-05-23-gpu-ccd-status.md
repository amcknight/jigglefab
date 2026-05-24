# GPU CCD — status as of 2026-05-23

A handoff doc so a fresh session can pick up where this one ended.

## Where things stand

**Built and correct:** A `Scheduler` trait with two implementations — `CpuSequential` (wraps the existing event loop) and `GpuEventLoop` (GPU-resident, 6 WGSL compute shaders, 8 dispatches per iteration). The full 12-task plan in `docs/superpowers/plans/2026-05-23-gpu-ccd.md` shipped. App defaults to GPU on native, CPU on WASM. Bench takes `--scheduler cpu|gpu`. Self-determinism test exists at `tests/gpu_determinism.rs` (ignored, requires GPU). 46+ lib tests pass; `cargo build --release` clean.

**Not done:** Performance characterization. The plan scoped *bit-identical correctness*, not *throughput*. That gap was relayed in passing during Task 12 ("GPU bench runs at ~0.1 fps at 300 beads") but never investigated. When the user tried to run the visual demo at chains_30x30 (900 beads, the original CPU cliff), it was unwatchably slow — comparable to or worse than CPU. The user's instinct that "GPU should handle 10k+" is correct in principle and the current implementation does not.

## What the numbers actually look like

CPU on this machine (Ryzen 7 9800X3D), 30-frame probe with 1-frame warmup, today's `main` (not the original `e1839ba` baseline):

```
chains_10x30   N=300   mean=1.59 ms   p99=1.92 ms   628 fps
chains_30x30   N=900   mean=17.4 ms   p99=21.1 ms    57 fps
chains_10x100  N=1000  mean=20.4 ms   p99=23.4 ms    49 fps
```

This is dramatically better than the original baseline doc (`docs/bench-results/2026-05-21-baseline.md`) reports for chains_30x30 (2.6 fps). Two possibilities, not yet investigated:

1. **The cliff was a knot-formation tail.** The baseline ran 600 frames; this probe ran 30. Knots that drive the heavy-tail substep distribution may not have formed yet. Need a long-warmup re-bench to confirm whether 57 fps holds at frame 300.
2. **Something changed in the scheduler.** The recent commits include `e1839ba "Revert speed-tunneling probes"` and `dd6a7d9 "Baseline doc: correct interpretation of bond invariant failures"`. Worth checking whether the revert improved the cliff, intentionally or otherwise.

**GPU numbers** are not directly measured in this session beyond the implementer's "~0.1 fps at 300 beads" note. That implies the GPU path is currently *orders of magnitude* slower than CPU at the only scale we have working visuals for. No GPU vs CPU sweep across bead counts was run.

## Why GPU is probably slow (root-cause hypotheses, unverified)

The implemented design is **inherently sequential**: each iteration resolves exactly one contact, advances all beads, then dispatches again. For N candidate pairs producing M contacts per frame, that's M full GPU passes. The parallelism is *within* each iteration's collision detection and reduction — not across iterations.

Concrete suspects, ordered by likely impact:

1. **Bind groups rebuilt every iteration.** `src/gpu/scheduler.rs` `encode_iteration()` calls `device.create_bind_group()` for all 8 dispatches inside the iteration loop. With BATCH_SIZE=64 iterations per submit, that's 512 bind group creations per submit. They should be created once at scheduler init and reused — buffer handles never change. This is a CPU-side overhead, separate from GPU time.

2. **Per-batch readback synchronizes CPU↔GPU.** Every 64 iterations the CPU does `map_async` + `poll(Maintain::Wait)` to read 12 bytes of status. Each readback is at least a millisecond of pipeline bubble. If a frame needs hundreds of contact resolutions, dozens of these stalls accumulate.

3. **CCD shader scans `max_pairs` threads regardless of actual pair count.** `max_pairs_for(N, 32) = N * 32 * 9 / 2 + 64`, capped at 65536. At N=900 that's 65536 threads (1024 workgroups) dispatched even if the real pair count is ~500. Same waste in `reduce_local`.

4. **The cap at 65536 pairs was a correctness fix.** `reduce_global` is a single 256-thread workgroup that can only reduce 256 scratch entries. The fix (commit `290baa8`) caps max_pairs at 256×256 so the reduce is correct, but at the cost of falling back to CPU on overflow. For 10k beads at moderate density, `n_beads * 32 * 9 / 2` is well over the cap — the overflow flag will fire and the GPU path will be unable to even attempt the frame. **The cap means the current GPU scheduler effectively cannot run at 10k+ beads.** The actual fix is a 3-pass reduction or a different reduce strategy.

5. **Eight dispatches per iteration with implicit barriers between each.** wgpu inserts memory barriers between compute passes. Cheap but not free. Could fuse some passes (grid_count + grid_fill done together with a workgroup barrier; reduce_local + reduce_global merged for small contact counts).

6. **Inherent serialization is the design.** Even with all overheads removed, finding one earliest contact across thousands of pairs, applying it, then redoing the whole search, will not scale to 100k beads no matter how fast each pass is. The spec called this out — `graph-coloring batched CCD` was listed as out-of-scope future work. **The honest path to 10k+ beads at 60fps is a parallel collision resolution algorithm, not a faster sequential one.**

None of these are confirmed by profiling. They are reasoned hypotheses based on reading the code.

## Files of interest

- `docs/superpowers/specs/2026-05-23-gpu-ccd-design.md` — original design
- `docs/superpowers/plans/2026-05-23-gpu-ccd.md` — 12-task implementation plan (all tasks completed)
- `docs/bench-results/2026-05-21-baseline.md` — pre-GPU baseline (may be stale, see point 1 above)
- `src/scheduler.rs` — trait + `CpuSequential`
- `src/gpu/scheduler.rs` — `GpuEventLoop::step` (see `encode_iteration` for the bind-group rebuilding)
- `src/gpu/buffers.rs` — `max_pairs_for` (note the 65536 cap)
- `shaders/reduce.wgsl` — the two-pass reduction with 256-entry limit
- `src/app.rs` — currently builds `DisconnectedChains { 30, 30, 128.0 }` on native (900 beads), `wire-10x30.toml` on WASM
- `Cargo.toml` — has `default-run = "jigglefab-bin"`

## Open questions for next session

1. **Is the original CPU cliff real?** Re-run with `--warmup 60 --frames 600` matching the baseline and see if chains_30x30 still cliffs to 2.6 fps or whether something fixed it.
2. **Where does GPU actually break even with CPU?** Sweep both schedulers across chains_NxM scenarios. Find the N where GPU stops losing. Useful even if the answer is "never, with this algorithm."
3. **Is bind-group reuse worth implementing?** A 30-minute fix that might 10× the GPU path. Cheap to try.
4. **Lift the 65536 pair cap.** Required to ever exercise 10k beads on the GPU path. Three-pass reduction is the standard fix.
5. **Is the sequential algorithm the wrong choice?** The user wants 10k+ beads at interactive rates. The spec acknowledged graph-coloring CCD as the path to that, deferred as future work. Worth revisiting that scope decision rather than tuning the sequential path indefinitely.

## What to tell the user when they come back

- The GPU work is correct but not yet a performance win
- Performance was not in the original plan scope (an oversight on my part not to flag during planning)
- Three credible paths forward, in increasing scope:
  - **Tune the existing GPU path** (bind-group reuse, lift the 65536 cap, profile and fix per-iteration overhead) — incremental wins, sequential algorithm caps the ceiling
  - **Sparse-layout demo** as a working visual even if it's not "100s of chains" (chains_10x100 holds 49 fps on CPU)
  - **Rescope to parallel CCD** (graph coloring or fixed-substep parallel resolution) — actual road to 10k+ beads
