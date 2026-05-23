# GPU CCD Scheduler — Design Spec
_2026-05-23_

## Goal

Replace the sequential CPU event loop with a GPU-resident event loop that preserves bit-identical physics while scaling to 100k+ beads. The web (WebGPU) build is the eventual target; native (Vulkan/Metal via wgpu) is the first milestone.

---

## Constraints and non-goals

- **Bit-identical results**: the GPU path must produce the same contact sequence as `CpuSequential` for the same seed and scenario. Tiebreak rule `(t, a, b)` is preserved exactly.
- **Chemistry-agnostic**: no assumptions about which pairs interact; all candidate pairs from the grid run CCD. Bonded-only filtering is not applied (non-bonded beads bounce externally in most chemistries).
- **Creation/deletion out of scope**: stick to chemistries where beads are not spawned or destroyed mid-run. The design must not break if a future chemistry rarely creates beads, but correctness is not guaranteed for that case yet. Spawning at distance < R is the main risk.
- **Bond topology changes**: rare but real (chain folding brings non-adjacent beads within R). Handled via a bond-dirty flag; CPU re-uploads the bond buffer on frames where topology changes.
- **No new dynamics**: velocities remain unit-speed; elasticity, wire chemistry semantics, and enforce_bonds are unchanged.

---

## Architecture

### Scheduler trait

The existing event loop is extracted from `Sim::step()` into a `Scheduler` trait:

```rust
pub trait Scheduler {
    fn step(&mut self, sim: &mut Sim, dt: f32) -> StepMetrics;
}
```

`Sim` holds a `Box<dyn Scheduler>`. Two implementations ship:

| Name | Description |
|---|---|
| `CpuSequential` | Existing loop, moved verbatim. Reference implementation. |
| `GpuEventLoop` | New. GPU-resident event loop, bit-identical results. |

Both are permanently available. Bench selects via `--scheduler cpu|gpu`. App defaults to `gpu` if a compatible adapter is found, falls back to `cpu`.

### Data ownership: CPU-primary

`Sim`'s `positions`, `velocities`, `states`, and `bonds` remain the source of truth on CPU. Per frame, `GpuEventLoop::step()`:

1. Uploads positions, velocities, states to GPU buffers.
2. Uploads bonds if the bond-dirty flag is set.
3. Dispatches the event loop in batches until done.
4. Reads back positions, velocities, states into `Sim` fields.

Upload cost at 100k beads: ~1.6 MB/frame. At PCIe bandwidth this is sub-millisecond and acceptable. Can migrate to GPU-primary later if upload becomes a bottleneck.

### Device sharing

`GpuEventLoop` takes `Arc<wgpu::Device>` and `Arc<wgpu::Queue>` from the existing `Renderer`. No second GPU context is created. For headless bench runs, a minimal wgpu context is created without a surface (already supported by wgpu).

---

## GPU buffers

Allocated at `GpuEventLoop::new()` for `max_beads` capacity. Resized if a scenario exceeds capacity.

| Buffer | Layout | Usage flags |
|---|---|---|
| `positions` | `N × vec2f` | STORAGE + COPY_DST + COPY_SRC |
| `velocities` | `N × vec2f` | STORAGE + COPY_DST + COPY_SRC |
| `states` | `N × u32` | STORAGE + COPY_DST + COPY_SRC |
| `bonds` | `M × (u32, u32)` | STORAGE + COPY_DST |
| `grid_counts` | `cells² × u32` | STORAGE (cleared each frame) |
| `grid_offsets` | `cells² × u32` | STORAGE (prefix-sum output) |
| `grid_beads` | `cells² × K × u32` | STORAGE (K = max beads per cell, default 32) |
| `contacts` | `P × Contact` | STORAGE (one slot per candidate pair) |
| `reduce_scratch` | `(P/256) × Contact` | STORAGE (inter-workgroup reduction) |
| `params` | uniform | N, M, P, world_size, dt_remaining, cells_per_axis, K |
| `chemistry` | uniform | action table: `states² × 2 × u32` |
| `status` | `(f32, u32)` | MAP_READ — dt_remaining + done flag |

`Contact` struct: `{ t: f32, a: u32, b: u32, exiting: u32 }`. A contact with `t = f32::MAX` means no contact found by that pair.

If a cell exceeds K beads, `status.done` is set to `2` (overflow sentinel); CPU falls back to `CpuSequential` for that frame. Expected to never fire at reasonable densities.

---

## Per-iteration dispatch sequence

One iteration resolves one contact (or advances the full remaining dt if none found).

```
Pass 1 — grid_count      N threads    each bead atomically increments grid_counts[cell]
Pass 2 — grid_scan       cells²       prefix-sum grid_counts → grid_offsets
Pass 3 — grid_fill       N threads    each bead writes its id into grid_beads[cell][slot]
Pass 4 — ccd             P threads    one per candidate pair; solves next_contact(); writes to contacts[]
Pass 5 — reduce (x2)     P threads    workgroup tree reduction → reduce_scratch; then single workgroup → contacts[0]
Pass 6 — advance_resolve N+1 threads  N threads advance all beads by contacts[0].t; 1 thread applies chemistry action
```

After pass 6, `status.dt_remaining` and `status.done` are updated by the shader.

### Candidate pair generation

Candidate pairs are enumerated during pass 3 or a separate pass 3b. For each cell, pairs are formed from beads in that cell plus beads in the 8 neighboring cells (9-cell neighborhood, torus-wrapped). Only pairs with `a < b` are emitted to avoid duplicates. The total count P is written atomically and capped at `contacts.len()`.

---

## Batch structure (CPU↔GPU sync)

WebGPU has no persistent GPU-side while-loop across dispatches. CPU drives iteration:

```rust
loop {
    // dispatch passes 1–6 a fixed number of times (e.g. 64)
    // shaders early-exit (write no-ops) once status.done is set
    encoder.dispatch(grid_count, ...);
    // ... other passes ...
    encoder.dispatch(advance_resolve, ...);

    queue.submit(encoder.finish());
    status = read_status_buffer(); // one async readback per 64 contacts
    if status.done != 0 || status.dt_remaining <= EPS { break; }
}
```

One round-trip per 64 contacts resolved. At 2000 contacts/frame this is ~32 round-trips, well under 1ms overhead on modern hardware. Batch size is a tunable constant.

---

## CCD shader (`ccd.wgsl`)

Direct port of `next_contact()` from `src/ccd.rs`:

- Reads `positions[a]`, `positions[b]`, `velocities[a]`, `velocities[b]`.
- Applies min-image torus wrapping to the displacement vector.
- Solves quadratic for `t ∈ [0, dt_remaining]` where `|d(t)|² = R²`.
- Computes `exiting = dot(d(t), dv) > 0`.
- Writes `Contact { t, a, b, exiting }` to `contacts[pair_id]`.
- Writes `Contact { t: f32::MAX, ... }` if no root in range.

The math is identical to the CPU version; floating-point order is preserved by using the same operations. This is what makes results bit-identical.

---

## Reduction (`reduce.wgsl`)

Two-pass tree reduction. No `f32` atomics required (WebGPU only supports `i32`/`u32` atomics).

Comparator: lexicographic on `(t, a, b)` — same tiebreak as CPU scheduler.

- **Pass 5a**: each workgroup (256 threads) reduces its 256 contacts to one winner in shared memory. Writes winner to `reduce_scratch[workgroup_id]`.
- **Pass 5b**: single workgroup reduces `reduce_scratch` to `contacts[0]`.

Result in `contacts[0]` is the global earliest contact. If `contacts[0].t == f32::MAX`, no contact was found; `advance_resolve` advances all beads by `dt_remaining` and sets `done = 1`.

---

## Advance and resolve (`advance_resolve.wgsl`)

- **N threads**: each bead `i` advances: `positions[i] += velocities[i] * contacts[0].t` (with torus wrap).
- **Thread 0**: looks up `chemistry[states[a] * S * 2 + states[b] * 2 + exiting]`. Applies:
  - `Reflect`: swap normal-component velocities of a and b.
  - `Pass`: no-op.
  - `ReflectSwap`: Reflect + swap `states[a]` and `states[b]`.
- Updates `status.dt_remaining -= contacts[0].t`.
- If a state swap occurred that changes bond topology, sets a bond-dirty flag in `status`. CPU re-uploads bonds next frame.

---

## File layout

```
src/
  gpu/
    mod.rs
    buffers.rs      — GpuBuffers: allocation, upload, readback helpers
    pipelines.rs    — pipeline + bind group layout construction
    scheduler.rs    — GpuEventLoop struct + impl Scheduler

shaders/
  grid_count.wgsl
  grid_scan.wgsl
  grid_fill.wgsl
  ccd.wgsl
  reduce.wgsl
  advance_resolve.wgsl
  beads.wgsl         — existing render shader, untouched
```

Existing `src/render.rs`, `src/sim.rs`, `src/ccd.rs`, `src/collide.rs` are minimally changed. `Sim::step()` becomes a thin delegator to `self.scheduler.step(self, dt)`.

---

## Integration points

### Bench

`src/bin/bench.rs` adds `--scheduler cpu|gpu`. Scenario `build()` is unchanged. `StepMetrics` is returned from both paths; all existing bench output works.

### App

`app.rs` creates `GpuEventLoop` if a wgpu adapter with compute support is available, otherwise `CpuSequential`. No other changes.

### Determinism test

Existing `tests/determinism.rs` is extended: run each scenario with both schedulers and assert bit-identical position/velocity sequences. This is the primary correctness gate.

---

## Known risks and mitigations

| Risk | Mitigation |
|---|---|
| Prefix-sum shader correctness | Use a reference implementation (e.g., from wgpu examples or a known-correct parallel scan) rather than writing from scratch |
| Torus wrapping bugs in WGSL | Port the exact `rem_euclid` logic from `grid.rs`; cover with determinism test |
| max-beads-per-cell overflow | Overflow sets a sentinel in `status`; CPU falls back to `CpuSequential` for that frame |
| Round-trip overhead at high contact rates | Batch size is tunable; start at 64, profile and adjust |
| WebGPU adapter availability | Graceful fallback to `CpuSequential`; app reports which scheduler is active |
| Bond topology change mid-frame | Bond-dirty flag triggers re-upload; rare enough that per-frame cost is negligible |

---

## Out of scope (future work)

- Bead creation/deletion in chemistry
- GPU-primary data layout (positions live on GPU, CPU never owns them)
- Graph-coloring batched CCD (non-identical but faster alternative)
- Multi-GPU or compute-only path for server-side simulation
