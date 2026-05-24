# Parallel-CCD Scheduler — Design Spec

_2026-05-23_

## Goal

Replace the existing sequential-on-GPU CCD scheduler ([src/gpu/](../../../src/gpu/)) with a parallel-CCD scheduler that can sustain **10,000 beads at 60 fps** in chain-heavy scenarios, while supporting the full expressivity of the Haskell `Sem` chemistry (action stacks, births, deaths, direction-dependent passthrough). The previous GPU scheduler shipped without a perf gate and runs ~0.1 fps at 300 beads because the algorithm is inherently sequential — find earliest contact, resolve, repeat. This design picks a different algorithm: graph-colored parallel CCD with fixed substeps.

The new algorithm cannot be bit-identical to the existing `CpuSequential` scheduler (different ordering semantics). A new CPU implementation of the parallel algorithm (`CpuParallel`) is built alongside and serves as the bit-identical oracle for the GPU path.

## Non-goals

- **Bit-identical match to `CpuSequential`.** Sacrificed knowingly. Sequential CCD's `(t, a, b)` tiebreak is not reproducible in a parallel-within-color scheme.
- **WebGPU compute on WASM** as part of v1. `CpuParallel` is the WASM scheduler. WebGPU support follows once the native GPU path is stable.
- **Variable bead speed support beyond what reactions naturally produce.** Bead initial speed remains 1.0; collisions amplify individual speeds (max can drift up by lucky reflections); the substep size is chosen conservatively for amplified speeds. Speed-adaptive substep is flagged as a future refinement.
- **GPU-primary data layout.** Data ownership stays CPU-primary as in the previous design. Migration to GPU-primary is a future option if upload bandwidth becomes the bottleneck (it won't at 10k beads).
- **Chemistries beyond Sem's algebraic shape.** Reactions remain pure functions of `(state_a, state_b, direction)` returning Exchange / LeftOnly / RightOnly / Birth. Future chemistries that need persistent per-bead memory beyond an action stack, or many-bead reactions, are out of scope.

---

## Architecture

### Scheduler hierarchy

The `Scheduler` trait in [src/scheduler.rs](../../../src/scheduler.rs) is unchanged. Three implementations after this work:

| Name | Algorithm | Purpose |
|---|---|---|
| `CpuSequential` | Existing event-driven loop with global earliest-TOI tiebreak | Frozen reference for prototype chems (grey, wire). Existing tests unchanged. |
| `CpuParallel` | New: graph-colored fixed-substep CCD, Sem-class chemistries | New correctness oracle. Bit-identical target for `GpuColored`. Native fallback when no GPU. WASM default. |
| `GpuColored` | Same algorithm as `CpuParallel`, on GPU via WGSL compute | Performance path. Replaces `GpuEventLoop` entirely. |

**Default selection:**
- Native: try `GpuColored` → fall back `CpuParallel` → final fallback `CpuSequential` *only* if the loaded chemistry is prototype-class (grey, wire).
- WASM: `CpuParallel` for all chemistries (including Sem-class) until WebGPU compute support is added.
- Bench: `--scheduler cpu-sequential | cpu-parallel | gpu-colored`.

### Deleted

The entire [src/gpu/](../../../src/gpu/) module (current `GpuEventLoop`) and its WGSL shaders ([shaders/](../../../shaders/) — `ccd.wgsl`, `reduce.wgsl`, `advance_resolve.wgsl`, `iter_init.wgsl`, plus the grid shaders which may be salvaged). The algorithm has no future and the code adds maintenance burden.

The existing `CpuSequential` scheduler stays as a frozen reference for the existing prototype chemistries. It's small, useful as a comparison baseline, and the existing test suite depends on it.

---

## Algorithm

### Per-substep dispatch

A frame is divided into `ceil(frame_dt / dt_sub)` substeps with `dt_sub` fixed at 1/240 s by default (configurable per scenario). At 60 fps that's 4 substeps per frame; at 30 fps, 8.

Each substep:

1. **Grid build** — bin all alive beads into a uniform spatial grid (cell size = R). Three passes: count, prefix-sum, fill. Same as today.
2. **Candidate pair enumeration** — for each cell, emit pairs with the 9-cell neighborhood (`a < b` to deduplicate). Atomic counter for pair count `P`.
3. **TOI computation** — `P` threads, one per pair. Each computes `next_contact(...)` for `t ∈ [0, dt_sub]`. Pairs with no contact in range, or pairs involving any bead with `flags.born_this_substep = 1`, are marked `t = f32::MAX`.
4. **Filter active contacts** — stream-compact pairs whose `t < f32::MAX` into a dense `active_contacts` buffer. (CPU implementation: filter into a Vec.)
5. **Graph coloring** — color the conflict graph so no two pairs sharing a bead are the same color. Algorithm: parallel deterministic coloring via Jones-Plassmann variant (each pair picks the lowest color not used by any neighbor with a smaller `(t, a, b)` key). On CPU, sequential greedy with the same key ordering for determinism.
6. **Resolve color-by-color** — for color `k = 0, 1, 2, ...`:
   - One kernel dispatch (CPU: one parallel for-each) over all pairs of color `k`.
   - Each thread: look up reaction in `chemistry_rule_table`, apply (Exchange / LeftOnly / RightOnly / Birth), update positions/velocities/states with the snap-back rules from [src/sim.rs](../../../src/sim.rs).
   - Births claim free slots via atomic increment on `free_list_head`; if `free_list_head` would exceed pool capacity, set overflow flag.
   - Deaths mark the slot's `alive` flag = 0 and atomically push the slot index onto the free list.
7. **Advance all alive beads** by `dt_sub`. Torus-wrap positions.
8. **Enforce bonds** — same logic as [src/sim.rs:112-144](../../../src/sim.rs#L112-L144), parallelized across bonds. Note: this runs per-substep, not per-frame as in `CpuSequential`. Bond repair is more frequent in `CpuParallel` / `GpuColored`; this is a deliberate accuracy improvement at slight cost.

### Coloring deterministically (the critical detail)

Determinism requires both the conflict-graph structure and the color assignment to be reproducible. The deterministic Jones-Plassmann variant:

- Each pair `p` has a key `(t_p, a_p, b_p)`. Keys are unique because `(a, b)` uniquely identify a pair and the same pair never appears twice in one substep's `active_contacts`.
- In parallel: each pair examines its neighbors (other pairs sharing one of its beads). If `p` has the smallest key among its uncolored neighbors, it gets the lowest color not yet taken by its already-colored neighbors. Otherwise it waits one round.
- Repeat until all pairs are colored. Provably finite rounds; in practice ≤chromatic-number + a few.

Neighbor enumeration: for each pair `(a, b)`, neighbors are other pairs containing `a` or `b`. With a per-bead "pairs touching this bead" index (built during pass 4), each pair has ≤2·(max_neighbors-1) = ≤10 neighbors in 2D.

### Bonds and bond changes

Bonds are a sorted `Vec<(u32, u32)>` on CPU, uploaded to GPU as a buffer when a dirty flag is set.

Bond changes within a substep:
- **Birth creates bonds** between the newborn and both parents (matching Haskell `addIn` with `bIns = [i1, i2]`). The new bonds are queued in a per-substep `pending_bonds` buffer, integrated at the end of the substep on CPU.
- **Death removes bonds** involving the dead bead. Queued in `pending_dead_slots` and processed at end-of-substep.
- **Chain folding** (non-adjacent beads coming within R) creates new bonds when their distance drops below R during `enforce_bonds`. Detected and queued there.

The bond buffer is reuploaded each frame when any bonds change. Cost is small (24 KB at 10k bonds).

---

## Bead state representation

### On-GPU and in-`CpuParallel` storage

A bead is a fixed-size struct in a pre-allocated pool of `pool_capacity` slots. `pool_capacity = max(2 * initial_bead_count, 1024)`, configurable per fab.

```wgsl
struct Bead {
    pos: vec2<f32>,                       //  8 bytes
    vel: vec2<f32>,                       //  8 bytes
    tag: u32,                             //  4 bytes — Wire | Port | Sensor | Creator | Destroyer | Rock | Dead
    payload: u32,                         //  4 bytes — overloaded by tag (e.g. Port's Active enum)
    flags: u32,                           //  4 bytes — alive bit, born-this-substep bit, reserved
    stack_len: u32,                       //  4 bytes — number of valid ops in `stack`
    stack: array<u32, STACK_CAP>,         // 64 bytes at STACK_CAP=16
}
// total: 96 bytes
```

`STACK_CAP = 16` opcodes. Sized for known Sem programs (longest literal: 9 ops flat) with headroom.

Pool buffer at 10k initial beads (20k capacity): 20,000 × 96 = 1.92 MB. Acceptable on any modern GPU.

### Action stack encoding

Each opcode is one u32:

```
bits 28-31: opcode kind (16 values)
bits  0-27: payload (interpretation depends on opcode)
```

Opcodes (matches Sem's `Act`):

| Kind | Name | Payload |
|---|---|---|
| 0 | `Nop` | — |
| 1 | `Sig` | 0=Red, 1=Blue |
| 2 | `Apply` | — |
| 3 | `Done` | — |
| 4 | `Wait` | — |
| 5 | `Take` | — |
| 6 | `Drop` | — |
| 7 | `Die` | — |
| 8 | `Spawn` | — |
| 9 | `Hold` | — |
| 10 | `Send` | program index (into `program_pool`) |
| 11-15 | reserved | for future chemistry opcodes |

### Chemistry compiled artifact

At chemistry-load time, the chemistry TOML/source is compiled to a GPU-uploadable artifact:

- **`program_pool: array<u32>`** — flat buffer of all Send sub-programs referenced anywhere in the chemistry's rules. Each program is a sequence of `(stack_len, op_0, op_1, ...)`.
- **`program_index: array<(start: u32, length: u32)>`** — for each named program, its offset and length in `program_pool`. Send opcodes reference programs by index.
- **`rule_table`** — lookup table keyed by `(tag_a, payload_a, tag_b, payload_b, side)`. Each entry specifies:
  - `kind`: Exchange | LeftOnly | RightOnly | Birth
  - `new_state_a`, `new_state_b`: tag + payload + optional program-load-into-stack
  - `birth_state` (if Birth): tag + payload + program for the new bead
- **`allow_thru`** lookup table keyed by `(tag_a, tag_b, side)` → bool.

The rule-table key is `(tag_a, top_op_a, tag_b, top_op_b, side)` where `top_op_*` is the top opcode of the bead's action stack (or 0 / "empty" if the stack is empty) for Wire beads, or the payload (e.g. Port's Active state) for non-Wire beads. This matches Sem's `innerReact` patterns: reactions depend on the top opcode of each bead's stack plus the tag, not on deeper stack content.

For Sem-class: ~6 tags × ~16 top-op values = 96 effective states per side, giving 96² × 2 = ~18K rule-table entries, each ~32 bytes = ~600 KB. Comfortably small.

If rule-table size becomes an issue, fall back to a small bytecode-VM evaluator on GPU (deferred — see Out of Scope).

---

## Births, deaths, slot recycling

### Pool layout

Three persistent buffers:
- **`beads: array<Bead>`** — slot 0 to slot `pool_capacity-1`. Slots may be alive or dead.
- **`alive_count: atomic<u32>`** — high-water mark of slot indices ever used (NOT the count of alive beads).
- **`free_list: array<u32>`** — stack of dead slot indices available for reuse.
- **`free_list_head: atomic<u32>`** — top of the free-list stack.

### Birth

When a reaction returns `Birth`:
1. Atomically pop from `free_list` (decrement `free_list_head`); if free list is empty, atomically increment `alive_count` and use that index. If `alive_count` exceeds `pool_capacity`, set an `overflow` flag — CPU detects and aborts the substep, falling back to a smaller substep or a CPU run.
2. Write the newborn's `pos = midpoint(parent_a, parent_b)`, `vel = -0.5 * (vel_a + vel_b)`, `tag/payload/stack` from the rule table.
3. Set `flags.alive = 1` and `flags.born_this_substep = 1` (the latter excludes it from contact participation this substep — newborns can't react in the substep they're born; they wait one substep).
4. Queue two new bond entries `(parent_a, newborn)` and `(parent_b, newborn)` in `pending_bonds`.

### Death

When a reaction returns `LeftOnly` or `RightOnly`:
1. Mark the dying bead's `flags.alive = 0`.
2. Atomically push the slot index onto `free_list`.
3. Queue removal of all bonds involving the dead slot in `pending_dead_slots`.

### End-of-substep cleanup (CPU)

- Merge `pending_bonds` into the bond set.
- Remove bonds touching any slot in `pending_dead_slots`.
- Re-upload bond buffer if dirty.
- Clear `pending_bonds`, `pending_dead_slots`, `born_this_substep` flags.

### Determinism note

Atomic free-list pops are non-deterministic in raw order, but the contact resolution is partitioned by color. Within one color, no two reactions touch the same bead, so the *set* of births and deaths in a color is fixed by the inputs even if the *order* of atomic-claims is racy. The assignment of newborns to slots is determinized by sorting `pending_bonds` and `pending_dead_slots` by `(parent_a, parent_b)` or `(dead_slot)` before allocating slot indices. This sort is done at the end of each color's resolution, before the next color runs.

---

## CPU↔GPU data flow

Per-frame loop in `GpuColored::step()`:

```
1. Upload positions, velocities, tags, payloads, stacks of alive beads (CPU → GPU)
2. Upload bond buffer if dirty (CPU → GPU)
3. For substep in 0..n_substeps:
   a. Build grid (3 dispatches)
   b. Enumerate candidate pairs (1 dispatch)
   c. Compute TOIs (1 dispatch)
   d. Compact active contacts (1 dispatch)
   e. Color (~ chromatic_number + a few dispatches, typically 8-12)
   f. For color in colors:
      - Resolve color (1 dispatch)
   g. Advance all alive beads (1 dispatch)
   h. Enforce bonds (1 dispatch)
   i. Readback overflow + pending_bonds + pending_dead_slots (one small map)
   j. CPU merges pending_bonds, pending_dead_slots, clears born_this_substep
4. Readback positions, velocities, tags, payloads, stacks, alive flags (GPU → CPU)
5. Update Sim state
```

Per-frame dispatch count: roughly `n_substeps * (5 + chromatic_number + 3)` = 4 * 20 ≈ 80 dispatches at 60 fps. Manageable.

Per-frame readback: small (a few KB for overflow + pending lists). The end-of-frame full position readback is the largest at ~640 KB.

### Bind group reuse

A failure mode in the current GPU scheduler is rebuilding bind groups per iteration ([src/gpu/scheduler.rs](../../../src/gpu/scheduler.rs) `encode_iteration`). In the new design, bind groups are created once at scheduler construction and reused; buffer handles never change. The only thing that varies per substep is uniform values, written via `Queue::write_buffer`.

---

## Walls

Rocks (immovable beads) are represented as beads with `tag = Rock`. The resolve kernel checks: if either bead in a contact has `tag = Rock`, the reflection updates only the non-rock bead's velocity. Rocks don't participate in births/deaths (the chemistry rule table never returns LeftOnly/RightOnly/Birth for rock pairs — caught at chemistry-compile time).

A separate scenario type can mark beads as rocks. The Haskell oracle's `wallStruct` becomes a list of `tag = Rock` beads at scenario load.

---

## Testing strategy

### Correctness oracle

`CpuParallel` is the oracle. `GpuColored` must produce bit-identical output to `CpuParallel` on every golden scenario. New `tests/parallel_determinism.rs` runs each scenario through both schedulers with identical seeds and asserts byte-equal `(positions, velocities, tags, payloads, stacks, alive)` arrays after N frames.

### Self-determinism

Both `CpuParallel` and `GpuColored` must be self-deterministic: running the same scenario twice produces identical output. Tested in `tests/parallel_determinism.rs::self_determinism_cpu_parallel` and `::self_determinism_gpu_colored`.

### Invariant tests

In addition to bit-equality, a small set of physical invariants gates correctness:
- No two alive beads have `|d| < R - ε`.
- No bonded pair has `|d| > R + ε` at end-of-frame.
- For wire-class chemistries with no births/deaths: signal count is conserved across substeps.
- Total alive bead count matches expected (initial + births - deaths) per frame.

Invariants run on every test scenario for both schedulers.

### Scenario coverage

- `chains_30x30` (900 beads, existing) — must run, parallel scheduler ≥ CPU-sequential perf.
- `chains_30x300` (9000 beads) — new scenario, parallel scheduler at 60 fps.
- `wire-10x100` (1000 beads) — wire signal propagation test.
- New `sem_basic` scenario exercising Spawn / Die / Send.

### Performance gate

`bench --scheduler gpu-colored --scenario chains_30x300` must achieve ≥60 fps mean over a 30-frame probe with 60-frame warmup. If it does not, the design has failed its perf goal and we revisit.

### Test schedule

- Unit tests (per-module, ~50 new) on `CpuParallel` correctness — bond mechanics, action-stack manipulation, rule-table lookup, free-list, coloring.
- Integration tests on `CpuParallel` vs `CpuSequential` for prototype chemistries (grey, wire) — *not* bit-identical (different algorithm) but invariants must hold.
- Determinism test on `CpuParallel` vs `GpuColored` — *bit-identical* required.
- Perf bench gates the merge.

---

## File layout

```
src/
  scheduler.rs                    — Scheduler trait (unchanged)
  sequential.rs                   — CpuSequential (renamed from inline in scheduler.rs)
  parallel/
    mod.rs                        — CpuParallel scheduler
    coloring.rs                   — parallel deterministic coloring
    pool.rs                       — bead pool, free list, slot recycling
    resolve.rs                    — reaction resolution
    state.rs                      — Bead struct, action stack, opcodes
  chemistry/
    mod.rs                        — re-exports
    compiler.rs                   — TOML / Haskell-Sem → CompiledChemistry
    runtime.rs                    — runtime reaction lookup and execution (used by CpuParallel)
    opcodes.rs                    — Opcode enum and encoding
  gpu/
    mod.rs                        — GpuColored scheduler
    buffers.rs                    — pool/bond/grid buffer allocation, upload, readback
    pipelines.rs                  — compute pipelines, bind groups (created once)
    dispatch.rs                   — per-substep dispatch sequencing

shaders/
  grid_count.wgsl
  grid_scan.wgsl
  grid_fill.wgsl
  enumerate_pairs.wgsl
  toi.wgsl
  compact_contacts.wgsl
  color.wgsl
  resolve_color.wgsl
  advance.wgsl
  enforce_bonds.wgsl

tests/
  parallel_determinism.rs         — new: CPU-parallel vs GPU-colored bit-identical
  parallel_invariants.rs          — new: physical invariants across schedulers
  determinism.rs                  — existing: CpuSequential self-determinism (kept)

chemistries/
  sem_basic.toml                  — new: minimal Sem chemistry (Wire+Port+Sig+Apply+Spawn+Die)
fabs/
  chains_30x300.toml              — new: 9000-bead chain scenario
  sem_basic_demo.toml             — new: demonstrates Sem semantics
```

Existing files modified minimally: [src/sim.rs](../../../src/sim.rs) becomes a thin delegator to `self.scheduler.step(self, dt)`; [src/lib.rs](../../../src/lib.rs) re-exports new modules; [src/app.rs](../../../src/app.rs) updated scheduler selection; [src/bin/bench.rs](../../../src/bin/bench.rs) gets new `--scheduler` options.

---

## Known risks and mitigations

| Risk | Mitigation |
|---|---|
| Coloring algorithm correctness | Reference implementation in `CpuParallel` first; unit-tested against hand-built conflict graphs; GPU port matches CPU output bit-equally. |
| Rule-table size explosion for deep action stacks | Bound the "reactive prefix" of the action stack to the top opcode + at most one operand (matches Sem's actual reaction patterns). If a future chemistry needs more, fall back to bytecode VM (future work). |
| Free-list race conditions on GPU | Resolution within a color guarantees no shared beads; the *set* of births/deaths per color is determined by the color members. Slot-index assignment is determinized by sorting pending_bonds/pending_deaths within each color before allocation. |
| Pool overflow | `overflow` flag; CPU detects, retries the frame with a doubled pool, then warns and either resizes or fails. Resize happens between frames. |
| Newborn participating in same-substep contacts | `born_this_substep` flag excludes newborns from contact participation in the substep they're born. They participate the next substep. |
| Coloring round count blowing up | Empirical chromatic number ≤12 in 2D unit-disk packing; the parallel algorithm converges in ≤chromatic_number+log(P) rounds in practice. If it doesn't, falls back to greedy sequential within `CpuParallel` (no GPU correctness impact). |
| `dt_sub` chosen wrong, accuracy degrades | Default 1/240s is conservative for collision-amplified speeds. Tunable per scenario via fab metadata. Speed-adaptive dt_sub is a future option. |
| Bond changes mid-substep | Births/deaths are queued; bond buffer reuploaded at substep boundary, not mid-substep. |
| Determinism breakage from float ordering | Within a color, no two reactions touch the same bead, so float results don't depend on resolution order. Across colors, color order is fixed. Across substeps, substep order is fixed. |

---

## Out of scope (future work)

- WebGPU compute on WASM (`GpuColored` web build).
- GPU-primary data layout (eliminate per-frame upload/readback).
- Speed-adaptive substep (CFL condition based on observed max speed).
- Bytecode-VM chemistry runtime (if rule tables get too big).
- Chemistries with persistent per-bead memory beyond action stack.
- Many-bead reactions (3+ beads at one contact).
- Walls beyond rocks (e.g. lines, halfplanes — currently only rocks).
- Multi-GPU.
- Server-side compute-only path.
