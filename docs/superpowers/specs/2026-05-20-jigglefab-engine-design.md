# JiggleFab — Engine Design

**Status:** Approved 2026-05-20. Implementation plan to follow.

## Purpose

Reimplement the engine of the [original Haskell jigglefab](../../../haskell/README.md) on the GPU; scale to roughly 10k–100k beads in real time; ship to the browser; keep `haskell/` untouched as a frozen reference.

The physics and chemistry of jigglefab are not being redesigned — they exist in `haskell/` and are load-bearing creative work. **Only the collision scheduling mechanism changes** (sequential event queue → GPU-parallel time-stepped per-pair CCD).

## North star

A real-time, GPU-scale, organic-feeling, browser-runnable, exhibitable self-replicating universal constructor in a continuous 2D artificial chemistry.

## What this engine keeps, replaces, and does not add

**Ported verbatim from `haskell/`:**

- Equal-radius unit-circle beads, constant mass, constant speed
- Linear motion, no forces, perfectly elastic reflections
- Binary collision outcome: **reflect** or **pass through**
- Outcome conditional on `(stateA, stateB, inside?)` per a symmetric chemistry rule table
- Bonds = containment (two beads bonded iff `|posA − posB| < radius`)
- All existing chemistries and prototype components (Wire, Port, Striped-Wire, Encoder, Gates, Turnbuckle)

**Replaced:**

- Event-queue scheduler → GPU-parallel time-stepped loop
- Global next-collision sort → local per-pair CCD on a uniform-grid spatial hash
- Hardcoded single chemistry → chemistries as swappable data

**Not added:**

- No new dynamics (no soft potentials, no thermostat, no forces, no two-tier physics)
- No new chemistry semantics

## Glossary

- **bead** — unit particle (equal-radius unit circle, constant mass)
- **bond** — containment relationship (`|posA − posB| < radius = 1`); symmetric; emergent from positions, not stored
- **chain** — linear bonded run of beads (primary term)
- **chemistry** — state set + symmetric `(stateA, stateB, inside?) → (reflect|pass, newStateA, newStateB)` rule table
- **fab** — a complete savable design (provisional)
- **knot** — small named sub-pattern within a fab (provisional, deferred)

"Lace" was considered as an alternative to "chain" but "chain" is what the original implementation was called and what we'll keep reverting to in conversation. Use "chain."

## Architecture

- **Language:** Rust
- **GPU API:** wgpu (the de-facto WebGPU implementation in the Rust ecosystem)
- **Shading language:** WGSL (compute and rendering)
- **Targets:** native desktop binary + WASM bundle running in any WebGPU-capable browser
- **Process model:** single binary; simulation + (eventual) editor + UI in one Rust process, sharing one wgpu pipeline and one canvas

### Rationale (Rust + wgpu)

The only stack with first-class GPU compute *and* a real browser story without a two-codebase split. Alternatives considered and rejected:

- **TypeScript + WebGPU.** Weaker host-side performance at scale; no shared types with the algorithmic core; the editor's chain-graph operations want a richer type system.
- **Python (Taichi / Warp).** Fastest prototyping path, but no realistic browser deployment. A Taichi prototype would be thrown away when porting to Rust+wgpu for the exhibit; the saved iteration time doesn't justify the duplicate implementation.
- **CUDA / C++.** Maximum raw throughput, but the 5070Ti is GPU-idle most of every frame at our scale anyway; no browser path.

### Explicitly not in the stack

- **No CUDA.** Skipped for portability. Reconsider only if v4-era design search demands it; would then live as a separate offline tool sharing the file format.
- **No Python prototype detour.** The algorithmic shape (per-pair CCD on a uniform-grid spatial hash) is already understood from the original implementation.
- **No database.** Designs and chemistries are small files on disk.
- **No deployment infrastructure designed yet.** Static hosting and release packaging are P4 concerns.

## World topology

The world is a **2D torus** — positions wrap around in both axes. No walls, no special-case wall-bead collision, no escape concern, no edge artifacts. Pair distance uses the minimum-image convention (the shortest of the wrapped distances). The spatial hash wraps too; the last grid cells in each axis are neighbours of the first.

World size is hardcoded for P1. A variable / per-fab world size lands later; the fab file's `[meta]` table is the natural home for it. Listed under deferred decisions.

## Collision mechanism

The Haskell uses a global event queue: each collision computes its time, all collisions are sorted, the earliest fires, ~2n events are removed and ~2n inserted, repeat. This is inherently sequential.

The engine's loop is fully data-parallel:

1. **Advance** all beads one frame's straight-line motion (closed form).
2. **Bin** all beads into a uniform-grid spatial hash. (Equal-radius beads make a flat grid ideal; quadtree is overkill.)
3. **Candidate pairs:** for each bead, examine beads in the same and adjacent cells.
4. **Per-pair CCD:** solve the closed-form quadratic for the exact within-frame contact time for each candidate. Linear motion guarantees ≤2 boundary crossings per pair per frame, so no in/out transition is ever missed.
5. **Earliest-contact resolution:** find the global minimum contact time (deterministic tiebreak: lower bead-index pair wins). Advance all beads to that moment. Resolve the colliding pair (reflect or pass per chemistry table; latch state on contact-state transition so reactions don't re-fire while a bond is stable).
6. **Recurse** on remaining `dt` until the frame's time budget is exhausted with no new contacts.

Steps 1–4 are fully parallel. Step 5's reduction is the only inherently-serial portion and is small.

The "many beads colliding in one frame" case — which the user identified as the determinism threat — is exactly what step 5's recursion handles. Each iteration resolves the single earliest contact (or a deterministic tied set), so even dense pileups untangle without leaving any in/out transition unhandled.

## File format

**TOML** for both fab files and chemistry files. Easy to swap formats later if needed.

### Fab file

```toml
[meta]
name = "30-bead vertical chain"
chemistry = "grey-v1"
seed = 42

# Pos required. Vel optional. If vel omitted, direction is sampled
# deterministically from `seed` + bead index; magnitude = 1.
[[bead]]
state = "grey"
pos = [0.0, 0.0]

[[bead]]
state = "grey"
pos = [0.0, 0.667]   # bonded to bead 0 (|Δ| = 0.667 < radius = 1.0)

# ...30 beads total
```

**Bonds are not stored.** They are a function of position; storing them would create a redundant source of truth that can drift from positions. Two beads are bonded iff their centers are within `radius`. This holds at every t, including t=0.

**Linter** (P2+): flag any pair with center-distance in `[radius − ε, radius + ε]` (ε ≈ 0.05) and refuse to load (or warn loudly). A fab where a small perturbation flips a bond is malformed — this enforces the "low-entropy design" principle at the format level.

A future topology-first authoring format (positions derived from a containment graph + layout seed) can compile down to this runtime format. The runtime format stays minimal.

### Chemistry file

```toml
states = ["grey"]

[[rule]]
states = ["grey", "grey"]
inside = false
action = "reflect"

[[rule]]
states = ["grey", "grey"]
inside = true
action = "reflect"
```

On GPU, the chemistry compiles to a dense lookup buffer indexed by `(stateA, stateB, inside?)`. Swapping chemistries = swapping the buffer; no engine recompile.

## Determinism

Target: bit-reproducible reruns from the same seed.

**Achievable** for the per-pair CCD + earliest-contact loop given a fixed tiebreak.

**Threat:** GPU atomic-reduction ordering is nondeterministic by default. Mitigate with deterministic-reduction patterns (e.g., fixed-structure parallel scan rather than atomic-add). Small constant-factor cost.

**Acceptable fallback:** deterministic-on-same-machine if a particular reduction proves bit-exact-too-expensive. Document where.

**RNG:** counter-based PRNG keyed on `(bead_id, tick)` rather than a stream-position RNG. Parallel-safe, reorderable, reproducible.

## Robustness

The Haskell "ball escapes its chain at speed" demon was a *scheduling* bug, not a chemistry bug — events fired out of order in clumps. Per-pair CCD kills it at the source. Layered defenses against the residual one-in-a-billion glitches that accumulate at millions of collisions per minute:

- **CCD-faithful in/out transitions** as primary correctness mechanism.
- **Hard speed clamp** as backstop: residual glitches become wobbles, never missiles.
- **Watched invariants** that trip on the violation frame: total energy, containment integrity (no bond appears/disappears without passing through its CCD-resolved transition), bead count conservation when chemistry doesn't create/destroy. (No "no-escape" invariant: the world is a torus, see above.)
- **Anomaly menagerie:** any tripped invariant or visually-flagged weirdness saves `(seed, initial_state, frame)` to a regression set. Triage: fix, or accept-and-document.

## Rendering

**Hard constraint:** symmetric — no pair of overlapping beads can have biased pixel ownership.

- **P1:** filled circles. Simplest; the chain is visibly jiggling.
- **Post-P1 candidates** (both GPU-cheap, possibly both supported):
  - **Voronoi via jump-flooding** — matches `haskell/`'s original aesthetic
  - **SDF / metaball union** — organic blobs, smoother for ≥3 overlapping beads

## Editor (deferred, principles only)

When the editor lands, it lives **in the same Rust binary** as the simulation. The requirement "delete a bead and see the consequence in the running sim" demands a shared process and a shared canvas. Off-the-shelf tools (tldraw, Konva, React Flow) don't fit — bead/bond/chain authoring is too custom (chain-stamping a stroke into a chain in a chosen style, sub-structure rotate/duplicate, bond-graph-aware selection).

UI framework (egui / iced / custom-on-wgpu) is deferred until editor implementation begins.

Web sharing happens via the same Rust binary's WASM build, possibly with reduced editing affordances initially.

## Testing strategy

`haskell/` is **not** a trajectory oracle — floats + chaos make trajectories diverge between any two implementations. Use it as:

- **Behavioral oracle:** known seeded constructor configurations should still complete/replicate in the engine.
- **Unit oracle:** hand-built 2–3 bead deterministic scenarios should match exactly.
- **Statistical oracle:** distributions (bond lifetimes, cluster sizes, reaction rates) match within tolerance.
- **Invariant oracle:** conserved quantities hold every frame.

A differential harness runs both engines on the same seed + initial state across several chemistries. *Agreement* is a robustness signal, not a contract — disagreement in cluttered cases is expected (the haskell event queue's global order is itself arbitrary in such cases) and informative.

Invariant assertions and anomaly menagerie land in P2, not P1.

## Phased plan

- **P1 — Hello jiggling chain.** Minimum runnable engine. Detailed below.
- **P2 — Chemistry engine.** Load chemistries from disk; port the haskell chemistry as instance #1; verify a second chemistry runs with no engine changes. Stand up invariants and the anomaly menagerie.
- **P3 — Constructor.** Bring up a known constructor design from `haskell/`; behavioral validation; first performance pass.
- **P4 — Exhibit.** Editor; UI; WASM build; visual polish (Voronoi/SDF); deployment.

The implementation plan that follows this design targets **P1 only**. Subsequent phases get their own plans.

## P1 detailed scope

**Done means:**

- Rust + wgpu native binary builds and runs, opens a window, renders to it.
- Loads `fabs/grey-30.toml`: 30 grey beads at `pos = (0, n × 0.667)` for n in 0..30. No explicit per-bead velocities.
- Velocities derived deterministically from `[meta].seed`: unit magnitude, direction uniform in `[0, 2π)`, one direction per bead.
- Loads `chemistries/grey.toml`: 1 state (`grey`), 2 rules (reflect on inside, reflect on outside).
- Runs the time-stepped per-pair CCD loop on a uniform-grid spatial hash.
- Renders all beads as filled circles, camera framed around the chain.
- Camera hardcoded; no zoom/pan/UI.
- Reruns with the same seed produce bit-identical pixel output frame-for-frame on the same machine.
- World is a hardcoded-size torus; positions wrap. No walls. Size chosen so the chain has room to wander without immediately self-overlapping across the wrap.

**Not in P1:**

- Multiple chemistries beyond grey
- Multiple fabs
- Editor of any kind
- Voronoi/SDF rendering
- Invariant assertions
- Anomaly menagerie
- Web/WASM build
- Format linting
- Performance work beyond "≥30 fps for 30 beads on the dev machine"

Hardcoding the chain instead of loading from TOML is **acceptable as an intermediate step**, but the P1 done-state includes TOML loading so the data path exists before P2 starts demanding it.

## Deferred decisions

Intentionally unpinned, with reasons:

- **Multi-contact solver** (Jacobi vs colored Gauss-Seidel) — defer until dense pileups demand it; may interact with chemistry semantics.
- **Editor UI framework** — defer until editor implementation starts.
- **Voronoi vs SDF metaball rendering** — defer until visual polish phase (post-P1).
- **Deployment / hosting specifics** — defer until P4.
- **Sub-structure naming** (knot, lace, fab) — provisional; will settle through use.
- **Overlapping-but-not-bonded beads** — leave bonds emergent for now; revisit only if a design legitimately requires the edge case (e.g., 4 beads stacked at the same point with varying bondedness for a logic-gate knot). May add an optional bond-override addendum to the format then.
- **Variable / per-fab world size** — hardcoded torus size for P1; move to the fab file's `[meta]` table when multiple fabs land.

## Guardrails

- `haskell/` stays frozen. Never edit. Reference and oracle only.
- The engine is chemistry-agnostic; chemistries are data, not code.
- Robustness comes from invariants, not exactness.
- Reproducible from seed always.
- Anomalies are cherished as reproducible regression tests.
- The constructor exists in `haskell/`. Porting is the tractable part; do not redesign the science.
- Earn each layer. The simplest thing that jiggles and renders comes first.
