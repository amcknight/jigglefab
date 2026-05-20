# jigglefab v2 — Metaplan

## How to use this document
You are a Claude Code session (or future-me) working **inside the existing jigglefab repo**, now laid out as:

```
jigglefab/
  haskell/        # the original — frozen, untouched; reference + oracle
  engine/         # the new GPU implementation; this just IS jigglefab going forward
  chemistries/    # shared, language-neutral chemistry definitions (data)
  METAPLAN.md     # this file
  PLAN.md DECISIONS.md CHEMISTRY_SPEC.md   # produced by running this
```

Nothing is called "v2." The split is by *what each thing is* (a Haskell reference vs a GPU engine), not by version number. Your job is **not** to start coding. It is to investigate `haskell/`, brainstorm with the human, resolve the open decisions, and produce a real **phased plan**.

Deliverables of running this metaplan:
- `PLAN.md` — the phased build plan
- `DECISIONS.md` — short architecture decision records (one per resolved fork in §7)
- `CHEMISTRY_SPEC.md` — the **format** for defining a chemistry; instances live in `chemistries/`, starting with v1's chemistry
- `haskell/` — left frozen and untouched

Work *with the human*. The forks in §7 are genuine negotiations. Where this doc states a conclusion you may verify, but don't relitigate without reason.

## Scope in one line
Keep the Haskell jigglefab's physics and chemistry **exactly**; replace only the **collision mechanism** (event queue → GPU-parallel time-stepping); and make the engine run **many chemistries** as swappable data.

---

## 1. North star
A real-time, GPU-scale, organic-feeling, **exhibitable** self-replicating *universal constructor* in a continuous 2D artificial chemistry — running on an engine that hosts **many chemistries**, not one hard-coded one.

To our knowledge the intersection — organic + continuous + real-time + GPU-scale + a *genuine* universal constructor — is unoccupied. Prior art to study:
- **JohnnyVon** (~2003): self-replicating machines drifting in a viscous 2D liquid; template-based; small, pre-GPU; a self-replicator more than a programmable universal constructor. **The closest ancestor — read it first.**
- Universal constructors otherwise live in **discrete/mechanical** substrates (von Neumann cellular automata, stringmol, DigiHive, Lano's self-replicating Turing machines). That is the crowded corner. **Do not drift into it.** The organic, continuous look is the entire differentiator.

## 2. What the Haskell original is (VERIFY against `haskell/` — memory is uncertain)
Believed to be: Haskell; 2D; equal-radius disks; "perfect billiards", no forces except reflection; **conditional pass-through** governed by a **symmetric** rule over `(state of A, state of B, inside/outside)`. Bonds = **containment** (a ball inside another) — logical and velocity-proof. Rendered with **Voronoi** tiling where balls overlap. Scheduling: **event-driven** — a global sorted queue of next-collision times; each collision removes ~2n and inserts ~2n events; possibly a quadtree (unsure).

The "chemistry" is exactly: the **state set** plus the **symmetric `(stateA, stateB, inside?)` rule table** (mechanical pass/reflect, plus any state change). This becomes swappable data (§6). Confirm everything by reading the code before planning.

## 3. What is ported verbatim vs what changes (REACHED — verify, don't relitigate)
**Ported verbatim:** the physics (equal-radius disks, linear motion, circular reflection, conditional in/out pass-through) and the chemistry (states + symmetric rule). No new dynamics, no soft potentials, no thermostat.

**Replaced:** only the collision *mechanism*.
- The GPU obstacle was never the physics — linear motion + circular reflections is GPU-ideal (straight-line advance is trivial; contact time is a closed-form quadratic). It was the **scheduling**: a global event queue is inherently sequential. **Fix the scheduler, not the physics.**
- Global *ordering* of spatially-separated events is physically meaningless (they commute). Only *local* order (events on the same ball) matters → a tiny deterministic tiebreak, never a global sort.
- The new loop is **time-stepped**: advance everyone → bin into a **uniform grid / spatial hash** (NOT a quadtree; equal radii make a flat grid ideal) → check candidate pairs locally → resolve → repeat. All data-parallel.
- **Per-pair CCD** recovers exactness: the closed-form quadratic gives exact within-frame contact times, and because motion is linear there are **≤2 boundary crossings per pair per frame**, so no entry/exit (hence no in/out transition) is ever missed.
- Reactions/state-changes fire on **contact-state transitions** (edge-trigger / latch), since a contained pair stays overlapping for many frames and must not re-fire every frame.

## 4. Robustness philosophy (THE HEART)
At millions of collisions per minute a one-in-a-billion glitch is constant. **Guarantee properties, not exactness.**
- **CCD-faithful boundary handling is the primary correctness mechanism.** Catching every boundary crossing exactly is what makes in/out containment hold — i.e. what kills the old "ball escapes its chain at speed" demon. That escape was a *mechanism* bug, not a chemistry property.
- **Hard speed clamp** as backstop → any residual glitch is a wobble, never a missile.
- **Energy / no-escape as watched invariants** → no silent drift, and a violation trips on the exact frame.
- **Cherish weird behavior:** every anomaly is saved as a reproducible `seed/initial-state + frame` in an *anomaly menagerie*, then triaged (fix, or accept-and-document).
- **Bit-reproducible:** deterministic engine + fixed tiebreak + deterministic reductions → identical reruns. (If the original uses randomness anywhere, reproduce it with a counter-based seeded PRNG keyed on `(particle, tick)`.) Every glitch becomes a replayable frame, not a once-a-minute ghost.

## 5. Bonds & chemistry: DECISION CLOSED
No bond-model fork. Bonds are the original's in/out **containment** — logical, velocity-proof by design. No sticks, no wells, no two-tier, no thermostat. Everything from earlier brainstorming about soft potentials / Verlet / stiffness / shoot-out is **out of scope** — it only applied to a soft-well chemistry we are not building.

## 6. Chemistry as swappable data (the multi-chemistry requirement)
Separate the **engine** (chemistry-agnostic: motion, collision detection, rendering, invariants) from the **chemistry** (pure data).

- A chemistry = a **state set** + a **symmetric lookup table**: `(stateA, stateB, inside?) → (reflect-or-pass, newStateA, newStateB)`, symmetric under A↔B swap.
- On GPU the table is a small buffer/texture the kernel indexes per pair — a lookup, negligible cost. **Swapping chemistries = swapping the buffer; no engine recompile.**
- The engine must respect what every such chemistry relies on: **isotropy** (radial), **off-lattice** continuous space (a grid may exist only as an *invisible* neighbour accelerator), and a **single primitive** (no separate bond objects — bonds are containment relationships).
- Decisions this raises (also in §7): a fixed **max state count** so the table is dense/fixed-size (e.g. N×N×2) vs dynamic; the **authoring format**; whether mechanical pass/reflect and the chemical state-change share one table or two.
- **Shared between both engines:** chemistries live in `chemistries/` as language-neutral data both `haskell/` and `engine/` can read. The realistic win is sharing the **format**, not guaranteed identical **behavior** — see §8. Sharing the format costs nothing and imposes no maintenance burden (it's a data format, not two implementations to sync).

## 7. Open decisions to negotiate (do NOT pre-decide — surface tradeoffs, write an ADR each)
- **Language / runtime.** Rust + wgpu (native *and* browser/WASM, one API for compute + render — ideal for an exhibit) vs NVIDIA Warp / Taichi (Python; fastest prototype-to-GPU; built-in spatial hashing) vs CUDA (max throughput; NVIDIA-only; no web). *Suggested:* prototype in Taichi/Warp, build the exhibit in Rust + wgpu.
- **Chemistry table.** Fixed max state count vs dynamic; the authoring format; one table or two (mechanical vs chemical).
- **Exactness dial.** Resolve-at-frame-boundary (simplest) vs per-pair CCD + iterative contact solve (exact; some warp divergence in dense clumps). Given the escape pain and the need for faithful containment, lean toward **CCD for boundary crossings**; add iteration only where invariants demand.
- **Rendering.** SDF / metaball union (organic blobs) vs screen-space Voronoi via Jump Flooding (matches the original's look). Both GPU-cheap; could support both.
- **Determinism level.** Bit-exact (fixed-point or carefully ordered float reductions) vs reproducible-enough. Human wants exact reruns → lean bit-exact.
- **Multi-contact solver.** Jacobi (fully parallel, more iterations) vs graph-colored Gauss–Seidel (faster convergence, needs a coloring pass).
- **Oracle scope** — see §8.

*(Removed as resolved by §5: bond model, pass-through. Both fixed by the original chemistry.)*

## 8. Oracle & testing strategy
`haskell/` is **not** a trajectory oracle: floats + chaotic divergence mean trajectories won't and needn't match. Use it as:
- **Behavioral** — does a known seeded constructor configuration still complete / replicate in the engine?
- **Deterministic unit scenarios** — tiny hand-built 2–3 ball setups with unambiguous outcomes; these *should* match exactly.
- **Statistical** — distributions (bond lifetimes, cluster sizes, reaction rates) match within tolerance.
- **Invariant** — conserved quantities, no-escape, bounded energy hold every frame.

**On shared chemistries and drift:** both engines read the same chemistry file, but expect *behavioral* divergence in cluttered/simultaneous clumps, because the engine resolves them in a different (local, per-frame) order than the event queue's global order. That is the one place the original's ordering was itself arbitrary and physically meaningless, so neither engine is "right." Treat behavioral *agreement* as a robustness signal and a free oracle check, not a contract: agreement ⇒ the chemistry is clutter-robust; divergence ⇒ the chemistry leans on clutter-order (useful to know — retune for robustness or accept it as engine-specific). `haskell/` stays frozen; running new chemistries through it is optional cross-validation, never an obligation.

Stand up the **anomaly menagerie** + invariant assertions from the first runnable build; build a differential harness on shared seeds, across **several chemistries** (not just the original's) to prove the engine is general.

## 9. Investigation tasks for this session (with `haskell/` access)
1. **Map the original.** Read `haskell/`; confirm/correct §2 and §3; short architecture summary.
2. **Design the chemistry format**, and express the original's chemistry in it → `CHEMISTRY_SPEC.md` + first file in `chemistries/`. Extract the exact state set + symmetric `(stateA, stateB, inside?)` table. *Crown jewels — must port faithfully.*
3. **Gather a second (and third) chemistry** to validate the engine is general — ask the human which other chemistries they have or want.
4. **Find existing constructs** in `haskell/` → behavioral oracle cases.
5. **Catalog the original's pain** (grep TODOs/hacks/comments; ask the human) → seed the anomaly menagerie.
6. **Resolve §7** with the human; one ADR each into `DECISIONS.md`.
7. **Write `PLAN.md`** (shape in §10).

## 10. Shape of the output `PLAN.md`
- **P0 — Foundations.** Chemistry format + the original's chemistry expressed in it; §7 decisions + ADRs; `engine/` scaffolding; language/runtime + rendering chosen.
- **P1 — Minimal substrate.** Particles + spatial hash + time-step + reflect-only interaction, on GPU, rendering *something*. Seed/determinism harness + first invariants. ("Hello, jiggling dots.")
- **P2 — Chemistry engine.** Load chemistry-as-data from `chemistries/`; per-pair CCD + in/out transition handling + state-changes via the table; **port the original's chemistry as instance #1** and **verify a second chemistry runs unchanged-engine**. Anomaly menagerie + oracle online.
- **P3 — Constructor.** Bring up a known constructor from `haskell/` in the engine; validate replication behaviorally; performance pass to target N; tune the exactness dial.
- **P4 — Exhibit.** Real-time interactivity + parameter-tuning UI; web/WASM deployment; visual polish (organic SDF / Voronoi).

## 11. Guardrails to carry through (so this doesn't drift)
- Port the original's physics **and** chemistry verbatim; only the collision mechanism changes.
- The **engine is chemistry-agnostic**; a chemistry is **data** in `chemistries/`, shared by format with `haskell/`.
- `haskell/` is **frozen**. Never maintain it. Behavioral agreement with it is a signal, not a contract; drift in cluttered cases is expected and informative.
- Robustness by **invariant**, not exactness.
- **Reproducible** from seed / initial state, always.
- **Cherish anomalies** as regression tests.
- The hard creative work — the constructor itself — already exists in `haskell/` and must carry over faithfully. The port is the *tractable* part; don't treat it as a rewrite of the science.
- Don't over-engineer: earn each layer. The simplest thing that jiggles and renders comes first.
