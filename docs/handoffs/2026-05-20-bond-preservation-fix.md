# Session handoff — bond preservation fix + torus wrap rendering

**Date:** 2026-05-20 (continuation)
**Session scope:** Fix the "chain disintegrates" bug from the prior session's handoff. Add torus-wrap ghost rendering so chains crossing the seam stop looking broken.

## State of the repo

Branch: `main` (still 23+ commits ahead of `origin/main`, not pushed).

All 26 tests pass in both debug and release:
- 22 unit tests (including new `two_bonded_beads_stay_bonded_over_time` in `src/sim.rs`)
- `tests/chain_integrity.rs`: 3 tests covering 1x and 3x speed over 30s and 100s sim time
- `tests/determinism.rs`: still passes — the new code paths are deterministic

## What was broken

Per [the prior handoff](2026-05-20-p1-and-web-deploy.md): "the chain disintegrates over ~5–10 seconds of simulation." Confirmed via visual capture and a 2-bead reproduction test.

## Root cause

The CCD scheduler resolved boundary crossings (`|d| = R`) accurately in time, but `t_root` is only ULP-accurate. After a reflect+advance, `|d|` lands a few ULPs to either side of R. The old code's `currently_inside = c < 0` then bucketed the pair on the float-noisy side. If a *bonded* pair landed at `|d| = R + δ`, the next iteration classified it as "free", found a spurious entry contact at `t ≈ δ/|v_n|`, reflected again — *exactly undoing* the first reflect. The pair sailed straight through the bond as if it weren't there.

This is the canonical "particles tunnel through their constraint via float-precision oscillation" hazard for discrete-event hard-sphere bonds.

## Fixes (three layers, each load-bearing)

### 1. Topology-tracked bonds (`src/sim.rs`)

`Sim` now owns `bonds: HashSet<(u32, u32)>`, populated from initial geometry in `from_fab` and queried via `is_bonded(a, b)`. For grey chemistry the set is invariant; future chemistries that form/break bonds will mutate it explicitly. **Topology is authoritative; `|d| vs R` is observation, not classification.**

### 2. Direction-aware action selection (`src/ccd.rs` + `src/sim.rs`)

`Contact` now reports `exiting: bool` (sign of `d|d|²/dt` at the crossing) instead of the side-the-pair-was-on. `next_contact` returns the *smallest non-negative root* in `(0, dt]`, regardless of side.

The sim picks the action from `(bonded, exiting)`:

| bonded | exiting | action                                            |
|--------|---------|---------------------------------------------------|
| true   | true    | `chemistry.lookup(inside=true)` (reflect for grey) |
| false  | false   | `chemistry.lookup(inside=false)` (reflect for grey) |
| true   | false   | `Pass` — drift correction (re-entering bond)       |
| false  | true    | `Pass` — drift correction (leaving free region)    |

After every resolution, snap the pair to `R ± BOUNDARY_EPS` (`1e-5`) on the topology-correct side. The snap target now depends on **topology**, not geometric `c`, so a drifted bonded pair gets pulled back inside instead of being snapped further outside.

### 3. End-of-step bond enforcement (`enforce_bonds`)

Topology + direction-aware action handles drift in 95% of cases, but the CCD scheduler is not exhaustive: when many pairs have contacts in the same frame, a pair can be consistently outpriced by others and never reached before `dt_remaining → 0`. Such a pair ends the frame at `|d| > R`.

`enforce_bonds` walks the bond set after the CCD loop. Any pair at `|d| ≥ R` is snapped back to `R - BOUNDARY_EPS`; if their relative velocity is still outward we also apply the missed reflect (swap normal v). This bounds total drift to one frame's worth — small enough to be invisible.

Calling at end-of-step (vs start) means **observers reading positions after `step` always see bonds within R**, which is what the test asserts.

## Torus-wrap ghost rendering (`shaders/beads.wgsl` + `src/render.rs`)

The first 3x video looked broken even though all bonds were intact: the chain's CoM had drifted across the torus seam, so half the chain rendered near `x=0` and half near `x=W`. They were one connected chain via wrap, but the renderer drew each bead exactly once.

Now each bead is rendered 9 times (instance index = `bead × 9 + ghost`), at offsets of `(i*W, j*W)` for `i, j ∈ {-1, 0, 1}`. Off-screen ghosts are clipped by the rasterizer for free. `CameraUbo` gained a `world_size` field.

## Lessons

- **The visual broke before the test did, and the test broke before I understood the bug.** The prior session shipped a demo that visibly disintegrated because there was no "does the chain still look like a chain?" check. Adding a two-bead integration test (the user's idea — "two beads stuck together bumping") repro'd in seconds and pointed straight at the root cause. The systematic-debugging skill is right: minimal repro first.

- **Snap-based precision fixes are a half-measure when geometry drives classification.** Three iterations on `BOUNDARY_EPS` couldn't fix the underlying issue: sibling-pair snaps perturbed third-party pairs across R, and once a bonded pair was on the wrong side, the code lost it forever. The architectural fix — topology as the source of truth, geometry as observation — solved it in a way no `ε` value could.

- **Bond enforcement at end-of-step, not start-of-step.** The test reads after `step()` returns. If enforcement runs at the *start* of the next step, the assertion still fails. Always close invariants on the boundary the caller observes.

- **A "broken" demo may be a render bug, not a physics bug.** When the user reported "it breaks in the last second" at 3x speed, the chain was physically fine; the chain had wrapped across the torus seam and the renderer only drew the central tile. Always check the rendering pipeline before assuming the sim is broken — the chain_integrity test would have ruled out the physics in one cargo invocation.

- **The user's "I wonder if we could increase velocity 3x" pushed the bug back out of hiding.** At 1x speed and 30s the snap-based fix passed. At 3x and 100s, a second-order failure mode (mid-frame drift past R that enforcement-at-start missed) appeared. Stress-test variants matter; the prior session's done-criteria didn't include any.

- **`SPEED` reverted to 1.0.** The 3x bump was experimental for the capture. Bump it again if you want the demo to feel snappier — the tests cover both 1x and 3x explicitly, so no surprises waiting.

## What to pick up next

- **Push main.** Still blocked locally (harness denies push-to-main); run `git push origin main` from your machine.
- **Capture a fresh 100-sec demo** at 3x with the wrap renderer — confirm visually that the chain holds for the full duration, then offer it on the web deploy.
- **P2 framing.** With bonds explicit, chemistry rules that *form* and *break* bonds (the actual point of jigglefab) are now well-typed: a chemistry reaction mutates `self.bonds` on contact. The 4-case decision table in `sim.rs:163-167` extends naturally.
- **Consider replacing `HashSet<(u32, u32)>` with a bitmatrix** for fast iteration in P2 when chemistries scan all bonded pairs each frame. Trivial at 30 beads; matters at 30,000.
