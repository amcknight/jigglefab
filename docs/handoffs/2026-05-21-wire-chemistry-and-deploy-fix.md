# Session handoff — Wire chemistry, web deploy fix, scale ceiling found

**Date:** 2026-05-21
**Session scope:** debug "Loading…" on amcknight.ca/jigglefab, add Wire chemistry, ramp up bead count, fix what broke at scale.

## State of the repo

Branches:
- `main` (local) and `web` (local + remote) are at the same commit. Push to `web` is what triggers GH Actions deploy. Push to `main` is still blocked by the harness.
- 33 tests passing (27 unit + 5 chain integrity + 1 determinism).
- Live URL: <https://amcknight.ca/jigglefab/> — 10 parallel 30-bead wire chains, 10× substeps, runs smoothly on desktop Chrome and on Pixel 9.

## What works

- Wire chemistry: bonded pair → reflect + state swap (signal walks along the chain); free pair → reflect only (chains have shape, no signal transfer between or within chains at non-adjacent beads). One amber "on" bead per chain at t=0; it walks via state swaps.
- Per-fab world size (`Fab.meta.world_size: Option<f32>`, default 30).
- Per-chemistry color palette (`Chemistry.colors`, default if omitted).
- Native and web build paths both load `fabs/wire-10x30.toml`.
- Playwright probe at `scripts/verify-web.py` for verifying deploys without a real browser tab. `python scripts/verify-web.py [URL] [--headed]` returns timeline snapshot hashes — if they're all identical, the sim froze; distinct hashes = live.

## What was tried and rejected

1. **`maxInterStageShaderComponents`** — wgpu 22.1.0 serialized this deprecated WebGPU limit in the `requestDevice` call; modern Chrome refuses devices that ask for unknown-named limits. Fixed by bumping wgpu 22 → 23. The earlier "Loading… that never resolves" hypothesis (canvas-size race) was wrong; `requestDevice()` panicked before any of that mattered. The `RendererReady` force-resize is still in place as defence in depth.
2. **`outside = pass` on wire** — chains collapsed into tight balls within ~2 s because there was no force keeping non-adjacent beads apart. The screenshot at `scripts/verify-out/` from that attempt is the smoking gun. Reverted to `outside = reflect` (no swap), which is what the user wanted ("only swap signal between bonded circles") *and* preserves chain shape.
3. **10 × 100 at 10×** — sequential CCD scheduler can't keep up. As chains tangle (even at this fab size), the intra-chain non-adjacent contact rate climbs faster than one CPU thread can drain. User reported "freezes after 1-2 s" which was actually "crawls at <1 fps," confirmed by playwright snapshots advancing slowly. Cap is roughly 300–500 beads at 10× substeps on this scheduler; bigger needs P2 GPU CCD.

## Sim correctness fix worth keeping

`src/sim.rs:218–225` — the post-contact snap target is now derived from `(action, exiting)` instead of `bonded`. Reflect bounces back to the side it came from; Pass continues to the opposite side. The old `if bonded { R-ε } else { R+ε }` rule was correct *only* as long as bonded ↔ inside, which broke the moment any chemistry put Pass on a free pair (free pair was snapped to R+ε while still moving inward → pinned at the boundary, every CCD iteration). The fix is general; tests pass with no other change, and `wire_100_chain_keeps_all_initial_bonds_for_30s` runs ~60× faster than before because pass-through contacts now terminate cleanly.

## What to pick up next

In rough priority order:

1. **P2: GPU-parallel CCD on the uniform grid.** Named in the design doc as the planned successor to the sequential scheduler. Lifts the ~300-bead ceiling. Approach is sketched in `docs/superpowers/specs/2026-05-20-jigglefab-engine-design.md`.
2. **Optional cleanup**: there's a tracked `dist/index.html` from the original P1 deploy attempt that's now stale and ignored. Could `git rm` it; not load-bearing.
3. **Chain stiffness** if you want straighter chains. Currently they bend freely under elastic collisions and curl over time. Adding an angular term (preferred angle between consecutive bond directions) is the natural mechanism. Chemistry-shaped, but the chemistry type system currently doesn't model triples — it's pair-based. New surface area.
4. **Push `main` to origin.** Still blocked locally; `git push origin main` from your machine when convenient. Both branches are at the same commit so it's safe.

## Critique

What went well:
- The playwright probe paid for itself instantly — without it I'd have kept guessing at the "freezes after 2 s" report instead of confirming the canvas was actually still updating (just slowly), and I never would have caught the deprecated-limit issue.
- User feedback "the freeze is right at the first cross-chain contact" was the key clue that turned this from a perf-tuning loop into a chemistry-design choice (only-bonded-swap).
- The post-action snap target fix is small, general, and replaces an ad-hoc rule with a principled one. Caught only because outside=pass exposed the latent bug — good "stress test the assumption" outcome.

What went poorly:
- Two wrong root-cause hypotheses before the playwright probe (canvas-size race, then bond-drift cascade). Both plausible and both wrong. Should have reached for browser-driving the deploy earlier — the cost of guessing in a feedback loop with no instrumentation was higher than the cost of writing the probe.
- The outside=pass attempt shipped before testing for the ball-collapse failure mode. A quick local trunk-serve + playwright would have caught it in two minutes; I went straight to deploy because the chain_integrity tests passed.
- Brief detour into multi-pass `enforce_bonds` that took 647 s wall on a 5 s sim test. Reverted before push, but I should have measured first. Would have shown the cost immediately.

## Files of note

- `chemistries/wire.toml` — full spec, both inside and outside rules listed explicitly for self-documentation.
- `chemistries/grey.toml` — unchanged, still the default for `grey-30.toml`.
- `fabs/wire-10x30.toml` — the deployed demo, 10 chains × 30 beads in 50×50.
- `fabs/wire-30.toml`, `fabs/wire-100.toml` — kept as test fixtures.
- `fabs/grey-30.toml` — kept; still backs the chain-integrity grey tests.
- `scripts/verify-web.py` — playwright timeline probe.
- `src/sim.rs:218–225` — the post-action snap fix.
- `src/app.rs:24` — `SUBSTEPS = 10`, with a one-liner explaining the ceiling.
