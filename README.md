# jigglefab

An attempt at the first organic-feeling visualizable Universal Constructor — a self-replicating machine in a simple, continuous 2D artificial chemistry.

The original four-month, hand-written Haskell implementation lives in [`haskell/`](haskell/) and is still runnable (`cd haskell && stack run`). Read [`haskell/README.md`](haskell/README.md) for the substance — the physics, the chemical laws, and the wire/port/biochemistry prototypes built so far.

A new GPU-parallel implementation in Rust + wgpu is being built at the repo root. Engine design and plans live in [`docs/`](docs/).

## Running P1

Requires Rust stable (rustup.rs) and a WebGPU-capable GPU.

```bash
cargo run --release
```

Loads `fabs/grey-30.toml` against `chemistries/grey.toml` and renders 30 grey beads jiggling in a vertical chain on a torus. Same seed produces a bit-identical run on the same machine (verify with `cargo test --test determinism`).

This is **Phase 1** ("hello jiggling chain") of the [engine design](docs/superpowers/specs/2026-05-20-jigglefab-engine-design.md). Subsequent phases (P2 chemistry engine, P3 constructor, P4 exhibit) get their own plans.
