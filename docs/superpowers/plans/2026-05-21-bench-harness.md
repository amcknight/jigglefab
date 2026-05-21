# Bench Harness Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build `cargo run --release --bin bench` — a standalone harness that sweeps wire-chain scenarios and reports per-substep / per-frame timings + bond-conservation invariants, so we have a baseline to measure Phase 2's GPU-CCD work against.

**Architecture:** A `Scenario` trait builds `(Sim, Invariants)`; a `runner` loops frames+substeps, samples timings via `web_time::Instant`, polls per-substep counters from a new `Sim::last_step_metrics()`; an `output` module emits a markdown table to stdout and optional CSV. CLI parsing is hand-rolled (no `clap` dep) — six flags.

**Tech Stack:** Rust 2021, existing project deps only (`glam`, `web-time`). No new crates.

**Spec:** `docs/superpowers/specs/2026-05-21-bench-harness-design.md`.

---

## File Structure

**New files:**
- `src/bench/mod.rs` — declares submodules, re-exports public types
- `src/bench/scenario.rs` — `Scenario` trait, `Invariants` struct, `geometric_bonds()` helper
- `src/bench/chains.rs` — `DisconnectedChains` scenario (single-row, multi-row, serpentine layouts)
- `src/bench/runner.rs` — `run_scenario`, `ScenarioResult`, percentile computation, truncation
- `src/bench/output.rs` — markdown + CSV formatters
- `src/bin/bench.rs` — CLI parsing + `main` entry
- `docs/bench-results/2026-05-21-baseline.md` — captured baseline run (Task 12)
- `fabs/wire-50x30.toml` — bumped deploy fab (Task 13)

**Modified files:**
- `src/lib.rs:1-9` — add `pub mod bench;`
- `src/sim.rs` — add `StepMetrics` struct + instrumented counters + `last_step_metrics()` accessor
- `Cargo.toml` — add explicit `[[bin]]` entry for the bench binary
- `src/app.rs:36, 98` — point to `wire-50x30.toml` (Task 13)

**Deleted:**
- `fabs/wire-10x30.toml` (Task 13; replaced by `wire-50x30.toml`)

---

## Task 1: Add `StepMetrics` to `Sim`

**Files:**
- Modify: `src/sim.rs`

- [ ] **Step 1.1: Write the failing test**

Add to the `mod tests` block at the bottom of `src/sim.rs`, just before the closing `}`:

```rust
    #[test]
    fn step_metrics_reports_contacts_and_pairs() {
        // Two beads on collision course at speed 1; one head-on contact
        // resolved in this step.
        let chem = load_chemistry("chemistries/grey.toml").unwrap();
        let g = chem.state_index("grey").unwrap() as u32;
        let mut sim = Sim {
            positions: vec![Vec2::new(5.0, 5.0), Vec2::new(7.0, 5.0)],
            velocities: vec![Vec2::new(1.0, 0.0), Vec2::new(-1.0, 0.0)],
            states: vec![g, g],
            chemistry: chem,
            grid: Grid::new(WORLD_SIZE),
            bonds: HashSet::new(),
            last_metrics: StepMetrics::default(),
            tick: 0,
        };
        sim.step(1.0);
        let m = sim.last_step_metrics();
        assert_eq!(m.contacts_resolved, 1, "exactly one head-on contact in this step");
        assert!(m.candidate_pairs >= 1, "at least one candidate pair scanned");
        assert!(!m.iter_cap_hit, "iter cap should not be near for two beads");
    }
```

- [ ] **Step 1.2: Run the test to verify it fails**

Run:
```powershell
cargo test --lib step_metrics_reports
```
Expected: FAIL — `StepMetrics` undefined, no `last_metrics` field, no `last_step_metrics()` method.

- [ ] **Step 1.3: Add the `StepMetrics` struct**

In `src/sim.rs`, add immediately after the `BOUNDARY_EPS` constant (line 22):

```rust
/// Per-step counters populated inside `Sim::step`. Returned by value via
/// `last_step_metrics()`. The cost of populating them is a few `+=`s per
/// CCD iteration — negligible vs the work being measured, so no cfg gating.
#[derive(Clone, Copy, Debug, Default)]
pub struct StepMetrics {
    /// Number of contacts the step resolved before `dt_remaining` ran out.
    pub contacts_resolved: u32,
    /// Sum of `grid.candidate_pairs()` lengths across all iterations of the
    /// step (not the per-iteration mean — divide by `contacts_resolved + 1`
    /// for that).
    pub candidate_pairs: u32,
    /// True if the step terminated by exhausting `iter_cap`. Indicates the
    /// scheduler ran out of budget before resolving all contacts.
    pub iter_cap_hit: bool,
}
```

- [ ] **Step 1.4: Add `last_metrics` field and accessor**

In the `pub struct Sim` block (around line 24-36), add the field after `tick`:

```rust
    last_metrics: StepMetrics,
```

In the `from_fab` function near line 76, change the final `Self { ... }` to include the new field:

```rust
        Self { positions, velocities, states, chemistry, grid, bonds, last_metrics: StepMetrics::default(), tick: 0 }
```

Inside the `impl Sim { ... }` block, add right after `pub fn tick(&self) -> u32 { self.tick }`:

```rust
    pub fn last_step_metrics(&self) -> StepMetrics { self.last_metrics }
```

- [ ] **Step 1.5: Instrument the `step` method**

In `src/sim.rs::Sim::step`, replace the body so it accumulates a fresh `StepMetrics` and writes it to `self.last_metrics` at the end.

Find the line near the top of `step`:
```rust
    pub fn step(&mut self, frame_dt: f32) {
        let mut dt_remaining = frame_dt;
        // Cap iterations to avoid pathological infinite loops (paranoia, shouldn't fire).
        let mut iter_cap = self.positions.len() * 64;
```

Add immediately after the `iter_cap` declaration:

```rust
        let mut metrics = StepMetrics::default();
```

Find the inner loop's candidate-pair scan (around line 141):
```rust
            for (a, b) in self.grid.candidate_pairs() {
```

Change to:

```rust
            let pairs = self.grid.candidate_pairs();
            metrics.candidate_pairs = metrics.candidate_pairs.saturating_add(pairs.len() as u32);
            for (a, b) in pairs {
```

Find the contact-resolved block, after `if let Some((_t, a, b, exiting)) = earliest {` (around line 180), add as the first line inside that `if let`:

```rust
                metrics.contacts_resolved = metrics.contacts_resolved.saturating_add(1);
```

After the existing `while dt_remaining > 0.0 && iter_cap > 0` loop body, before `self.enforce_bonds();` (around line 244), add:

```rust
        if iter_cap == 0 {
            metrics.iter_cap_hit = true;
        }
```

At the very end of `step` (after `self.tick += 1;`), add:

```rust
        self.last_metrics = metrics;
```

- [ ] **Step 1.6: Update existing hand-rolled `Sim {}` literals in tests**

The five existing tests in `mod tests` of `src/sim.rs` construct `Sim` directly with field names. Each needs `last_metrics: StepMetrics::default(),` added.

Find each occurrence of `bonds,` (without a value, struct-init shorthand for the local `bonds`) or `bonds: HashSet::new(),` followed by `tick: 0,` and insert before `tick`:

For `two_beads_head_on_swap_velocities` (line 262-270):
```rust
        let mut sim = Sim {
            positions: vec![Vec2::new(5.0, 5.0), Vec2::new(7.0, 5.0)],
            velocities: vec![Vec2::new(1.0, 0.0), Vec2::new(-1.0, 0.0)],
            states: vec![g, g],
            chemistry: chem,
            grid: Grid::new(WORLD_SIZE),
            bonds: HashSet::new(),
            last_metrics: StepMetrics::default(),
            tick: 0,
        };
```

Same pattern for the other three constructed-by-hand sims (`two_bonded_beads_stay_bonded_over_time`, `wire_bonded_pair_swaps_states_on_contact`, `wire_free_pair_reflects_without_swap`).

- [ ] **Step 1.7: Run all tests**

Run:
```powershell
cargo test --lib
```
Expected: PASS, including `step_metrics_reports_contacts_and_pairs` and the four pre-existing tests that we just updated.

Run the integration tests too:
```powershell
cargo test --test chain_integrity --test determinism
```
Expected: PASS (unchanged behavior).

- [ ] **Step 1.8: Commit**

```powershell
git add src/sim.rs
git commit -m "Add StepMetrics instrumentation to Sim::step"
```

---

## Task 2: Bench module skeleton

**Files:**
- Modify: `src/lib.rs`
- Create: `src/bench/mod.rs`
- Modify: `Cargo.toml`

- [ ] **Step 2.1: Add `pub mod bench;` to `src/lib.rs`**

In `src/lib.rs`, insert after line 8 (`pub mod app;`):

```rust
pub mod bench;
```

- [ ] **Step 2.2: Create `src/bench/mod.rs`**

Write to `src/bench/mod.rs`:

```rust
pub mod scenario;
pub mod chains;
pub mod runner;
pub mod output;

pub use scenario::{Scenario, Invariants, geometric_bonds};
pub use chains::DisconnectedChains;
pub use runner::{run_scenario, ScenarioResult, BenchArgs};
pub use output::{format_markdown, format_csv};
```

- [ ] **Step 2.3: Add empty placeholder source files for submodules**

So `cargo check` succeeds before Task 3 fills them in. Write each as a one-line file:

`src/bench/scenario.rs`:
```rust
// Filled in by Task 3.
```

`src/bench/chains.rs`:
```rust
// Filled in by Task 4.
```

`src/bench/runner.rs`:
```rust
// Filled in by Task 6.
```

`src/bench/output.rs`:
```rust
// Filled in by Task 9.
```

- [ ] **Step 2.4: Verify compile fails on the re-exports**

Run:
```powershell
cargo check --lib
```
Expected: compile error in `src/bench/mod.rs` re-exporting undefined items.

That's correct — we'll fix it in subsequent tasks. To unblock the build for now, comment out the re-exports temporarily:

`src/bench/mod.rs`:
```rust
pub mod scenario;
pub mod chains;
pub mod runner;
pub mod output;

// Re-exports added back as submodules are implemented.
// pub use scenario::{Scenario, Invariants, geometric_bonds};
// pub use chains::DisconnectedChains;
// pub use runner::{run_scenario, ScenarioResult, BenchArgs};
// pub use output::{format_markdown, format_csv};
```

- [ ] **Step 2.5: Verify clean build**

Run:
```powershell
cargo check --lib
```
Expected: PASS.

- [ ] **Step 2.6: Add `[[bin]]` entry for bench binary in `Cargo.toml`**

In `Cargo.toml`, add after the existing `[[bin]]` block (lines 9-11):

```toml
[[bin]]
name = "bench"
path = "src/bin/bench.rs"
```

- [ ] **Step 2.7: Create stub `src/bin/bench.rs`**

```rust
fn main() {
    println!("bench harness — not yet implemented");
}
```

- [ ] **Step 2.8: Verify the bin compiles**

Run:
```powershell
cargo build --release --bin bench
```
Expected: PASS, produces `target/release/bench.exe`.

- [ ] **Step 2.9: Commit**

```powershell
git add src/lib.rs src/bench/ src/bin/bench.rs Cargo.toml
git commit -m "Scaffold bench module and bin entrypoint"
```

---

## Task 3: `Scenario` trait, `Invariants`, geometric bond helper

**Files:**
- Modify: `src/bench/scenario.rs`

- [ ] **Step 3.1: Write the failing test**

Replace `src/bench/scenario.rs` contents with:

```rust
use std::collections::HashSet;
use glam::Vec2;

use crate::ccd::RADIUS;
use crate::sim::Sim;

/// Captures the initial configuration of a Sim for end-of-run invariant checks.
pub struct Invariants {
    pub initial_bond_set: HashSet<(u32, u32)>,
    pub initial_state_histogram: Vec<usize>,
}

pub trait Scenario {
    /// Stable identifier used in CSV / report rows. snake_case, no spaces.
    fn name(&self) -> String;
    /// Construct a fresh Sim and snapshot its initial invariants.
    fn build(&self) -> (Sim, Invariants);
}

/// Set of all unordered pairs (a, b) with a < b whose torus-min-image distance
/// is strictly less than RADIUS (the bond threshold). Used to capture
/// initial bonds and check final bonds. O(N²) — only run at scenario setup
/// and at end-of-run, never per-step.
pub fn geometric_bonds(positions: &[Vec2], world_size: f32) -> HashSet<(u32, u32)> {
    let mut bonds = HashSet::new();
    let half = world_size * 0.5;
    for i in 0..positions.len() {
        for j in (i + 1)..positions.len() {
            let mut d = positions[j] - positions[i];
            if d.x >  half { d.x -= world_size; }
            if d.x < -half { d.x += world_size; }
            if d.y >  half { d.y -= world_size; }
            if d.y < -half { d.y += world_size; }
            if d.length() < RADIUS {
                bonds.insert((i as u32, j as u32));
            }
        }
    }
    bonds
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn geometric_bonds_finds_close_pair() {
        let positions = vec![
            Vec2::new(5.0, 5.0),
            Vec2::new(5.0, 5.5),  // 0.5 apart — bonded
            Vec2::new(5.0, 10.0), // far — not bonded
        ];
        let bonds = geometric_bonds(&positions, 50.0);
        assert_eq!(bonds.len(), 1);
        assert!(bonds.contains(&(0u32, 1u32)));
    }

    #[test]
    fn geometric_bonds_respects_torus_wrap() {
        // Beads at opposite edges of a 10-wide world should bond (short way is 0.4).
        let positions = vec![
            Vec2::new(0.2, 5.0),
            Vec2::new(9.8, 5.0),
        ];
        let bonds = geometric_bonds(&positions, 10.0);
        assert_eq!(bonds.len(), 1);
        assert!(bonds.contains(&(0u32, 1u32)));
    }

    #[test]
    fn geometric_bonds_excludes_pairs_at_radius() {
        // Exactly at R — not bonded (strict inequality |d| < R).
        let positions = vec![
            Vec2::new(5.0, 5.0),
            Vec2::new(5.0, 6.0),  // exactly 1.0 apart
        ];
        let bonds = geometric_bonds(&positions, 50.0);
        assert_eq!(bonds.len(), 0);
    }
}
```

- [ ] **Step 3.2: Run the tests to verify they pass**

Run:
```powershell
cargo test --lib bench::scenario
```
Expected: 3 tests PASS.

- [ ] **Step 3.3: Re-enable the `scenario` re-export**

In `src/bench/mod.rs`, uncomment the first re-export line:

```rust
pub use scenario::{Scenario, Invariants, geometric_bonds};
```

Run `cargo check --lib` — expected PASS.

- [ ] **Step 3.4: Commit**

```powershell
git add src/bench/scenario.rs src/bench/mod.rs
git commit -m "Add Scenario trait, Invariants, and geometric_bonds helper"
```

---

## Task 4: `DisconnectedChains` scenario

**Files:**
- Modify: `src/bench/chains.rs`

- [ ] **Step 4.1: Write the failing tests**

Replace `src/bench/chains.rs` contents with:

```rust
use std::collections::HashSet;
use glam::Vec2;

use crate::chemistry::load_chemistry;
use crate::sim::{Sim, WORLD_SIZE};
use crate::fab::{Fab, FabMeta, BeadSpec};

use super::scenario::{Scenario, Invariants, geometric_bonds};

const BEAD_SPACING: f32 = 0.667;
const CHAIN_SPACING_X: f32 = 5.0;
const CHAIN_ROW_GAP_Y: f32 = 2.0;
const SEED: u32 = 42;

/// Vertical bonded chains laid out in a 2D grid across the world. Beads
/// within a chain are at 0.667 spacing (just inside the bond threshold).
/// Chains within a row are at 5.0 horizontal spacing. Rows wrap when the
/// world width is exhausted, with vertical gap `chain_len * 0.667 + 2.0`.
///
/// If a single chain's vertical extent exceeds world height, the chain
/// serpentines (alternating columns, snake-style) so it fits without
/// self-wrapping on the torus.
///
/// One `on` bead at index 0 of each chain; rest `off`. Uses the wire
/// chemistry — bonds are invariant by topology.
pub struct DisconnectedChains {
    pub chain_count: u32,
    pub chain_len: u32,
    pub world_size: f32,
}

impl DisconnectedChains {
    fn layout(&self) -> Vec<Vec2> {
        let chain_extent_y = (self.chain_len as f32 - 1.0) * BEAD_SPACING;
        if chain_extent_y < self.world_size {
            self.grid_layout()
        } else {
            self.serpentine_layout()
        }
    }

    fn grid_layout(&self) -> Vec<Vec2> {
        let chains_per_row = (self.world_size / CHAIN_SPACING_X).floor() as u32;
        assert!(chains_per_row >= 1, "world too narrow to fit even one chain");
        let row_height = (self.chain_len as f32 - 1.0) * BEAD_SPACING + CHAIN_ROW_GAP_Y;
        let mut positions = Vec::with_capacity((self.chain_count * self.chain_len) as usize);
        for c in 0..self.chain_count {
            let row = c / chains_per_row;
            let col = c % chains_per_row;
            let x = CHAIN_SPACING_X * (col as f32) + (CHAIN_SPACING_X / 2.0);
            let y0 = row_height * (row as f32) + (CHAIN_ROW_GAP_Y / 2.0);
            for b in 0..self.chain_len {
                positions.push(Vec2::new(x, y0 + (b as f32) * BEAD_SPACING));
            }
        }
        positions
    }

    fn serpentine_layout(&self) -> Vec<Vec2> {
        assert_eq!(self.chain_count, 1, "serpentine only supports single-chain scenarios");
        // Chain runs down a column, hits world_size, jumps over to the next
        // column with horizontal offset CHAIN_SPACING_X, and runs up. Net
        // effect: a single long chain folded to fit in a small world.
        let mut positions = Vec::with_capacity(self.chain_len as usize);
        let column_height = (self.world_size / BEAD_SPACING).floor() as u32;
        for b in 0..self.chain_len {
            let col = b / column_height;
            let row_in_col = b % column_height;
            let x = CHAIN_SPACING_X * (col as f32) + (CHAIN_SPACING_X / 2.0);
            let y = if col % 2 == 0 {
                BEAD_SPACING * (row_in_col as f32) + 1.0
            } else {
                self.world_size - BEAD_SPACING * (row_in_col as f32) - 1.0
            };
            positions.push(Vec2::new(x, y));
        }
        positions
    }
}

impl Scenario for DisconnectedChains {
    fn name(&self) -> String {
        format!("chains_{}x{}", self.chain_count, self.chain_len)
    }

    fn build(&self) -> (Sim, Invariants) {
        let positions = self.layout();
        let n = positions.len();
        let beads: Vec<BeadSpec> = positions.iter().enumerate().map(|(i, p)| {
            let state = if i as u32 % self.chain_len == 0 { "on" } else { "off" };
            BeadSpec {
                state: state.to_string(),
                pos: [p.x, p.y],
                vel: None,
            }
        }).collect();
        let fab = Fab {
            meta: FabMeta {
                name: self.name(),
                chemistry: "wire".to_string(),
                seed: SEED,
                world_size: Some(self.world_size),
            },
            beads,
        };
        let chemistry = load_chemistry("chemistries/wire.toml").expect("load wire chemistry");
        let sim = Sim::from_fab(&fab, chemistry);

        let initial_bond_set = geometric_bonds(&sim.positions, sim.world_size());
        let mut histogram = vec![0usize; 2]; // wire has "off", "on"
        for &s in &sim.states {
            histogram[s as usize] += 1;
        }
        let _ = n; // silence unused warning if assertions stripped
        let _ = WORLD_SIZE;
        (sim, Invariants { initial_bond_set, initial_state_histogram: histogram })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ten_by_thirty_single_row_layout() {
        let s = DisconnectedChains { chain_count: 10, chain_len: 30, world_size: 50.0 };
        let (sim, inv) = s.build();
        assert_eq!(sim.positions.len(), 300);
        // 10 chains × 29 intra-chain bonds = 290 bonds, no cross-chain bonds.
        assert_eq!(inv.initial_bond_set.len(), 290);
        // First bead of each chain is "on", rest are "off". State indices:
        // wire chemistry has states ["off", "on"], so on=1, off=0.
        assert_eq!(inv.initial_state_histogram[1], 10);
        assert_eq!(inv.initial_state_histogram[0], 290);
    }

    #[test]
    fn multi_row_layout_no_cross_chain_bonds() {
        let s = DisconnectedChains { chain_count: 30, chain_len: 30, world_size: 128.0 };
        let (sim, inv) = s.build();
        assert_eq!(sim.positions.len(), 900);
        // 30 chains × 29 = 870 intra-chain bonds expected.
        assert_eq!(inv.initial_bond_set.len(), 870, "no cross-chain bonds should form at this spacing");
    }

    #[test]
    fn serpentine_layout_fits_long_chain_in_small_world() {
        // Single chain of 300 beads × 0.667 = 200 vertical extent.
        // In a world of 64, must serpentine. World 64 means ~96 beads per column,
        // so 300 beads needs ~3 columns.
        let s = DisconnectedChains { chain_count: 1, chain_len: 300, world_size: 64.0 };
        let (sim, inv) = s.build();
        assert_eq!(sim.positions.len(), 300);
        // Single chain → 299 intra-chain bonds, no others.
        assert_eq!(inv.initial_bond_set.len(), 299);
    }

    #[test]
    fn name_is_snake_case_with_dims() {
        let s = DisconnectedChains { chain_count: 50, chain_len: 30, world_size: 256.0 };
        assert_eq!(s.name(), "chains_50x30");
    }
}
```

- [ ] **Step 4.2: Check that `BeadSpec` and `FabMeta` are pub**

Read `src/fab.rs` to confirm `BeadSpec`, `FabMeta`, and `Fab` are all `pub`. If `BeadSpec.state` is not `pub`, or `vel` is not the right type, this task will fail. (Spec note: as of writing, they are pub.) If a field is private, **stop and adjust** — add `pub` to the relevant struct field in `src/fab.rs` and include the change in this task's commit.

Run:
```powershell
cargo check --lib
```
Expected: PASS (or compile errors that pinpoint which `fab.rs` field needs `pub`).

- [ ] **Step 4.3: Run the tests**

Run:
```powershell
cargo test --lib bench::chains
```
Expected: 4 tests PASS.

- [ ] **Step 4.4: Re-enable the chains re-export**

In `src/bench/mod.rs`, uncomment the chains line:

```rust
pub use chains::DisconnectedChains;
```

Run `cargo check --lib` — expected PASS.

- [ ] **Step 4.5: Commit**

```powershell
git add src/bench/chains.rs src/bench/mod.rs
git commit -m "Add DisconnectedChains scenario (grid + serpentine layouts)"
```

(If `src/fab.rs` was modified for `pub` access, include it: `git add src/fab.rs`.)

---

## Task 5: `BenchArgs`, `ScenarioResult`, percentile computation

**Files:**
- Modify: `src/bench/runner.rs`

- [ ] **Step 5.1: Write the failing tests**

Replace `src/bench/runner.rs` contents with:

```rust
#[derive(Clone, Debug)]
pub struct BenchArgs {
    pub substeps: u32,
    pub frames: u32,
    pub warmup_frames: u32,
    pub max_wall_seconds: f64,
    pub verify_determinism: bool,
}

impl Default for BenchArgs {
    fn default() -> Self {
        Self {
            substeps: 10,
            frames: 3000,
            warmup_frames: 60,
            max_wall_seconds: 300.0,
            verify_determinism: false,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Percentiles {
    pub mean: f64,
    pub p50: f64,
    pub p99: f64,
    pub max: f64,
}

impl Percentiles {
    /// Compute percentiles from a vector of samples. Sorts in-place.
    /// Panics on empty input — callers should guard.
    pub fn from_samples(samples: &mut [f64]) -> Self {
        assert!(!samples.is_empty(), "Percentiles::from_samples on empty");
        samples.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        let sum: f64 = samples.iter().sum();
        let mean = sum / samples.len() as f64;
        let p50 = samples[samples.len() / 2];
        let p99 = samples[((samples.len() as f64) * 0.99) as usize];
        let max = *samples.last().unwrap();
        Self { mean, p50, p99, max }
    }
}

#[derive(Clone, Debug)]
pub struct ScenarioResult {
    pub name: String,
    pub bead_count: u32,
    pub frames_completed: u32,
    pub frames_requested: u32,
    pub truncated: bool,
    pub frame_time_ms: Percentiles,
    pub substep_time_us: Percentiles,
    pub contacts_per_substep: Percentiles,
    pub candidate_pairs_per_substep_mean: f64,
    pub iter_cap_saturation_rate: f64,
    pub effective_fps: f64,
    pub substeps_per_16ms_budget: u32,
    pub bonds_preserved: bool,
    pub bonds_lost: usize,
    pub bonds_added: usize,
    pub determinism_verified: Option<bool>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn percentiles_basic() {
        let mut s = vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0];
        let p = Percentiles::from_samples(&mut s);
        assert!((p.mean - 5.5).abs() < 1e-6);
        assert_eq!(p.p50, 6.0); // samples[5]
        assert_eq!(p.p99, 10.0); // samples[9]
        assert_eq!(p.max, 10.0);
    }

    #[test]
    fn percentiles_single_sample() {
        let mut s = vec![42.0];
        let p = Percentiles::from_samples(&mut s);
        assert_eq!(p.mean, 42.0);
        assert_eq!(p.p50, 42.0);
        assert_eq!(p.p99, 42.0);
        assert_eq!(p.max, 42.0);
    }

    #[test]
    fn bench_args_default() {
        let a = BenchArgs::default();
        assert_eq!(a.substeps, 10);
        assert_eq!(a.frames, 3000);
        assert_eq!(a.warmup_frames, 60);
        assert_eq!(a.max_wall_seconds, 300.0);
        assert!(!a.verify_determinism);
    }
}
```

- [ ] **Step 5.2: Run the tests**

Run:
```powershell
cargo test --lib bench::runner
```
Expected: 3 tests PASS.

- [ ] **Step 5.3: Commit**

```powershell
git add src/bench/runner.rs
git commit -m "Add BenchArgs, ScenarioResult, and percentile computation"
```

---

## Task 6: `run_scenario` loop

**Files:**
- Modify: `src/bench/runner.rs`

- [ ] **Step 6.1: Write the failing test**

In `src/bench/runner.rs`, *before* the `#[cfg(test)]` block, add:

```rust
use web_time::Instant;

use super::scenario::{Scenario, geometric_bonds};

pub fn run_scenario(scenario: &dyn Scenario, args: &BenchArgs) -> ScenarioResult {
    let (mut sim, invariants) = scenario.build();
    let bead_count = sim.positions.len() as u32;
    let frame_dt = 1.0 / 60.0;

    // Warmup — discard timings.
    for _ in 0..args.warmup_frames {
        for _ in 0..args.substeps {
            sim.step(frame_dt);
        }
    }

    let run_start = Instant::now();
    let total_substeps_planned = (args.frames as usize) * (args.substeps as usize);
    let mut frame_times_ms = Vec::with_capacity(args.frames as usize);
    let mut substep_times_us = Vec::with_capacity(total_substeps_planned);
    let mut contacts_per_substep = Vec::with_capacity(total_substeps_planned);
    let mut candidate_pairs_total: u64 = 0;
    let mut iter_cap_hits: u64 = 0;
    let mut frames_completed: u32 = 0;
    let mut truncated = false;

    for _ in 0..args.frames {
        if run_start.elapsed().as_secs_f64() > args.max_wall_seconds {
            truncated = true;
            break;
        }
        let frame_start = Instant::now();
        for _ in 0..args.substeps {
            let substep_start = Instant::now();
            sim.step(frame_dt);
            let elapsed_us = substep_start.elapsed().as_micros() as f64;
            substep_times_us.push(elapsed_us);
            let m = sim.last_step_metrics();
            contacts_per_substep.push(m.contacts_resolved as f64);
            candidate_pairs_total = candidate_pairs_total.saturating_add(m.candidate_pairs as u64);
            if m.iter_cap_hit { iter_cap_hits += 1; }
        }
        frame_times_ms.push(frame_start.elapsed().as_secs_f64() * 1000.0);
        frames_completed += 1;
    }

    // Guard empty samples (truncated immediately).
    if frame_times_ms.is_empty() {
        frame_times_ms.push(0.0);
    }
    if substep_times_us.is_empty() {
        substep_times_us.push(0.0);
        contacts_per_substep.push(0.0);
    }

    let frame_time = Percentiles::from_samples(&mut frame_times_ms.clone());
    let substep_time = Percentiles::from_samples(&mut substep_times_us.clone());
    let contacts = Percentiles::from_samples(&mut contacts_per_substep.clone());
    let total_substeps = substep_times_us.len().max(1);
    let candidate_pairs_mean = candidate_pairs_total as f64 / total_substeps as f64;
    let iter_cap_saturation_rate = iter_cap_hits as f64 / total_substeps as f64;
    let effective_fps = if frame_time.mean > 0.0 { 1000.0 / frame_time.mean } else { 0.0 };
    let substeps_per_16ms_budget = if substep_time.mean > 0.0 {
        (16_600.0 / substep_time.mean).floor() as u32
    } else { 0 };

    let final_bonds = geometric_bonds(&sim.positions, sim.world_size());
    let bonds_preserved = final_bonds == invariants.initial_bond_set;
    let bonds_lost = invariants.initial_bond_set.difference(&final_bonds).count();
    let bonds_added = final_bonds.difference(&invariants.initial_bond_set).count();

    ScenarioResult {
        name: scenario.name(),
        bead_count,
        frames_completed,
        frames_requested: args.frames,
        truncated,
        frame_time_ms: frame_time,
        substep_time_us: substep_time,
        contacts_per_substep: contacts,
        candidate_pairs_per_substep_mean: candidate_pairs_mean,
        iter_cap_saturation_rate,
        effective_fps,
        substeps_per_16ms_budget,
        bonds_preserved,
        bonds_lost,
        bonds_added,
        determinism_verified: None,
    }
}
```

Inside the `#[cfg(test)] mod tests` block, add (before the closing `}`):

```rust
    use crate::bench::chains::DisconnectedChains;

    #[test]
    fn run_scenario_produces_result_with_correct_shape() {
        let scenario = DisconnectedChains { chain_count: 2, chain_len: 5, world_size: 30.0 };
        let args = BenchArgs {
            substeps: 2,
            frames: 10,
            warmup_frames: 1,
            max_wall_seconds: 60.0,
            verify_determinism: false,
        };
        let r = run_scenario(&scenario, &args);
        assert_eq!(r.name, "chains_2x5");
        assert_eq!(r.bead_count, 10);
        assert_eq!(r.frames_completed, 10);
        assert!(!r.truncated);
        assert!(r.bonds_preserved, "tiny chain scenario should preserve bonds");
        assert_eq!(r.bonds_lost, 0);
        assert_eq!(r.bonds_added, 0);
    }
```

- [ ] **Step 6.2: Run the test**

Run:
```powershell
cargo test --lib bench::runner::tests::run_scenario_produces
```
Expected: PASS.

- [ ] **Step 6.3: Re-enable the runner re-export**

In `src/bench/mod.rs`, uncomment the runner re-export:

```rust
pub use runner::{run_scenario, ScenarioResult, BenchArgs};
```

Run `cargo check --lib` — expected PASS.

- [ ] **Step 6.4: Commit**

```powershell
git add src/bench/runner.rs src/bench/mod.rs
git commit -m "Add run_scenario loop with timing and invariant checks"
```

---

## Task 7: Truncation regression test

**Files:**
- Modify: `src/bench/runner.rs`

The truncation logic was added in Task 6 (the `max_wall_seconds` check at the top of the frame loop). Verify it actually works with a regression test.

- [ ] **Step 7.1: Write the failing test**

Add to `src/bench/runner.rs`'s `#[cfg(test)] mod tests` (before closing `}`):

```rust
    #[test]
    fn run_scenario_truncates_on_wall_clock_cap() {
        // A long-running scenario with a tight wall budget should report
        // truncated=true and frames_completed < frames_requested.
        let scenario = DisconnectedChains { chain_count: 5, chain_len: 30, world_size: 50.0 };
        let args = BenchArgs {
            substeps: 10,
            frames: 100_000,           // way more than will fit
            warmup_frames: 0,
            max_wall_seconds: 0.05,    // 50 ms budget
            verify_determinism: false,
        };
        let r = run_scenario(&scenario, &args);
        assert!(r.truncated, "should have truncated under 50ms budget");
        assert!(r.frames_completed < 100_000, "fewer frames than requested");
        // Even truncated, the result struct should be filled in (percentiles
        // not panic, mean > 0 once we have samples).
        assert!(r.frame_time_ms.mean >= 0.0);
    }
```

- [ ] **Step 7.2: Run the test**

Run:
```powershell
cargo test --lib bench::runner::tests::run_scenario_truncates
```
Expected: PASS (truncation logic was already implemented in Task 6).

- [ ] **Step 7.3: Commit**

```powershell
git add src/bench/runner.rs
git commit -m "Add regression test for --max-wall-seconds truncation"
```

---

## Task 8: Determinism verification

**Files:**
- Modify: `src/bench/runner.rs`

- [ ] **Step 8.1: Write the failing test**

Add to `src/bench/runner.rs`'s `#[cfg(test)] mod tests`:

```rust
    #[test]
    fn run_scenario_with_verify_determinism_sets_field() {
        let scenario = DisconnectedChains { chain_count: 2, chain_len: 5, world_size: 30.0 };
        let args = BenchArgs {
            substeps: 2,
            frames: 5,
            warmup_frames: 0,
            max_wall_seconds: 60.0,
            verify_determinism: true,
        };
        let r = run_scenario(&scenario, &args);
        // Two runs of the same scenario with the same seed should be bit-identical.
        assert_eq!(r.determinism_verified, Some(true));
    }
```

- [ ] **Step 8.2: Run the test to verify it fails**

Run:
```powershell
cargo test --lib bench::runner::tests::run_scenario_with_verify_determinism
```
Expected: FAIL — `determinism_verified` is currently always `None`.

- [ ] **Step 8.3: Implement the determinism check**

In `src/bench/runner.rs::run_scenario`, just before the final `ScenarioResult { ... }` literal, replace the line `determinism_verified: None,` (in the struct literal) by computing it first.

Change the variable construction to add (just before the `ScenarioResult` literal):

```rust
    let determinism_verified = if args.verify_determinism {
        let (mut sim2, _) = scenario.build();
        for _ in 0..args.warmup_frames {
            for _ in 0..args.substeps {
                sim2.step(frame_dt);
            }
        }
        for _ in 0..frames_completed {
            for _ in 0..args.substeps {
                sim2.step(frame_dt);
            }
        }
        Some(sim2.positions == sim.positions && sim2.states == sim.states)
    } else {
        None
    };
```

And change the `ScenarioResult` literal's last field from `determinism_verified: None,` to `determinism_verified,`.

- [ ] **Step 8.4: Run the test**

Run:
```powershell
cargo test --lib bench::runner::tests::run_scenario_with_verify_determinism
```
Expected: PASS.

- [ ] **Step 8.5: Run all bench tests**

```powershell
cargo test --lib bench
```
Expected: all PASS.

- [ ] **Step 8.6: Commit**

```powershell
git add src/bench/runner.rs
git commit -m "Implement --verify-determinism: re-run and compare final state"
```

---

## Task 9: Markdown + CSV output

**Files:**
- Modify: `src/bench/output.rs`

- [ ] **Step 9.1: Write the failing tests**

Replace `src/bench/output.rs` contents with:

```rust
use crate::bench::runner::ScenarioResult;

/// Pretty markdown table for stdout. Truncated scenarios get a `*` next to
/// their name; the footer lists their actual frame counts.
pub fn format_markdown(results: &[ScenarioResult]) -> String {
    let mut out = String::new();
    out.push_str("| scenario          |   N   | frame_ms (mean/p99) | substep_us (mean/p99) | contacts/ss (mean/p99) |   fps   | sub/16ms | iter_cap_sat | bonds OK |\n");
    out.push_str("|-------------------|-------|---------------------|-----------------------|------------------------|---------|----------|--------------|----------|\n");
    let mut truncated_notes = Vec::new();
    for r in results {
        let name_display = if r.truncated {
            truncated_notes.push(format!("- `{}` truncated after {}/{} frames", r.name, r.frames_completed, r.frames_requested));
            format!("{} *", r.name)
        } else {
            r.name.clone()
        };
        let bonds_ok = if r.bonds_preserved { "y" } else {
            &format!("n (-{}/+{})", r.bonds_lost, r.bonds_added).to_string()[..]
                .to_string()
        }.to_string();
        out.push_str(&format!(
            "| {:<17} | {:>5} | {:>8.2} / {:>8.2} | {:>8.0} / {:>8.0} | {:>8.1} / {:>8.1} | {:>7.1} | {:>8} | {:>12.4} | {:>8} |\n",
            name_display,
            r.bead_count,
            r.frame_time_ms.mean,
            r.frame_time_ms.p99,
            r.substep_time_us.mean,
            r.substep_time_us.p99,
            r.contacts_per_substep.mean,
            r.contacts_per_substep.p99,
            r.effective_fps,
            r.substeps_per_16ms_budget,
            r.iter_cap_saturation_rate,
            bonds_ok,
        ));
    }
    if !truncated_notes.is_empty() {
        out.push('\n');
        for n in &truncated_notes {
            out.push_str(n);
            out.push('\n');
        }
    }
    out
}

/// CSV: one header row + one row per scenario. Wide format so each metric
/// is its own column.
pub fn format_csv(results: &[ScenarioResult]) -> String {
    let mut out = String::new();
    out.push_str("scenario,bead_count,frames_completed,frames_requested,truncated,frame_ms_mean,frame_ms_p50,frame_ms_p99,frame_ms_max,substep_us_mean,substep_us_p50,substep_us_p99,substep_us_max,contacts_mean,contacts_p50,contacts_p99,contacts_max,candidate_pairs_mean,iter_cap_saturation,effective_fps,substeps_per_16ms,bonds_preserved,bonds_lost,bonds_added,determinism_verified\n");
    for r in results {
        out.push_str(&format!(
            "{},{},{},{},{},{:.4},{:.4},{:.4},{:.4},{:.4},{:.4},{:.4},{:.4},{:.4},{:.4},{:.4},{:.4},{:.4},{:.6},{:.4},{},{},{},{},{}\n",
            r.name,
            r.bead_count,
            r.frames_completed,
            r.frames_requested,
            r.truncated,
            r.frame_time_ms.mean, r.frame_time_ms.p50, r.frame_time_ms.p99, r.frame_time_ms.max,
            r.substep_time_us.mean, r.substep_time_us.p50, r.substep_time_us.p99, r.substep_time_us.max,
            r.contacts_per_substep.mean, r.contacts_per_substep.p50, r.contacts_per_substep.p99, r.contacts_per_substep.max,
            r.candidate_pairs_per_substep_mean,
            r.iter_cap_saturation_rate,
            r.effective_fps,
            r.substeps_per_16ms_budget,
            r.bonds_preserved,
            r.bonds_lost,
            r.bonds_added,
            match r.determinism_verified { Some(true) => "true", Some(false) => "false", None => "" },
        ));
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bench::runner::Percentiles;

    fn fixture() -> ScenarioResult {
        ScenarioResult {
            name: "chains_test".to_string(),
            bead_count: 100,
            frames_completed: 3000,
            frames_requested: 3000,
            truncated: false,
            frame_time_ms: Percentiles { mean: 1.0, p50: 1.0, p99: 1.5, max: 2.0 },
            substep_time_us: Percentiles { mean: 100.0, p50: 95.0, p99: 200.0, max: 300.0 },
            contacts_per_substep: Percentiles { mean: 3.0, p50: 3.0, p99: 5.0, max: 7.0 },
            candidate_pairs_per_substep_mean: 50.0,
            iter_cap_saturation_rate: 0.0,
            effective_fps: 1000.0,
            substeps_per_16ms_budget: 166,
            bonds_preserved: true,
            bonds_lost: 0,
            bonds_added: 0,
            determinism_verified: None,
        }
    }

    #[test]
    fn markdown_has_header_and_one_row() {
        let md = format_markdown(&[fixture()]);
        assert!(md.contains("scenario"));
        assert!(md.contains("chains_test"));
        assert!(md.contains("100"));
        // Untruncated runs should not get a star or footnote.
        assert!(!md.contains(" *"));
    }

    #[test]
    fn markdown_marks_truncated_scenarios() {
        let mut f = fixture();
        f.truncated = true;
        f.frames_completed = 47;
        let md = format_markdown(&[f]);
        assert!(md.contains("chains_test *"));
        assert!(md.contains("truncated after 47/3000"));
    }

    #[test]
    fn csv_has_header_and_one_row() {
        let csv = format_csv(&[fixture()]);
        let mut lines = csv.lines();
        let header = lines.next().unwrap();
        assert!(header.starts_with("scenario,"));
        assert!(header.contains("bonds_preserved"));
        let row = lines.next().unwrap();
        assert!(row.starts_with("chains_test,"));
    }

    #[test]
    fn csv_renders_determinism_field_as_string() {
        let mut f = fixture();
        f.determinism_verified = Some(true);
        let csv = format_csv(&[f]);
        assert!(csv.lines().nth(1).unwrap().ends_with(",true\n") || csv.lines().nth(1).unwrap().ends_with(",true"));
    }
}
```

- [ ] **Step 9.2: Run the tests**

Run:
```powershell
cargo test --lib bench::output
```
Expected: 4 tests PASS.

- [ ] **Step 9.3: Re-enable the output re-export**

In `src/bench/mod.rs`, uncomment:

```rust
pub use output::{format_markdown, format_csv};
```

Run `cargo check --lib` — expected PASS.

- [ ] **Step 9.4: Commit**

```powershell
git add src/bench/output.rs src/bench/mod.rs
git commit -m "Add markdown + CSV formatters for scenario results"
```

---

## Task 10: CLI arg parsing and main bench binary

**Files:**
- Modify: `src/bin/bench.rs`

- [ ] **Step 10.1: Replace stub with full main**

Write to `src/bin/bench.rs`:

```rust
use std::env;
use std::process::ExitCode;
use std::fs;

use jigglefab::bench::{
    BenchArgs, DisconnectedChains, Scenario, ScenarioResult, format_csv, format_markdown,
    run_scenario,
};

struct ParsedArgs {
    bench: BenchArgs,
    scenarios_filter: Option<Vec<String>>,
    csv_path: Option<String>,
}

fn print_usage() {
    eprintln!("Usage: cargo run --release --bin bench -- [OPTIONS]");
    eprintln!("Options:");
    eprintln!("  --scenarios <a,b,c>     Subset of scenarios to run (default: all default-sweep)");
    eprintln!("  --substeps <N>          Substeps per frame (default: 10)");
    eprintln!("  --frames <N>            Frames post-warmup (default: 3000)");
    eprintln!("  --warmup <N>            Warmup frames discarded (default: 60)");
    eprintln!("  --max-wall-seconds <S>  Per-scenario wall cap (default: 300)");
    eprintln!("  --csv <path>            Write CSV to this path");
    eprintln!("  --verify-determinism    Re-run each scenario and check bit-equality");
    eprintln!("  --help                  Show this message");
}

fn parse_args() -> Result<ParsedArgs, String> {
    let mut bench = BenchArgs::default();
    let mut scenarios_filter: Option<Vec<String>> = None;
    let mut csv_path: Option<String> = None;
    let argv: Vec<String> = env::args().skip(1).collect();
    let mut i = 0;
    while i < argv.len() {
        match argv[i].as_str() {
            "--help" => {
                print_usage();
                std::process::exit(0);
            }
            "--scenarios" => {
                i += 1;
                let v = argv.get(i).ok_or("--scenarios needs a value")?;
                scenarios_filter = Some(v.split(',').map(|s| s.trim().to_string()).collect());
            }
            "--substeps" => {
                i += 1;
                bench.substeps = argv.get(i).ok_or("--substeps needs a value")?.parse().map_err(|e: std::num::ParseIntError| e.to_string())?;
            }
            "--frames" => {
                i += 1;
                bench.frames = argv.get(i).ok_or("--frames needs a value")?.parse().map_err(|e: std::num::ParseIntError| e.to_string())?;
            }
            "--warmup" => {
                i += 1;
                bench.warmup_frames = argv.get(i).ok_or("--warmup needs a value")?.parse().map_err(|e: std::num::ParseIntError| e.to_string())?;
            }
            "--max-wall-seconds" => {
                i += 1;
                bench.max_wall_seconds = argv.get(i).ok_or("--max-wall-seconds needs a value")?.parse().map_err(|e: std::num::ParseFloatError| e.to_string())?;
            }
            "--csv" => {
                i += 1;
                csv_path = Some(argv.get(i).ok_or("--csv needs a value")?.clone());
            }
            "--verify-determinism" => {
                bench.verify_determinism = true;
            }
            other => return Err(format!("unknown arg: {}", other)),
        }
        i += 1;
    }
    Ok(ParsedArgs { bench, scenarios_filter, csv_path })
}

/// The default sweep. Excludes `chains_100x100` (opt-in via --scenarios).
fn default_scenarios() -> Vec<Box<dyn Scenario>> {
    vec![
        Box::new(DisconnectedChains { chain_count: 10,  chain_len: 30,  world_size: 50.0 }),
        Box::new(DisconnectedChains { chain_count: 30,  chain_len: 30,  world_size: 128.0 }),
        Box::new(DisconnectedChains { chain_count: 50,  chain_len: 30,  world_size: 256.0 }),
        Box::new(DisconnectedChains { chain_count: 10,  chain_len: 100, world_size: 128.0 }),
        Box::new(DisconnectedChains { chain_count: 1,   chain_len: 300, world_size: 64.0 }),
        Box::new(DisconnectedChains { chain_count: 100, chain_len: 30,  world_size: 256.0 }),
    ]
}

/// All known scenarios — used when --scenarios filters by name and chains_100x100
/// is requested explicitly.
fn all_scenarios() -> Vec<Box<dyn Scenario>> {
    let mut s = default_scenarios();
    s.push(Box::new(DisconnectedChains { chain_count: 100, chain_len: 100, world_size: 256.0 }));
    s
}

fn select_scenarios(filter: Option<Vec<String>>) -> Vec<Box<dyn Scenario>> {
    match filter {
        None => default_scenarios(),
        Some(names) => {
            let all = all_scenarios();
            all.into_iter()
                .filter(|s| names.iter().any(|n| n == &s.name()))
                .collect()
        }
    }
}

fn main() -> ExitCode {
    let parsed = match parse_args() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("error: {}", e);
            print_usage();
            return ExitCode::from(2);
        }
    };

    let scenarios = select_scenarios(parsed.scenarios_filter.clone());
    if scenarios.is_empty() {
        eprintln!("no scenarios match filter: {:?}", parsed.scenarios_filter);
        return ExitCode::from(2);
    }

    let mut results: Vec<ScenarioResult> = Vec::with_capacity(scenarios.len());
    for scenario in &scenarios {
        eprintln!("running {} (N={})...", scenario.name(), {
            let (s, _) = scenario.build();
            s.positions.len()
        });
        let r = run_scenario(scenario.as_ref(), &parsed.bench);
        eprintln!(
            "  {} frame_ms mean={:.2} p99={:.2} fps={:.1} bonds_ok={} truncated={}",
            r.name, r.frame_time_ms.mean, r.frame_time_ms.p99, r.effective_fps, r.bonds_preserved, r.truncated
        );
        results.push(r);
    }

    print!("{}", format_markdown(&results));

    if let Some(path) = parsed.csv_path {
        let csv = format_csv(&results);
        if let Err(e) = fs::write(&path, csv) {
            eprintln!("failed to write CSV to {}: {}", path, e);
            return ExitCode::from(1);
        }
        eprintln!("wrote CSV to {}", path);
    }

    ExitCode::SUCCESS
}
```

- [ ] **Step 10.2: Build the binary**

Run:
```powershell
cargo build --release --bin bench
```
Expected: PASS.

- [ ] **Step 10.3: Smoke test with a tiny scenario**

Run:
```powershell
cargo run --release --bin bench -- --scenarios chains_10x30 --frames 100 --warmup 10
```
Expected: prints a one-row markdown table after a few seconds; exit code 0.

- [ ] **Step 10.4: Smoke test CSV output**

Run:
```powershell
cargo run --release --bin bench -- --scenarios chains_10x30 --frames 100 --warmup 10 --csv smoke.csv
type smoke.csv
del smoke.csv
```
Expected: CSV file written with one header + one data row; output matches the markdown info.

- [ ] **Step 10.5: Test --help**

Run:
```powershell
cargo run --release --bin bench -- --help
```
Expected: usage printed; exit 0.

- [ ] **Step 10.6: Commit**

```powershell
git add src/bin/bench.rs
git commit -m "Wire up bench CLI: arg parsing, scenario selection, output dispatch"
```

---

## Task 11: Verify all tests still pass

**Files:** None new.

- [ ] **Step 11.1: Run the full test suite**

Run:
```powershell
cargo test
```
Expected: all PASS — unit tests for bench module, plus the existing `chain_integrity` and `determinism` integration tests should be unchanged.

- [ ] **Step 11.2: Run with --release once for sanity**

Run:
```powershell
cargo test --release
```
Expected: all PASS.

- [ ] **Step 11.3: If anything fails, investigate; do NOT commit a broken state**

If anything is red, stop and address it before continuing. Test failures may indicate the instrumentation in Task 1 affected timing-sensitive integration tests (e.g. `determinism.rs`), which should not happen if the metrics population is purely additive — but verify.

---

## Task 12: Capture baseline run

**Files:**
- Create: `docs/bench-results/2026-05-21-baseline.md`

- [ ] **Step 12.1: Create the directory**

Run:
```powershell
mkdir docs\bench-results
```

(Skip if it already exists from a prior run.)

- [ ] **Step 12.2: Run the full default sweep**

Run:
```powershell
cargo run --release --bin bench -- --csv docs\bench-results\2026-05-21-baseline.csv > docs\bench-results\2026-05-21-baseline-table.txt
```

Expected: takes several minutes. Stops cleanly at most after `(6 default scenarios) × 300s max_wall_seconds = 30 min` worst-case. In practice the small scenarios finish in seconds; the past-the-wall ones (chains_100x30) likely truncate.

- [ ] **Step 12.3: Write the baseline markdown wrapper**

Write to `docs/bench-results/2026-05-21-baseline.md`:

```markdown
# Bench baseline — 2026-05-21

Run before Phase 2 GPU-parallel CCD landed. This is the table we'll be measuring deltas against.

**Machine:** [fill in: dev machine spec — CPU model, OS, anything material]
**Build:** `cargo run --release --bin bench`
**Args:** defaults (substeps=10, frames=3000, warmup=60, max_wall_seconds=300)
**Sim version:** commit `[fill in: git rev-parse HEAD]`

## Default sweep

[paste contents of 2026-05-21-baseline-table.txt here]

## Observations

[fill in: any surprises; which scenarios truncated; iter_cap_sat rate; bonds_preserved status]

## Raw data

CSV: `2026-05-21-baseline.csv`
```

After the run completes, paste the table from `2026-05-21-baseline-table.txt` into the markdown placeholder, fill in the machine spec and observations, and remove the intermediate `.txt` file.

- [ ] **Step 12.4: Commit**

```powershell
git add docs\bench-results\2026-05-21-baseline.md docs\bench-results\2026-05-21-baseline.csv
git commit -m "Capture pre-Phase-2 bench baseline"
```

---

## Task 13: Bump deployed fab

**Files:**
- Delete: `fabs/wire-10x30.toml`
- Create: `fabs/wire-50x30.toml`
- Modify: `src/app.rs:36, 98`

- [ ] **Step 13.1: Generate the new fab file**

Write a small one-shot PowerShell snippet to generate the new fab (50 chains × 30 beads in world 256, laid out at chain spacing 5.0 across the bottom row, same layout rule as `DisconnectedChains` but emitted as TOML):

Save the following to `scripts/gen_wire_fab.ps1` (create the `scripts/` dir if needed):

```powershell
# Generates fabs/wire-50x30.toml using the same layout rule as
# DisconnectedChains in src/bench/chains.rs. Pure data generator —
# no chemistry logic; lays beads at (chain_spacing_x * col + 2.5, row_gap_y/2 + bead_spacing * b).

$chain_count = 50
$chain_len = 30
$world_size = 256.0
$bead_spacing = 0.667
$chain_spacing_x = 5.0
$chain_row_gap_y = 2.0

$chains_per_row = [Math]::Floor($world_size / $chain_spacing_x)
$row_height = ($chain_len - 1) * $bead_spacing + $chain_row_gap_y

$out = @()
$out += "[meta]"
$out += "name = `"50 parallel 30-bead wire chains, deployed bump 2026-05-21`""
$out += "chemistry = `"wire`""
$out += "seed = 42"
$out += "world_size = $world_size"
$out += ""

for ($c = 0; $c -lt $chain_count; $c++) {
    $row = [Math]::Floor($c / $chains_per_row)
    $col = $c % $chains_per_row
    $x = $chain_spacing_x * $col + ($chain_spacing_x / 2.0)
    $y0 = $row_height * $row + ($chain_row_gap_y / 2.0)
    for ($b = 0; $b -lt $chain_len; $b++) {
        $state = if ($b -eq 0) { "on" } else { "off" }
        $y = $y0 + $b * $bead_spacing
        $out += "[[bead]]"
        $out += "state = `"$state`""
        $out += ("pos = [{0:F4}, {1:F4}]" -f $x, $y)
    }
}

Set-Content -Path "fabs/wire-50x30.toml" -Value ($out -join "`n") -Encoding UTF8
Write-Host "Wrote fabs/wire-50x30.toml"
```

Run it:
```powershell
.\scripts\gen_wire_fab.ps1
```

Verify the new file has 50 × 30 = 1500 bead entries:
```powershell
(Select-String -Path fabs\wire-50x30.toml -Pattern '\[\[bead\]\]').Count
```
Expected: `1500`.

- [ ] **Step 13.2: Update `src/app.rs` to point at the new fab**

In `src/app.rs`, change line 36:
```rust
const FAB_TOML: &str = include_str!("../fabs/wire-10x30.toml");
```
to:
```rust
const FAB_TOML: &str = include_str!("../fabs/wire-50x30.toml");
```

And line 98:
```rust
let fab = load_fab("fabs/wire-10x30.toml").expect("load fab");
```
to:
```rust
let fab = load_fab("fabs/wire-50x30.toml").expect("load fab");
```

- [ ] **Step 13.3: Delete the old fab**

Run:
```powershell
git rm fabs\wire-10x30.toml
```

- [ ] **Step 13.4: Build native to confirm the fab loads**

Run:
```powershell
cargo build --release
```
Expected: PASS.

- [ ] **Step 13.5: Smoke-run the native binary briefly**

Run for ~5 seconds and check the window appears with chains rendering. Expect significant FPS drop versus the prior 300-bead deploy — this is the "deployed bump despite the crawl" the spec calls out.

```powershell
$proc = Start-Process -PassThru .\target\release\jigglefab-bin.exe
Start-Sleep -Seconds 5
Stop-Process -Id $proc.Id
```

Expected: window opens, fills with smaller-looking chains, no panic, process exits cleanly when killed.

- [ ] **Step 13.6: Build WASM to confirm the embedded TOML compiles**

Run:
```powershell
cargo build --release --target wasm32-unknown-unknown
```
Expected: PASS (the `include_str!` for the new fab path resolves correctly).

(If `wasm32-unknown-unknown` target isn't installed locally, this can be deferred to CI — GitHub Actions builds it for the deploy.)

- [ ] **Step 13.7: Commit**

```powershell
git add fabs\wire-50x30.toml src\app.rs scripts\gen_wire_fab.ps1
git commit -m "Bump deployed fab: 10×30 → 50×30 in 256-wide world

Generated by scripts/gen_wire_fab.ps1. World grows 50→256 (powers-of-2
cells_per_axis for the grid). Chain count grows 10→50 at constant 5.0
horizontal spacing, single row.

Expected to crawl on the current sequential scheduler — this is the
motivation for Phase 2."
```

---

## Self-Review (writer's pass)

**Spec coverage check:**

Walking through the spec section by section against the plan:

- "Binary `src/bin/bench.rs`, invoked as `cargo run --release --bin bench`" → Task 2 (stub) + Task 10 (full).
- All six default CLI args (`--scenarios`, `--substeps`, `--frames`, `--warmup`, `--max-wall-seconds`, `--csv`, `--verify-determinism`) → Task 10.
- `Scenario` trait + `Invariants` struct → Task 3.
- `DisconnectedChains` with grid + serpentine layout → Task 4.
- Seven scenarios (six default + one opt-in) → Task 10 (`default_scenarios()` + `all_scenarios()`).
- Headline metrics (frame_time, substep_time, effective_fps) → Tasks 5–6.
- Diagnostic metrics (contacts/ss, candidate_pairs/ss mean, iter_cap_saturation_rate) → Tasks 5–6.
- Derived `substeps_per_16ms_budget` → Task 6.
- Bond preservation invariant (`bonds_preserved`, `bonds_lost`, `bonds_added`) → Tasks 3 (helper) + 6 (check).
- `StepMetrics` instrumentation on `Sim` → Task 1.
- Markdown stdout + CSV file output → Task 9.
- Truncation footnote → Task 9 (formatter) + Task 7 (regression test).
- `--max-wall-seconds` semantics → Task 6 (logic) + Task 7 (test).
- `--verify-determinism` → Task 8.
- Baseline result capture → Task 12.
- Deployed fab bump (separate deliverable per spec) → Task 13.

All sections covered.

**Type consistency check:**

- `StepMetrics`: `contacts_resolved: u32`, `candidate_pairs: u32`, `iter_cap_hit: bool` — consistent across Task 1 (def), Task 6 (read).
- `ScenarioResult`: defined in Task 5, populated in Task 6, read in Task 9 — fields match.
- `BenchArgs`: defined in Task 5, populated by CLI in Task 10, consumed in Task 6 — match.
- `Scenario` trait: `name()` returning `String`, `build()` returning `(Sim, Invariants)` — consistent across Tasks 3, 4, 10.
- `geometric_bonds(positions: &[Vec2], world_size: f32) -> HashSet<(u32, u32)>` — consistent across Tasks 3, 4, 6.

**Placeholder scan:**

- Task 12 baseline markdown has `[fill in: …]` placeholders for *human-judgment* fields (machine spec, observations). These are appropriate — not implementation placeholders; the engineer fills them in from the actual run. Acceptable.

No other placeholders. Plan is complete.
