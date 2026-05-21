# JiggleFab P1 — Hello Jiggling Chain Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the minimum runnable jigglefab engine — a 30-bead grey chain jiggling on screen with deterministic per-pair CCD physics on a 2D torus — as a native Rust binary.

**Architecture:** Single Rust binary. Physics on the CPU (deterministic, 30 beads is trivial). Rendering on the GPU via wgpu (filled-circle instances, one per bead). Loop: load fab → per frame { advance + collide repeatedly until frame time exhausted → upload bead positions → render }. World is a hardcoded-size 2D torus; positions wrap. Chemistry is the trivial 1-state always-reflect rule.

**Tech Stack:** Rust (stable), wgpu, winit 0.30, glam (math), serde + toml (file parsing), bytemuck (Pod derives), pollster (block_on for async wgpu init).

**Reference design doc:** [`docs/superpowers/specs/2026-05-20-jigglefab-engine-design.md`](../specs/2026-05-20-jigglefab-engine-design.md). Read it first if you have no context.

**Out of scope for P1** (do not implement, do not add): GPU compute shaders for physics (P2), multiple chemistries beyond grey (P2), Voronoi/SDF rendering (post-P1), editor (P4), WASM build (P4), invariant assertions (P2), anomaly menagerie (P2), format linting (P2), performance work beyond ≥30 fps for 30 beads. The plan never touches `haskell/` — that directory is frozen.

---

## File structure

What this plan creates:

```
Cargo.toml
src/
  main.rs          – binary entry
  lib.rs           – module declarations + run()
  fab.rs           – Fab and Bead structs + TOML loading
  chemistry.rs     – Chemistry and Rule structs + TOML loading + lookup
  rng.rs           – counter-based PRNG
  ccd.rs           – per-pair CCD quadratic
  collide.rs       – elastic-collision math
  grid.rs          – uniform-grid spatial hash (with torus wrap)
  sim.rs           – Sim struct + frame stepping
  render.rs        – wgpu context + bead instance rendering pipeline
  app.rs           – winit event loop + glue
shaders/
  beads.wgsl       – vertex+fragment shader for filled circles
fabs/
  grey-30.toml     – 30-bead vertical chain
chemistries/
  grey.toml        – 1 state, 2 always-reflect rules
tests/
  determinism.rs   – integration test for bit-exact replay
```

Each module has one clear responsibility. `fab.rs` and `chemistry.rs` are pure data; `rng.rs`, `ccd.rs`, `collide.rs` are pure functions; `grid.rs` and `sim.rs` hold state; `render.rs` and `app.rs` handle wgpu + winit.

---

## Task 1: Cargo init and dependencies

**Files:**
- Create: `Cargo.toml`
- Create: `src/main.rs`
- Create: `src/lib.rs`

- [ ] **Step 1: Initialize cargo binary crate**

Run: `cargo init --name jigglefab`

Expected: `Cargo.toml` and `src/main.rs` are created. Output: `Created binary (application) package`.

- [ ] **Step 2: Set dependencies in `Cargo.toml`**

Replace the contents of `Cargo.toml` with:

```toml
[package]
name = "jigglefab"
version = "0.1.0"
edition = "2021"

[dependencies]
wgpu = "22"
winit = "0.30"
glam = { version = "0.29", features = ["bytemuck"] }
bytemuck = { version = "1.16", features = ["derive"] }
serde = { version = "1", features = ["derive"] }
toml = "0.8"
pollster = "0.3"
anyhow = "1"
log = "0.4"
env_logger = "0.11"
```

(Versions are minimum-compatible; later patch versions are fine.)

- [ ] **Step 3: Create `src/lib.rs`**

```rust
pub mod fab;
pub mod chemistry;
pub mod rng;
pub mod ccd;
pub mod collide;
pub mod grid;
pub mod sim;
pub mod render;
pub mod app;

pub fn run() -> anyhow::Result<()> {
    env_logger::init();
    println!("jigglefab P1");
    Ok(())
}
```

- [ ] **Step 4: Create `src/main.rs`**

Replace the contents of `src/main.rs` with:

```rust
fn main() -> anyhow::Result<()> {
    jigglefab::run()
}
```

- [ ] **Step 5: Create empty module files so `lib.rs` compiles**

Create each of these as empty files (we'll fill them in later tasks):

```bash
# These commands create empty placeholders.
# On PowerShell, use: New-Item src\fab.rs, src\chemistry.rs, ...
touch src/fab.rs src/chemistry.rs src/rng.rs src/ccd.rs src/collide.rs src/grid.rs src/sim.rs src/render.rs src/app.rs
```

(PowerShell equivalent: `"" | Out-File src/fab.rs, src/chemistry.rs, src/rng.rs, src/ccd.rs, src/collide.rs, src/grid.rs, src/sim.rs, src/render.rs, src/app.rs`)

- [ ] **Step 6: Verify it builds and runs**

Run: `cargo run`

Expected: builds (will take a while first time, downloading deps), prints `jigglefab P1` to stdout.

- [ ] **Step 7: Commit**

```bash
git add Cargo.toml Cargo.lock src/
git commit -m "Task 1: scaffold cargo binary with deps"
```

---

## Task 2: Fab file types and TOML loading

**Files:**
- Modify: `src/fab.rs`
- Create: `fabs/grey-30.toml`

- [ ] **Step 1: Write the example fab file**

Create `fabs/grey-30.toml`. All 30 beads, spacing 0.667 in y (> radius/2 = 0.5 and < radius = 1.0, so adjacent pairs are bonded but not coincident), centered at x=15 inside a 30×30 torus, starting at y=5:

```toml
[meta]
name = "30-bead vertical chain"
chemistry = "grey"
seed = 42

[[bead]]
state = "grey"
pos = [15.0, 5.000]
[[bead]]
state = "grey"
pos = [15.0, 5.667]
[[bead]]
state = "grey"
pos = [15.0, 6.334]
[[bead]]
state = "grey"
pos = [15.0, 7.001]
[[bead]]
state = "grey"
pos = [15.0, 7.668]
[[bead]]
state = "grey"
pos = [15.0, 8.335]
[[bead]]
state = "grey"
pos = [15.0, 9.002]
[[bead]]
state = "grey"
pos = [15.0, 9.669]
[[bead]]
state = "grey"
pos = [15.0, 10.336]
[[bead]]
state = "grey"
pos = [15.0, 11.003]
[[bead]]
state = "grey"
pos = [15.0, 11.670]
[[bead]]
state = "grey"
pos = [15.0, 12.337]
[[bead]]
state = "grey"
pos = [15.0, 13.004]
[[bead]]
state = "grey"
pos = [15.0, 13.671]
[[bead]]
state = "grey"
pos = [15.0, 14.338]
[[bead]]
state = "grey"
pos = [15.0, 15.005]
[[bead]]
state = "grey"
pos = [15.0, 15.672]
[[bead]]
state = "grey"
pos = [15.0, 16.339]
[[bead]]
state = "grey"
pos = [15.0, 17.006]
[[bead]]
state = "grey"
pos = [15.0, 17.673]
[[bead]]
state = "grey"
pos = [15.0, 18.340]
[[bead]]
state = "grey"
pos = [15.0, 19.007]
[[bead]]
state = "grey"
pos = [15.0, 19.674]
[[bead]]
state = "grey"
pos = [15.0, 20.341]
[[bead]]
state = "grey"
pos = [15.0, 21.008]
[[bead]]
state = "grey"
pos = [15.0, 21.675]
[[bead]]
state = "grey"
pos = [15.0, 22.342]
[[bead]]
state = "grey"
pos = [15.0, 23.009]
[[bead]]
state = "grey"
pos = [15.0, 23.676]
[[bead]]
state = "grey"
pos = [15.0, 24.343]
```

- [ ] **Step 2: Write the failing test**

In `src/fab.rs`:

```rust
use serde::Deserialize;
use glam::Vec2;

#[derive(Debug, Deserialize)]
pub struct Fab {
    pub meta: Meta,
    #[serde(rename = "bead")]
    pub beads: Vec<BeadSpec>,
}

#[derive(Debug, Deserialize)]
pub struct Meta {
    pub name: String,
    pub chemistry: String,
    pub seed: u64,
}

#[derive(Debug, Deserialize)]
pub struct BeadSpec {
    pub state: String,
    pub pos: [f32; 2],
    #[serde(default)]
    pub vel: Option<[f32; 2]>,
}

impl BeadSpec {
    pub fn pos(&self) -> Vec2 {
        Vec2::from(self.pos)
    }
}

pub fn load_fab(path: &str) -> anyhow::Result<Fab> {
    let text = std::fs::read_to_string(path)?;
    let fab: Fab = toml::from_str(&text)?;
    Ok(fab)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn loads_grey_30() {
        let fab = load_fab("fabs/grey-30.toml").unwrap();
        assert_eq!(fab.meta.name, "30-bead vertical chain");
        assert_eq!(fab.meta.chemistry, "grey");
        assert_eq!(fab.meta.seed, 42);
        assert_eq!(fab.beads.len(), 30);
        assert_eq!(fab.beads[0].pos(), Vec2::new(15.0, 5.0));
        assert!((fab.beads[1].pos().y - 5.667).abs() < 1e-5);
        assert!((fab.beads[29].pos().y - 24.343).abs() < 1e-4);
        for b in &fab.beads {
            assert_eq!(b.pos().x, 15.0);
        }
    }
}
```

- [ ] **Step 3: Run the test, see it pass**

Run: `cargo test --lib fab::tests::loads_grey_30`

Expected: PASS. If `fabs/grey-30.toml` is missing beads, the `beads.len() == 30` assertion catches it.

- [ ] **Step 4: Commit**

```bash
git add src/fab.rs fabs/grey-30.toml
git commit -m "Task 2: fab file types and TOML loading"
```

---

## Task 3: Chemistry file types and lookup table

**Files:**
- Modify: `src/chemistry.rs`
- Create: `chemistries/grey.toml`

- [ ] **Step 1: Write the chemistry file**

Create `chemistries/grey.toml`:

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

- [ ] **Step 2: Write Chemistry types and the failing test**

In `src/chemistry.rs`:

```rust
use serde::Deserialize;
use anyhow::{Result, bail};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Action {
    Reflect,
    Pass,
}

#[derive(Debug, Deserialize)]
struct ChemistryFile {
    states: Vec<String>,
    #[serde(rename = "rule")]
    rules: Vec<RuleSpec>,
}

#[derive(Debug, Deserialize)]
struct RuleSpec {
    states: [String; 2],
    inside: bool,
    action: String,
}

#[derive(Debug)]
pub struct Chemistry {
    pub states: Vec<String>,
    // Dense lookup: [stateA][stateB][inside as usize] -> Action
    table: Vec<Vec<[Action; 2]>>,
}

impl Chemistry {
    pub fn state_index(&self, name: &str) -> Option<usize> {
        self.states.iter().position(|s| s == name)
    }

    pub fn lookup(&self, a: usize, b: usize, inside: bool) -> Action {
        self.table[a][b][inside as usize]
    }
}

pub fn load_chemistry(path: &str) -> Result<Chemistry> {
    let text = std::fs::read_to_string(path)?;
    let file: ChemistryFile = toml::from_str(&text)?;
    let n = file.states.len();
    // Default everything to Reflect, then overwrite per rule.
    let mut table: Vec<Vec<[Action; 2]>> = (0..n)
        .map(|_| (0..n).map(|_| [Action::Reflect; 2]).collect())
        .collect();
    for rule in &file.rules {
        let a = file.states.iter().position(|s| s == &rule.states[0])
            .ok_or_else(|| anyhow::anyhow!("rule references unknown state {:?}", rule.states[0]))?;
        let b = file.states.iter().position(|s| s == &rule.states[1])
            .ok_or_else(|| anyhow::anyhow!("rule references unknown state {:?}", rule.states[1]))?;
        let action = match rule.action.as_str() {
            "reflect" => Action::Reflect,
            "pass" => Action::Pass,
            other => bail!("unknown action {:?}", other),
        };
        let inside_idx = rule.inside as usize;
        // Enforce symmetry: rule applies to (a,b) and (b,a).
        table[a][b][inside_idx] = action;
        table[b][a][inside_idx] = action;
    }
    Ok(Chemistry { states: file.states, table })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn loads_grey_chemistry() {
        let chem = load_chemistry("chemistries/grey.toml").unwrap();
        assert_eq!(chem.states, vec!["grey"]);
        let g = chem.state_index("grey").unwrap();
        assert_eq!(chem.lookup(g, g, false), Action::Reflect);
        assert_eq!(chem.lookup(g, g, true), Action::Reflect);
    }
}
```

- [ ] **Step 3: Run the test**

Run: `cargo test --lib chemistry::tests::loads_grey_chemistry`

Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add src/chemistry.rs chemistries/grey.toml
git commit -m "Task 3: chemistry types, TOML loading, dense lookup table"
```

---

## Task 4: Counter-based PRNG

**Files:**
- Modify: `src/rng.rs`

The simulation must be deterministic from a `(seed, bead_id, tick)` triple. We use SplitMix64 — a tiny, fast, well-distributed PRNG.

- [ ] **Step 1: Write the PRNG and failing tests**

In `src/rng.rs`:

```rust
/// Counter-based PRNG. Returns a uniform u64 from a (seed, bead_id, tick) triple.
/// Implementation: hash the three inputs together with SplitMix64.
pub fn prng_u64(seed: u64, bead_id: u32, tick: u32) -> u64 {
    let mut x = seed
        ^ ((bead_id as u64).wrapping_mul(0x9E3779B97F4A7C15))
        ^ ((tick as u64).wrapping_mul(0xBF58476D1CE4E5B9));
    x = splitmix64(x);
    x
}

/// Returns a uniform f32 in [0.0, 1.0).
pub fn prng_f32(seed: u64, bead_id: u32, tick: u32) -> f32 {
    // Top 24 bits of the u64, scaled to [0, 1).
    let u = prng_u64(seed, bead_id, tick);
    ((u >> 40) as f32) / ((1u64 << 24) as f32)
}

fn splitmix64(mut x: u64) -> u64 {
    x = x.wrapping_add(0x9E3779B97F4A7C15);
    x = (x ^ (x >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
    x = (x ^ (x >> 27)).wrapping_mul(0x94D049BB133111EB);
    x ^ (x >> 31)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn deterministic_same_input() {
        assert_eq!(prng_u64(42, 0, 0), prng_u64(42, 0, 0));
        assert_eq!(prng_f32(42, 7, 13), prng_f32(42, 7, 13));
    }

    #[test]
    fn differs_per_id() {
        assert_ne!(prng_u64(42, 0, 0), prng_u64(42, 1, 0));
        assert_ne!(prng_u64(42, 0, 0), prng_u64(42, 0, 1));
        assert_ne!(prng_u64(42, 0, 0), prng_u64(43, 0, 0));
    }

    #[test]
    fn f32_in_unit_interval() {
        for id in 0..1000 {
            let v = prng_f32(42, id, 0);
            assert!(v >= 0.0 && v < 1.0, "value out of range: {}", v);
        }
    }
}
```

- [ ] **Step 2: Run the tests**

Run: `cargo test --lib rng::tests`

Expected: 3 tests pass.

- [ ] **Step 3: Commit**

```bash
git add src/rng.rs
git commit -m "Task 4: counter-based PRNG (SplitMix64)"
```

---

## Task 5: Per-pair CCD quadratic

**Files:**
- Modify: `src/ccd.rs`

Solve the quadratic for the within-frame contact time between two beads in linear motion. Returns the contact time and whether it's an "inside" collision (they're currently bonded and about to un-bond) or "outside" (currently unbonded and about to bond).

- [ ] **Step 1: Write CCD math and failing tests**

In `src/ccd.rs`:

```rust
use glam::Vec2;

pub const RADIUS: f32 = 1.0;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Contact {
    pub t: f32,        // time of contact in [0, dt]
    pub inside: bool,  // true if pair is currently bonded (|d| < r)
}

/// Returns the next contact in [0, dt] between two beads, or None if none.
/// Pair positions/velocities can be passed in any order; the result is symmetric.
pub fn next_contact(p1: Vec2, v1: Vec2, p2: Vec2, v2: Vec2, dt: f32) -> Option<Contact> {
    let d = p2 - p1;
    let dv = v2 - v1;
    let r = RADIUS;

    // Solve |d + dv * t|^2 = r^2
    //   => (dv·dv) t^2 + 2 (d·dv) t + (d·d - r^2) = 0
    let a = dv.dot(dv);
    let b = 2.0 * d.dot(dv);
    let c = d.dot(d) - r * r;

    // If beads have zero relative velocity, no contact will be formed.
    if a < 1e-12 {
        return None;
    }

    let disc = b * b - 4.0 * a * c;
    if disc < 0.0 {
        return None;
    }
    let sqrt_disc = disc.sqrt();
    let t_early = (-b - sqrt_disc) / (2.0 * a);
    let t_late = (-b + sqrt_disc) / (2.0 * a);

    let currently_inside = c < 0.0; // |d|^2 < r^2

    // Which root is "the next boundary crossing from where we are"?
    // If currently outside (c > 0): we want t_early (the entry).
    // If currently inside  (c < 0): t_early is negative-or-already-past;
    //   t_late is the exit. We want t_late.
    let t = if currently_inside { t_late } else { t_early };

    if t < 0.0 || t > dt {
        return None;
    }

    Some(Contact { t, inside: currently_inside })
}

#[cfg(test)]
mod tests {
    use super::*;
    use glam::Vec2;

    #[test]
    fn head_on_outside_collision() {
        // Two beads on x-axis at x = -2 and +2, moving toward each other at speed 1.
        // Relative position is (4, 0), relative velocity is (-2, 0).
        // They touch when |d + dv*t| = 1, i.e. |4 - 2t| = 1 => t = 1.5 (first).
        let p1 = Vec2::new(-2.0, 0.0);
        let p2 = Vec2::new( 2.0, 0.0);
        let v1 = Vec2::new( 1.0, 0.0);
        let v2 = Vec2::new(-1.0, 0.0);
        let c = next_contact(p1, v1, p2, v2, 2.0).unwrap();
        assert!((c.t - 1.5).abs() < 1e-5);
        assert!(!c.inside);
    }

    #[test]
    fn parallel_motion_no_contact() {
        let p1 = Vec2::new(0.0, 0.0);
        let p2 = Vec2::new(5.0, 0.0);
        let v1 = Vec2::new(1.0, 0.0);
        let v2 = Vec2::new(1.0, 0.0);
        assert!(next_contact(p1, v1, p2, v2, 10.0).is_none());
    }

    #[test]
    fn diverging_no_contact() {
        // Two beads separating; no contact possible.
        let p1 = Vec2::new(0.0, 0.0);
        let p2 = Vec2::new(2.0, 0.0);
        let v1 = Vec2::new(-1.0, 0.0);
        let v2 = Vec2::new( 1.0, 0.0);
        assert!(next_contact(p1, v1, p2, v2, 10.0).is_none());
    }

    #[test]
    fn inside_pair_exits() {
        // Two beads bonded (|d| = 0.5), moving apart at relative speed 1.
        // They reach the boundary |d| = 1 at t = 0.5.
        let p1 = Vec2::new(-0.25, 0.0);
        let p2 = Vec2::new( 0.25, 0.0);
        let v1 = Vec2::new(-0.5, 0.0);
        let v2 = Vec2::new( 0.5, 0.0);
        let c = next_contact(p1, v1, p2, v2, 1.0).unwrap();
        assert!((c.t - 0.5).abs() < 1e-5);
        assert!(c.inside);
    }

    #[test]
    fn contact_outside_dt_window() {
        // Contact would be at t = 1.5 but dt = 1.0.
        let p1 = Vec2::new(-2.0, 0.0);
        let p2 = Vec2::new( 2.0, 0.0);
        let v1 = Vec2::new( 1.0, 0.0);
        let v2 = Vec2::new(-1.0, 0.0);
        assert!(next_contact(p1, v1, p2, v2, 1.0).is_none());
    }
}
```

- [ ] **Step 2: Run the tests**

Run: `cargo test --lib ccd::tests`

Expected: 5 tests pass.

- [ ] **Step 3: Commit**

```bash
git add src/ccd.rs
git commit -m "Task 5: per-pair CCD quadratic with inside/outside detection"
```

---

## Task 6: Elastic collision resolution

**Files:**
- Modify: `src/collide.rs`

Equal-mass equal-radius elastic collision: at contact, the velocity components along the normal connecting the centers swap; perpendicular components are unchanged. Same math for inside and outside collisions.

- [ ] **Step 1: Write resolve math and failing tests**

In `src/collide.rs`:

```rust
use glam::Vec2;

/// Reflect two beads' velocities given their positions at the moment of contact.
/// Both inside and outside collisions use the same formula.
pub fn reflect(p1: Vec2, v1: Vec2, p2: Vec2, v2: Vec2) -> (Vec2, Vec2) {
    let n = (p2 - p1).normalize_or_zero();
    if n.length_squared() < 1e-12 {
        // Coincident centers (degenerate). Leave velocities unchanged.
        return (v1, v2);
    }
    let v1n = v1.dot(n);
    let v2n = v2.dot(n);
    // Equal-mass elastic: swap normal components.
    let delta = (v2n - v1n) * n;
    (v1 + delta, v2 - delta)
}

#[cfg(test)]
mod tests {
    use super::*;
    use glam::Vec2;

    #[test]
    fn head_on_equal_speed_swaps() {
        // Both beads moving toward each other at unit speed along x; should bounce back.
        let p1 = Vec2::new(-0.5, 0.0);
        let p2 = Vec2::new( 0.5, 0.0);
        let v1 = Vec2::new( 1.0, 0.0);
        let v2 = Vec2::new(-1.0, 0.0);
        let (v1p, v2p) = reflect(p1, v1, p2, v2);
        assert!((v1p - Vec2::new(-1.0, 0.0)).length() < 1e-5);
        assert!((v2p - Vec2::new( 1.0, 0.0)).length() < 1e-5);
    }

    #[test]
    fn perpendicular_unchanged() {
        // One bead has only normal-direction velocity, other only tangential.
        // After collision: normal swaps, tangential unchanged.
        let p1 = Vec2::new(-0.5, 0.0);
        let p2 = Vec2::new( 0.5, 0.0);
        let v1 = Vec2::new( 1.0, 0.0); // pure normal toward p2
        let v2 = Vec2::new( 0.0, 2.0); // pure tangential
        let (v1p, v2p) = reflect(p1, v1, p2, v2);
        assert!((v1p - Vec2::new(0.0, 0.0)).length() < 1e-5);  // gave up its normal
        assert!((v2p - Vec2::new(1.0, 2.0)).length() < 1e-5);  // gained the normal
    }

    #[test]
    fn momentum_and_energy_conserved() {
        let p1 = Vec2::new(0.0, 0.0);
        let p2 = Vec2::new(0.7, 0.5).normalize();
        let v1 = Vec2::new( 0.3, 0.9);
        let v2 = Vec2::new(-0.6, 0.2);
        let (v1p, v2p) = reflect(p1, v1, p2, v2);
        let p_before = v1 + v2;
        let p_after = v1p + v2p;
        assert!((p_before - p_after).length() < 1e-5);
        let e_before = v1.length_squared() + v2.length_squared();
        let e_after = v1p.length_squared() + v2p.length_squared();
        assert!((e_before - e_after).abs() < 1e-5);
    }
}
```

- [ ] **Step 2: Run the tests**

Run: `cargo test --lib collide::tests`

Expected: 3 tests pass.

- [ ] **Step 3: Commit**

```bash
git add src/collide.rs
git commit -m "Task 6: elastic collision resolution (reflect)"
```

---

## Task 7: Uniform-grid spatial hash with torus wrap

**Files:**
- Modify: `src/grid.rs`

For unit-radius beads, cell size = 1.0 means any bondable pair (distance < 1) lives in the same or adjacent cells. The grid wraps in both axes.

- [ ] **Step 1: Write the grid and failing tests**

In `src/grid.rs`:

```rust
use glam::Vec2;

pub const CELL_SIZE: f32 = 1.0;

pub struct Grid {
    world_size: f32,
    cells_per_axis: usize,
    // cells[cy * n + cx] = Vec<bead_id>
    cells: Vec<Vec<u32>>,
}

impl Grid {
    pub fn new(world_size: f32) -> Self {
        let cells_per_axis = (world_size / CELL_SIZE).ceil() as usize;
        let cells = (0..cells_per_axis * cells_per_axis).map(|_| Vec::new()).collect();
        Self { world_size, cells_per_axis, cells }
    }

    pub fn clear(&mut self) {
        for c in &mut self.cells {
            c.clear();
        }
    }

    pub fn insert(&mut self, bead_id: u32, pos: Vec2) {
        let (cx, cy) = self.cell_of(pos);
        let idx = cy * self.cells_per_axis + cx;
        self.cells[idx].push(bead_id);
    }

    fn cell_of(&self, pos: Vec2) -> (usize, usize) {
        let wrapped = self.wrap_pos(pos);
        let cx = (wrapped.x / CELL_SIZE) as usize % self.cells_per_axis;
        let cy = (wrapped.y / CELL_SIZE) as usize % self.cells_per_axis;
        (cx, cy)
    }

    /// Wraps a position into [0, world_size) in both axes.
    pub fn wrap_pos(&self, pos: Vec2) -> Vec2 {
        let mut x = pos.x.rem_euclid(self.world_size);
        let mut y = pos.y.rem_euclid(self.world_size);
        if x == self.world_size { x = 0.0; }
        if y == self.world_size { y = 0.0; }
        Vec2::new(x, y)
    }

    pub fn world_size(&self) -> f32 { self.world_size }

    /// Yields each unordered candidate pair (a, b) with a < b such that the
    /// two beads sit in the same or adjacent cells (with torus wrap).
    pub fn candidate_pairs(&self) -> Vec<(u32, u32)> {
        let n = self.cells_per_axis;
        let mut pairs = Vec::new();
        for cy in 0..n {
            for cx in 0..n {
                let here = &self.cells[cy * n + cx];
                // Pairs within this cell
                for i in 0..here.len() {
                    for j in (i + 1)..here.len() {
                        let (a, b) = (here[i], here[j]);
                        pairs.push((a.min(b), a.max(b)));
                    }
                }
                // Pairs with the 4 neighbours we haven't visited (avoid double-counting):
                // (+1, 0), (-1, +1), (0, +1), (+1, +1)
                let neighbours = [(1, 0), (-1, 1), (0, 1), (1, 1)];
                for (dx, dy) in neighbours {
                    let nx = ((cx as isize + dx).rem_euclid(n as isize)) as usize;
                    let ny = ((cy as isize + dy).rem_euclid(n as isize)) as usize;
                    let there = &self.cells[ny * n + nx];
                    for &a in here {
                        for &b in there {
                            if a != b {
                                pairs.push((a.min(b), a.max(b)));
                            }
                        }
                    }
                }
            }
        }
        pairs
    }

    /// Shortest displacement from `from` to `to` under torus topology.
    /// Returns the vector with components in [-world_size/2, world_size/2].
    pub fn min_image(&self, from: Vec2, to: Vec2) -> Vec2 {
        let half = self.world_size * 0.5;
        let mut d = to - from;
        if d.x >  half { d.x -= self.world_size; }
        if d.x < -half { d.x += self.world_size; }
        if d.y >  half { d.y -= self.world_size; }
        if d.y < -half { d.y += self.world_size; }
        d
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insert_and_find_pair_same_cell() {
        let mut g = Grid::new(10.0);
        g.insert(0, Vec2::new(2.1, 2.1));
        g.insert(1, Vec2::new(2.5, 2.5));
        let pairs = g.candidate_pairs();
        assert_eq!(pairs, vec![(0, 1)]);
    }

    #[test]
    fn insert_and_find_pair_adjacent_cell() {
        let mut g = Grid::new(10.0);
        g.insert(0, Vec2::new(2.1, 2.1)); // cell (2,2)
        g.insert(1, Vec2::new(3.5, 2.5)); // cell (3,2)
        let pairs = g.candidate_pairs();
        assert_eq!(pairs, vec![(0, 1)]);
    }

    #[test]
    fn far_beads_not_paired() {
        let mut g = Grid::new(10.0);
        g.insert(0, Vec2::new(1.0, 1.0));
        g.insert(1, Vec2::new(5.0, 5.0));
        let pairs = g.candidate_pairs();
        assert!(pairs.is_empty());
    }

    #[test]
    fn wraps_across_torus() {
        let mut g = Grid::new(10.0);
        g.insert(0, Vec2::new(0.1, 5.0)); // cell (0,5)
        g.insert(1, Vec2::new(9.9, 5.0)); // cell (9,5) — adjacent under wrap
        let pairs = g.candidate_pairs();
        assert_eq!(pairs, vec![(0, 1)]);
    }

    #[test]
    fn min_image_picks_short_side() {
        let g = Grid::new(10.0);
        // from (9.5, 5) to (0.5, 5): naive diff is (-9, 0); short way is (+1, 0).
        let d = g.min_image(Vec2::new(9.5, 5.0), Vec2::new(0.5, 5.0));
        assert!((d - Vec2::new(1.0, 0.0)).length() < 1e-5);
    }

    #[test]
    fn wrap_pos_into_unit_interval() {
        let g = Grid::new(10.0);
        assert!((g.wrap_pos(Vec2::new(10.5, -0.5)) - Vec2::new(0.5, 9.5)).length() < 1e-5);
    }
}
```

- [ ] **Step 2: Run the tests**

Run: `cargo test --lib grid::tests`

Expected: 6 tests pass.

- [ ] **Step 3: Commit**

```bash
git add src/grid.rs
git commit -m "Task 7: uniform-grid spatial hash with torus wrap and min-image"
```

---

## Task 8: Sim struct and frame stepping

**Files:**
- Modify: `src/sim.rs`

This is the heart of P1. Holds bead positions, velocities, states. Steps a frame via the advance-and-collide loop: bin into grid, find candidate pairs, compute earliest contact across all pairs, advance to it, resolve, repeat until frame time exhausted.

- [ ] **Step 1: Write Sim + step logic and failing tests**

In `src/sim.rs`:

```rust
use glam::Vec2;
use std::f32::consts::TAU;

use crate::ccd::{next_contact, RADIUS};
use crate::chemistry::{Action, Chemistry};
use crate::collide::reflect;
use crate::fab::Fab;
use crate::grid::{Grid, CELL_SIZE};
use crate::rng::prng_f32;

pub const WORLD_SIZE: f32 = 30.0;
pub const SPEED: f32 = 1.0;

pub struct Sim {
    pub positions: Vec<Vec2>,
    pub velocities: Vec<Vec2>,
    pub states: Vec<u32>,
    chemistry: Chemistry,
    grid: Grid,
    tick: u32,
}

impl Sim {
    pub fn from_fab(fab: &Fab, chemistry: Chemistry) -> Self {
        let n = fab.beads.len();
        let mut positions = Vec::with_capacity(n);
        let mut velocities = Vec::with_capacity(n);
        let mut states = Vec::with_capacity(n);
        for (i, bs) in fab.beads.iter().enumerate() {
            positions.push(bs.pos());
            // If vel not specified, derive from seed.
            let v = if let Some([vx, vy]) = bs.vel {
                Vec2::new(vx, vy)
            } else {
                let angle = prng_f32(fab.meta.seed, i as u32, 0) * TAU;
                Vec2::new(angle.cos(), angle.sin()) * SPEED
            };
            velocities.push(v);
            let state_idx = chemistry.state_index(&bs.state)
                .expect("bead state not in chemistry") as u32;
            states.push(state_idx);
        }
        let grid = Grid::new(WORLD_SIZE);
        Self { positions, velocities, states, chemistry, grid, tick: 0 }
    }

    pub fn step(&mut self, frame_dt: f32) {
        let mut dt_remaining = frame_dt;
        // Cap iterations to avoid pathological infinite loops (paranoia, shouldn't fire).
        let mut iter_cap = self.positions.len() * 64;
        while dt_remaining > 0.0 && iter_cap > 0 {
            iter_cap -= 1;
            // 1) Bin into grid.
            self.grid.clear();
            for (i, &p) in self.positions.iter().enumerate() {
                self.grid.insert(i as u32, p);
            }

            // 2) Find earliest contact across candidate pairs.
            let mut earliest: Option<(f32, u32, u32, bool)> = None;
            for (a, b) in self.grid.candidate_pairs() {
                let pa = self.positions[a as usize];
                let pb_raw = self.positions[b as usize];
                // Use min-image so pairs across the wrap see the short distance.
                let pb = pa + self.grid.min_image(pa, pb_raw);
                let va = self.velocities[a as usize];
                let vb = self.velocities[b as usize];
                if let Some(c) = next_contact(pa, va, pb, vb, dt_remaining) {
                    // Deterministic tiebreak: earlier t wins; for ties, lower (a,b) wins.
                    let key = (c.t, a, b);
                    let new_best = match earliest {
                        None => true,
                        Some((t0, a0, b0, _)) => key < (t0, a0, b0),
                    };
                    if new_best {
                        earliest = Some((c.t, a, b, c.inside));
                    }
                }
            }

            // 3) Advance everyone to the earliest contact (or full frame_dt if none).
            let advance_dt = match earliest {
                Some((t, _, _, _)) => t,
                None => dt_remaining,
            };
            for (p, v) in self.positions.iter_mut().zip(self.velocities.iter()) {
                *p += *v * advance_dt;
                *p = self.grid.wrap_pos(*p);
            }
            dt_remaining -= advance_dt;

            // 4) Resolve the contact (if any) per chemistry.
            if let Some((_t, a, b, inside)) = earliest {
                let sa = self.states[a as usize] as usize;
                let sb = self.states[b as usize] as usize;
                let action = self.chemistry.lookup(sa, sb, inside);
                if action == Action::Reflect {
                    let pa = self.positions[a as usize];
                    let pb_raw = self.positions[b as usize];
                    let pb = pa + self.grid.min_image(pa, pb_raw);
                    let va = self.velocities[a as usize];
                    let vb = self.velocities[b as usize];
                    let (va_new, vb_new) = reflect(pa, va, pb, vb);
                    self.velocities[a as usize] = va_new;
                    self.velocities[b as usize] = vb_new;
                }
                // Action::Pass: no state change in P1's grey chemistry.
                // (State-change logic lands in P2.)
            } else {
                break; // no contact this frame
            }
        }
        self.tick += 1;
    }

    pub fn tick(&self) -> u32 { self.tick }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chemistry::load_chemistry;
    use crate::fab::load_fab;

    #[test]
    fn two_beads_head_on_swap_velocities() {
        // Build a tiny custom Sim by hand to test step() in isolation.
        let chem = load_chemistry("chemistries/grey.toml").unwrap();
        let g = chem.state_index("grey").unwrap() as u32;
        let mut sim = Sim {
            positions: vec![Vec2::new(5.0, 5.0), Vec2::new(7.0, 5.0)],
            velocities: vec![Vec2::new(1.0, 0.0), Vec2::new(-1.0, 0.0)],
            states: vec![g, g],
            chemistry: chem,
            grid: Grid::new(WORLD_SIZE),
            tick: 0,
        };
        // Step a frame long enough to cover the collision (t = 0.5).
        sim.step(1.0);
        // After collision, velocities should be reversed.
        assert!((sim.velocities[0] - Vec2::new(-1.0, 0.0)).length() < 1e-3);
        assert!((sim.velocities[1] - Vec2::new( 1.0, 0.0)).length() < 1e-3);
    }

    #[test]
    fn from_fab_loads_grey_30_with_unit_speed() {
        let fab = load_fab("fabs/grey-30.toml").unwrap();
        let chem = load_chemistry("chemistries/grey.toml").unwrap();
        let sim = Sim::from_fab(&fab, chem);
        assert_eq!(sim.positions.len(), 30);
        for v in &sim.velocities {
            assert!((v.length() - SPEED).abs() < 1e-5);
        }
    }
}
```

- [ ] **Step 2: Run the tests**

Run: `cargo test --lib sim::tests`

Expected: 2 tests pass.

- [ ] **Step 3: Commit**

```bash
git add src/sim.rs
git commit -m "Task 8: Sim struct and CCD-based frame step"
```

---

## Task 9: Open a winit window

**Files:**
- Modify: `src/app.rs`
- Modify: `src/lib.rs`

- [ ] **Step 1: Write the App with a winit handler**

In `src/app.rs`:

```rust
use winit::application::ApplicationHandler;
use winit::event::WindowEvent;
use winit::event_loop::{ActiveEventLoop, EventLoop};
use winit::window::{Window, WindowId};
use std::sync::Arc;

pub struct App {
    window: Option<Arc<Window>>,
}

impl App {
    pub fn new() -> Self {
        Self { window: None }
    }
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        let attrs = Window::default_attributes()
            .with_title("JiggleFab P1");
        let window = event_loop.create_window(attrs).expect("create window");
        self.window = Some(Arc::new(window));
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, _id: WindowId, event: WindowEvent) {
        match event {
            WindowEvent::CloseRequested => event_loop.exit(),
            WindowEvent::RedrawRequested => {
                if let Some(w) = &self.window {
                    w.request_redraw();
                }
            }
            _ => {}
        }
    }
}

pub fn run() -> anyhow::Result<()> {
    let event_loop = EventLoop::new()?;
    let mut app = App::new();
    event_loop.run_app(&mut app)?;
    Ok(())
}
```

- [ ] **Step 2: Update `src/lib.rs` to call into `app::run`**

Replace the body of `run()` in `src/lib.rs`:

```rust
pub mod fab;
pub mod chemistry;
pub mod rng;
pub mod ccd;
pub mod collide;
pub mod grid;
pub mod sim;
pub mod render;
pub mod app;

pub fn run() -> anyhow::Result<()> {
    env_logger::init();
    app::run()
}
```

- [ ] **Step 3: Run it and visually verify**

Run: `cargo run`

Expected: a window titled "JiggleFab P1" opens. It will be blank/black/whatever default. Closing it exits the program.

- [ ] **Step 4: Commit**

```bash
git add src/app.rs src/lib.rs
git commit -m "Task 9: open a winit window"
```

---

## Task 10: wgpu render context and clear pass

**Files:**
- Modify: `src/render.rs`
- Modify: `src/app.rs`

Stand up the wgpu Device/Queue/Surface and clear the screen to a known background color each frame. No beads yet.

- [ ] **Step 1: Write the wgpu context**

In `src/render.rs`:

```rust
use std::sync::Arc;
use winit::window::Window;
use anyhow::Result;

pub struct Renderer {
    pub surface: wgpu::Surface<'static>,
    pub device: wgpu::Device,
    pub queue: wgpu::Queue,
    pub config: wgpu::SurfaceConfiguration,
    pub size: winit::dpi::PhysicalSize<u32>,
}

impl Renderer {
    pub async fn new(window: Arc<Window>) -> Result<Self> {
        let size = window.inner_size();
        let instance = wgpu::Instance::default();
        let surface = instance.create_surface(window.clone())?;
        let adapter = instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            compatible_surface: Some(&surface),
            force_fallback_adapter: false,
        }).await.ok_or_else(|| anyhow::anyhow!("no adapter found"))?;

        let (device, queue) = adapter.request_device(&wgpu::DeviceDescriptor {
            label: Some("jigglefab device"),
            required_features: wgpu::Features::empty(),
            required_limits: wgpu::Limits::default(),
            memory_hints: wgpu::MemoryHints::Performance,
        }, None).await?;

        let surface_caps = surface.get_capabilities(&adapter);
        let format = surface_caps.formats.iter().copied()
            .find(|f| f.is_srgb()).unwrap_or(surface_caps.formats[0]);

        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format,
            width: size.width.max(1),
            height: size.height.max(1),
            present_mode: wgpu::PresentMode::Fifo,
            alpha_mode: surface_caps.alpha_modes[0],
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        };
        surface.configure(&device, &config);

        Ok(Self { surface, device, queue, config, size })
    }

    pub fn resize(&mut self, new_size: winit::dpi::PhysicalSize<u32>) {
        if new_size.width > 0 && new_size.height > 0 {
            self.size = new_size;
            self.config.width = new_size.width;
            self.config.height = new_size.height;
            self.surface.configure(&self.device, &self.config);
        }
    }

    pub fn render_clear(&self) -> Result<()> {
        let frame = self.surface.get_current_texture()?;
        let view = frame.texture.create_view(&Default::default());
        let mut encoder = self.device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: Some("clear encoder"),
        });
        {
            let _pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("clear pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color { r: 0.05, g: 0.05, b: 0.07, a: 1.0 }),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_writes: None,
            });
        }
        self.queue.submit(std::iter::once(encoder.finish()));
        frame.present();
        Ok(())
    }
}
```

- [ ] **Step 2: Wire renderer into `App`**

Update `src/app.rs`:

```rust
use winit::application::ApplicationHandler;
use winit::event::WindowEvent;
use winit::event_loop::{ActiveEventLoop, EventLoop};
use winit::window::{Window, WindowId};
use std::sync::Arc;

use crate::render::Renderer;

pub struct App {
    window: Option<Arc<Window>>,
    renderer: Option<Renderer>,
}

impl App {
    pub fn new() -> Self {
        Self { window: None, renderer: None }
    }
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        let attrs = Window::default_attributes()
            .with_title("JiggleFab P1");
        let window = Arc::new(event_loop.create_window(attrs).expect("create window"));
        let renderer = pollster::block_on(Renderer::new(window.clone()))
            .expect("create renderer");
        self.window = Some(window);
        self.renderer = Some(renderer);
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, _id: WindowId, event: WindowEvent) {
        let Some(window) = &self.window else { return };
        let Some(renderer) = &mut self.renderer else { return };
        match event {
            WindowEvent::CloseRequested => event_loop.exit(),
            WindowEvent::Resized(size) => renderer.resize(size),
            WindowEvent::RedrawRequested => {
                if let Err(e) = renderer.render_clear() {
                    log::warn!("render error: {e:?}");
                }
                window.request_redraw();
            }
            _ => {}
        }
    }
}

pub fn run() -> anyhow::Result<()> {
    let event_loop = EventLoop::new()?;
    let mut app = App::new();
    event_loop.run_app(&mut app)?;
    Ok(())
}
```

- [ ] **Step 3: Run and visually verify**

Run: `cargo run`

Expected: window opens with a dark blue-grey background `(0.05, 0.05, 0.07)`. Resizing the window does not crash. Closing exits.

- [ ] **Step 4: Commit**

```bash
git add src/render.rs src/app.rs
git commit -m "Task 10: wgpu context and clear pass"
```

---

## Task 11: Bead instance rendering pipeline

**Files:**
- Create: `shaders/beads.wgsl`
- Modify: `src/render.rs`
- Modify: `Cargo.toml` (resource path) — no change needed if loading at runtime
- Modify: `src/app.rs` (pass bead positions to renderer)

Render N filled circles, one per bead, by instancing a unit quad and discarding fragments outside the unit circle in the fragment shader. The vertex shader scales+translates per-instance by (bead position, radius). Bead positions come from a uniform/storage buffer updated each frame.

- [ ] **Step 1: Write the shader**

Create `shaders/beads.wgsl`:

```wgsl
struct Bead {
    pos: vec2<f32>,
};

struct Camera {
    view_proj: mat4x4<f32>,
    radius: f32,
    _pad0: f32,
    _pad1: f32,
    _pad2: f32,
};

@group(0) @binding(0) var<uniform> camera: Camera;
@group(0) @binding(1) var<storage, read> beads: array<Bead>;

struct VsIn {
    @location(0) quad_uv: vec2<f32>, // unit-quad corner in [-1, 1]
    @builtin(instance_index) inst: u32,
};

struct VsOut {
    @builtin(position) clip: vec4<f32>,
    @location(0) local: vec2<f32>,
};

@vertex
fn vs_main(in: VsIn) -> VsOut {
    let center = beads[in.inst].pos;
    let world = center + in.quad_uv * camera.radius;
    var out: VsOut;
    out.clip = camera.view_proj * vec4<f32>(world, 0.0, 1.0);
    out.local = in.quad_uv;
    return out;
}

@fragment
fn fs_main(in: VsOut) -> @location(0) vec4<f32> {
    let d = length(in.local);
    if (d > 1.0) {
        discard;
    }
    // Soft edge so the disc looks like a disc, not a polygon.
    let a = smoothstep(1.0, 0.95, d);
    return vec4<f32>(0.78, 0.78, 0.80, a);
}
```

- [ ] **Step 2: Extend the Renderer with the bead pipeline**

Replace `src/render.rs` with:

```rust
use std::sync::Arc;
use winit::window::Window;
use anyhow::Result;
use bytemuck::{Pod, Zeroable};
use glam::{Mat4, Vec2};
use wgpu::util::DeviceExt;

#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
struct BeadGpu {
    pos: [f32; 2],
    _pad: [f32; 2],
}

#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
struct CameraUbo {
    view_proj: [[f32; 4]; 4],
    radius: f32,
    _pad: [f32; 3],
}

pub struct Renderer {
    pub surface: wgpu::Surface<'static>,
    pub device: wgpu::Device,
    pub queue: wgpu::Queue,
    pub config: wgpu::SurfaceConfiguration,
    pub size: winit::dpi::PhysicalSize<u32>,
    pipeline: wgpu::RenderPipeline,
    quad_vbuf: wgpu::Buffer,
    bead_buf: wgpu::Buffer,
    bead_capacity: usize,
    camera_buf: wgpu::Buffer,
    bind_group: wgpu::BindGroup,
    bind_layout: wgpu::BindGroupLayout,
}

impl Renderer {
    pub async fn new(window: Arc<Window>, initial_bead_count: usize) -> Result<Self> {
        let size = window.inner_size();
        let instance = wgpu::Instance::default();
        let surface = instance.create_surface(window.clone())?;
        let adapter = instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            compatible_surface: Some(&surface),
            force_fallback_adapter: false,
        }).await.ok_or_else(|| anyhow::anyhow!("no adapter found"))?;

        let (device, queue) = adapter.request_device(&wgpu::DeviceDescriptor {
            label: Some("jigglefab device"),
            required_features: wgpu::Features::empty(),
            required_limits: wgpu::Limits::default(),
            memory_hints: wgpu::MemoryHints::Performance,
        }, None).await?;

        let surface_caps = surface.get_capabilities(&adapter);
        let format = surface_caps.formats.iter().copied()
            .find(|f| f.is_srgb()).unwrap_or(surface_caps.formats[0]);

        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format,
            width: size.width.max(1),
            height: size.height.max(1),
            present_mode: wgpu::PresentMode::Fifo,
            alpha_mode: surface_caps.alpha_modes[0],
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        };
        surface.configure(&device, &config);

        // Unit quad as 6 vertices (two triangles).
        let quad: [[f32; 2]; 6] = [
            [-1.0, -1.0], [ 1.0, -1.0], [ 1.0,  1.0],
            [-1.0, -1.0], [ 1.0,  1.0], [-1.0,  1.0],
        ];
        let quad_vbuf = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("quad"),
            contents: bytemuck::cast_slice(&quad),
            usage: wgpu::BufferUsages::VERTEX,
        });

        let bead_capacity = initial_bead_count.max(1);
        let bead_buf = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("beads"),
            size: (bead_capacity * std::mem::size_of::<BeadGpu>()) as u64,
            usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        let camera_buf = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("camera"),
            size: std::mem::size_of::<CameraUbo>() as u64,
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        let bind_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("beads bind"),
            entries: &[
                wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::VERTEX | wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    binding: 1,
                    visibility: wgpu::ShaderStages::VERTEX,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Storage { read_only: true },
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
            ],
        });

        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("beads bg"),
            layout: &bind_layout,
            entries: &[
                wgpu::BindGroupEntry { binding: 0, resource: camera_buf.as_entire_binding() },
                wgpu::BindGroupEntry { binding: 1, resource: bead_buf.as_entire_binding() },
            ],
        });

        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("beads"),
            source: wgpu::ShaderSource::Wgsl(include_str!("../shaders/beads.wgsl").into()),
        });

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("beads layout"),
            bind_group_layouts: &[&bind_layout],
            push_constant_ranges: &[],
        });

        let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("beads pipeline"),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: "vs_main",
                buffers: &[wgpu::VertexBufferLayout {
                    array_stride: 8,
                    step_mode: wgpu::VertexStepMode::Vertex,
                    attributes: &[wgpu::VertexAttribute {
                        offset: 0,
                        shader_location: 0,
                        format: wgpu::VertexFormat::Float32x2,
                    }],
                }],
                compilation_options: Default::default(),
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader,
                entry_point: "fs_main",
                targets: &[Some(wgpu::ColorTargetState {
                    format,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
                compilation_options: Default::default(),
            }),
            primitive: wgpu::PrimitiveState::default(),
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
            multiview: None,
            cache: None,
        });

        Ok(Self {
            surface, device, queue, config, size,
            pipeline, quad_vbuf, bead_buf, bead_capacity, camera_buf, bind_group, bind_layout,
        })
    }

    pub fn resize(&mut self, new_size: winit::dpi::PhysicalSize<u32>) {
        if new_size.width > 0 && new_size.height > 0 {
            self.size = new_size;
            self.config.width = new_size.width;
            self.config.height = new_size.height;
            self.surface.configure(&self.device, &self.config);
        }
    }

    pub fn update_beads(&mut self, positions: &[Vec2]) {
        // Re-allocate the storage buffer if it's too small.
        if positions.len() > self.bead_capacity {
            self.bead_capacity = positions.len().next_power_of_two();
            self.bead_buf = self.device.create_buffer(&wgpu::BufferDescriptor {
                label: Some("beads"),
                size: (self.bead_capacity * std::mem::size_of::<BeadGpu>()) as u64,
                usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_DST,
                mapped_at_creation: false,
            });
            self.bind_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: Some("beads bg"),
                layout: &self.bind_layout,
                entries: &[
                    wgpu::BindGroupEntry { binding: 0, resource: self.camera_buf.as_entire_binding() },
                    wgpu::BindGroupEntry { binding: 1, resource: self.bead_buf.as_entire_binding() },
                ],
            });
        }
        let gpu_beads: Vec<BeadGpu> = positions.iter()
            .map(|p| BeadGpu { pos: [p.x, p.y], _pad: [0.0; 2] })
            .collect();
        self.queue.write_buffer(&self.bead_buf, 0, bytemuck::cast_slice(&gpu_beads));
    }

    pub fn update_camera(&mut self, world_size: f32) {
        // Orthographic projection covering the whole world, square, centered.
        let aspect = self.size.width as f32 / self.size.height as f32;
        let (w, h) = if aspect >= 1.0 {
            (world_size * aspect, world_size)
        } else {
            (world_size, world_size / aspect)
        };
        let proj = Mat4::orthographic_rh(0.0, w, 0.0, h, -1.0, 1.0);
        // Center the world inside the view if aspect > 1.
        let offset_x = (w - world_size) * 0.5;
        let offset_y = (h - world_size) * 0.5;
        let view = Mat4::from_translation(glam::Vec3::new(offset_x, offset_y, 0.0));
        let vp = proj * view;
        let ubo = CameraUbo {
            view_proj: vp.to_cols_array_2d(),
            radius: crate::ccd::RADIUS,
            _pad: [0.0; 3],
        };
        self.queue.write_buffer(&self.camera_buf, 0, bytemuck::bytes_of(&ubo));
    }

    pub fn render(&self, bead_count: usize) -> Result<()> {
        let frame = self.surface.get_current_texture()?;
        let view = frame.texture.create_view(&Default::default());
        let mut encoder = self.device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: Some("bead encoder"),
        });
        {
            let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("bead pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color { r: 0.05, g: 0.05, b: 0.07, a: 1.0 }),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_writes: None,
            });
            pass.set_pipeline(&self.pipeline);
            pass.set_bind_group(0, &self.bind_group, &[]);
            pass.set_vertex_buffer(0, self.quad_vbuf.slice(..));
            pass.draw(0..6, 0..bead_count as u32);
        }
        self.queue.submit(std::iter::once(encoder.finish()));
        frame.present();
        Ok(())
    }
}
```

(Note: `render_clear` is removed; the bead `render` includes the clear in its load op.)

- [ ] **Step 3: Wire the sim into `App`**

Replace `src/app.rs`:

```rust
use winit::application::ApplicationHandler;
use winit::event::WindowEvent;
use winit::event_loop::{ActiveEventLoop, EventLoop};
use winit::window::{Window, WindowId};
use std::sync::Arc;
use std::time::Instant;

use crate::chemistry::load_chemistry;
use crate::fab::load_fab;
use crate::render::Renderer;
use crate::sim::{Sim, WORLD_SIZE};

const FRAME_DT: f32 = 1.0 / 60.0;

pub struct App {
    window: Option<Arc<Window>>,
    renderer: Option<Renderer>,
    sim: Option<Sim>,
    last_frame: Instant,
}

impl App {
    pub fn new() -> Self {
        Self { window: None, renderer: None, sim: None, last_frame: Instant::now() }
    }
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        let attrs = Window::default_attributes().with_title("JiggleFab P1");
        let window = Arc::new(event_loop.create_window(attrs).expect("create window"));

        let fab = load_fab("fabs/grey-30.toml").expect("load fab");
        let chem = load_chemistry("chemistries/grey.toml").expect("load chem");
        let sim = Sim::from_fab(&fab, chem);

        let mut renderer = pollster::block_on(Renderer::new(window.clone(), sim.positions.len()))
            .expect("create renderer");
        renderer.update_camera(WORLD_SIZE);

        self.window = Some(window);
        self.renderer = Some(renderer);
        self.sim = Some(sim);
        self.last_frame = Instant::now();
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, _id: WindowId, event: WindowEvent) {
        let Some(window) = &self.window else { return };
        let Some(renderer) = &mut self.renderer else { return };
        let Some(sim) = &mut self.sim else { return };
        match event {
            WindowEvent::CloseRequested => event_loop.exit(),
            WindowEvent::Resized(size) => {
                renderer.resize(size);
                renderer.update_camera(WORLD_SIZE);
            }
            WindowEvent::RedrawRequested => {
                // Use a fixed dt for deterministic stepping.
                sim.step(FRAME_DT);
                renderer.update_beads(&sim.positions);
                if let Err(e) = renderer.render(sim.positions.len()) {
                    log::warn!("render error: {e:?}");
                }
                window.request_redraw();
                self.last_frame = Instant::now();
            }
            _ => {}
        }
    }
}

pub fn run() -> anyhow::Result<()> {
    let event_loop = EventLoop::new()?;
    let mut app = App::new();
    event_loop.run_app(&mut app)?;
    Ok(())
}
```

- [ ] **Step 4: Run and visually verify**

Run: `cargo run`

Expected: a window opens showing 30 grey filled circles forming a vertical chain, jiggling. The chain stays bonded (adjacent beads never fully separate). When a bead drifts off one edge, it reappears on the opposite edge (torus wrap). The whole thing should feel organic; if anything looks like a missile or escapes, something is wrong.

If circles are tiny or huge: the camera math may need tweaking — adjust `WORLD_SIZE` in `sim.rs` or the projection in `update_camera`.

- [ ] **Step 5: Commit**

```bash
git add shaders/ src/render.rs src/app.rs
git commit -m "Task 11: bead instance rendering pipeline; jiggling visible"
```

---

## Task 12: Determinism integration test

**Files:**
- Create: `tests/determinism.rs`

Run the sim for N frames from a fixed seed twice; assert the position arrays are bit-identical.

- [ ] **Step 1: Write the test**

Create `tests/determinism.rs`:

```rust
use jigglefab::chemistry::load_chemistry;
use jigglefab::fab::load_fab;
use jigglefab::sim::Sim;

#[test]
fn same_seed_produces_same_state_after_n_frames() {
    let fab = load_fab("fabs/grey-30.toml").unwrap();
    let chem_a = load_chemistry("chemistries/grey.toml").unwrap();
    let chem_b = load_chemistry("chemistries/grey.toml").unwrap();

    let mut a = Sim::from_fab(&fab, chem_a);
    let mut b = Sim::from_fab(&fab, chem_b);

    let dt = 1.0 / 60.0;
    for _ in 0..600 { // 10 seconds of sim time
        a.step(dt);
        b.step(dt);
    }

    for i in 0..a.positions.len() {
        assert_eq!(a.positions[i].to_array(), b.positions[i].to_array(),
                   "position mismatch at bead {}", i);
        assert_eq!(a.velocities[i].to_array(), b.velocities[i].to_array(),
                   "velocity mismatch at bead {}", i);
    }
}
```

- [ ] **Step 2: Run the test**

Run: `cargo test --test determinism`

Expected: PASS. If it fails, something in the simulation has nondeterminism (likely a HashMap or iteration order). The grid uses Vec<Vec<u32>> precisely to avoid that.

- [ ] **Step 3: Commit**

```bash
git add tests/determinism.rs
git commit -m "Task 12: determinism integration test"
```

---

## Task 13: Final polish

**Files:**
- Modify: `README.md`

- [ ] **Step 1: Add a "Run P1" section to the root README**

Read the current `README.md`. Append a section:

```markdown
## Running P1

Requires Rust stable (rustup.rs) and a WebGPU-capable GPU.

```bash
cargo run --release
```

Loads `fabs/grey-30.toml` against `chemistries/grey.toml` and renders 30 grey beads jiggling in a vertical chain on a torus. Same seed produces a bit-identical run on the same machine (verify with `cargo test --test determinism`).

This is **Phase 1** ("hello jiggling chain") of the [engine design](docs/superpowers/specs/2026-05-20-jigglefab-engine-design.md). Subsequent phases (P2 chemistry engine, P3 constructor, P4 exhibit) get their own plans.
```

- [ ] **Step 2: Verify everything still works**

Run: `cargo test`

Expected: all unit tests + the determinism integration test pass.

Run: `cargo run --release`

Expected: 30 beads jiggling as before, at solid framerate.

- [ ] **Step 3: Commit**

```bash
git add README.md
git commit -m "Task 13: document P1 run instructions"
```

---

## Definition of done

After all tasks complete, the following are true:

- `cargo build --release` succeeds with no warnings or errors.
- `cargo test` runs all unit tests and the determinism integration test, all pass.
- `cargo run --release` opens a window showing 30 grey beads jiggling in a vertical chain on a 2D torus.
- The chain stays bonded — adjacent beads never fully separate.
- Beads wrap around the world (appear on opposite edge when they cross).
- Two consecutive runs with the same seed produce visually-identical motion and bit-identical sim state at any given tick.
- `haskell/` is untouched.
- The git log on `main` contains 13 commits matching the task names above.

P2 ("chemistry engine: load arbitrary chemistries, port the haskell chemistry as instance #1, verify a second chemistry runs unchanged-engine, stand up invariants and anomaly menagerie") gets its own plan.
