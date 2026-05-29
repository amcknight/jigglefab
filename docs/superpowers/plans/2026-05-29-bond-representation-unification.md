# Bond-Representation Unification Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the four scattered bond-pair representations (`Fab` `[u32;2]`, `Scene`/`Sim`/parallel `(u32,u32)`, GPU sorted `Vec<[u32;2]>`) with a single `BondPair` newtype that owns canonical ordering, eliminating the `~7` ad-hoc `min/max` / `if a<b` canonicalization sites.

**Architecture:** Introduce `BondPair { lo: u32, hi: u32 }` in a new `src/bond.rs`. Its constructor canonicalizes once (`lo <= hi`); it is `Copy + Eq + Hash + Ord` with a memory layout identical to `(u32, u32)` and a derived `Ord` that matches tuple lexicographic ordering — so it drops into existing `HashSet`s, `sort_unstable`, and `dedup` with zero behavior change. It (de)serializes as a `[u32; 2]` array so on-disk `.toml` stays `bonds = [[0, 1]]`. The GPU byte-layout boundary (`buffers.rs`) keeps `[u32; 2]`; the GPU scheduler converts `BondPair -> [u32; 2]` once at upload.

**Tech Stack:** Rust, serde (custom `Serialize`/`Deserialize` on `BondPair`), `std::collections::HashSet`. No new dependencies.

---

## Why this is safe (correctness invariants — read before starting)

This is a pervasive but **behavior-preserving** type swap. Three invariants make it safe; every task must preserve them:

1. **Layout/perf neutral.** `struct BondPair { lo: u32, hi: u32 }` is two `u32`s, same as `(u32, u32)`. No heap, no indirection. The hot collision loop (`parallel/substep.rs`, `parallel/resolve.rs`) sees no perf change.
2. **Ordering identical.** Canonical tuples are already `(min, max)`. `#[derive(PartialOrd, Ord)]` on `BondPair` with fields declared `lo` then `hi` yields the same lexicographic order as `(lo, hi)` tuples. This is load-bearing: `pending_bonds.sort_unstable()` + `dedup()` and the determinism tests (`bonds_seq == bonds_par`) depend on stable ordering.
3. **Serde stays `[u32;2]`.** `BondPair` serializes/deserializes as a 2-element array, and deserialization canonicalizes. So `.toml` files, the GPU upload, and the in-flight `fab_serializes_and_reparses` round-trip test are unaffected on disk.

**The existing test suite is the safety net.** The bit-exact determinism tests (`do_substep_mt_bit_matches_do_substep`, `parallel_resolve_color_bit_matches_sequential`), the bond-preservation bench tests, and the editor bond tests collectively exercise every migrated site. If they stay green, the swap is correct.

**Scope note:** This touches 9 files including the determinism-critical parallel solver and the bench harness. It is mechanical, not a redesign — do not change collision physics, bond-formation distance rules, or the GPU buffer layout.

---

### Task 1: `BondPair` newtype

**Files:**
- Create: `src/bond.rs`
- Modify: `src/lib.rs:1-20` (add module declaration)

- [ ] **Step 1: Write the failing tests**

Create `src/bond.rs` with the test module first (the type doesn't exist yet, so this won't compile — that's the "failing test"):

```rust
use serde::{Deserialize, Deserializer, Serialize, Serializer};

/// An unordered pair of bead indices, stored canonically as `lo <= hi`.
///
/// Layout and ordering are identical to the `(u32, u32)` tuple this replaces:
/// two `u32`s, derived `Ord` is lexicographic on `(lo, hi)`. Construct via
/// `new`, which canonicalizes once, so callers never repeat `min`/`max`.
/// Serializes as a `[u32; 2]` array so on-disk `.toml` stays `bonds = [[0, 1]]`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BondPair {
    lo: u32,
    hi: u32,
}

impl BondPair {
    pub fn new(a: u32, b: u32) -> Self {
        if a <= b { BondPair { lo: a, hi: b } } else { BondPair { lo: b, hi: a } }
    }
    pub fn lo(&self) -> u32 { self.lo }
    pub fn hi(&self) -> u32 { self.hi }
    pub fn as_array(&self) -> [u32; 2] { [self.lo, self.hi] }
    /// True if `idx` is one of the two endpoints.
    pub fn contains(&self, idx: u32) -> bool { self.lo == idx || self.hi == idx }
}

impl Serialize for BondPair {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        [self.lo, self.hi].serialize(s)
    }
}

impl<'de> Deserialize<'de> for BondPair {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        let [a, b] = <[u32; 2]>::deserialize(d)?;
        Ok(BondPair::new(a, b))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_canonicalizes_regardless_of_order() {
        assert_eq!(BondPair::new(3, 1), BondPair::new(1, 3));
        assert_eq!(BondPair::new(1, 3).lo(), 1);
        assert_eq!(BondPair::new(1, 3).hi(), 3);
    }

    #[test]
    fn ordering_matches_tuple_lexicographic() {
        let mut pairs = vec![BondPair::new(2, 0), BondPair::new(0, 1), BondPair::new(0, 0)];
        pairs.sort_unstable();
        assert_eq!(pairs, vec![BondPair::new(0, 0), BondPair::new(0, 1), BondPair::new(0, 2)]);
    }

    #[test]
    fn contains_reports_both_endpoints() {
        let bp = BondPair::new(4, 7);
        assert!(bp.contains(4) && bp.contains(7));
        assert!(!bp.contains(5));
    }

    #[test]
    fn serializes_as_array_and_canonicalizes_on_load() {
        // Round-trips through TOML as a bare array; reversed input canonicalizes.
        let bp = BondPair::new(0, 1);
        let json = serde_json::to_string(&bp).unwrap();
        assert_eq!(json, "[0,1]");
        let back: BondPair = serde_json::from_str("[1,0]").unwrap();
        assert_eq!(back, bp);
    }
}
```

- [ ] **Step 2: Add a dev-dependency for the serde test (only if absent)**

Check `Cargo.toml` for `serde_json` under `[dev-dependencies]`. Run: `grep -n "serde_json" Cargo.toml`
If absent, add under `[dev-dependencies]`:
```toml
serde_json = "1"
```
(If you prefer not to add a dev-dep, replace the `serializes_as_array...` test body with a `toml`-based round-trip using the already-present `toml` crate: serialize a `struct W { b: Vec<BondPair> }` and assert the text contains `[[0, 1]]`. Either is acceptable; do not skip the serde coverage.)

- [ ] **Step 3: Wire the module**

In `src/lib.rs`, add after line 1 (`pub mod fab;`):
```rust
pub mod bond;
```

- [ ] **Step 4: Run the tests**

Run: `cargo test --lib bond::`
Expected: 4 tests pass.

- [ ] **Step 5: Commit**

```bash
git add src/bond.rs src/lib.rs Cargo.toml
git commit -m "feat(bond): BondPair newtype with canonical ordering + array serde"
```

---

### Task 2: Migrate the core bond set (`fab` + `sim` + `parallel/*` + `editor`) to `BondPair`

These files share the bond-set type directly (via `Sim.bonds`, `Scene.bonds`, and `&HashSet<...>` solver params), so they must migrate in one compiling commit. The existing determinism + bond tests are the guard. Edit file-by-file, then build + test + commit once at the end.

**Files:**
- Modify: `src/fab.rs:1-26,~91,~124`
- Modify: `src/sim.rs:50,58,66,78,105-109,116-119,128-130,~334,~396,~472-474,~521-522`
- Modify: `src/parallel/resolve.rs:16-17,118-119,169-171,220-222`
- Modify: `src/parallel/substep.rs:18-20,62,73,95,105,120,155,284-285,304,307,343-345,~543-544,~619,~664-668,~726-729,~756,~771`
- Modify: `src/parallel/scheduler.rs:14,58-60`
- Modify: `src/parallel/scheduler_mt.rs:18`
- Modify: `src/editor.rs:110,129,138-141,161,170,214,231-232,319-327,~624,~627,~658,~684-686,~710-712`

- [ ] **Step 1: `src/fab.rs` — Fab uses `Vec<BondPair>`**

Add import near the top (after line 2 `use glam::Vec2;`):
```rust
use crate::bond::BondPair;
```
Change the `bonds` field (line 19):
```rust
    pub bonds: Option<Vec<BondPair>>,
```
Change `bonds()` (lines 23-25):
```rust
    pub fn bonds(&self) -> Option<&Vec<BondPair>> {
        self.meta.bonds.as_ref()
    }
```
Update the two test assertions that compare against array literals. In `parses_bonds` (~line 91):
```rust
        assert_eq!(fab.bonds(), Some(&vec![BondPair::new(0, 1)]));
```
In `fab_serializes_and_reparses` (~line 124, the in-flight WIP test):
```rust
        assert_eq!(reparsed.bonds(), Some(&vec![BondPair::new(0, 1)]));
```
Leave the `assert!(!out.contains("vel"))` line and the `bonds = [[0, 1]]` TOML fixtures unchanged — serde still emits `[[0, 1]]`.

- [ ] **Step 2: `src/sim.rs` — Sim bond set + helpers**

Add import (with the other `use crate::...` lines near the top):
```rust
use crate::bond::BondPair;
```
Line 50:
```rust
    pub(crate) bonds: HashSet<BondPair>,
```
Line 58 (signature) and line 66 (insert), inside `derive_bonds_by_distance`:
```rust
pub(crate) fn derive_bonds_by_distance(positions: &[Vec2], grid: &Grid) -> HashSet<BondPair> {
```
```rust
                bonds.insert(BondPair::new(i as u32, j as u32));
```
Line 78:
```rust
    pub fn bonds(&self) -> &HashSet<BondPair> { &self.bonds }
```
Lines 105-109 (`from_fab` bond ingestion) — explicit bonds are already `BondPair`, just collect them:
```rust
        let bonds = match fab.bonds() {
            Some(explicit) => explicit.iter().copied().collect(),
            None => derive_bonds_by_distance(&positions, &grid),
        };
```
Lines 116-119 (`is_bonded`):
```rust
    fn is_bonded(&self, a: u32, b: u32) -> bool {
        self.bonds.contains(&BondPair::new(a, b))
    }
```
Lines 128-130 (`enforce_bonds` — change the collected type and the loop destructure; body unchanged):
```rust
    pub(crate) fn enforce_bonds(&mut self) {
        let pairs: Vec<BondPair> = self.bonds.iter().copied().collect();
        for bond in pairs {
            let (a, b) = (bond.lo(), bond.hi());
```
Test sites: lines ~334 and ~396 `bonds.insert((0u32, 1u32));` become:
```rust
        bonds.insert(BondPair::new(0, 1));
```
Lines ~472-474 (`contains` assertions):
```rust
        assert!(sim.bonds().contains(&BondPair::new(0, 1)));
        assert!(sim.bonds().contains(&BondPair::new(1, 2)));
        assert!(!sim.bonds().contains(&BondPair::new(0, 2)), "explicit bonds must not be widened");
```
Lines ~521-522 (the 30k perf test now collects `BondPair` directly):
```rust
        let bonds_vec: Vec<BondPair> = sim_warm.bonds().iter().copied().collect();
        fab.meta.bonds = Some(bonds_vec);
```

- [ ] **Step 3: `src/parallel/resolve.rs` — ctx + pending + helper**

Add import:
```rust
use crate::bond::BondPair;
```
Lines 16-17 (`ResolveCtx` fields):
```rust
    pub bonds: &'a HashSet<BondPair>,
    pub pending_bonds: &'a mut Vec<BondPair>,
```
Lines 118-119 (birth path — replace inline `min`/`max`):
```rust
                ctx.pending_bonds.push(BondPair::new(a, new_slot));
                ctx.pending_bonds.push(BondPair::new(b, new_slot));
```
Lines 169-171 (`is_bonded` helper):
```rust
fn is_bonded(bonds: &HashSet<BondPair>, a: u32, b: u32) -> bool {
    bonds.contains(&BondPair::new(a, b))
}
```
Line 220 test fixture:
```rust
        let bonds: HashSet<BondPair> = Default::default();
```
Line 222 — if `pending_bonds` is annotated, set `Vec<BondPair>`; if inferred, leave it (it will infer from the ctx field type).

- [ ] **Step 4: `src/parallel/substep.rs` — solver bond params**

Add import:
```rust
use crate::bond::BondPair;
```
Lines 18-20 (`enforce_bonds` signature + collect + loop destructure; body unchanged through line 46):
```rust
pub fn enforce_bonds(pool: &mut BeadPool, grid: &Grid, bonds: &HashSet<BondPair>) {
    let pairs: Vec<BondPair> = bonds.iter().copied().collect();
    for bond in pairs {
        let (a, b) = (bond.lo(), bond.hi());
```
Lines 62 and 105 (`do_substep` / `do_substep_mt` `bonds` param):
```rust
    bonds: &mut HashSet<BondPair>,
```
Lines 73 and 120 (`pending_bonds` locals):
```rust
    let mut pending_bonds: Vec<BondPair> = Vec::new();
```
Lines 95 and 155 (death retain — replace tuple destructure with `contains`):
```rust
        bonds.retain(|bp| !bp.contains(slot));
```
Lines 284-285 and 330-331 (`resolve_color*` params):
```rust
    bonds: &HashSet<BondPair>,
    pending_bonds: &mut Vec<BondPair>,
```
Line 304 (`per_pair` accumulator type):
```rust
    let per_pair: Vec<(Vec<BondPair>, Vec<u32>)> = pairs_in_color
```
Line 307 (inner `pb` local):
```rust
            let mut pb: Vec<BondPair> = Vec::new();
```
Lines 343-345 (the `bonded` lookup — replace the inline `if a<b` key):
```rust
    let bonded = bonds.contains(&BondPair::new(a, b));
```
(The `sort_unstable()` + `dedup()` + `insert()` calls at lines ~88-91 / ~147-150 need no change — `BondPair: Ord + Eq + Hash`.)

Test sites in the same file:
- ~543-544: `let mut bonds: std::collections::HashSet<(u32, u32)> = Default::default();` then `bonds.insert((a.min(b), a.max(b)));` →
```rust
        let mut bonds: std::collections::HashSet<BondPair> = Default::default();
        bonds.insert(BondPair::new(a, b));
```
- ~619: `let mut bonds: std::collections::HashSet<(u32, u32)> = Default::default();` → `HashSet<BondPair>`.
- ~648 / ~710 `build_chain` return type `(BeadPool, HashSet<(u32, u32)>)` → `(BeadPool, HashSet<BondPair>)`.
- ~664-668: `bonds.insert((i, i + 1));` → `bonds.insert(BondPair::new(i, i + 1));`
- ~726-729: `bonds.insert((0u32, 1u32));` / `((2u32, 3u32))` → `BondPair::new(0, 1)` / `BondPair::new(2, 3)`.
- ~756 / ~771: `let mut pb_seq: Vec<(u32, u32)> = Vec::new();` / `pb_par` → `Vec<BondPair>`.
- Line ~703 `assert_eq!(bonds_seq, bonds_par);` — unchanged (`HashSet<BondPair>: Eq`).

- [ ] **Step 5: `src/parallel/scheduler.rs` and `scheduler_mt.rs` — shadow bond fields**

Add import to each:
```rust
use crate::bond::BondPair;
```
`scheduler.rs` line 14:
```rust
    bonds: HashSet<BondPair>,
```
`scheduler.rs` lines 58-60 (`sim_bonds_clone` return type):
```rust
fn sim_bonds_clone(sim: &Sim) -> HashSet<BondPair> {
    sim.bonds.clone()
}
```
`scheduler_mt.rs` line 18:
```rust
    bonds: HashSet<BondPair>,
```
(Lines 86 / 78 `sim.bonds = self.bonds.clone();` are unchanged — both sides are now `HashSet<BondPair>`.)

- [ ] **Step 6: `src/editor.rs` — Scene + ScenePayload bond set**

Add import (with the other `use crate::...` lines):
```rust
use crate::bond::BondPair;
```
Line 110 (`Scene.bonds`) and line 129 (`ScenePayload.bonds`):
```rust
    pub bonds: HashSet<BondPair>,
```
Lines 138-141 (`from_fab` — explicit bonds already `BondPair`):
```rust
        let bonds = match fab.bonds() {
            Some(explicit) => explicit.iter().copied().collect(),
            None => crate::sim::derive_bonds_by_distance(&positions, &grid),
        };
```
Lines 161-170 (`to_sim` — collect `BondPair`, still sorted for deterministic Fab output):
```rust
        let mut bonds_vec: Vec<BondPair> = self.bonds.iter().copied().collect();
        // Stable order so debug prints / fixture snapshots are deterministic.
        bonds_vec.sort_unstable();
```
and the `Meta { ... bonds: Some(bonds_vec), ... }` field stays as-is (now `Vec<BondPair>`).
Line 214 (`place`):
```rust
                self.bonds.insert(BondPair::new(i as u32, new_idx));
```
Lines 231-232 (`append_chain_bead` — drop the inline `if prev<new` branch):
```rust
        self.bonds.insert(BondPair::new(prev_idx, new_idx));
```
(Delete the now-unused `let key = if prev_idx < new_idx ...` line that precedes it.)
Lines 319-327 (`delete_selection` remap — replace `if na<nb` with `BondPair::new`):
```rust
        let new_bonds: HashSet<BondPair> = self.bonds.iter().filter_map(|&bond| {
            match (remap[bond.lo() as usize], remap[bond.hi() as usize]) {
                (Some(na), Some(nb)) => Some(BondPair::new(na, nb)),
                _ => None,
            }
        }).collect();
```
Test sites: line ~624 `scene.bonds.insert((0, 1));` → `BondPair::new(0, 1)`. Lines ~627, ~658, ~710-712 `contains(&(x, y))` → `contains(&BondPair::new(x, y))`. Lines ~684-686 `contains(&(a, b))` / `(b, c)` / `(a, c)` → `BondPair::new(a, b)` etc. Lines ~554, ~638 `assert_eq!(scene.bonds, ...)` unchanged.

- [ ] **Step 7: Build (debug)**

Run: `cargo build`
Expected: clean compile. If a `(u32, u32)` / `[u32; 2]` mismatch remains, the compiler names the exact file:line — fix it with the same `BondPair::new` / `bp.lo()/.hi()` pattern.

- [ ] **Step 8: Run the full test suite (debug + release)**

Run: `cargo test`
Expected: all pass — especially `editor::`, `sim::`, and `parallel::` bond tests.
Run: `cargo test --release`
Expected: all pass — this is the only build that runs the bit-exact determinism tests (`do_substep_mt_bit_matches_do_substep`, `parallel_resolve_color_bit_matches_sequential`) and the gated 30k perf test. Green here is the core correctness proof for this migration.

- [ ] **Step 9: Commit**

```bash
git add src/fab.rs src/sim.rs src/parallel/resolve.rs src/parallel/substep.rs src/parallel/scheduler.rs src/parallel/scheduler_mt.rs src/editor.rs
git commit -m "refactor(bond): use BondPair across fab/sim/parallel/editor bond sets"
```

---

### Task 3: Migrate the bench harness bond set

`bench/scenario.rs` owns an independent bond set (`geometric_bonds` / `initial_bond_set`) used for invariant checking. It does not share types with `Sim.bonds`, so it migrates on its own.

**Files:**
- Modify: `src/bench/scenario.rs:9,24,35,~55,~67`
- Modify: `src/bench/chains.rs` / `src/bench/runner.rs` — verify only (see Step 2)

- [ ] **Step 1: `src/bench/scenario.rs`**

Add import:
```rust
use crate::bond::BondPair;
```
Line 9 (`Scenario.initial_bond_set`):
```rust
    pub initial_bond_set: HashSet<BondPair>,
```
Line 24 (`geometric_bonds` signature) and line 35 (insert):
```rust
pub fn geometric_bonds(positions: &[Vec2], world_size: f32) -> HashSet<BondPair> {
```
```rust
                bonds.insert(BondPair::new(i as u32, j as u32));
```
Test assertions ~55 and ~67 `bonds.contains(&(0u32, 1u32))`:
```rust
        assert!(bonds.contains(&BondPair::new(0, 1)));
```

- [ ] **Step 2: Verify the bench consumers need no change**

`runner.rs:140-141` uses `initial_bond_set.difference(&final_bonds).count()` and `chains.rs:119` uses `.len()` — both are `HashSet`/iterator methods that work unchanged on `HashSet<BondPair>`. Confirm by building.

- [ ] **Step 3: Build the bench target (native only — bench is `#[cfg(not(target_arch = "wasm32"))]`)**

Run: `cargo build --bin bench`
Expected: clean compile.

- [ ] **Step 4: Run bench tests**

Run: `cargo test --release bench::`
Expected: `bonds_preserved` / `multi_row_layout_no_cross_chain_bonds` etc. pass.

- [ ] **Step 5: Commit**

```bash
git add src/bench/scenario.rs
git commit -m "refactor(bond): bench geometric_bonds uses BondPair"
```

---

### Task 4: GPU upload boundary derives `[u32; 2]` from `BondPair`

The GPU buffer (`buffers.rs`) legitimately keeps `[u32; 2]` for the bytemuck byte layout. Only the scheduler's conversion changes: it now derives arrays from `BondPair` instead of re-canonicalizing tuples.

**Files:**
- Modify: `src/gpu/scheduler.rs:41-47`
- Leave unchanged: `src/gpu/buffers.rs` (`upload_bonds(&[[u32; 2]])` stays — the byte-layout boundary)

- [ ] **Step 1: `src/gpu/scheduler.rs` — `sort_bonds`**

Replace `sort_bonds` (lines 41-47) so it maps `BondPair -> [u32; 2]` via `as_array()`; ordering is unchanged because `[u32;2]` and `BondPair` sort identically:
```rust
    fn sort_bonds(sim: &Sim) -> Vec<[u32; 2]> {
        let mut v: Vec<[u32; 2]> = sim.bonds.iter().map(|bp| bp.as_array()).collect();
        v.sort_unstable();
        v
    }
```
(`bonds_sorted: Vec<[u32; 2]>` field at line 15 and `_chemistry_table` stay as-is — the dead-field cleanup is a separate scan item, not part of this refactor.)

- [ ] **Step 2: Build with the GPU path (native)**

Run: `cargo build`
Expected: clean compile. (`gpu` module builds on native; the WASM target compiles it too via WebGPU — Step 3 of Task 5 covers wasm.)

- [ ] **Step 3: Commit**

```bash
git add src/gpu/scheduler.rs
git commit -m "refactor(bond): GPU sort_bonds derives [u32;2] from BondPair at upload"
```

---

### Task 5: Full verification

**Files:** none modified — this is the merge-readiness gate.

- [ ] **Step 1: Confirm no stray bond canonicalization remains**

Run: `grep -rn "min(.*max\|if a < b\|if na < nb\|p\[0\].min" src/ --include=*.rs`
Expected: no bond-related hits. (Unrelated `min`/`max` on positions/clamps are fine; confirm none are constructing bond pairs.)
Run: `grep -rn "bonds.*(u32, u32)\|HashSet<(u32, u32)>" src/ --include=*.rs`
Expected: zero hits — every bond set is now `HashSet<BondPair>`.

- [ ] **Step 2: Full native test suite, debug + release**

Run: `cargo test`
Run: `cargo test --release`
Expected: all green, including the bit-exact determinism tests and the gated 30k perf test (release).

- [ ] **Step 3: WASM build**

Run: `cargo build --target wasm32-unknown-unknown`
Expected: clean compile (this is the deploy target; bond changes touch WASM-compiled modules `fab`, `sim`, `editor`, `gpu`).

- [ ] **Step 4: Web smoke test (editor round-trips bonds through chain/place/delete + revert)**

Run: `python scripts/verify-web.py` (per the project's web verification flow; build the trunk/wasm bundle first if the script doesn't do it itself — see CLAUDE.md / project memory for the exact serve+build command).
Expected: the editor smoke (chain/rect/lasso/move/delete + revert round-trip) passes — this exercises `Scene.bonds` through `to_sim`/`snapshot_from_sim`/`capture_payload` end to end in a real browser.

- [ ] **Step 5: Final confirmation**

No commit needed (no source changed in this task). Report: which suites ran, debug + release status, wasm build status, web smoke status. If any step is red, stop and surface it — do not paper over a determinism-test failure.

---

## Self-Review

- **Spec coverage:** All four representations are addressed — Fab `[u32;2]` (Task 2 Step 1), Sim/parallel `(u32,u32)` (Task 2 Steps 2-5), Scene/ScenePayload (Task 2 Step 6), bench (Task 3), GPU `[u32;2]` upload boundary (Task 4). The `~7` canonicalization sites (sim `is_bonded`, editor `place`/`append_chain_bead`/`delete_selection`, resolve birth-push + `is_bonded`, substep `bonded` lookup, GPU `sort_bonds`) all collapse to `BondPair::new`. Verified by the Task 5 Step 1 grep gate.
- **Type consistency:** `BondPair::new`, `lo()`, `hi()`, `as_array()`, `contains()` are used identically everywhere they appear. `HashSet<BondPair>` is the single bond-set type post-migration; `Vec<BondPair>` is the Fab/sorted form; `[u32;2]` survives only behind the GPU bytemuck boundary.
- **No placeholders:** every step shows exact before→after code or an exact command with expected output.
- **In-flight WIP preserved:** the uncommitted `fab.rs` `Serialize` work + `fab_serializes_and_reparses` test is carried on this branch; Task 2 Step 1 updates its one array-literal assertion and confirms serde still emits `bonds = [[0, 1]]`.
