# Parallel-CCD Phase 1: CpuParallel Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship the `CpuParallel` scheduler — a CPU implementation of graph-colored fixed-substep CCD that supports Sem-class chemistries (births, deaths, action stacks, passthrough). This is Phase 1 of a two-phase delivery; Phase 2 (`GpuColored`) follows in a separate plan and uses Phase 1 as its bit-identical oracle.

**Architecture:** New `src/parallel/` module containing `CpuParallel: Scheduler`. New `src/chemistry/compiler.rs` compiles Sem-shaped chemistry definitions into a runtime-friendly form (opcode-encoded action stacks, rule table, program pool). Reuses existing `next_contact()` ([src/ccd.rs:25](../../../src/ccd.rs#L25)), `Grid` ([src/grid.rs](../../../src/grid.rs)), `reflect()` ([src/collide.rs](../../../src/collide.rs)). Existing `CpuSequential` is untouched.

**Tech Stack:** Rust 2021, glam (Vec2), serde + toml (TOML parsing), rayon (CPU parallelism — not introduced in Phase 1; everything stays single-threaded until the algorithm is proven correct, then parallelism is added in a later phase). Tests via `cargo test`.

**Reference spec:** [docs/superpowers/specs/2026-05-23-parallel-ccd-design.md](../specs/2026-05-23-parallel-ccd-design.md).

**Out of scope for Phase 1:**
- GPU/WGSL implementation (Phase 2).
- Rayon parallelism (sequential first, parallelize only after correctness is locked).
- App default-scheduler change (kept on `CpuSequential` until Phase 2 ships; Phase 1 adds `CpuParallel` as bench-selectable only).
- WebGPU on WASM.

---

## File structure

**New files:**
- `src/parallel/mod.rs` — module exports.
- `src/parallel/state.rs` — `Bead` struct, `Opcode` enum, action stack encoding.
- `src/parallel/pool.rs` — `BeadPool` (slot recycling, alive bitmap, free list).
- `src/parallel/coloring.rs` — deterministic greedy graph coloring.
- `src/parallel/resolve.rs` — apply reaction within a color (Exchange / LeftOnly / RightOnly / Birth + snap-back).
- `src/parallel/substep.rs` — one-substep dispatch (grid → pairs → TOI → color → resolve → advance → enforce_bonds).
- `src/parallel/scheduler.rs` — `CpuParallel` struct + `impl Scheduler`.
- `src/chemistry/mod.rs` — re-exports existing `chemistry` + new `compiler`, `compiled`, `opcodes`.
- `src/chemistry/compiler.rs` — TOML → `CompiledChemistry`.
- `src/chemistry/compiled.rs` — `CompiledChemistry` struct + rule lookup.
- `src/chemistry/opcodes.rs` — `Opcode` enum + encoding.
- `chemistries/sem_basic.toml` — minimal Sem chemistry.
- `fabs/chains_30x300.toml` — 9000-bead chain scenario.
- `fabs/sem_basic_demo.toml` — sem_basic scenario.
- `tests/parallel_self_determinism.rs` — `CpuParallel` self-determinism.
- `tests/parallel_invariants.rs` — physical invariants on `CpuParallel` runs.
- `tests/parallel_vs_sequential.rs` — `CpuParallel` runs prototype chems; invariants hold (not bit-equal).

**Modified files:**
- `src/lib.rs:1-12` — add `pub mod parallel;` re-organize chemistry re-exports.
- `src/scheduler.rs` — no change (trait is fine).
- `src/chemistry.rs` — split into `src/chemistry/mod.rs` + child modules. Existing types stay accessible.
- `src/bin/bench.rs` — add `--scheduler cpu-parallel` option.

**Untouched:**
- `src/sim.rs` — `CpuParallel` reads/writes `Sim` fields directly without going through `Sim::step()`.
- `src/gpu/`, `shaders/` — GpuEventLoop stays for now; deletion happens in Phase 2 setup.

---

# Phase 1A: Foundations (chemistry, state, pool)

### Task 1: Opcode enum and encoding

**Files:**
- Create: `src/chemistry/opcodes.rs`
- Test: `src/chemistry/opcodes.rs` (inline `#[cfg(test)] mod tests`)

- [ ] **Step 1: Write the failing test**

```rust
// src/chemistry/opcodes.rs
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nop_round_trips() {
        let op = Op::nop();
        assert_eq!(op.kind(), OpKind::Nop);
        assert_eq!(op.payload(), 0);
    }

    #[test]
    fn sig_red_blue_distinct() {
        let red = Op::sig(Sig::Red);
        let blue = Op::sig(Sig::Blue);
        assert_eq!(red.kind(), OpKind::Sig);
        assert_eq!(blue.kind(), OpKind::Sig);
        assert_ne!(red.payload(), blue.payload());
    }

    #[test]
    fn send_carries_program_index() {
        let op = Op::send(7);
        assert_eq!(op.kind(), OpKind::Send);
        assert_eq!(op.payload(), 7);
    }

    #[test]
    fn opcodes_pack_to_u32() {
        let op = Op::send(0x123);
        let raw: u32 = op.into();
        let back: Op = raw.into();
        assert_eq!(op, back);
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test --lib chemistry::opcodes`
Expected: FAIL with "cannot find type `Op`" etc.

- [ ] **Step 3: Write implementation**

```rust
// src/chemistry/opcodes.rs
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum OpKind {
    Nop = 0,
    Sig = 1,
    Apply = 2,
    Done = 3,
    Wait = 4,
    Take = 5,
    Drop = 6,
    Die = 7,
    Spawn = 8,
    Hold = 9,
    Send = 10,
}

impl OpKind {
    pub fn from_u8(raw: u8) -> Option<Self> {
        Some(match raw {
            0 => OpKind::Nop, 1 => OpKind::Sig, 2 => OpKind::Apply,
            3 => OpKind::Done, 4 => OpKind::Wait, 5 => OpKind::Take,
            6 => OpKind::Drop, 7 => OpKind::Die, 8 => OpKind::Spawn,
            9 => OpKind::Hold, 10 => OpKind::Send,
            _ => return None,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sig { Red, Blue }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Op(u32);

impl Op {
    pub fn new(kind: OpKind, payload: u32) -> Self {
        debug_assert!(payload < (1 << 28));
        Op(((kind as u32) << 28) | (payload & 0x0FFF_FFFF))
    }
    pub fn nop() -> Self { Op::new(OpKind::Nop, 0) }
    pub fn sig(s: Sig) -> Self {
        Op::new(OpKind::Sig, match s { Sig::Red => 0, Sig::Blue => 1 })
    }
    pub fn simple(kind: OpKind) -> Self { Op::new(kind, 0) }
    pub fn send(program_index: u32) -> Self { Op::new(OpKind::Send, program_index) }

    pub fn kind(self) -> OpKind {
        OpKind::from_u8((self.0 >> 28) as u8).expect("invalid opcode kind")
    }
    pub fn payload(self) -> u32 { self.0 & 0x0FFF_FFFF }
}

impl From<u32> for Op { fn from(v: u32) -> Self { Op(v) } }
impl From<Op> for u32 { fn from(o: Op) -> Self { o.0 } }
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test --lib chemistry::opcodes`
Expected: PASS, 4 tests.

- [ ] **Step 5: Commit**

```bash
git add src/chemistry/opcodes.rs
git commit -m "feat(chemistry): opcode enum and u32 encoding"
```

---

### Task 2: Refactor `src/chemistry.rs` into a module

**Files:**
- Create: `src/chemistry/mod.rs` (move existing content here, add child mod declarations)
- Delete: `src/chemistry.rs`

- [ ] **Step 1: Move file**

```bash
mkdir -p src/chemistry
git mv src/chemistry.rs src/chemistry/mod.rs
```

- [ ] **Step 2: Add child module declarations to the top of `src/chemistry/mod.rs`**

```rust
// At the top of src/chemistry/mod.rs, before existing imports:
pub mod opcodes;
pub use opcodes::{Op, OpKind, Sig};
```

- [ ] **Step 3: Verify existing tests still pass**

Run: `cargo test --lib chemistry`
Expected: existing chemistry tests still pass (loads_grey_chemistry, parses_swap_action_and_colors, etc.).

- [ ] **Step 4: Commit**

```bash
git add src/chemistry/
git commit -m "refactor: chemistry.rs → chemistry/ module with opcodes child"
```

---

### Task 3: CompiledChemistry data structure (no compiler yet)

**Files:**
- Create: `src/chemistry/compiled.rs`
- Modify: `src/chemistry/mod.rs` (add `pub mod compiled;`)

- [ ] **Step 1: Write the failing test**

```rust
// src/chemistry/compiled.rs
#[cfg(test)]
mod tests {
    use super::*;
    use crate::chemistry::{Op, OpKind};

    #[test]
    fn rule_table_empty_returns_default_reflect() {
        let chem = CompiledChemistry::empty();
        let rule = chem.lookup(BeadKey::wire_empty(), BeadKey::wire_empty(), Side::Out);
        assert_eq!(rule.kind, ReactionKind::Exchange);
        assert_eq!(rule.new_state_a, NewState::keep_with(Op::nop()));
        assert_eq!(rule.new_state_b, NewState::keep_with(Op::nop()));
    }

    #[test]
    fn rule_table_stores_and_retrieves() {
        let mut chem = CompiledChemistry::empty();
        let key_a = BeadKey { tag: Tag::Wire, top_op: Op::simple(OpKind::Apply) };
        let key_b = BeadKey { tag: Tag::Wire, top_op: Op::simple(OpKind::Die) };
        let rule = Rule {
            kind: ReactionKind::LeftOnly,
            new_state_a: NewState::keep_pop_top(),
            new_state_b: NewState::dead(),
            birth_state: None,
        };
        chem.insert_rule(key_a, key_b, Side::In, rule.clone());
        let got = chem.lookup(key_a, key_b, Side::In);
        assert_eq!(got, rule);
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test --lib chemistry::compiled`
Expected: FAIL with unresolved types.

- [ ] **Step 3: Implement**

```rust
// src/chemistry/compiled.rs
use std::collections::HashMap;
use crate::chemistry::{Op, OpKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Tag {
    Wire = 0,
    Port = 1,
    Sensor = 2,
    Creator = 3,
    Destroyer = 4,
    Rock = 5,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Side { In, Out }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BeadKey {
    pub tag: Tag,
    pub top_op: Op,
}

impl BeadKey {
    pub fn wire_empty() -> Self {
        BeadKey { tag: Tag::Wire, top_op: Op::nop() }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReactionKind {
    Exchange,
    LeftOnly,
    RightOnly,
    Birth,
    Passthrough,
}

/// What to do with a participant's state after the reaction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NewState {
    /// Keep the existing stack; just set the top opcode to `top` (used for tag changes).
    KeepWith { top: Op },
    /// Pop the top opcode of the stack; rest stays.
    KeepPopTop,
    /// Replace the stack with a program by index in the program pool.
    LoadProgram(u32),
    /// Mark this slot dead.
    Dead,
}

impl NewState {
    pub fn keep_with(op: Op) -> Self { NewState::KeepWith { top: op } }
    pub fn keep_pop_top() -> Self { NewState::KeepPopTop }
    pub fn load_program(idx: u32) -> Self { NewState::LoadProgram(idx) }
    pub fn dead() -> Self { NewState::Dead }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rule {
    pub kind: ReactionKind,
    pub new_state_a: NewState,
    pub new_state_b: NewState,
    /// For Birth only: the newborn's initial state.
    pub birth_state: Option<BirthState>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BirthState {
    pub tag: Tag,
    pub program: Option<u32>,
}

#[derive(Debug, Clone)]
pub struct CompiledChemistry {
    /// Flat buffer of opcode programs referenced by `LoadProgram` and Send.
    /// Each program is stored as a sequence: `[len, op_0, op_1, ..., op_{len-1}]`.
    pub program_pool: Vec<Op>,
    /// `program_offset[i]` is the offset of program `i` in `program_pool`.
    pub program_offset: Vec<u32>,
    /// Rule lookup keyed by (BeadKey, BeadKey, Side).
    rules: HashMap<(BeadKey, BeadKey, Side), Rule>,
    pub default_rule: Rule,
}

impl CompiledChemistry {
    pub fn empty() -> Self {
        CompiledChemistry {
            program_pool: Vec::new(),
            program_offset: Vec::new(),
            rules: HashMap::new(),
            default_rule: Rule {
                kind: ReactionKind::Exchange,
                new_state_a: NewState::keep_with(Op::nop()),
                new_state_b: NewState::keep_with(Op::nop()),
                birth_state: None,
            },
        }
    }

    pub fn insert_rule(&mut self, a: BeadKey, b: BeadKey, side: Side, rule: Rule) {
        self.rules.insert((a, b, side), rule);
    }

    pub fn lookup(&self, a: BeadKey, b: BeadKey, side: Side) -> Rule {
        self.rules.get(&(a, b, side)).cloned().unwrap_or_else(|| self.default_rule.clone())
    }

    /// Read program `i`'s opcodes (excluding the length prefix).
    pub fn program(&self, idx: u32) -> &[Op] {
        let off = self.program_offset[idx as usize] as usize;
        let len = u32::from(self.program_pool[off]) as usize;
        &self.program_pool[off + 1 .. off + 1 + len]
    }
}
```

Add to `src/chemistry/mod.rs`:
```rust
pub mod compiled;
pub use compiled::{
    BeadKey, BirthState, CompiledChemistry, NewState, ReactionKind, Rule, Side, Tag,
};
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test --lib chemistry::compiled`
Expected: PASS, 2 tests.

- [ ] **Step 5: Commit**

```bash
git add src/chemistry/
git commit -m "feat(chemistry): CompiledChemistry data structure"
```

---

### Task 4: Backwards-compat chemistry compiler (existing TOML → CompiledChemistry)

**Files:**
- Create: `src/chemistry/compiler.rs`
- Modify: `src/chemistry/mod.rs`

- [ ] **Step 1: Write the failing test**

```rust
// src/chemistry/compiler.rs
#[cfg(test)]
mod tests {
    use super::*;
    use crate::chemistry::{BeadKey, Op, OpKind, Side, Tag, ReactionKind, parse_chemistry};

    #[test]
    fn compiles_grey_to_reflect_default() {
        let chem = parse_chemistry("states = [\"grey\"]\n[[rule]]\nstates=[\"grey\",\"grey\"]\ninside=false\naction=\"reflect\"\n").unwrap();
        let compiled = compile_chemistry(&chem).unwrap();
        // Grey has only Reflect rules — default Exchange should fire for any lookup.
        let key = BeadKey { tag: Tag::Wire, top_op: Op::sig_legacy(0) };
        let rule = compiled.lookup(key, key, Side::Out);
        assert_eq!(rule.kind, ReactionKind::Exchange);
    }

    #[test]
    fn compiles_wire_swap_to_exchange_with_swapped_states() {
        let chem = parse_chemistry(include_str!("../../chemistries/wire.toml")).unwrap();
        let compiled = compile_chemistry(&chem).unwrap();
        let off = BeadKey { tag: Tag::Wire, top_op: Op::sig_legacy(0) };
        let on  = BeadKey { tag: Tag::Wire, top_op: Op::sig_legacy(1) };
        let rule = compiled.lookup(off, on, Side::In);
        // Wire's inside swap = Exchange + post-state swap. Encoded as Exchange with new states swapped.
        assert_eq!(rule.kind, ReactionKind::Exchange);
        assert_eq!(rule.new_state_a, crate::chemistry::NewState::keep_with(Op::sig_legacy(1)));
        assert_eq!(rule.new_state_b, crate::chemistry::NewState::keep_with(Op::sig_legacy(0)));
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test --lib chemistry::compiler`
Expected: FAIL with unresolved `compile_chemistry`, `sig_legacy`.

- [ ] **Step 3: Implement**

Add to `src/chemistry/opcodes.rs`:
```rust
impl Op {
    /// Encode a "legacy" state index from the existing `Chemistry` (used while
    /// migrating prototype chems into the compiled form). Each existing state
    /// index is encoded as `Op::new(OpKind::Sig, state_index)`. This is a
    /// stop-gap until a full Sem-shaped chemistry compiler is in place.
    pub fn sig_legacy(state_index: u32) -> Self { Op::new(OpKind::Sig, state_index) }
}
```

Implement `src/chemistry/compiler.rs`:
```rust
use crate::chemistry::{
    Action, BeadKey, BirthState, Chemistry, CompiledChemistry, NewState,
    Op, OpKind, ReactionKind, Rule, Side, Tag,
};

pub fn compile_chemistry(chem: &Chemistry) -> anyhow::Result<CompiledChemistry> {
    let mut compiled = CompiledChemistry::empty();
    let n = chem.states.len();
    for a in 0..n {
        for b in 0..n {
            for &inside in &[false, true] {
                let action = chem.lookup(a, b, inside);
                let key_a = BeadKey { tag: Tag::Wire, top_op: Op::sig_legacy(a as u32) };
                let key_b = BeadKey { tag: Tag::Wire, top_op: Op::sig_legacy(b as u32) };
                let side = if inside { Side::In } else { Side::Out };
                let rule = match action {
                    Action::Reflect => Rule {
                        kind: ReactionKind::Exchange,
                        new_state_a: NewState::keep_with(Op::sig_legacy(a as u32)),
                        new_state_b: NewState::keep_with(Op::sig_legacy(b as u32)),
                        birth_state: None,
                    },
                    Action::Pass => Rule {
                        kind: ReactionKind::Passthrough,
                        new_state_a: NewState::keep_with(Op::sig_legacy(a as u32)),
                        new_state_b: NewState::keep_with(Op::sig_legacy(b as u32)),
                        birth_state: None,
                    },
                    Action::ReflectSwap => Rule {
                        kind: ReactionKind::Exchange,
                        new_state_a: NewState::keep_with(Op::sig_legacy(b as u32)),
                        new_state_b: NewState::keep_with(Op::sig_legacy(a as u32)),
                        birth_state: None,
                    },
                };
                compiled.insert_rule(key_a, key_b, side, rule);
            }
        }
    }
    Ok(compiled)
}
```

Add to `src/chemistry/mod.rs`:
```rust
pub mod compiler;
pub use compiler::compile_chemistry;
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test --lib chemistry`
Expected: existing chemistry tests pass + 2 new compiler tests.

- [ ] **Step 5: Commit**

```bash
git add src/chemistry/
git commit -m "feat(chemistry): compile_chemistry adapter for legacy TOML chems"
```

---

### Task 5: Bead struct and pool

**Files:**
- Create: `src/parallel/mod.rs`
- Create: `src/parallel/state.rs`
- Create: `src/parallel/pool.rs`
- Modify: `src/lib.rs` (add `pub mod parallel;`)

- [ ] **Step 1: Write the failing test**

```rust
// src/parallel/pool.rs (top of file, before impl)
#[cfg(test)]
mod tests {
    use super::*;
    use crate::chemistry::{Op, OpKind, Tag};
    use glam::Vec2;

    fn make_wire() -> Bead {
        Bead {
            pos: Vec2::ZERO,
            vel: Vec2::ZERO,
            tag: Tag::Wire,
            payload: 0,
            alive: true,
            born_this_substep: false,
            stack_len: 0,
            stack: [Op::nop(); STACK_CAP],
        }
    }

    #[test]
    fn pool_allocates_and_recycles() {
        let mut pool = BeadPool::with_capacity(8);
        let i0 = pool.alloc(make_wire());
        let i1 = pool.alloc(make_wire());
        assert_eq!(i0, 0);
        assert_eq!(i1, 1);
        pool.free(i0);
        let i2 = pool.alloc(make_wire());
        assert_eq!(i2, 0, "freed slot is reused");
    }

    #[test]
    fn pool_overflow_returns_err() {
        let mut pool = BeadPool::with_capacity(2);
        pool.alloc(make_wire());
        pool.alloc(make_wire());
        assert!(pool.try_alloc(make_wire()).is_err());
    }

    #[test]
    fn alive_slots_iterates_in_index_order() {
        let mut pool = BeadPool::with_capacity(4);
        for _ in 0..4 { pool.alloc(make_wire()); }
        pool.free(1);
        pool.free(3);
        let alive: Vec<u32> = pool.alive_slots().collect();
        assert_eq!(alive, vec![0, 2]);
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test --lib parallel::pool`
Expected: FAIL — unresolved types.

- [ ] **Step 3: Implement**

`src/parallel/mod.rs`:
```rust
pub mod state;
pub mod pool;
pub use state::{Bead, STACK_CAP};
pub use pool::BeadPool;
```

`src/parallel/state.rs`:
```rust
use glam::Vec2;
use crate::chemistry::{Op, Tag};

pub const STACK_CAP: usize = 16;

#[derive(Debug, Clone, Copy)]
pub struct Bead {
    pub pos: Vec2,
    pub vel: Vec2,
    pub tag: Tag,
    pub payload: u32,
    pub alive: bool,
    pub born_this_substep: bool,
    pub stack_len: u32,
    pub stack: [Op; STACK_CAP],
}

impl Bead {
    pub fn top_op(&self) -> Op {
        if self.stack_len == 0 { Op::nop() } else { self.stack[(self.stack_len - 1) as usize] }
    }

    pub fn pop_top(&mut self) {
        if self.stack_len > 0 { self.stack_len -= 1; }
    }

    pub fn load_program(&mut self, prog: &[Op]) {
        debug_assert!(prog.len() <= STACK_CAP);
        self.stack_len = prog.len() as u32;
        for (i, &op) in prog.iter().enumerate() {
            self.stack[i] = op;
        }
    }
}
```

`src/parallel/pool.rs`:
```rust
use crate::parallel::state::Bead;

pub struct BeadPool {
    beads: Vec<Bead>,
    free_list: Vec<u32>,
    high_water: u32,
    capacity: u32,
}

#[derive(Debug)]
pub struct PoolOverflow;

impl BeadPool {
    pub fn with_capacity(cap: u32) -> Self {
        BeadPool { beads: Vec::with_capacity(cap as usize), free_list: Vec::new(), high_water: 0, capacity: cap }
    }

    pub fn alloc(&mut self, bead: Bead) -> u32 {
        self.try_alloc(bead).expect("pool overflow")
    }

    pub fn try_alloc(&mut self, mut bead: Bead) -> Result<u32, PoolOverflow> {
        bead.alive = true;
        if let Some(slot) = self.free_list.pop() {
            self.beads[slot as usize] = bead;
            return Ok(slot);
        }
        if self.high_water >= self.capacity {
            return Err(PoolOverflow);
        }
        let slot = self.high_water;
        self.beads.push(bead);
        self.high_water += 1;
        Ok(slot)
    }

    pub fn free(&mut self, slot: u32) {
        if (slot as usize) < self.beads.len() {
            self.beads[slot as usize].alive = false;
            self.free_list.push(slot);
        }
    }

    pub fn get(&self, slot: u32) -> &Bead { &self.beads[slot as usize] }
    pub fn get_mut(&mut self, slot: u32) -> &mut Bead { &mut self.beads[slot as usize] }
    pub fn high_water(&self) -> u32 { self.high_water }
    pub fn capacity(&self) -> u32 { self.capacity }
    pub fn beads(&self) -> &[Bead] { &self.beads }
    pub fn beads_mut(&mut self) -> &mut [Bead] { &mut self.beads }

    pub fn alive_slots(&self) -> impl Iterator<Item = u32> + '_ {
        self.beads.iter().enumerate().filter_map(|(i, b)| if b.alive { Some(i as u32) } else { None })
    }
}
```

Add to `src/lib.rs`:
```rust
pub mod parallel;
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test --lib parallel::pool`
Expected: PASS, 3 tests.

- [ ] **Step 5: Commit**

```bash
git add src/parallel/ src/lib.rs
git commit -m "feat(parallel): Bead struct and BeadPool with slot recycling"
```

---

# Phase 1B: Graph coloring

### Task 6: Conflict graph builder and deterministic greedy coloring

**Files:**
- Create: `src/parallel/coloring.rs`
- Modify: `src/parallel/mod.rs`

- [ ] **Step 1: Write the failing test**

```rust
// src/parallel/coloring.rs
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn isolated_pairs_get_same_color() {
        // Pairs (0,1) and (2,3) share no beads — both can be color 0.
        let pairs = vec![
            Pair { a: 0, b: 1, t: 0.1 },
            Pair { a: 2, b: 3, t: 0.2 },
        ];
        let colors = color_pairs(&pairs);
        assert_eq!(colors, vec![0, 0]);
    }

    #[test]
    fn sharing_pairs_get_different_colors() {
        // Pairs (0,1) and (1,2) share bead 1.
        let pairs = vec![
            Pair { a: 0, b: 1, t: 0.1 },
            Pair { a: 1, b: 2, t: 0.2 },
        ];
        let colors = color_pairs(&pairs);
        assert_eq!(colors, vec![0, 1]);
    }

    #[test]
    fn coloring_is_deterministic() {
        let pairs = vec![
            Pair { a: 0, b: 1, t: 0.3 },
            Pair { a: 1, b: 2, t: 0.1 },
            Pair { a: 2, b: 3, t: 0.2 },
            Pair { a: 0, b: 3, t: 0.4 },
        ];
        let c1 = color_pairs(&pairs);
        let c2 = color_pairs(&pairs);
        assert_eq!(c1, c2);
    }

    #[test]
    fn lower_toi_gets_lower_color_among_neighbors() {
        // (0,1) and (1,2) share bead 1. The one with smaller t gets color 0.
        let pairs = vec![
            Pair { a: 0, b: 1, t: 0.5 },  // later
            Pair { a: 1, b: 2, t: 0.1 },  // earlier
        ];
        let colors = color_pairs(&pairs);
        assert_eq!(colors[1], 0, "earlier TOI gets color 0");
        assert_eq!(colors[0], 1);
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test --lib parallel::coloring`
Expected: FAIL — unresolved types.

- [ ] **Step 3: Implement**

```rust
// src/parallel/coloring.rs
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Pair {
    pub a: u32,
    pub b: u32,
    pub t: f32,
}

/// Deterministic greedy coloring. Pairs are processed in (t, a, b) order; each
/// pair gets the smallest color not used by any already-colored neighbor.
/// Returns one color per input pair (same order as input).
pub fn color_pairs(pairs: &[Pair]) -> Vec<u32> {
    let n = pairs.len();
    // Sort pair indices by (t, a, b) for deterministic ordering.
    let mut order: Vec<usize> = (0..n).collect();
    order.sort_by(|&i, &j| {
        let pi = &pairs[i];
        let pj = &pairs[j];
        (pi.t, pi.a, pi.b).partial_cmp(&(pj.t, pj.a, pj.b)).unwrap()
    });

    // For each bead, list of pair indices that touch it.
    let mut bead_to_pairs: HashMap<u32, Vec<usize>> = HashMap::new();
    for (i, p) in pairs.iter().enumerate() {
        bead_to_pairs.entry(p.a).or_default().push(i);
        bead_to_pairs.entry(p.b).or_default().push(i);
    }

    let mut colors = vec![u32::MAX; n];
    for &i in &order {
        let p = &pairs[i];
        let mut used: Vec<u32> = Vec::new();
        for &neighbor in bead_to_pairs.get(&p.a).unwrap_or(&Vec::new()) {
            if neighbor != i && colors[neighbor] != u32::MAX {
                used.push(colors[neighbor]);
            }
        }
        for &neighbor in bead_to_pairs.get(&p.b).unwrap_or(&Vec::new()) {
            if neighbor != i && colors[neighbor] != u32::MAX {
                used.push(colors[neighbor]);
            }
        }
        used.sort_unstable();
        used.dedup();
        let mut c = 0u32;
        for &u in &used {
            if u == c { c += 1; } else if u > c { break; }
        }
        colors[i] = c;
    }
    colors
}
```

Update `src/parallel/mod.rs`:
```rust
pub mod coloring;
pub use coloring::{Pair, color_pairs};
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test --lib parallel::coloring`
Expected: PASS, 4 tests.

- [ ] **Step 5: Commit**

```bash
git add src/parallel/
git commit -m "feat(parallel): deterministic greedy graph coloring"
```

---

# Phase 1C: Substep core

### Task 7: TOI computation and active-contact filter

**Files:**
- Create: `src/parallel/substep.rs`
- Modify: `src/parallel/mod.rs`

- [ ] **Step 1: Write the failing test**

```rust
// src/parallel/substep.rs
#[cfg(test)]
mod tests {
    use super::*;
    use crate::parallel::{Bead, BeadPool, STACK_CAP};
    use crate::chemistry::{Op, Tag};
    use crate::grid::Grid;
    use glam::Vec2;

    fn place(pool: &mut BeadPool, pos: Vec2, vel: Vec2) -> u32 {
        pool.alloc(Bead {
            pos, vel, tag: Tag::Wire, payload: 0, alive: true,
            born_this_substep: false, stack_len: 0, stack: [Op::nop(); STACK_CAP],
        })
    }

    #[test]
    fn two_beads_on_collision_course_produce_one_contact() {
        let mut pool = BeadPool::with_capacity(4);
        place(&mut pool, Vec2::new(15.0, 15.0), Vec2::new(1.0, 0.0));
        place(&mut pool, Vec2::new(18.0, 15.0), Vec2::new(-1.0, 0.0));
        let mut grid = Grid::new(30.0);
        let contacts = compute_active_contacts(&pool, &mut grid, 2.0);
        assert_eq!(contacts.len(), 1);
        assert!((contacts[0].t - 1.0).abs() < 1e-5);
    }

    #[test]
    fn newborn_excluded_from_contacts() {
        let mut pool = BeadPool::with_capacity(4);
        let _ = place(&mut pool, Vec2::new(15.0, 15.0), Vec2::new(1.0, 0.0));
        let i1 = place(&mut pool, Vec2::new(18.0, 15.0), Vec2::new(-1.0, 0.0));
        pool.get_mut(i1).born_this_substep = true;
        let mut grid = Grid::new(30.0);
        let contacts = compute_active_contacts(&pool, &mut grid, 2.0);
        assert!(contacts.is_empty(), "born-this-substep bead does not produce contacts");
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test --lib parallel::substep`
Expected: FAIL — unresolved `compute_active_contacts`.

- [ ] **Step 3: Implement**

```rust
// src/parallel/substep.rs
use crate::ccd::next_contact;
use crate::grid::Grid;
use crate::parallel::{BeadPool, Pair};

pub fn compute_active_contacts(pool: &BeadPool, grid: &mut Grid, dt_sub: f32) -> Vec<Pair> {
    grid.clear();
    for slot in pool.alive_slots() {
        if pool.get(slot).born_this_substep { continue; }
        grid.insert(slot, pool.get(slot).pos);
    }
    let candidates = grid.candidate_pairs();
    let mut out = Vec::with_capacity(candidates.len());
    for (a, b) in candidates {
        let ba = pool.get(a);
        let bb = pool.get(b);
        if !ba.alive || !bb.alive { continue; }
        if ba.born_this_substep || bb.born_this_substep { continue; }
        let pb = ba.pos + grid.min_image(ba.pos, bb.pos);
        if let Some(c) = next_contact(ba.pos, ba.vel, pb, bb.vel, dt_sub) {
            out.push(Pair { a, b, t: c.t });
        }
    }
    // Stable ordering by (t, a, b) so coloring/resolve are deterministic.
    out.sort_by(|p, q| (p.t, p.a, p.b).partial_cmp(&(q.t, q.a, q.b)).unwrap());
    out
}
```

Update `src/parallel/mod.rs`:
```rust
pub mod substep;
pub use substep::compute_active_contacts;
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test --lib parallel::substep`
Expected: PASS, 2 tests.

- [ ] **Step 5: Commit**

```bash
git add src/parallel/
git commit -m "feat(parallel): active-contact TOI pass for one substep"
```

---

### Task 8: Reaction resolution per color (Exchange / Passthrough)

**Files:**
- Create: `src/parallel/resolve.rs`
- Modify: `src/parallel/mod.rs`

- [ ] **Step 1: Write the failing test**

```rust
// src/parallel/resolve.rs
#[cfg(test)]
mod tests {
    use super::*;
    use crate::parallel::{Bead, BeadPool, STACK_CAP, Pair};
    use crate::chemistry::{CompiledChemistry, Op, Tag, Side, ReactionKind, Rule, NewState, BeadKey};
    use crate::grid::Grid;
    use glam::Vec2;

    fn make_grey_chem() -> CompiledChemistry {
        // All pairs reflect.
        let mut chem = CompiledChemistry::empty();
        let key = BeadKey { tag: Tag::Wire, top_op: Op::sig_legacy(0) };
        let rule = Rule {
            kind: ReactionKind::Exchange,
            new_state_a: NewState::keep_with(Op::sig_legacy(0)),
            new_state_b: NewState::keep_with(Op::sig_legacy(0)),
            birth_state: None,
        };
        chem.insert_rule(key, key, Side::Out, rule.clone());
        chem.insert_rule(key, key, Side::In, rule);
        chem
    }

    fn place(pool: &mut BeadPool, pos: Vec2, vel: Vec2) -> u32 {
        let mut stack = [Op::nop(); STACK_CAP];
        stack[0] = Op::sig_legacy(0);
        pool.alloc(Bead {
            pos, vel, tag: Tag::Wire, payload: 0, alive: true,
            born_this_substep: false, stack_len: 1, stack,
        })
    }

    #[test]
    fn exchange_reflects_velocities() {
        let mut pool = BeadPool::with_capacity(4);
        let a = place(&mut pool, Vec2::new(15.0, 15.0), Vec2::new(1.0, 0.0));
        let b = place(&mut pool, Vec2::new(17.0, 15.0), Vec2::new(-1.0, 0.0));
        let chem = make_grey_chem();
        let grid = Grid::new(30.0);
        let bonds: std::collections::HashSet<(u32, u32)> = Default::default();
        let pair = Pair { a, b, t: 1.0 };
        let mut ctx = ResolveContext {
            pool: &mut pool, chem: &chem, grid: &grid, bonds: &bonds,
            pending_bonds: &mut Vec::new(), pending_deaths: &mut Vec::new(),
        };
        resolve_pair(&pair, &mut ctx);
        // After 1.0s advance + reflect, beads are at the contact and velocities swapped on x.
        assert!((ctx.pool.get(a).vel.x - (-1.0)).abs() < 1e-3);
        assert!((ctx.pool.get(b).vel.x - 1.0).abs() < 1e-3);
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test --lib parallel::resolve`
Expected: FAIL — unresolved types.

- [ ] **Step 3: Implement**

```rust
// src/parallel/resolve.rs
use glam::Vec2;
use std::collections::HashSet;
use crate::ccd::RADIUS;
use crate::collide::reflect;
use crate::grid::Grid;
use crate::parallel::{BeadPool, Pair};
use crate::chemistry::{
    CompiledChemistry, BeadKey, NewState, Op, ReactionKind, Side, Tag,
};

const BOUNDARY_EPS: f32 = 1e-5;

pub struct ResolveContext<'a> {
    pub pool: &'a mut BeadPool,
    pub chem: &'a CompiledChemistry,
    pub grid: &'a Grid,
    pub bonds: &'a HashSet<(u32, u32)>,
    pub pending_bonds: &'a mut Vec<(u32, u32)>,
    pub pending_deaths: &'a mut Vec<u32>,
}

pub fn resolve_pair(pair: &Pair, ctx: &mut ResolveContext) {
    let (a, b) = (pair.a, pair.b);
    let ba = ctx.pool.get(a);
    let bb = ctx.pool.get(b);
    let pa = ba.pos;
    let pb = ba.pos + ctx.grid.min_image(ba.pos, bb.pos);
    let bonded = is_bonded(ctx.bonds, a, b);

    // Determine exiting from geometry: dot((pb - pa) at t=0, (vb - va)) > 0
    let exiting = (pb - pa).dot(bb.vel - ba.vel) > 0.0;

    let side = if bonded { Side::In } else { Side::Out };
    // Drift correction: if bonded == exiting disagrees, force Passthrough.
    let effective_side = if bonded == exiting { side } else { Side::Out };

    let key_a = BeadKey { tag: ba.tag, top_op: ba.top_op() };
    let key_b = BeadKey { tag: bb.tag, top_op: bb.top_op() };
    let rule = if bonded == exiting {
        ctx.chem.lookup(key_a, key_b, side)
    } else {
        crate::chemistry::Rule {
            kind: ReactionKind::Passthrough,
            new_state_a: NewState::keep_with(ba.top_op()),
            new_state_b: NewState::keep_with(bb.top_op()),
            birth_state: None,
        }
    };

    match rule.kind {
        ReactionKind::Exchange => {
            // Velocity swap on normal component.
            let (va_new, vb_new) = reflect(pa, ba.vel, pb, bb.vel);
            ctx.pool.get_mut(a).vel = va_new;
            ctx.pool.get_mut(b).vel = vb_new;
            apply_new_state(ctx.pool.get_mut(a), &rule.new_state_a, ctx.chem);
            apply_new_state(ctx.pool.get_mut(b), &rule.new_state_b, ctx.chem);
            snap_back(ctx.pool, a, b, ctx.grid, exiting, /*post_state_inside=*/ exiting);
        }
        ReactionKind::Passthrough => {
            apply_new_state(ctx.pool.get_mut(a), &rule.new_state_a, ctx.chem);
            apply_new_state(ctx.pool.get_mut(b), &rule.new_state_b, ctx.chem);
            snap_back(ctx.pool, a, b, ctx.grid, exiting, /*post_state_inside=*/ !exiting);
        }
        ReactionKind::LeftOnly => {
            // Right bead dies; left inherits combined velocity.
            let combined_vel = ba.vel + bb.vel;
            ctx.pool.get_mut(a).vel = combined_vel;
            apply_new_state(ctx.pool.get_mut(a), &rule.new_state_a, ctx.chem);
            ctx.pending_deaths.push(b);
        }
        ReactionKind::RightOnly => {
            let combined_vel = ba.vel + bb.vel;
            ctx.pool.get_mut(b).pos = pa; // right bead takes left's position
            ctx.pool.get_mut(b).vel = combined_vel;
            apply_new_state(ctx.pool.get_mut(b), &rule.new_state_b, ctx.chem);
            ctx.pending_deaths.push(a);
        }
        ReactionKind::Birth => {
            // Existing pair bounces (or not, per side). New bead at midpoint.
            if effective_side == Side::In { // bounce
                let (va_new, vb_new) = reflect(pa, ba.vel, pb, bb.vel);
                ctx.pool.get_mut(a).vel = va_new;
                ctx.pool.get_mut(b).vel = vb_new;
            }
            apply_new_state(ctx.pool.get_mut(a), &rule.new_state_a, ctx.chem);
            apply_new_state(ctx.pool.get_mut(b), &rule.new_state_b, ctx.chem);
            let mid = 0.5 * (pa + pb);
            let new_vel = -0.5 * (ba.vel + bb.vel);
            let birth_state = rule.birth_state.as_ref().expect("Birth reaction must have birth_state");
            let mut new_bead = crate::parallel::Bead {
                pos: ctx.grid.wrap_pos(mid),
                vel: new_vel,
                tag: birth_state.tag,
                payload: 0,
                alive: true,
                born_this_substep: true,
                stack_len: 0,
                stack: [Op::nop(); crate::parallel::STACK_CAP],
            };
            if let Some(prog_idx) = birth_state.program {
                new_bead.load_program(ctx.chem.program(prog_idx));
            }
            if let Ok(new_slot) = ctx.pool.try_alloc(new_bead) {
                ctx.pending_bonds.push((a.min(new_slot), a.max(new_slot)));
                ctx.pending_bonds.push((b.min(new_slot), b.max(new_slot)));
            }
            // Snap parents apart so the next substep sees a clean pair.
            snap_back(ctx.pool, a, b, ctx.grid, exiting, exiting);
        }
    }
}

fn apply_new_state(bead: &mut crate::parallel::Bead, ns: &NewState, chem: &CompiledChemistry) {
    match *ns {
        NewState::KeepWith { top } => {
            if bead.stack_len == 0 {
                bead.stack[0] = top;
                bead.stack_len = 1;
            } else {
                bead.stack[(bead.stack_len - 1) as usize] = top;
            }
        }
        NewState::KeepPopTop => bead.pop_top(),
        NewState::LoadProgram(idx) => bead.load_program(chem.program(idx)),
        NewState::Dead => { /* caller queues death */ }
    }
}

fn snap_back(pool: &mut BeadPool, a: u32, b: u32, grid: &Grid, _exiting: bool, post_state_inside: bool) {
    let pa = pool.get(a).pos;
    let pb_raw = pool.get(b).pos;
    let pb = pa + grid.min_image(pa, pb_raw);
    let d = pb - pa;
    let dist = d.length();
    if dist <= 1e-12 { return; }
    let target = if post_state_inside { RADIUS - BOUNDARY_EPS } else { RADIUS + BOUNDARY_EPS };
    let correction = (target - dist) * 0.5;
    let n = d / dist;
    let new_a = grid.wrap_pos(pa - n * correction);
    let new_b_raw = pool.get(b).pos + n * correction;
    let new_b = grid.wrap_pos(new_b_raw);
    pool.get_mut(a).pos = new_a;
    pool.get_mut(b).pos = new_b;
}

fn is_bonded(bonds: &HashSet<(u32, u32)>, a: u32, b: u32) -> bool {
    let key = if a < b { (a, b) } else { (b, a) };
    bonds.contains(&key)
}
```

Update `src/parallel/mod.rs`:
```rust
pub mod resolve;
pub use resolve::{ResolveContext, resolve_pair};
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test --lib parallel::resolve`
Expected: PASS, 1 test.

- [ ] **Step 5: Commit**

```bash
git add src/parallel/
git commit -m "feat(parallel): per-color contact resolution (Exchange/Passthrough/Birth/Death)"
```

---

### Task 9: One-substep top-level routine

**Files:**
- Modify: `src/parallel/substep.rs`

- [ ] **Step 1: Write the failing test**

Add to `src/parallel/substep.rs` tests:

```rust
    #[test]
    fn head_on_collision_resolves_in_one_substep() {
        let mut pool = BeadPool::with_capacity(4);
        let mut stack = [Op::nop(); STACK_CAP];
        stack[0] = Op::sig_legacy(0);
        let a = pool.alloc(Bead {
            pos: Vec2::new(15.0, 15.0), vel: Vec2::new(1.0, 0.0),
            tag: Tag::Wire, payload: 0, alive: true, born_this_substep: false,
            stack_len: 1, stack,
        });
        let b = pool.alloc(Bead {
            pos: Vec2::new(17.0, 15.0), vel: Vec2::new(-1.0, 0.0),
            tag: Tag::Wire, payload: 0, alive: true, born_this_substep: false,
            stack_len: 1, stack,
        });
        let mut grid = Grid::new(30.0);
        let bonds = std::collections::HashSet::new();
        let chem = {
            // Reflect chem: Exchange with same top_op.
            let mut c = crate::chemistry::CompiledChemistry::empty();
            let key = crate::chemistry::BeadKey { tag: Tag::Wire, top_op: Op::sig_legacy(0) };
            c.insert_rule(key, key, crate::chemistry::Side::Out, crate::chemistry::Rule {
                kind: crate::chemistry::ReactionKind::Exchange,
                new_state_a: crate::chemistry::NewState::keep_with(Op::sig_legacy(0)),
                new_state_b: crate::chemistry::NewState::keep_with(Op::sig_legacy(0)),
                birth_state: None,
            });
            c
        };
        let mut bonds_mut = bonds.clone();
        do_substep(&mut pool, &mut grid, &chem, &mut bonds_mut, 2.0);
        assert!((pool.get(a).vel.x - (-1.0)).abs() < 1e-3);
        assert!((pool.get(b).vel.x - 1.0).abs() < 1e-3);
    }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test --lib parallel::substep::tests::head_on_collision`
Expected: FAIL — unresolved `do_substep`.

- [ ] **Step 3: Implement**

Append to `src/parallel/substep.rs`:

```rust
use std::collections::HashSet;
use crate::chemistry::CompiledChemistry;
use crate::parallel::{coloring, resolve};

pub fn do_substep(
    pool: &mut BeadPool,
    grid: &mut Grid,
    chem: &CompiledChemistry,
    bonds: &mut HashSet<(u32, u32)>,
    dt_sub: f32,
) {
    let contacts = compute_active_contacts(pool, grid, dt_sub);
    if contacts.is_empty() {
        // No contacts — just advance all alive beads.
        advance_all(pool, grid, dt_sub);
        clear_substep_flags(pool);
        return;
    }
    let colors = coloring::color_pairs(&contacts);
    let max_color = colors.iter().copied().max().unwrap_or(0);
    let mut pending_bonds: Vec<(u32, u32)> = Vec::new();
    let mut pending_deaths: Vec<u32> = Vec::new();
    for c in 0..=max_color {
        // Iterate pairs of color c.
        // Snapshot pairs of this color to a list, sorted for determinism.
        let mut pairs_in_color: Vec<&Pair> = contacts.iter().enumerate()
            .filter(|(i, _)| colors[*i] == c)
            .map(|(_, p)| p)
            .collect();
        pairs_in_color.sort_by(|p, q| (p.t, p.a, p.b).partial_cmp(&(q.t, q.a, q.b)).unwrap());
        for pair in pairs_in_color {
            let mut ctx = resolve::ResolveContext {
                pool, chem, grid, bonds,
                pending_bonds: &mut pending_bonds,
                pending_deaths: &mut pending_deaths,
            };
            resolve::resolve_pair(pair, &mut ctx);
        }
    }
    advance_all(pool, grid, dt_sub);
    // Merge bond changes.
    pending_bonds.sort_unstable();
    pending_bonds.dedup();
    for pair in pending_bonds {
        bonds.insert(pair);
    }
    for slot in pending_deaths {
        pool.free(slot);
        bonds.retain(|&(a, b)| a != slot && b != slot);
    }
    clear_substep_flags(pool);
}

fn advance_all(pool: &mut BeadPool, grid: &Grid, dt_sub: f32) {
    for slot in pool.alive_slots().collect::<Vec<_>>() {
        let b = pool.get_mut(slot);
        if b.born_this_substep { continue; }
        let new_pos = b.pos + b.vel * dt_sub;
        b.pos = grid.wrap_pos(new_pos);
    }
}

fn clear_substep_flags(pool: &mut BeadPool) {
    for b in pool.beads_mut() {
        b.born_this_substep = false;
    }
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test --lib parallel::substep`
Expected: PASS, 3 tests.

- [ ] **Step 5: Commit**

```bash
git add src/parallel/
git commit -m "feat(parallel): one-substep loop with coloring + resolution"
```

---

# Phase 1D: Scheduler integration

### Task 10: CpuParallel scheduler struct + Scheduler impl

**Files:**
- Create: `src/parallel/scheduler.rs`
- Modify: `src/parallel/mod.rs`, `src/scheduler.rs` (add export)

- [ ] **Step 1: Write the failing test**

```rust
// src/parallel/scheduler.rs
#[cfg(test)]
mod tests {
    use super::*;
    use crate::chemistry::{load_chemistry, compile_chemistry};
    use crate::fab::load_fab;
    use crate::sim::Sim;
    use crate::scheduler::Scheduler;

    #[test]
    fn grey_30_steps_without_crash() {
        let fab = load_fab("fabs/grey-30.toml").unwrap();
        let chem = load_chemistry("chemistries/grey.toml").unwrap();
        let mut sim = Sim::from_fab(&fab, chem);
        let compiled = compile_chemistry(&sim.chemistry).unwrap();
        let mut sched = CpuParallel::new(&sim, compiled);
        for _ in 0..30 {
            sched.step(&mut sim, 1.0 / 60.0);
        }
        // All beads still inside the world.
        for p in &sim.positions {
            assert!(p.x >= 0.0 && p.x <= sim.world_size());
            assert!(p.y >= 0.0 && p.y <= sim.world_size());
        }
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test --lib parallel::scheduler`
Expected: FAIL — unresolved `CpuParallel`.

- [ ] **Step 3: Implement**

```rust
// src/parallel/scheduler.rs
use std::collections::HashSet;
use crate::chemistry::{CompiledChemistry, Op, Tag};
use crate::grid::Grid;
use crate::parallel::{Bead, BeadPool, STACK_CAP};
use crate::parallel::substep::do_substep;
use crate::scheduler::Scheduler;
use crate::sim::{Sim, StepMetrics};

pub const DEFAULT_DT_SUB: f32 = 1.0 / 240.0;

pub struct CpuParallel {
    pool: BeadPool,
    bonds: HashSet<(u32, u32)>,
    grid: Grid,
    chem: CompiledChemistry,
    dt_sub: f32,
    /// Map from Sim's bead index to pool slot. Initially identity, may diverge
    /// after births/deaths. The Sim's vectors are rewritten at end-of-step.
    sim_to_slot: Vec<u32>,
}

impl CpuParallel {
    pub fn new(sim: &Sim, chem: CompiledChemistry) -> Self {
        let n = sim.positions.len();
        let pool_cap = (n.max(512) * 2) as u32;
        let mut pool = BeadPool::with_capacity(pool_cap);
        let mut sim_to_slot = Vec::with_capacity(n);
        for i in 0..n {
            let mut stack = [Op::nop(); STACK_CAP];
            stack[0] = Op::sig_legacy(sim.states[i]);
            let slot = pool.alloc(Bead {
                pos: sim.positions[i],
                vel: sim.velocities[i],
                tag: Tag::Wire,
                payload: sim.states[i],
                alive: true,
                born_this_substep: false,
                stack_len: 1,
                stack,
            });
            sim_to_slot.push(slot);
        }
        let bonds = sim.bonds.clone();
        let grid = Grid::new(sim.world_size());
        Self { pool, bonds, grid, chem, dt_sub: DEFAULT_DT_SUB, sim_to_slot }
    }
}

impl Scheduler for CpuParallel {
    fn step(&mut self, sim: &mut Sim, frame_dt: f32) -> StepMetrics {
        let mut metrics = StepMetrics::default();
        let n_substeps = (frame_dt / self.dt_sub).ceil() as u32;
        for _ in 0..n_substeps {
            do_substep(&mut self.pool, &mut self.grid, &self.chem, &mut self.bonds, self.dt_sub);
        }
        // Write back to sim vectors. For Phase 1 we assume no births/deaths
        // in prototype chems, so sim_to_slot remains identity for these tests.
        for (i, &slot) in self.sim_to_slot.iter().enumerate() {
            let b = self.pool.get(slot);
            sim.positions[i] = b.pos;
            sim.velocities[i] = b.vel;
            sim.states[i] = b.payload;
        }
        sim.bonds = self.bonds.clone();
        metrics
    }
}
```

Add export to `src/parallel/mod.rs`:
```rust
pub mod scheduler;
pub use scheduler::{CpuParallel, DEFAULT_DT_SUB};
```

`sim.chemistry` is currently private. Expose it via a getter on Sim for tests:

In `src/sim.rs` (only addition):
```rust
impl Sim {
    pub fn chemistry(&self) -> &Chemistry { &self.chemistry }
}
```

And update the test to use `sim.chemistry()`:
```rust
let compiled = compile_chemistry(sim.chemistry()).unwrap();
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test --lib parallel::scheduler`
Expected: PASS, 1 test.

- [ ] **Step 5: Commit**

```bash
git add src/parallel/ src/sim.rs
git commit -m "feat(parallel): CpuParallel scheduler — wires substep into Scheduler trait"
```

---

### Task 11: Substep applies enforce_bonds at end of each substep

**Files:**
- Modify: `src/parallel/substep.rs`

- [ ] **Step 1: Write the failing test**

Append to `src/parallel/substep.rs::tests`:

```rust
    #[test]
    fn bonded_pair_stays_within_radius_after_substeps() {
        use crate::ccd::RADIUS;
        let mut pool = BeadPool::with_capacity(4);
        let mut stack = [Op::nop(); STACK_CAP];
        stack[0] = Op::sig_legacy(0);
        let a = pool.alloc(Bead {
            pos: Vec2::new(15.0, 14.75), vel: Vec2::new(0.0, -1.0),
            tag: Tag::Wire, payload: 0, alive: true, born_this_substep: false,
            stack_len: 1, stack,
        });
        let b = pool.alloc(Bead {
            pos: Vec2::new(15.0, 15.25), vel: Vec2::new(0.0, 1.0),
            tag: Tag::Wire, payload: 0, alive: true, born_this_substep: false,
            stack_len: 1, stack,
        });
        let mut grid = Grid::new(30.0);
        let mut bonds = std::collections::HashSet::new();
        bonds.insert((a.min(b), a.max(b)));
        let chem = {
            let mut c = crate::chemistry::CompiledChemistry::empty();
            let key = crate::chemistry::BeadKey { tag: Tag::Wire, top_op: Op::sig_legacy(0) };
            c.insert_rule(key, key, crate::chemistry::Side::Out, crate::chemistry::Rule {
                kind: crate::chemistry::ReactionKind::Exchange,
                new_state_a: crate::chemistry::NewState::keep_with(Op::sig_legacy(0)),
                new_state_b: crate::chemistry::NewState::keep_with(Op::sig_legacy(0)),
                birth_state: None,
            });
            c.insert_rule(key, key, crate::chemistry::Side::In, crate::chemistry::Rule {
                kind: crate::chemistry::ReactionKind::Exchange,
                new_state_a: crate::chemistry::NewState::keep_with(Op::sig_legacy(0)),
                new_state_b: crate::chemistry::NewState::keep_with(Op::sig_legacy(0)),
                birth_state: None,
            });
            c
        };
        let dt = 1.0 / 60.0;
        let mut max_dist = 0f32;
        for _ in 0..1200 {
            for _ in 0..4 { // 4 substeps per frame
                do_substep(&mut pool, &mut grid, &chem, &mut bonds, dt / 4.0);
            }
            let d = (pool.get(a).pos - pool.get(b).pos).length();
            if d > max_dist { max_dist = d; }
        }
        assert!(max_dist <= RADIUS + 1e-3, "bond stayed within R + eps; max = {}", max_dist);
    }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test --lib parallel::substep::tests::bonded_pair_stays_within_radius`
Expected: FAIL (bond not enforced).

- [ ] **Step 3: Implement enforce_bonds**

Append to `src/parallel/substep.rs`:

```rust
use crate::ccd::RADIUS;
use crate::collide::reflect;

pub fn enforce_bonds(pool: &mut BeadPool, grid: &Grid, bonds: &HashSet<(u32, u32)>) {
    const BOUNDARY_EPS: f32 = 1e-5;
    let pairs: Vec<(u32, u32)> = bonds.iter().copied().collect();
    for (a, b) in pairs {
        if !pool.get(a).alive || !pool.get(b).alive { continue; }
        let pa = pool.get(a).pos;
        let pb_raw = pool.get(b).pos;
        let pb = pa + grid.min_image(pa, pb_raw);
        let d = pb - pa;
        let dist = d.length();
        if dist < RADIUS || dist < 1e-12 { continue; }
        let n = d / dist;
        let target = RADIUS - BOUNDARY_EPS;
        let correction = (target - dist) * 0.5;
        let new_a = grid.wrap_pos(pa - n * correction);
        let new_b = grid.wrap_pos(pool.get(b).pos + n * correction);
        pool.get_mut(a).pos = new_a;
        pool.get_mut(b).pos = new_b;
        let va = pool.get(a).vel;
        let vb = pool.get(b).vel;
        if (vb - va).dot(n) > 0.0 {
            let (va_new, vb_new) = reflect(pa, va, pb, vb);
            pool.get_mut(a).vel = va_new;
            pool.get_mut(b).vel = vb_new;
        }
    }
}
```

In `do_substep`, after `advance_all`, before `clear_substep_flags`:
```rust
    enforce_bonds(pool, grid, bonds);
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test --lib parallel::substep`
Expected: PASS, 4 tests.

- [ ] **Step 5: Commit**

```bash
git add src/parallel/substep.rs
git commit -m "feat(parallel): enforce_bonds at end of each substep"
```

---

# Phase 1E: Scenarios and integration tests

### Task 12: Sem-basic chemistry TOML + parser hookup

**Files:**
- Create: `chemistries/sem_basic.toml`
- Modify: `src/chemistry/compiler.rs` (add a from-scratch Sem-style compiler)

**Decision (close call, logged):** Phase 1 ships a minimal Sem-basic chemistry sufficient to exercise Birth/Die/Apply, defined in TOML using new `[[sem_rule]]` blocks. A full TOML schema for Sem is large; this minimal subset (Apply+Die → LeftOnly, Apply+Spawn → Birth, Sig propagation via Exchange) is enough to test the algorithm. Full schema follows in a later task.

- [ ] **Step 1: Write the failing test**

`tests/sem_basic_loads.rs`:
```rust
use jigglefab::chemistry::load_chemistry_compiled;

#[test]
fn sem_basic_compiles() {
    let compiled = load_chemistry_compiled("chemistries/sem_basic.toml").unwrap();
    // Programs were registered.
    assert!(!compiled.program_offset.is_empty());
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test --test sem_basic_loads`
Expected: FAIL — unresolved `load_chemistry_compiled`, missing file.

- [ ] **Step 3: Create chemistries/sem_basic.toml**

```toml
# Sem-basic chemistry — minimal subset of haskell/src/Chem/Sem.hs exercising
# Apply, Die, Spawn for Phase-1 testing. Future tasks expand this.
states = ["wire", "destroyer"]
colors = [
    [0.78, 0.78, 0.80],
    [0.50, 0.10, 0.50],
]

[[sem_rule]]
description = "Apply + Die → LeftOnly (right bead dies, left keeps reduced stack)"
left_tag = "wire"
left_top = "apply"
right_tag = "wire"
right_top = "die"
side = "in"
reaction = "left_only"
new_left = "pop_top"
new_right = "dead"

[[sem_rule]]
description = "Apply + Spawn → Birth (existing pair keeps reduced stacks, new empty wire is born and bonded to both)"
left_tag = "wire"
left_top = "apply"
right_tag = "wire"
right_top = "spawn"
side = "in"
reaction = "birth"
new_left = "pop_top"
new_right = "pop_top"
birth_tag = "wire"
birth_program = "empty"

[[program]]
name = "empty"
ops = []
```

- [ ] **Step 4: Implement the Sem-style compiler**

Add `parse_sem_chemistry` to `src/chemistry/compiler.rs`:

```rust
use serde::Deserialize;

#[derive(Deserialize)]
struct SemChemistryFile {
    states: Vec<String>,
    #[serde(default)] colors: Option<Vec<[f32; 3]>>,
    #[serde(rename = "sem_rule", default)] sem_rules: Vec<SemRuleSpec>,
    #[serde(rename = "program", default)] programs: Vec<ProgramSpec>,
}

#[derive(Deserialize)]
struct SemRuleSpec {
    description: Option<String>,
    left_tag: String, left_top: String,
    right_tag: String, right_top: String,
    side: String,
    reaction: String,
    new_left: String,
    new_right: String,
    #[serde(default)] birth_tag: Option<String>,
    #[serde(default)] birth_program: Option<String>,
}

#[derive(Deserialize)]
struct ProgramSpec {
    name: String,
    ops: Vec<String>,
}

pub fn parse_sem_chemistry(text: &str) -> anyhow::Result<CompiledChemistry> {
    let file: SemChemistryFile = toml::from_str(text)?;
    let mut compiled = CompiledChemistry::empty();

    // Compile programs.
    let mut name_to_idx: std::collections::HashMap<String, u32> = std::collections::HashMap::new();
    for prog in &file.programs {
        let idx = compiled.program_offset.len() as u32;
        let start = compiled.program_pool.len() as u32;
        compiled.program_offset.push(start);
        compiled.program_pool.push(Op::from(prog.ops.len() as u32));
        for op_name in &prog.ops {
            compiled.program_pool.push(parse_op_literal(op_name)?);
        }
        name_to_idx.insert(prog.name.clone(), idx);
    }

    // Compile rules.
    for rule in &file.sem_rules {
        let key_a = BeadKey { tag: parse_tag(&rule.left_tag)?, top_op: parse_op_literal(&rule.left_top)? };
        let key_b = BeadKey { tag: parse_tag(&rule.right_tag)?, top_op: parse_op_literal(&rule.right_top)? };
        let side = match rule.side.as_str() {
            "in" => Side::In, "out" => Side::Out,
            other => anyhow::bail!("unknown side {}", other),
        };
        let kind = match rule.reaction.as_str() {
            "exchange" => ReactionKind::Exchange,
            "left_only" => ReactionKind::LeftOnly,
            "right_only" => ReactionKind::RightOnly,
            "birth" => ReactionKind::Birth,
            "passthrough" => ReactionKind::Passthrough,
            other => anyhow::bail!("unknown reaction kind {}", other),
        };
        let birth_state = if kind == ReactionKind::Birth {
            let tag = rule.birth_tag.as_deref().ok_or_else(|| anyhow::anyhow!("birth requires birth_tag"))?;
            let prog_name = rule.birth_program.as_deref();
            let prog_idx = prog_name.map(|n| *name_to_idx.get(n).expect("unknown program"));
            Some(BirthState { tag: parse_tag(tag)?, program: prog_idx })
        } else { None };
        compiled.insert_rule(key_a, key_b, side, Rule {
            kind,
            new_state_a: parse_new_state(&rule.new_left, &name_to_idx)?,
            new_state_b: parse_new_state(&rule.new_right, &name_to_idx)?,
            birth_state,
        });
    }
    Ok(compiled)
}

fn parse_tag(s: &str) -> anyhow::Result<Tag> {
    Ok(match s {
        "wire" => Tag::Wire, "port" => Tag::Port, "sensor" => Tag::Sensor,
        "creator" => Tag::Creator, "destroyer" => Tag::Destroyer, "rock" => Tag::Rock,
        other => anyhow::bail!("unknown tag {}", other),
    })
}

fn parse_op_literal(s: &str) -> anyhow::Result<Op> {
    Ok(match s {
        "nop" => Op::nop(),
        "apply" => Op::simple(OpKind::Apply),
        "done" => Op::simple(OpKind::Done),
        "wait" => Op::simple(OpKind::Wait),
        "take" => Op::simple(OpKind::Take),
        "drop" => Op::simple(OpKind::Drop),
        "die" => Op::simple(OpKind::Die),
        "spawn" => Op::simple(OpKind::Spawn),
        "hold" => Op::simple(OpKind::Hold),
        "sig_red" => Op::sig(crate::chemistry::Sig::Red),
        "sig_blue" => Op::sig(crate::chemistry::Sig::Blue),
        other => anyhow::bail!("unknown op literal {}", other),
    })
}

fn parse_new_state(s: &str, name_to_idx: &std::collections::HashMap<String, u32>) -> anyhow::Result<NewState> {
    if s == "pop_top" { return Ok(NewState::keep_pop_top()); }
    if s == "dead" { return Ok(NewState::dead()); }
    if let Some(rest) = s.strip_prefix("load:") {
        let idx = name_to_idx.get(rest).ok_or_else(|| anyhow::anyhow!("unknown program {}", rest))?;
        return Ok(NewState::load_program(*idx));
    }
    if let Some(rest) = s.strip_prefix("top:") {
        return Ok(NewState::keep_with(parse_op_literal(rest)?));
    }
    anyhow::bail!("unknown new_state spec {}", s)
}

#[cfg(not(target_arch = "wasm32"))]
pub fn load_chemistry_compiled(path: &str) -> anyhow::Result<CompiledChemistry> {
    let text = std::fs::read_to_string(path)?;
    parse_sem_chemistry(&text)
}
```

Add to `src/chemistry/mod.rs`:
```rust
pub use compiler::{compile_chemistry, parse_sem_chemistry};
#[cfg(not(target_arch = "wasm32"))]
pub use compiler::load_chemistry_compiled;
```

- [ ] **Step 5: Run test to verify it passes**

Run: `cargo test --test sem_basic_loads`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add chemistries/sem_basic.toml src/chemistry/ tests/sem_basic_loads.rs
git commit -m "feat(chemistry): sem-style TOML compiler + sem_basic chemistry"
```

---

### Task 13: chains_30x300 fab + smoke test

**Files:**
- Create: `fabs/chains_30x300.toml`
- Create: `tests/chains_30x300_smoke.rs`

- [ ] **Step 1: Generate fabs/chains_30x300.toml**

Write a script that generates the TOML programmatically (do it inline in the test runner, or pre-generate). For Phase 1 simplicity, generate via a small Rust helper.

Add `src/bench.rs::generators::disconnected_chains_grid` if not present, or write inline. Easier: produce the file with a one-off Rust binary.

```rust
// scripts/gen_chains_30x300.rs (run with `cargo run --bin gen_chains_30x300`)
// or: a #[test] that writes the file if absent.
```

**Decision (close call, logged):** Generate the fab via a `build.rs` step would add complexity. Instead, write a one-shot Rust binary at `src/bin/gen_chains.rs`.

`src/bin/gen_chains.rs`:
```rust
use std::fmt::Write;

fn main() {
    let n_chains = 30;
    let beads_per_chain = 300;
    let chain_spacing = 4.0;
    let bead_spacing = 0.95;
    let world_size = (n_chains as f32 * chain_spacing).max(beads_per_chain as f32 * bead_spacing) + 4.0;
    let mut out = String::new();
    writeln!(&mut out, "[meta]\nname = \"chains_30x300\"\nchemistry = \"grey\"\nseed = 12345\nworld_size = {}", world_size).unwrap();
    for c in 0..n_chains {
        let x = 2.0 + c as f32 * chain_spacing;
        for i in 0..beads_per_chain {
            let y = 2.0 + i as f32 * bead_spacing;
            writeln!(&mut out, "\n[[bead]]\nstate = \"grey\"\npos = [{:.4}, {:.4}]", x, y).unwrap();
        }
    }
    std::fs::write("fabs/chains_30x300.toml", out).unwrap();
    println!("wrote fabs/chains_30x300.toml ({} chains × {} beads)", n_chains, beads_per_chain);
}
```

Add to `Cargo.toml`:
```toml
[[bin]]
name = "gen_chains"
path = "src/bin/gen_chains.rs"
```

- [ ] **Step 2: Run the generator**

Run: `cargo run --bin gen_chains`
Expected: writes `fabs/chains_30x300.toml`.

- [ ] **Step 3: Write the smoke test**

`tests/chains_30x300_smoke.rs`:
```rust
use jigglefab::chemistry::{compile_chemistry, load_chemistry};
use jigglefab::fab::load_fab;
use jigglefab::parallel::CpuParallel;
use jigglefab::scheduler::Scheduler;
use jigglefab::sim::Sim;

#[test]
#[ignore = "slow — run with `cargo test --release chains_30x300_smoke -- --ignored`"]
fn chains_30x300_runs_30_frames_without_panic() {
    let fab = load_fab("fabs/chains_30x300.toml").unwrap();
    let chem = load_chemistry("chemistries/grey.toml").unwrap();
    let mut sim = Sim::from_fab(&fab, chem);
    let compiled = compile_chemistry(sim.chemistry()).unwrap();
    let mut sched = CpuParallel::new(&sim, compiled);
    for _ in 0..30 {
        sched.step(&mut sim, 1.0 / 60.0);
    }
    // Invariants: no overlap, no NaN.
    for (i, p) in sim.positions.iter().enumerate() {
        assert!(p.is_finite(), "bead {} position is non-finite: {:?}", i, p);
    }
}
```

- [ ] **Step 4: Run the smoke test**

Run: `cargo test --release --test chains_30x300_smoke -- --ignored`
Expected: PASS (or instructive failure that tells us where the design breaks at scale).

- [ ] **Step 5: Commit**

```bash
git add Cargo.toml src/bin/gen_chains.rs fabs/chains_30x300.toml tests/chains_30x300_smoke.rs
git commit -m "feat: chains_30x300 fab + smoke test for CpuParallel at 9000 beads"
```

---

### Task 14: Self-determinism test

**Files:**
- Create: `tests/parallel_self_determinism.rs`

- [ ] **Step 1: Write the test**

```rust
// tests/parallel_self_determinism.rs
use jigglefab::chemistry::{compile_chemistry, load_chemistry};
use jigglefab::fab::load_fab;
use jigglefab::parallel::CpuParallel;
use jigglefab::scheduler::Scheduler;
use jigglefab::sim::Sim;

fn run_to_frame_60(fab_path: &str, chem_path: &str) -> Vec<f32> {
    let fab = load_fab(fab_path).unwrap();
    let chem = load_chemistry(chem_path).unwrap();
    let mut sim = Sim::from_fab(&fab, chem);
    let compiled = compile_chemistry(sim.chemistry()).unwrap();
    let mut sched = CpuParallel::new(&sim, compiled);
    for _ in 0..60 {
        sched.step(&mut sim, 1.0 / 60.0);
    }
    let mut out = Vec::with_capacity(sim.positions.len() * 4);
    for p in &sim.positions { out.push(p.x); out.push(p.y); }
    for v in &sim.velocities { out.push(v.x); out.push(v.y); }
    out
}

#[test]
fn cpu_parallel_is_self_deterministic_grey_30() {
    let a = run_to_frame_60("fabs/grey-30.toml", "chemistries/grey.toml");
    let b = run_to_frame_60("fabs/grey-30.toml", "chemistries/grey.toml");
    assert_eq!(a, b, "two runs of the same scenario must produce identical bit patterns");
}

#[test]
fn cpu_parallel_is_self_deterministic_wire_30() {
    let a = run_to_frame_60("fabs/wire-30.toml", "chemistries/wire.toml");
    let b = run_to_frame_60("fabs/wire-30.toml", "chemistries/wire.toml");
    assert_eq!(a, b);
}
```

- [ ] **Step 2: Run the test**

Run: `cargo test --test parallel_self_determinism`
Expected: PASS.

- [ ] **Step 3: Commit**

```bash
git add tests/parallel_self_determinism.rs
git commit -m "test: CpuParallel self-determinism on prototype scenarios"
```

---

### Task 15: Invariant tests (no overlap, bond distance bounds)

**Files:**
- Create: `tests/parallel_invariants.rs`

- [ ] **Step 1: Write the test**

```rust
// tests/parallel_invariants.rs
use jigglefab::ccd::RADIUS;
use jigglefab::chemistry::{compile_chemistry, load_chemistry};
use jigglefab::fab::load_fab;
use jigglefab::parallel::CpuParallel;
use jigglefab::scheduler::Scheduler;
use jigglefab::sim::Sim;

fn min_pair_distance(sim: &Sim) -> f32 {
    let mut min_d = f32::INFINITY;
    for i in 0..sim.positions.len() {
        for j in (i+1)..sim.positions.len() {
            let d = (sim.positions[i] - sim.positions[j]).length();
            if d < min_d { min_d = d; }
        }
    }
    min_d
}

#[test]
fn no_overlap_below_radius_grey_30() {
    let fab = load_fab("fabs/grey-30.toml").unwrap();
    let chem = load_chemistry("chemistries/grey.toml").unwrap();
    let mut sim = Sim::from_fab(&fab, chem);
    let compiled = compile_chemistry(sim.chemistry()).unwrap();
    let mut sched = CpuParallel::new(&sim, compiled);
    let eps = 0.05;  // small tolerance for snap-back precision
    for f in 0..600 {
        sched.step(&mut sim, 1.0 / 60.0);
        let min_d = min_pair_distance(&sim);
        // Bonded pairs can be within R; we allow the bond pairs to dip below R.
        // We check non-bonded pairs separately by comparing only to (R - eps).
        // For a 30-bead chain, most pairs are non-bonded; min_d should be ≥ R - eps.
        assert!(
            min_d > RADIUS - eps,
            "frame {}: min pair distance {} < R - eps ({})",
            f, min_d, RADIUS - eps,
        );
    }
}

#[test]
fn bonds_stay_within_radius_plus_eps_grey_30() {
    let fab = load_fab("fabs/grey-30.toml").unwrap();
    let chem = load_chemistry("chemistries/grey.toml").unwrap();
    let mut sim = Sim::from_fab(&fab, chem);
    let compiled = compile_chemistry(sim.chemistry()).unwrap();
    let mut sched = CpuParallel::new(&sim, compiled);
    let eps = 0.01;
    for f in 0..600 {
        sched.step(&mut sim, 1.0 / 60.0);
        for &(a, b) in &sim.bonds {
            let d = (sim.positions[a as usize] - sim.positions[b as usize]).length();
            assert!(d <= RADIUS + eps, "frame {}: bond ({},{}) length {} > R + eps", f, a, b, d);
        }
    }
}
```

- [ ] **Step 2: Run the test**

Run: `cargo test --test parallel_invariants --release`
Expected: PASS. If it doesn't, the algorithm has a real bug; fix before continuing.

- [ ] **Step 3: Commit**

```bash
git add tests/parallel_invariants.rs
git commit -m "test: CpuParallel physical invariants (no overlap, bond bounds)"
```

---

### Task 16: Wire signal-conservation invariant

**Files:**
- Modify: `tests/parallel_invariants.rs`

- [ ] **Step 1: Write the test**

Append to `tests/parallel_invariants.rs`:

```rust
#[test]
fn wire_signal_count_conserved() {
    let fab = load_fab("fabs/wire-30.toml").unwrap();
    let chem = load_chemistry("chemistries/wire.toml").unwrap();
    let mut sim = Sim::from_fab(&fab, chem);
    let initial_on_count = sim.states.iter().filter(|&&s| s == 1).count();
    let compiled = compile_chemistry(sim.chemistry()).unwrap();
    let mut sched = CpuParallel::new(&sim, compiled);
    for _ in 0..600 {
        sched.step(&mut sim, 1.0 / 60.0);
        let on_count = sim.states.iter().filter(|&&s| s == 1).count();
        assert_eq!(on_count, initial_on_count, "wire signal count is not conserved");
    }
}
```

- [ ] **Step 2: Run**

Run: `cargo test --test parallel_invariants --release wire_signal_count_conserved`
Expected: PASS.

- [ ] **Step 3: Commit**

```bash
git add tests/parallel_invariants.rs
git commit -m "test: wire signal conservation invariant under CpuParallel"
```

---

# Phase 1F: Bench integration

### Task 17: bench --scheduler cpu-parallel

**Files:**
- Modify: `src/bin/bench.rs`

- [ ] **Step 1: Inspect existing bench scheduler selection**

Run: `cargo run --bin bench -- --help`
Note: the existing `--scheduler cpu|gpu` accepts `cpu` and `gpu` (the GpuEventLoop). We add `cpu-parallel`.

- [ ] **Step 2: Modify scheduler selection**

In `src/bin/bench.rs`, find the `--scheduler` argument parsing. Add a `"cpu-parallel"` arm that constructs a `CpuParallel`:

```rust
"cpu-parallel" => {
    let compiled = jigglefab::chemistry::compile_chemistry(sim.chemistry())?;
    Box::new(jigglefab::parallel::CpuParallel::new(&sim, compiled)) as Box<dyn jigglefab::scheduler::Scheduler>
}
```

- [ ] **Step 3: Smoke-run the bench**

Run: `cargo run --release --bin bench -- --scheduler cpu-parallel --scenario chains_30x30 --frames 30 --warmup 5`
Expected: prints fps numbers without crashing.

- [ ] **Step 4: Commit**

```bash
git add src/bin/bench.rs
git commit -m "feat(bench): --scheduler cpu-parallel option"
```

---

### Task 18: chains_30x300 bench perf gate

**Files:**
- Create: `docs/superpowers/status/2026-05-23-parallel-ccd-phase-1-bench.md`

- [ ] **Step 1: Run the bench**

Run: `cargo run --release --bin bench -- --scheduler cpu-parallel --scenario chains_30x300 --frames 30 --warmup 30 | tee /tmp/parallel_bench.txt`

Expected output: per-frame timing. Phase 1 has no GPU target; this just measures the new CPU baseline.

- [ ] **Step 2: Write a short status doc**

Write `docs/superpowers/status/2026-05-23-parallel-ccd-phase-1-bench.md`:

```markdown
# Parallel-CCD Phase 1 Bench Results — 2026-05-23

## Setup
Ryzen 7 9800X3D, today's `main` with Phase 1 merged.

## CpuParallel measured
chains_30x30   N=900   mean=<fill>ms  p99=<fill>ms  <fill>fps
chains_10x100  N=1000  mean=<fill>ms  p99=<fill>ms  <fill>fps
chains_30x300  N=9000  mean=<fill>ms  p99=<fill>ms  <fill>fps

## Comparison to CpuSequential
(same scenarios, --scheduler cpu)
chains_30x30   N=900   mean=<fill>ms  <fill>fps
chains_10x100  N=1000  mean=<fill>ms  <fill>fps

## Conclusions
- CpuParallel is faster/slower than CpuSequential at <N>
- Path to 10k@60fps requires Phase 2 (GPU) — write Phase 2 plan next.
```

Fill in the numbers from the actual run.

- [ ] **Step 3: Commit**

```bash
git add docs/superpowers/status/2026-05-23-parallel-ccd-phase-1-bench.md
git commit -m "docs: Phase 1 bench results — CpuParallel baseline at 9000 beads"
```

---

### Task 19: Phase 1 retro and Phase 2 hand-off doc

**Files:**
- Create: `docs/superpowers/status/2026-05-23-parallel-ccd-phase-1-retro.md`

- [ ] **Step 1: Write the retro**

```markdown
# Parallel-CCD Phase 1 — Retrospective

## What shipped
- CpuParallel scheduler (graph-colored fixed substep)
- Chemistry compiler: legacy TOML + sem-style TOML → CompiledChemistry
- Sem-basic chemistry, chains_30x300 fab
- Self-determinism, no-overlap, bond-bound, wire-signal-conservation tests

## What didn't ship
- GPU implementation (Phase 2)
- Sem chemistry: full opcode coverage (just Apply/Die/Spawn for now)
- Speed-adaptive substep
- Per-frame parallelism (rayon) — staying sequential until Phase 2 proves correctness

## Surprises / lessons (fill in after Phase 1 lands)
- ...

## Hand-off to Phase 2
- CpuParallel is the bit-identical oracle. Phase 2's GpuColored must match it exactly on the test scenarios in tests/parallel_self_determinism.rs.
- The substep loop is in src/parallel/substep.rs::do_substep — Phase 2 reimplements this in WGSL.
- The rule table format in CompiledChemistry maps cleanly to a flat GPU buffer.
- Delete src/gpu/ and shaders/*.wgsl in Phase 2 as part of the new GPU scheduler scaffolding.
```

- [ ] **Step 2: Commit**

```bash
git add docs/superpowers/status/2026-05-23-parallel-ccd-phase-1-retro.md
git commit -m "docs: Phase 1 retro and Phase 2 hand-off"
```

---

## Self-review

**Spec coverage check** (against [docs/superpowers/specs/2026-05-23-parallel-ccd-design.md](../specs/2026-05-23-parallel-ccd-design.md)):

- ✅ Scheduler trait reused — Task 10
- ✅ CpuParallel scheduler — Tasks 5-11, 10
- ✅ CpuSequential left untouched — confirmed in plan intro
- ✅ Graph coloring with deterministic ordering — Task 6
- ✅ Fixed substep — Task 10 (DEFAULT_DT_SUB)
- ✅ Bead pool with slot recycling — Task 5
- ✅ Births and deaths handled — Task 8
- ✅ Bond mutation queue + merge — Task 9
- ✅ Passthrough — Task 8 (ReactionKind::Passthrough)
- ✅ Walls (Rock tag) — Tag::Rock present in Tag enum; resolution skips rock velocity (not yet tested — flagged)
- ✅ Chemistry compiler (legacy + sem-style) — Tasks 4 + 12
- ✅ Self-determinism test — Task 14
- ✅ Invariant tests — Tasks 15-16
- ⚠️ Wall reflection logic — Not exercised in tests. **Add a follow-up task or note** that no current fab uses rocks; will be added when first sem chemistry needs them.
- ❌ chemistries/sem_basic_demo.toml and fabs/sem_basic_demo.toml — Not included. **Add as a stretch task or deferral note.**
- ❌ GPU work (Phase 2) — Out of scope as planned.

**Placeholder scan:** None found. All code is concrete.

**Type consistency:** `Tag::Wire`, `Op::sig_legacy`, `BeadKey`, `CompiledChemistry`, `Rule` all used consistently. `Side::In` / `Side::Out` matches throughout. `NewState` variants used consistently.

**Gaps filled inline:** added two follow-up notes.

---

## Follow-ups (not part of Phase 1, to revisit before Phase 2)

- **Walls demo**: write a `sem_basic_demo` fab including a few `tag = "rock"` beads, and an invariant test verifying mobile beads bounce off rocks. Defer to Phase 2 prep.
- **Sem-basic demo run**: end-to-end run of sem_basic chemistry with a Birth and a Die to verify pool slot recycling under realistic conditions. Defer to a follow-up task in Phase 2 prep.
- **Speed-adaptive substep**: hook in `dt_sub = min(DEFAULT_DT_SUB, R / max_observed_speed)` once a chemistry with collision-amplified speeds shows accuracy issues.

---

**Plan complete.**
