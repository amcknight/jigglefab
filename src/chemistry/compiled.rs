use std::collections::HashMap;

use crate::chemistry::Op;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Tag {
    Wire = 0,
    Port = 1,
    Sensor = 2,
    Creator = 3,
    Destroyer = 4,
    Rock = 5,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Side {
    In,
    Out,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BeadKey {
    pub tag: Tag,
    pub top_op: Op,
}

impl BeadKey {
    pub fn wire_empty() -> Self {
        BeadKey {
            tag: Tag::Wire,
            top_op: Op::nop(),
        }
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

// What to do with a participant's state after the reaction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NewState {
    // Keep the existing stack; set the top opcode to `top` (used for tag changes).
    KeepWith { top: Op },
    // Pop the top opcode of the stack; rest stays.
    KeepPopTop,
    // Replace the stack with a program by index in the program pool.
    LoadProgram(u32),
    // Mark this slot dead.
    Dead,
}

impl NewState {
    pub fn keep_with(op: Op) -> Self {
        NewState::KeepWith { top: op }
    }
    pub fn keep_pop_top() -> Self {
        NewState::KeepPopTop
    }
    pub fn load_program(idx: u32) -> Self {
        NewState::LoadProgram(idx)
    }
    pub fn dead() -> Self {
        NewState::Dead
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rule {
    pub kind: ReactionKind,
    pub new_state_a: NewState,
    pub new_state_b: NewState,
    // For Birth only: the newborn's initial state.
    pub birth_state: Option<BirthState>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BirthState {
    pub tag: Tag,
    pub program: Option<u32>,
}

#[derive(Debug, Clone)]
pub struct CompiledChemistry {
    // Flat buffer of opcode programs referenced by `LoadProgram` and Send.
    // Each program is stored as a sequence: `[len, op_0, op_1, ..., op_{len-1}]`.
    pub program_pool: Vec<Op>,
    // `program_offset[i]` is the offset of program `i` in `program_pool`.
    pub program_offset: Vec<u32>,
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
        self.rules
            .get(&(a, b, side))
            .cloned()
            .unwrap_or_else(|| self.default_rule.clone())
    }

    pub fn program(&self, idx: u32) -> &[Op] {
        let off = self.program_offset[idx as usize] as usize;
        let len = u32::from(self.program_pool[off]) as usize;
        &self.program_pool[off + 1..off + 1 + len]
    }
}

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
        let key_a = BeadKey {
            tag: Tag::Wire,
            top_op: Op::simple(OpKind::Apply),
        };
        let key_b = BeadKey {
            tag: Tag::Wire,
            top_op: Op::simple(OpKind::Die),
        };
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
