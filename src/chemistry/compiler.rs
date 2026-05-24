use std::collections::HashMap;

use serde::Deserialize;

use crate::chemistry::{
    Action, BeadKey, BirthState, Chemistry, CompiledChemistry, NewState, Op, OpKind, ReactionKind,
    Rule, Side, Sig, Tag,
};

// Adapter for legacy dense-state-index chemistries (grey, wire). Stuffs the
// state index into `Op::sig_legacy` and emits one rule per (a, b, side) cell
// in the legacy table.
pub fn compile_chemistry(chem: &Chemistry) -> anyhow::Result<CompiledChemistry> {
    let mut compiled = CompiledChemistry::empty();
    let n = chem.states.len();
    for a in 0..n {
        for b in 0..n {
            for &inside in &[false, true] {
                let action = chem.lookup(a, b, inside);
                let key_a = BeadKey {
                    tag: Tag::Wire,
                    top_op: Op::sig_legacy(a as u32),
                };
                let key_b = BeadKey {
                    tag: Tag::Wire,
                    top_op: Op::sig_legacy(b as u32),
                };
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

#[derive(Deserialize)]
struct SemChemistryFile {
    #[allow(dead_code)]
    states: Vec<String>,
    #[serde(default)]
    #[allow(dead_code)]
    colors: Option<Vec<[f32; 3]>>,
    #[serde(rename = "sem_rule", default)]
    sem_rules: Vec<SemRuleSpec>,
    #[serde(rename = "program", default)]
    programs: Vec<ProgramSpec>,
}

#[derive(Deserialize)]
struct SemRuleSpec {
    #[allow(dead_code)]
    description: Option<String>,
    left_tag: String,
    left_top: String,
    right_tag: String,
    right_top: String,
    side: String,
    reaction: String,
    new_left: String,
    new_right: String,
    #[serde(default)]
    birth_tag: Option<String>,
    #[serde(default)]
    birth_program: Option<String>,
}

#[derive(Deserialize)]
struct ProgramSpec {
    name: String,
    ops: Vec<String>,
}

pub fn parse_sem_chemistry(text: &str) -> anyhow::Result<CompiledChemistry> {
    let file: SemChemistryFile = toml::from_str(text)?;
    let mut compiled = CompiledChemistry::empty();

    let mut name_to_idx: HashMap<String, u32> = HashMap::new();
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

    for rule in &file.sem_rules {
        let key_a = BeadKey {
            tag: parse_tag(&rule.left_tag)?,
            top_op: parse_op_literal(&rule.left_top)?,
        };
        let key_b = BeadKey {
            tag: parse_tag(&rule.right_tag)?,
            top_op: parse_op_literal(&rule.right_top)?,
        };
        let side = match rule.side.as_str() {
            "in" => Side::In,
            "out" => Side::Out,
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
            let tag = rule
                .birth_tag
                .as_deref()
                .ok_or_else(|| anyhow::anyhow!("birth requires birth_tag"))?;
            let prog_idx = match rule.birth_program.as_deref() {
                Some(n) => Some(
                    *name_to_idx
                        .get(n)
                        .ok_or_else(|| anyhow::anyhow!("unknown birth_program {}", n))?,
                ),
                None => None,
            };
            Some(BirthState {
                tag: parse_tag(tag)?,
                program: prog_idx,
            })
        } else {
            None
        };
        compiled.insert_rule(
            key_a,
            key_b,
            side,
            Rule {
                kind,
                new_state_a: parse_new_state(&rule.new_left, &name_to_idx)?,
                new_state_b: parse_new_state(&rule.new_right, &name_to_idx)?,
                birth_state,
            },
        );
    }
    Ok(compiled)
}

fn parse_tag(s: &str) -> anyhow::Result<Tag> {
    Ok(match s {
        "wire" => Tag::Wire,
        "port" => Tag::Port,
        "sensor" => Tag::Sensor,
        "creator" => Tag::Creator,
        "destroyer" => Tag::Destroyer,
        "rock" => Tag::Rock,
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
        "sig_red" => Op::sig(Sig::Red),
        "sig_blue" => Op::sig(Sig::Blue),
        other => anyhow::bail!("unknown op literal {}", other),
    })
}

fn parse_new_state(s: &str, name_to_idx: &HashMap<String, u32>) -> anyhow::Result<NewState> {
    if s == "pop_top" {
        return Ok(NewState::keep_pop_top());
    }
    if s == "dead" {
        return Ok(NewState::dead());
    }
    if let Some(rest) = s.strip_prefix("load:") {
        let idx = name_to_idx
            .get(rest)
            .ok_or_else(|| anyhow::anyhow!("unknown program {}", rest))?;
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chemistry::parse_chemistry;

    #[test]
    fn compiles_grey_to_reflect_default() {
        let chem = parse_chemistry(
            "states = [\"grey\"]\n[[rule]]\nstates=[\"grey\",\"grey\"]\ninside=false\naction=\"reflect\"\n",
        )
        .unwrap();
        let compiled = compile_chemistry(&chem).unwrap();
        let key = BeadKey {
            tag: Tag::Wire,
            top_op: Op::sig_legacy(0),
        };
        let rule = compiled.lookup(key, key, Side::Out);
        assert_eq!(rule.kind, ReactionKind::Exchange);
    }

    #[test]
    fn compiles_wire_swap_to_exchange_with_swapped_states() {
        let chem = parse_chemistry(include_str!("../../chemistries/wire.toml")).unwrap();
        let compiled = compile_chemistry(&chem).unwrap();
        let off = BeadKey {
            tag: Tag::Wire,
            top_op: Op::sig_legacy(0),
        };
        let on = BeadKey {
            tag: Tag::Wire,
            top_op: Op::sig_legacy(1),
        };
        let rule = compiled.lookup(off, on, Side::In);
        // Wire's inside swap = Exchange + post-state swap. Encoded as Exchange
        // with new states swapped.
        assert_eq!(rule.kind, ReactionKind::Exchange);
        assert_eq!(rule.new_state_a, NewState::keep_with(Op::sig_legacy(1)));
        assert_eq!(rule.new_state_b, NewState::keep_with(Op::sig_legacy(0)));
    }
}
