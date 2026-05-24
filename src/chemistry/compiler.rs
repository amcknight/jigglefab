use crate::chemistry::{
    Action, BeadKey, Chemistry, CompiledChemistry, NewState, Op, ReactionKind, Rule, Side, Tag,
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
