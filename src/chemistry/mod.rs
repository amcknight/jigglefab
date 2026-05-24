use serde::Deserialize;
use anyhow::{Result, bail};

pub mod opcodes;
pub use opcodes::{Op, OpKind, Sig};

pub mod compiled;
pub use compiled::{
    BeadKey, BirthState, CompiledChemistry, NewState, ReactionKind, Rule, Side, Tag,
};

pub mod compiler;
pub use compiler::{compile_chemistry, parse_sem_chemistry};
#[cfg(not(target_arch = "wasm32"))]
pub use compiler::load_chemistry_compiled;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Action {
    Reflect,
    Pass,
    // Reflect AND swap the two beads' state indices. The "wire" chemistry uses
    // this for every contact: signal transfer is just state propagation along
    // a chain of bonded beads.
    ReflectSwap,
}

// Fallback colors used when a chemistry TOML doesn't specify `colors`. Indexed
// by state. Beyond this length, colors cycle.
const DEFAULT_PALETTE: [[f32; 3]; 8] = [
    [0.78, 0.78, 0.80], // pale grey  — grey chemistry default
    [1.00, 0.80, 0.25], // amber      — natural "charged / hot" cue
    [0.30, 0.45, 0.85], // cool blue
    [0.85, 0.40, 0.45], // rose
    [0.50, 0.85, 0.40], // green
    [0.85, 0.40, 0.85], // magenta
    [0.40, 0.85, 0.85], // cyan
    [0.85, 0.75, 0.40], // yellow
];

#[derive(Debug, Deserialize)]
struct ChemistryFile {
    states: Vec<String>,
    #[serde(default)]
    colors: Option<Vec<[f32; 3]>>,
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
    pub colors: Vec<[f32; 3]>,
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

    /// Linearised action table for GPU upload.
    /// Index: `state_a * n_states * 2 + state_b * 2 + inside as usize`
    /// Values: 0 = Reflect, 1 = Pass, 2 = ReflectSwap
    pub fn action_table_flat(&self) -> Vec<u32> {
        let n = self.states.len();
        let mut out = Vec::with_capacity(n * n * 2);
        for a in 0..n {
            for b in 0..n {
                for inside in [false, true] {
                    out.push(match self.table[a][b][inside as usize] {
                        Action::Reflect => 0,
                        Action::Pass => 1,
                        Action::ReflectSwap => 2,
                    });
                }
            }
        }
        out
    }
}

pub fn parse_chemistry(text: &str) -> Result<Chemistry> {
    let file: ChemistryFile = toml::from_str(text)?;
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
            "swap" | "reflect_swap" => Action::ReflectSwap,
            other => bail!("unknown action {:?}", other),
        };
        let inside_idx = rule.inside as usize;
        // Enforce symmetry: rule applies to (a,b) and (b,a).
        table[a][b][inside_idx] = action;
        table[b][a][inside_idx] = action;
    }
    let colors = match file.colors {
        Some(c) => {
            if c.len() != n {
                bail!("colors length {} does not match states length {}", c.len(), n);
            }
            c
        }
        None => (0..n).map(|i| DEFAULT_PALETTE[i % DEFAULT_PALETTE.len()]).collect(),
    };
    Ok(Chemistry { states: file.states, colors, table })
}

#[cfg(not(target_arch = "wasm32"))]
pub fn load_chemistry(path: &str) -> Result<Chemistry> {
    let text = std::fs::read_to_string(path)?;
    parse_chemistry(&text)
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
        // No `colors` in grey.toml — should fall back to the default palette.
        assert_eq!(chem.colors.len(), 1);
        assert_eq!(chem.colors[0], DEFAULT_PALETTE[0]);
    }

    #[test]
    fn parses_swap_action_and_colors() {
        let text = r#"
states = ["off", "on"]
colors = [[0.1, 0.2, 0.3], [0.9, 0.8, 0.7]]

[[rule]]
states = ["off", "on"]
inside = false
action = "swap"
"#;
        let chem = parse_chemistry(text).unwrap();
        let off = chem.state_index("off").unwrap();
        let on = chem.state_index("on").unwrap();
        assert_eq!(chem.lookup(off, on, false), Action::ReflectSwap);
        // Symmetry: rule (off,on) also applies to (on,off).
        assert_eq!(chem.lookup(on, off, false), Action::ReflectSwap);
        // Other entries fall back to the table default (Reflect).
        assert_eq!(chem.lookup(off, off, false), Action::Reflect);
        assert_eq!(chem.colors[0], [0.1, 0.2, 0.3]);
        assert_eq!(chem.colors[1], [0.9, 0.8, 0.7]);
    }

    #[test]
    fn rejects_colors_count_mismatch() {
        let text = r#"
states = ["a", "b"]
colors = [[0.1, 0.2, 0.3]]

[[rule]]
states = ["a", "b"]
inside = false
action = "reflect"
"#;
        assert!(parse_chemistry(text).is_err());
    }

    #[test]
    fn action_table_flat_wire() {
        // Load wire chemistry
        let chem = load_chemistry("chemistries/wire.toml").unwrap();
        let table = chem.action_table_flat();
        let n = chem.states.len();
        assert_eq!(table.len(), n * n * 2);
        assert!(table.iter().all(|&v| v <= 2));
    }
}
