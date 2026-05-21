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
            other => bail!("unknown action {:?}", other),
        };
        let inside_idx = rule.inside as usize;
        // Enforce symmetry: rule applies to (a,b) and (b,a).
        table[a][b][inside_idx] = action;
        table[b][a][inside_idx] = action;
    }
    Ok(Chemistry { states: file.states, table })
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
    }
}
