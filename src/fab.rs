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
    #[serde(default)]
    pub world_size: Option<f32>,
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

pub fn parse_fab(text: &str) -> anyhow::Result<Fab> {
    let fab: Fab = toml::from_str(text)?;
    Ok(fab)
}

#[cfg(not(target_arch = "wasm32"))]
pub fn load_fab(path: &str) -> anyhow::Result<Fab> {
    let text = std::fs::read_to_string(path)?;
    parse_fab(&text)
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
