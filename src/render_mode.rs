use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum RenderMode {
    Disc,
    Voronoi,
    SoftVoronoi,
    Worley,
    MetaballBlend,
    MetaballArgmax,
}

impl RenderMode {
    pub const ALL: [RenderMode; 6] = [
        RenderMode::Disc,
        RenderMode::Voronoi,
        RenderMode::SoftVoronoi,
        RenderMode::Worley,
        RenderMode::MetaballBlend,
        RenderMode::MetaballArgmax,
    ];

    /// Numeric discriminant uploaded to the shader as a `u32`.
    /// Disc never reaches the field shader, but giving it id 0 makes the
    /// enum-to-id mapping uniform.
    pub fn shader_id(self) -> u32 {
        match self {
            RenderMode::Disc => 0,
            RenderMode::Voronoi => 0,
            RenderMode::SoftVoronoi => 1,
            RenderMode::Worley => 2,
            RenderMode::MetaballBlend => 3,
            RenderMode::MetaballArgmax => 4,
        }
    }

    pub fn is_field(self) -> bool {
        !matches!(self, RenderMode::Disc)
    }

    pub fn cycle(self, forward: bool) -> RenderMode {
        let idx = Self::ALL.iter().position(|m| *m == self).unwrap();
        let next = if forward {
            (idx + 1) % Self::ALL.len()
        } else {
            (idx + Self::ALL.len() - 1) % Self::ALL.len()
        };
        Self::ALL[next]
    }

    pub fn label(self) -> &'static str {
        match self {
            RenderMode::Disc => "Disc",
            RenderMode::Voronoi => "Voronoi",
            RenderMode::SoftVoronoi => "Soft Voronoi",
            RenderMode::Worley => "Worley",
            RenderMode::MetaballBlend => "Metaball Blend",
            RenderMode::MetaballArgmax => "Metaball Argmax",
        }
    }

    pub fn label_kebab(self) -> &'static str {
        match self {
            RenderMode::Disc => "disc",
            RenderMode::Voronoi => "voronoi",
            RenderMode::SoftVoronoi => "soft-voronoi",
            RenderMode::Worley => "worley",
            RenderMode::MetaballBlend => "metaball-blend",
            RenderMode::MetaballArgmax => "metaball-argmax",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cycle_forward_wraps() {
        let last = RenderMode::ALL[RenderMode::ALL.len() - 1];
        assert_eq!(last.cycle(true), RenderMode::ALL[0]);
    }

    #[test]
    fn cycle_back_wraps() {
        assert_eq!(RenderMode::ALL[0].cycle(false),
                   RenderMode::ALL[RenderMode::ALL.len() - 1]);
    }

    #[test]
    fn serde_kebab_case_roundtrip() {
        let json = serde_json::to_string(&RenderMode::MetaballBlend).unwrap();
        assert_eq!(json, "\"metaball-blend\"");
        let back: RenderMode = serde_json::from_str(&json).unwrap();
        assert_eq!(back, RenderMode::MetaballBlend);
    }

    #[test]
    fn is_field_only_false_for_disc() {
        assert!(!RenderMode::Disc.is_field());
        for m in RenderMode::ALL.iter().filter(|m| **m != RenderMode::Disc) {
            assert!(m.is_field(), "{:?} should be a field mode", m);
        }
    }
}
