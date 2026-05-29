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
        let bp = BondPair::new(0, 1);
        let json = serde_json::to_string(&bp).unwrap();
        assert_eq!(json, "[0,1]");
        let back: BondPair = serde_json::from_str("[1,0]").unwrap();
        assert_eq!(back, bp);
    }
}
