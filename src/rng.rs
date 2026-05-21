/// Counter-based PRNG. Returns a uniform u64 from a (seed, bead_id, tick) triple.
/// Implementation: hash the three inputs together with SplitMix64.
pub fn prng_u64(seed: u64, bead_id: u32, tick: u32) -> u64 {
    let mut x = seed
        ^ ((bead_id as u64).wrapping_mul(0x9E3779B97F4A7C15))
        ^ ((tick as u64).wrapping_mul(0xBF58476D1CE4E5B9));
    x = splitmix64(x);
    x
}

/// Returns a uniform f32 in [0.0, 1.0).
pub fn prng_f32(seed: u64, bead_id: u32, tick: u32) -> f32 {
    // Top 24 bits of the u64, scaled to [0, 1).
    let u = prng_u64(seed, bead_id, tick);
    ((u >> 40) as f32) / ((1u64 << 24) as f32)
}

fn splitmix64(mut x: u64) -> u64 {
    x = x.wrapping_add(0x9E3779B97F4A7C15);
    x = (x ^ (x >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
    x = (x ^ (x >> 27)).wrapping_mul(0x94D049BB133111EB);
    x ^ (x >> 31)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn deterministic_same_input() {
        assert_eq!(prng_u64(42, 0, 0), prng_u64(42, 0, 0));
        assert_eq!(prng_f32(42, 7, 13), prng_f32(42, 7, 13));
    }

    #[test]
    fn differs_per_id() {
        assert_ne!(prng_u64(42, 0, 0), prng_u64(42, 1, 0));
        assert_ne!(prng_u64(42, 0, 0), prng_u64(42, 0, 1));
        assert_ne!(prng_u64(42, 0, 0), prng_u64(43, 0, 0));
    }

    #[test]
    fn f32_in_unit_interval() {
        for id in 0..1000 {
            let v = prng_f32(42, id, 0);
            assert!(v >= 0.0 && v < 1.0, "value out of range: {}", v);
        }
    }
}
