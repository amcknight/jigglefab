//! Speed multiplier state for the web demo's substep loop.
//!
//! The multiplier scales the number of substeps run per rendered frame
//! at constant `dt`. URL hash format: `#<fab>[&speed=<value>]`, where
//! `<value>` is one of [`ALLOWED_SPEED_STRINGS`]. Missing, unknown, or
//! malformed values resolve to `1.0`.

use std::sync::atomic::{AtomicU32, Ordering};

/// Substeps per render frame at `speed = 1.0×`. Matches the historic
/// hard-coded `SUBSTEPS` constant in `app.rs`.
pub const BASE_SUBSTEPS: u32 = 10;

/// Allowed speed pill values, as the exact strings used in URL hashes
/// and JS. Compared textually to avoid float-precision surprises when
/// parsing `0.1` and `0.3`.
pub const ALLOWED_SPEED_STRINGS: &[&str] =
    &["0.1", "0.3", "1", "3", "10", "30", "100", "300", "1000"];

/// Live multiplier, stored as fixed-point `speed × 1000` so we can use
/// `AtomicU32` (no stable `AtomicF32`). `1000` == `1.0×`.
pub static SPEED_FIXED: AtomicU32 = AtomicU32::new(1000);

pub fn set_speed(multiplier: f32) {
    let fixed = (multiplier * 1000.0).round().max(1.0) as u32;
    SPEED_FIXED.store(fixed, Ordering::Relaxed);
}

pub fn current_speed() -> f32 {
    SPEED_FIXED.load(Ordering::Relaxed) as f32 / 1000.0
}

pub fn current_substeps() -> u32 {
    substeps_for_speed(current_speed())
}

fn substeps_for_speed(speed: f32) -> u32 {
    let n = (BASE_SUBSTEPS as f32 * speed).round() as u32;
    n.max(1)
}

/// Parse a speed multiplier out of a URL hash like `#wire-20x30&speed=10`.
/// Returns `1.0` for missing, unknown, or malformed values.
pub fn parse_speed_from_hash(hash: &str) -> f32 {
    let hash = hash.trim_start_matches('#');
    for segment in hash.split('&').skip(1) {
        if let Some(value) = segment.strip_prefix("speed=") {
            if ALLOWED_SPEED_STRINGS.contains(&value) {
                if let Ok(v) = value.parse::<f32>() {
                    return v;
                }
            }
        }
    }
    1.0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn substeps_at_default_speed() {
        assert_eq!(substeps_for_speed(1.0), 10);
    }

    #[test]
    fn substeps_at_10x() {
        assert_eq!(substeps_for_speed(10.0), 100);
    }

    #[test]
    fn substeps_at_1000x() {
        assert_eq!(substeps_for_speed(1000.0), 10_000);
    }

    #[test]
    fn substeps_at_0_1x_clamps_to_one() {
        // 10 * 0.1 = 1.0, rounds to 1.
        assert_eq!(substeps_for_speed(0.1), 1);
    }

    #[test]
    fn substeps_min_one_even_for_tiny_speed() {
        assert_eq!(substeps_for_speed(0.0001), 1);
        assert_eq!(substeps_for_speed(0.0), 1);
    }

    #[test]
    fn parse_speed_missing_returns_one() {
        assert_eq!(parse_speed_from_hash(""), 1.0);
        assert_eq!(parse_speed_from_hash("#"), 1.0);
        assert_eq!(parse_speed_from_hash("#wire-20x30"), 1.0);
    }

    #[test]
    fn parse_speed_recognises_each_allowed_value() {
        for &s in ALLOWED_SPEED_STRINGS {
            let hash = format!("#wire-20x30&speed={s}");
            let parsed = parse_speed_from_hash(&hash);
            let expected: f32 = s.parse().unwrap();
            assert!(
                (parsed - expected).abs() < f32::EPSILON,
                "{s}: parsed {parsed} != expected {expected}"
            );
        }
    }

    #[test]
    fn parse_speed_disallowed_returns_one() {
        // 5 isn't in ALLOWED_SPEED_STRINGS.
        assert_eq!(parse_speed_from_hash("#wire-20x30&speed=5"), 1.0);
        // Negative is disallowed.
        assert_eq!(parse_speed_from_hash("#wire-20x30&speed=-1"), 1.0);
    }

    #[test]
    fn parse_speed_garbage_returns_one() {
        assert_eq!(parse_speed_from_hash("#wire-20x30&speed=foo"), 1.0);
        assert_eq!(parse_speed_from_hash("#wire-20x30&speed="), 1.0);
    }

    #[test]
    fn parse_speed_ignores_unknown_segments() {
        assert_eq!(
            parse_speed_from_hash("#wire-20x30&foo=bar&speed=10&baz=qux"),
            10.0
        );
    }

    #[test]
    fn set_and_read_back_round_trips() {
        set_speed(3.0);
        assert!((current_speed() - 3.0).abs() < f32::EPSILON);
        assert_eq!(current_substeps(), 30);
        // Restore default so other tests (or the running app) aren't affected.
        set_speed(1.0);
    }
}
