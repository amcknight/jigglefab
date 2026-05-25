//! Per-frame sim telemetry — currently bead speed distribution. Exposed to
//! the web HUD via `window.__jigglefabSpeeds()`. Used to verify empirically
//! that `unit speed` is initial-only ([[jigglefab-speeds-not-invariant]])
//! and to inform thresholds in perf optimisations (e.g. the CCD early-out).
//!
//! Values are f32 packed as u32 bits in atomics — single-threaded wasm so
//! `Relaxed` is fine; native MT writes via [`update_from_velocities`] from
//! the event-loop thread after `scheduler.step`, so no contention either.

use std::sync::atomic::{AtomicU32, Ordering};

use glam::Vec2;

static MIN_BITS: AtomicU32 = AtomicU32::new(0);
static MEAN_BITS: AtomicU32 = AtomicU32::new(0);
static MAX_BITS: AtomicU32 = AtomicU32::new(0);

pub fn update_from_velocities(vels: &[Vec2]) {
    if vels.is_empty() {
        return;
    }
    let mut min_sq = f32::INFINITY;
    let mut max_sq = 0.0_f32;
    let mut sum: f64 = 0.0;
    for v in vels {
        let s = v.length();
        if s.is_finite() {
            if (s * s) < min_sq {
                min_sq = s * s;
            }
            if (s * s) > max_sq {
                max_sq = s * s;
            }
            sum += s as f64;
        }
    }
    let n = vels.len() as f64;
    let min = min_sq.sqrt();
    let max = max_sq.sqrt();
    let mean = (sum / n) as f32;
    MIN_BITS.store(min.to_bits(), Ordering::Relaxed);
    MEAN_BITS.store(mean.to_bits(), Ordering::Relaxed);
    MAX_BITS.store(max.to_bits(), Ordering::Relaxed);
}

pub fn min() -> f32 {
    f32::from_bits(MIN_BITS.load(Ordering::Relaxed))
}
pub fn mean() -> f32 {
    f32::from_bits(MEAN_BITS.load(Ordering::Relaxed))
}
pub fn max() -> f32 {
    f32::from_bits(MAX_BITS.load(Ordering::Relaxed))
}
