//! Per-phase timing accumulators for the CpuParallel substep loop. Lets
//! the bench (and, via JS exposure, the web HUD) attribute time to each
//! phase: contact detection, coloring, resolve, advance, bond enforcement.
//!
//! Cost of instrumentation: 5 `Instant::now()` calls and 5 `fetch_add`s per
//! substep. At 30 k beads / substeps=10 that's ~50 ns × 50 = 2.5 µs overhead
//! per app-frame, dwarfed by the 700 ms of work it's measuring.

use std::sync::atomic::{AtomicU64, Ordering};

pub static CONTACTS_NS: AtomicU64 = AtomicU64::new(0);
// Sub-phases inside compute_active_contacts:
pub static CT_BIN_NS: AtomicU64 = AtomicU64::new(0);
pub static CT_CANDIDATES_NS: AtomicU64 = AtomicU64::new(0);
pub static CT_CCD_NS: AtomicU64 = AtomicU64::new(0);
pub static CT_SORT_NS: AtomicU64 = AtomicU64::new(0);

pub static COLOR_NS: AtomicU64 = AtomicU64::new(0);
pub static RESOLVE_NS: AtomicU64 = AtomicU64::new(0);
pub static ADVANCE_NS: AtomicU64 = AtomicU64::new(0);
pub static BONDS_NS: AtomicU64 = AtomicU64::new(0);
pub static SUBSTEPS: AtomicU64 = AtomicU64::new(0);

#[derive(Clone, Copy, Debug, Default)]
pub struct PhaseProfile {
    pub contacts_ns: u64,
    pub ct_bin_ns: u64,
    pub ct_candidates_ns: u64,
    pub ct_ccd_ns: u64,
    pub ct_sort_ns: u64,
    pub color_ns: u64,
    pub resolve_ns: u64,
    pub advance_ns: u64,
    pub bonds_ns: u64,
    pub substeps: u64,
}

impl PhaseProfile {
    pub fn total_ns(&self) -> u64 {
        self.contacts_ns + self.color_ns + self.resolve_ns + self.advance_ns + self.bonds_ns
    }
}

pub fn reset() {
    for a in [
        &CONTACTS_NS, &CT_BIN_NS, &CT_CANDIDATES_NS, &CT_CCD_NS, &CT_SORT_NS,
        &COLOR_NS, &RESOLVE_NS, &ADVANCE_NS, &BONDS_NS, &SUBSTEPS,
    ] {
        a.store(0, Ordering::Relaxed);
    }
}

pub fn snapshot() -> PhaseProfile {
    PhaseProfile {
        contacts_ns: CONTACTS_NS.load(Ordering::Relaxed),
        ct_bin_ns: CT_BIN_NS.load(Ordering::Relaxed),
        ct_candidates_ns: CT_CANDIDATES_NS.load(Ordering::Relaxed),
        ct_ccd_ns: CT_CCD_NS.load(Ordering::Relaxed),
        ct_sort_ns: CT_SORT_NS.load(Ordering::Relaxed),
        color_ns: COLOR_NS.load(Ordering::Relaxed),
        resolve_ns: RESOLVE_NS.load(Ordering::Relaxed),
        advance_ns: ADVANCE_NS.load(Ordering::Relaxed),
        bonds_ns: BONDS_NS.load(Ordering::Relaxed),
        substeps: SUBSTEPS.load(Ordering::Relaxed),
    }
}
