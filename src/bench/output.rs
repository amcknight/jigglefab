use crate::bench::runner::ScenarioResult;

/// Pretty markdown table for stdout. Truncated scenarios get a `*` next to
/// their name; the footer lists their actual frame counts.
pub fn format_markdown(results: &[ScenarioResult]) -> String {
    let mut out = String::new();
    out.push_str("| scenario          |   N   | frame_ms (mean/p99) | substep_us (mean/p99) | contacts/ss (mean/p99) |   fps   | sub/16ms | iter_cap_sat | bonds OK |\n");
    out.push_str("|-------------------|-------|---------------------|-----------------------|------------------------|---------|----------|--------------|----------|\n");
    let mut truncated_notes = Vec::new();
    for r in results {
        let name_display = if r.truncated {
            truncated_notes.push(format!(
                "- `{}` truncated after {}/{} frames",
                r.name, r.frames_completed, r.frames_requested
            ));
            format!("{} *", r.name)
        } else {
            r.name.clone()
        };
        let bonds_ok: String = if r.bonds_preserved {
            "y".to_string()
        } else {
            format!("n (-{}/+{})", r.bonds_lost, r.bonds_added)
        };
        out.push_str(&format!(
            "| {:<17} | {:>5} | {:>8.2} / {:>8.2} | {:>8.0} / {:>8.0} | {:>8.1} / {:>8.1} | {:>7.1} | {:>8} | {:>12.4} | {:>8} |\n",
            name_display,
            r.bead_count,
            r.frame_time_ms.mean,
            r.frame_time_ms.p99,
            r.substep_time_us.mean,
            r.substep_time_us.p99,
            r.contacts_per_substep.mean,
            r.contacts_per_substep.p99,
            r.effective_fps,
            r.substeps_per_16ms_budget,
            r.iter_cap_saturation_rate,
            bonds_ok,
        ));
    }
    if !truncated_notes.is_empty() {
        out.push('\n');
        for n in &truncated_notes {
            out.push_str(n);
            out.push('\n');
        }
    }
    out
}

/// CSV: one header row + one row per scenario. Wide format so each metric
/// is its own column.
pub fn format_csv(results: &[ScenarioResult]) -> String {
    let mut out = String::new();
    out.push_str("scenario,bead_count,frames_completed,frames_requested,truncated,frame_ms_mean,frame_ms_p50,frame_ms_p99,frame_ms_max,substep_us_mean,substep_us_p50,substep_us_p99,substep_us_max,contacts_mean,contacts_p50,contacts_p99,contacts_max,candidate_pairs_mean,iter_cap_saturation,effective_fps,substeps_per_16ms,bonds_preserved,bonds_lost,bonds_added,determinism_verified\n");
    for r in results {
        let det = match r.determinism_verified {
            Some(true) => "true",
            Some(false) => "false",
            None => "",
        };
        out.push_str(&format!(
            "{},{},{},{},{},{:.4},{:.4},{:.4},{:.4},{:.4},{:.4},{:.4},{:.4},{:.4},{:.4},{:.4},{:.4},{:.4},{:.6},{:.4},{},{},{},{},{}\n",
            r.name,
            r.bead_count,
            r.frames_completed,
            r.frames_requested,
            r.truncated,
            r.frame_time_ms.mean, r.frame_time_ms.p50, r.frame_time_ms.p99, r.frame_time_ms.max,
            r.substep_time_us.mean, r.substep_time_us.p50, r.substep_time_us.p99, r.substep_time_us.max,
            r.contacts_per_substep.mean, r.contacts_per_substep.p50, r.contacts_per_substep.p99, r.contacts_per_substep.max,
            r.candidate_pairs_per_substep_mean,
            r.iter_cap_saturation_rate,
            r.effective_fps,
            r.substeps_per_16ms_budget,
            r.bonds_preserved,
            r.bonds_lost,
            r.bonds_added,
            det,
        ));
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bench::runner::Percentiles;

    fn fixture() -> ScenarioResult {
        ScenarioResult {
            name: "chains_test".to_string(),
            bead_count: 100,
            frames_completed: 3000,
            frames_requested: 3000,
            truncated: false,
            frame_time_ms: Percentiles { mean: 1.0, p50: 1.0, p99: 1.5, max: 2.0 },
            substep_time_us: Percentiles { mean: 100.0, p50: 95.0, p99: 200.0, max: 300.0 },
            contacts_per_substep: Percentiles { mean: 3.0, p50: 3.0, p99: 5.0, max: 7.0 },
            candidate_pairs_per_substep_mean: 50.0,
            iter_cap_saturation_rate: 0.0,
            effective_fps: 1000.0,
            substeps_per_16ms_budget: 166,
            bonds_preserved: true,
            bonds_lost: 0,
            bonds_added: 0,
            determinism_verified: None,
        }
    }

    #[test]
    fn markdown_has_header_and_one_row() {
        let md = format_markdown(&[fixture()]);
        assert!(md.contains("scenario"));
        assert!(md.contains("chains_test"));
        assert!(md.contains("100"));
        // Untruncated runs should not get a star or footnote.
        assert!(!md.contains(" *"));
    }

    #[test]
    fn markdown_marks_truncated_scenarios() {
        let mut f = fixture();
        f.truncated = true;
        f.frames_completed = 47;
        let md = format_markdown(&[f]);
        assert!(md.contains("chains_test *"));
        assert!(md.contains("truncated after 47/3000"));
    }

    #[test]
    fn csv_has_header_and_one_row() {
        let csv = format_csv(&[fixture()]);
        let mut lines = csv.lines();
        let header = lines.next().unwrap();
        assert!(header.starts_with("scenario,"));
        assert!(header.contains("bonds_preserved"));
        let row = lines.next().unwrap();
        assert!(row.starts_with("chains_test,"));
    }

    #[test]
    fn csv_renders_determinism_field_as_string() {
        let mut f = fixture();
        f.determinism_verified = Some(true);
        let csv = format_csv(&[f]);
        // Last column is the determinism field; row ends with ",true\n" for Some(true).
        let row = csv.lines().nth(1).unwrap();
        assert!(row.ends_with(",true"), "row was: {}", row);
    }
}
