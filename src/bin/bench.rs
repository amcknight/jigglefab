use std::env;
use std::process::ExitCode;
use std::fs;

use jigglefab::bench::{
    BenchArgs, DisconnectedChains, Scenario, ScenarioResult, format_csv, format_markdown,
    run_scenario,
};
use jigglefab::scheduler::{CpuSequential, Scheduler};
#[cfg(not(target_arch = "wasm32"))]
use jigglefab::gpu::context::GpuContext;
#[cfg(not(target_arch = "wasm32"))]
use jigglefab::gpu::scheduler::GpuEventLoop;

struct ParsedArgs {
    bench: BenchArgs,
    scenarios_filter: Option<Vec<String>>,
    csv_path: Option<String>,
    scheduler: String,
}

fn print_usage() {
    eprintln!("Usage: cargo run --release --bin bench -- [OPTIONS]");
    eprintln!("Options:");
    eprintln!("  --scenarios <a,b,c>     Subset of scenarios to run (default: all default-sweep)");
    eprintln!("  --substeps <N>          Substeps per frame (default: 10)");
    eprintln!("  --frames <N>            Frames post-warmup (default: 3000)");
    eprintln!("  --warmup <N>            Warmup frames discarded (default: 60)");
    eprintln!("  --max-wall-seconds <S>  Per-scenario wall cap (default: 300)");
    eprintln!("  --csv <path>            Write CSV to this path");
    eprintln!("  --verify-determinism    Re-run each scenario and check bit-equality");
    eprintln!("  --scheduler <name>      Scheduler to use: cpu, gpu (default: cpu)");
    eprintln!("  --help                  Show this message");
}

fn parse_args() -> Result<ParsedArgs, String> {
    let mut bench = BenchArgs::default();
    let mut scenarios_filter: Option<Vec<String>> = None;
    let mut csv_path: Option<String> = None;
    let mut scheduler = String::from("cpu");
    let argv: Vec<String> = env::args().skip(1).collect();
    let mut i = 0;
    while i < argv.len() {
        match argv[i].as_str() {
            "--help" => {
                print_usage();
                std::process::exit(0);
            }
            "--scenarios" => {
                i += 1;
                let v = argv.get(i).ok_or("--scenarios needs a value")?;
                scenarios_filter = Some(v.split(',').map(|s| s.trim().to_string()).collect());
            }
            "--substeps" => {
                i += 1;
                bench.substeps = argv.get(i).ok_or("--substeps needs a value")?
                    .parse().map_err(|e: std::num::ParseIntError| e.to_string())?;
            }
            "--frames" => {
                i += 1;
                bench.frames = argv.get(i).ok_or("--frames needs a value")?
                    .parse().map_err(|e: std::num::ParseIntError| e.to_string())?;
            }
            "--warmup" => {
                i += 1;
                bench.warmup_frames = argv.get(i).ok_or("--warmup needs a value")?
                    .parse().map_err(|e: std::num::ParseIntError| e.to_string())?;
            }
            "--max-wall-seconds" => {
                i += 1;
                bench.max_wall_seconds = argv.get(i).ok_or("--max-wall-seconds needs a value")?
                    .parse().map_err(|e: std::num::ParseFloatError| e.to_string())?;
            }
            "--csv" => {
                i += 1;
                csv_path = Some(argv.get(i).ok_or("--csv needs a value")?.clone());
            }
            "--verify-determinism" => {
                bench.verify_determinism = true;
            }
            "--scheduler" => {
                i += 1;
                scheduler = argv.get(i).ok_or("--scheduler needs a value")?.clone();
            }
            other => return Err(format!("unknown arg: {}", other)),
        }
        i += 1;
    }
    Ok(ParsedArgs { bench, scenarios_filter, csv_path, scheduler })
}

/// The default sweep. Excludes `chains_100x100` (opt-in via --scenarios).
fn default_scenarios() -> Vec<Box<dyn Scenario>> {
    vec![
        Box::new(DisconnectedChains { chain_count: 10,  chain_len: 30,  world_size: 50.0 }),
        Box::new(DisconnectedChains { chain_count: 30,  chain_len: 30,  world_size: 128.0 }),
        Box::new(DisconnectedChains { chain_count: 50,  chain_len: 30,  world_size: 256.0 }),
        Box::new(DisconnectedChains { chain_count: 10,  chain_len: 100, world_size: 128.0 }),
        Box::new(DisconnectedChains { chain_count: 5,   chain_len: 300, world_size: 256.0 }),
        Box::new(DisconnectedChains { chain_count: 100, chain_len: 30,  world_size: 256.0 }),
    ]
}

/// All known scenarios — used when --scenarios filters by name and
/// chains_100x100 is requested explicitly.
fn all_scenarios() -> Vec<Box<dyn Scenario>> {
    let mut s = default_scenarios();
    s.push(Box::new(DisconnectedChains { chain_count: 100, chain_len: 100, world_size: 256.0 }));
    s
}

fn select_scenarios(filter: Option<Vec<String>>) -> Vec<Box<dyn Scenario>> {
    match filter {
        None => default_scenarios(),
        Some(names) => {
            let all = all_scenarios();
            all.into_iter()
                .filter(|s| names.iter().any(|n| n == &s.name()))
                .collect()
        }
    }
}

fn main() -> ExitCode {
    let parsed = match parse_args() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("error: {}", e);
            print_usage();
            return ExitCode::from(2);
        }
    };

    // Validate scheduler name before doing any work.
    match parsed.scheduler.as_str() {
        "cpu" | "gpu" => {}
        other => {
            eprintln!("error: unknown scheduler {:?} (valid: cpu, gpu)", other);
            print_usage();
            return ExitCode::from(2);
        }
    }

    let scenarios = select_scenarios(parsed.scenarios_filter.clone());
    if scenarios.is_empty() {
        eprintln!("no scenarios match filter: {:?}", parsed.scenarios_filter);
        return ExitCode::from(2);
    }

    let mut results: Vec<ScenarioResult> = Vec::with_capacity(scenarios.len());
    for scenario in &scenarios {
        eprintln!("running {}...", scenario.name());

        // For GPU, GpuEventLoop is baked to a specific sim's buffer sizes, so
        // we build a sizing sim first, then create a fresh scheduler for each
        // scenario. A fresh GpuContext (new headless device) is used per
        // scenario to avoid sharing ownership across the move into GpuEventLoop.
        #[cfg(not(target_arch = "wasm32"))]
        let r = if parsed.scheduler == "gpu" {
            let (sizing_sim, _) = scenario.build();
            let ctx = match GpuContext::new_headless() {
                Ok(c) => c,
                Err(e) => {
                    eprintln!("error: GPU context failed for {}: {e}", scenario.name());
                    return ExitCode::from(1);
                }
            };
            let mut gpu_sched: Box<dyn Scheduler> = Box::new(GpuEventLoop::new(ctx, &sizing_sim));
            run_scenario(scenario.as_ref(), &parsed.bench, gpu_sched.as_mut())
        } else {
            let mut cpu_sched: Box<dyn Scheduler> = Box::new(CpuSequential);
            run_scenario(scenario.as_ref(), &parsed.bench, cpu_sched.as_mut())
        };

        #[cfg(target_arch = "wasm32")]
        let r = {
            let mut cpu_sched: Box<dyn Scheduler> = Box::new(CpuSequential);
            run_scenario(scenario.as_ref(), &parsed.bench, cpu_sched.as_mut())
        };

        eprintln!(
            "  {} N={} frame_ms mean={:.2} p99={:.2} fps={:.1} bonds_ok={} truncated={}",
            r.name, r.bead_count, r.frame_time_ms.mean, r.frame_time_ms.p99,
            r.effective_fps, r.bonds_preserved, r.truncated,
        );
        results.push(r);
    }

    print!("{}", format_markdown(&results));

    if let Some(path) = parsed.csv_path {
        let csv = format_csv(&results);
        if let Err(e) = fs::write(&path, csv) {
            eprintln!("failed to write CSV to {}: {}", path, e);
            return ExitCode::from(1);
        }
        eprintln!("wrote CSV to {}", path);
    }

    ExitCode::SUCCESS
}
