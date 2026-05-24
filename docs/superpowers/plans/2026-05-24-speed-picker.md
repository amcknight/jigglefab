# Speed Picker Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a discrete speed multiplier (0.1× … 1000×) for the web demo that scales substeps-per-frame at constant `dt`, with live updates and URL-hash persistence.

**Architecture:** A small `src/speed.rs` module owns the multiplier state (an `AtomicU32` storing `speed × 1000` fixed-point), the URL-hash parser, and the `current_substeps()` derivation. `src/app.rs` initializes the atomic from the URL hash at wasm startup, installs `window.__jigglefabSetSpeed` so JS can update it live, and reads `current_substeps()` each frame instead of the old `SUBSTEPS` constant. `index.html` paints a second pill bar above the fab picker; clicks rewrite the hash via `history.replaceState` and call the window setter — no page reload.

**Tech Stack:** Rust + wasm-bindgen + web-sys + js-sys for the WASM side; inline ES JS in `index.html` for the UI.

**Spec:** `docs/superpowers/specs/2026-05-24-speed-picker-design.md`

---

## File Map

- **Create** `src/speed.rs` — speed state (atomic), `current_substeps()`, `parse_speed_from_hash()`, allowed-values list, unit tests.
- **Modify** `src/lib.rs` — add `pub mod speed;`.
- **Modify** `src/app.rs` — drop `const SUBSTEPS`, call `current_substeps()` in the substep loop; on wasm startup parse the hash for speed and install the window setter.
- **Modify** `Cargo.toml` — add `js-sys = "0.3"` to the wasm target dependencies.
- **Modify** `index.html` — add `SPEEDS` constant, `currentSpeed()`, `paintSpeedPicker()`, hash-update on click, styling, and new `<nav id="speed-picker">` element.

---

### Task 1: Pure speed module with TDD

**Files:**
- Create: `src/speed.rs`
- Modify: `src/lib.rs`

- [ ] **Step 1: Create `src/speed.rs` with the failing tests first**

Write this file (tests use only pure helpers, no atomic state, so they're parallel-safe):

```rust
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
```

- [ ] **Step 2: Register the module**

Edit `src/lib.rs` — add `pub mod speed;` after the existing `pub mod scheduler;` line (alphabetical-ish doesn't matter here; just put it before the `#[cfg(target_arch = "wasm32")]` block at the bottom).

```rust
pub mod scheduler;
pub mod gpu;
pub mod parallel;
pub mod speed;
```

- [ ] **Step 3: Run the tests and verify they pass**

Run: `cargo test --lib speed`
Expected: 11 tests pass (`substeps_at_default_speed`, `substeps_at_10x`, `substeps_at_1000x`, `substeps_at_0_1x_clamps_to_one`, `substeps_min_one_even_for_tiny_speed`, `parse_speed_missing_returns_one`, `parse_speed_recognises_each_allowed_value`, `parse_speed_disallowed_returns_one`, `parse_speed_garbage_returns_one`, `parse_speed_ignores_unknown_segments`, `set_and_read_back_round_trips`).

If `cargo` is not on PATH in your shell, use `cargo.exe` (Windows GNU toolchain) or check the build-env memory for the right invocation.

- [ ] **Step 4: Commit**

```bash
git add src/speed.rs src/lib.rs
git commit -m "feat(speed): pure module with multiplier state and URL parser"
```

---

### Task 2: Wire speed into the substep loop

**Files:**
- Modify: `src/app.rs`

- [ ] **Step 1: Drop the `SUBSTEPS` constant and use `current_substeps()` in the loop**

In `src/app.rs`, delete lines 26–28 (the `// Substeps per rendered frame…` comment block and `const SUBSTEPS: u32 = 10;`).

Replace the substep loop body inside `WindowEvent::RedrawRequested` (currently at `src/app.rs:244`):

```rust
                    for _ in 0..SUBSTEPS {
                        self.scheduler.step(sim, FRAME_DT);
                    }
```

with:

```rust
                    for _ in 0..crate::speed::current_substeps() {
                        self.scheduler.step(sim, FRAME_DT);
                    }
```

Native and web builds both call `current_substeps()`; on native the atomic stays at its default (`1000` == `1.0×`), so native behavior is unchanged.

- [ ] **Step 2: Parse the speed from the URL hash on wasm startup**

In the `#[cfg(target_arch = "wasm32")]` branch of `resumed()` (currently around `src/app.rs:141-148`), right after the `let (name, fab_toml) = pick_fab_from_url();` line, parse and apply the initial speed:

```rust
        #[cfg(target_arch = "wasm32")]
        let sim = {
            let (name, fab_toml) = pick_fab_from_url();
            log::info!("loading fab {name}");
            let hash = web_sys::window()
                .and_then(|w| w.location().hash().ok())
                .unwrap_or_default();
            let speed = crate::speed::parse_speed_from_hash(&hash);
            crate::speed::set_speed(speed);
            log::info!("initial speed = {speed}×");
            let fab = parse_fab(fab_toml).expect("parse fab");
            let chem = parse_chemistry(CHEMISTRY_TOML).expect("parse chem");
            Sim::from_fab(&fab, chem)
        };
```

- [ ] **Step 3: Build for native and wasm**

Run: `cargo build --lib`
Expected: clean build.

Run: `cargo build --lib --target wasm32-unknown-unknown` (or whatever wasm build command the project uses — `trunk build` is the integration path; see CLAUDE.md / build-env memory).
Expected: clean build.

- [ ] **Step 4: Native smoke run (sanity)**

Run: `cargo run --bin jigglefab` (briefly — kill after a few seconds).
Expected: window opens, chains jiggle as before. Native path is unaffected.

- [ ] **Step 5: Commit**

```bash
git add src/app.rs
git commit -m "feat(app): drive substep count from speed module"
```

---

### Task 3: Live update entry point — install `window.__jigglefabSetSpeed`

**Files:**
- Modify: `Cargo.toml`
- Modify: `src/app.rs`

- [ ] **Step 1: Add `js-sys` to wasm dependencies**

Edit `Cargo.toml`, in the `[target.'cfg(target_arch = "wasm32")'.dependencies]` block, add `js-sys = "0.3"`:

```toml
[target.'cfg(target_arch = "wasm32")'.dependencies]
wasm-bindgen = "=0.2.95"
wasm-bindgen-futures = "0.4"
console_error_panic_hook = "0.1"
console_log = "1"
web-sys = { version = "0.3", features = ["Document", "Element", "HtmlCanvasElement", "Window", "Location"] }
js-sys = "0.3"
```

- [ ] **Step 2: Add the installer function in `src/app.rs`**

At the top of `src/app.rs`, near the other `#[cfg(target_arch = "wasm32")]` `use`s, add the helper:

```rust
#[cfg(target_arch = "wasm32")]
fn install_window_speed_setter() {
    use wasm_bindgen::closure::Closure;
    use wasm_bindgen::JsCast;

    // Closure is leaked intentionally — it lives for the lifetime of the page.
    let cb = Closure::wrap(Box::new(|m: f32| {
        crate::speed::set_speed(m);
    }) as Box<dyn Fn(f32)>);

    if let Some(window) = web_sys::window() {
        let _ = js_sys::Reflect::set(
            &window,
            &wasm_bindgen::JsValue::from_str("__jigglefabSetSpeed"),
            cb.as_ref().unchecked_ref(),
        );
    }
    cb.forget();
}
```

- [ ] **Step 3: Call the installer once at startup**

In the `#[cfg(target_arch = "wasm32")]` block inside `resumed()` (currently around `src/app.rs:174-195`), add a call to `install_window_speed_setter()` near the top of the block — right after the `compile_chemistry` / scheduler setup is fine, before the `spawn_local` that starts the renderer:

```rust
        #[cfg(target_arch = "wasm32")]
        {
            use crate::chemistry::compile_chemistry;
            use crate::parallel::CpuParallel;
            let compiled = compile_chemistry(sim.chemistry()).expect("compile chemistry");
            self.scheduler = Box::new(CpuParallel::new(&sim, compiled));

            install_window_speed_setter();

            let proxy = self.proxy.clone().expect("proxy not set before resumed()");
            // …rest unchanged…
```

- [ ] **Step 4: Build wasm**

Run: `trunk build` (or the project's standard web build command).
Expected: clean build, fresh `dist/jigglefab-*.js` and `.wasm` artifacts.

- [ ] **Step 5: Commit**

```bash
git add Cargo.toml src/app.rs
git commit -m "feat(app): expose live speed setter to JS via window"
```

---

### Task 4: Speed picker UI in `index.html`

**Files:**
- Modify: `index.html`

- [ ] **Step 1: Add styling for the speed picker**

In the `<style>` block, add new rules right after the `#picker a.active .beads { color: #cee; }` line (around `index.html:104`):

```css
        #speed-picker {
            position: absolute;
            bottom: 56px;
            left: 50%;
            transform: translateX(-50%);
            max-width: calc(100vw - 16px);
            display: flex;
            flex-wrap: wrap;
            gap: 4px;
            padding: 6px;
            background: rgba(0, 0, 0, 0.45);
            border-radius: 10px;
            font: 12px/1.2 ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
            z-index: 10;
            justify-content: center;
        }
        #speed-picker .group-label {
            color: #888;
            padding: 4px 6px;
            align-self: center;
        }
        #speed-picker a {
            color: #cfd;
            text-decoration: none;
            padding: 4px 10px;
            border-radius: 6px;
            background: rgba(255, 255, 255, 0.06);
            border: 1px solid transparent;
            white-space: nowrap;
            cursor: pointer;
        }
        #speed-picker a:hover {
            background: rgba(255, 255, 255, 0.12);
        }
        #speed-picker a.active {
            background: #2a4d3a;
            color: #fff;
            border-color: #4a8;
        }
```

(The `bottom: 56px` lifts this bar above the existing `#picker` which sits at `bottom: 12px` and is roughly 32–40 px tall depending on wrap — a 56 px offset clears it on a single-row picker. If the fab picker wraps on a narrow viewport, the speed bar overlaps; acceptable for P1 — phone testing will confirm.)

- [ ] **Step 2: Add the `<nav>` element**

In the body, right above `<nav id="picker"></nav>` (around `index.html:122`), add:

```html
    <nav id="speed-picker"></nav>
    <nav id="picker"></nav>
```

- [ ] **Step 3: Add the SPEEDS constant and helpers in the inline `<script>`**

Just after the `DEFAULT_KEY` / `FABS_BY_KEY` declarations (around `index.html:137-138`), add:

```javascript
        // Speed pill values. Must match ALLOWED_SPEED_STRINGS in src/speed.rs.
        // 1× is the default and styled active when no &speed= is in the hash.
        const SPEEDS = ["0.1", "0.3", "1", "3", "10", "30", "100", "300", "1000"];
        const DEFAULT_SPEED = "1";

        function parseHash() {
            // Hash format: #<fab-key>[&key=value][&key=value]...
            const raw = (location.hash || "").replace(/^#/, "");
            const segments = raw.split("&");
            const fab = segments[0] || "";
            const params = {};
            for (let i = 1; i < segments.length; i++) {
                const eq = segments[i].indexOf("=");
                if (eq > 0) params[segments[i].slice(0, eq)] = segments[i].slice(eq + 1);
            }
            return { fab, params };
        }

        function currentSpeed() {
            const { params } = parseHash();
            const s = params.speed;
            return SPEEDS.includes(s) ? s : DEFAULT_SPEED;
        }

        function buildHash(fabKey, speed) {
            // Omit &speed= when default, to keep canonical URLs clean.
            if (speed === DEFAULT_SPEED) return "#" + fabKey;
            return "#" + fabKey + "&speed=" + speed;
        }
```

- [ ] **Step 4: Add `paintSpeedPicker` and wire the click handler**

After `paintPicker()` (around `index.html:170`), add:

```javascript
        function paintSpeedPicker() {
            const bar = document.getElementById("speed-picker");
            const active = currentSpeed();
            bar.innerHTML = "";
            const label = document.createElement("span");
            label.className = "group-label";
            label.textContent = "speed";
            bar.appendChild(label);
            for (const s of SPEEDS) {
                const a = document.createElement("a");
                a.href = "#";
                a.textContent = s + "×";
                a.className = s === active ? "active" : "";
                a.addEventListener("click", (e) => {
                    e.preventDefault();
                    selectSpeed(s);
                });
                bar.appendChild(a);
            }
        }

        function selectSpeed(s) {
            const { fab } = parseHash();
            const fabKey = fab || DEFAULT_KEY;
            const newHash = buildHash(fabKey, s);
            history.replaceState(null, "", newHash);
            paintSpeedPicker();
            if (typeof window.__jigglefabSetSpeed === "function") {
                window.__jigglefabSetSpeed(parseFloat(s));
            }
        }
```

- [ ] **Step 5: Update `currentFab` to handle the new hash format**

`currentFab()` currently does `(location.hash || "").replace(/^#/, "")` and treats the whole thing as the key. With `&speed=10` appended, that key won't match. Update it (around `index.html:140-143`) to:

```javascript
        function currentFab() {
            const { fab } = parseHash();
            return FABS_BY_KEY[fab] || FABS_BY_KEY[DEFAULT_KEY];
        }
```

- [ ] **Step 6: Call `paintSpeedPicker()` at startup**

Right after the existing `paintPicker();` line (around `index.html:176`), add:

```javascript
        paintSpeedPicker();
```

- [ ] **Step 7: Manual smoke test in the browser**

Run: `trunk serve --open` (or your usual dev-server command).
Expected:
- Page loads, default speed pill `1×` is active styling.
- Clicking `10×` accelerates the chains, URL becomes `#wire-20x30&speed=10`, pill `10×` is now active.
- Clicking `0.1×` slows the chains to a crawl, URL becomes `#wire-20x30&speed=0.1`.
- Clicking `1×` returns to default speed; URL goes back to `#wire-20x30` (no `&speed=`).
- Reload while on `#wire-20x30&speed=30` → speed pill `30×` is active and sim runs that fast.
- Click a fab pill (e.g. `40×30`) — page reloads, speed pill respects whatever is in the hash (default `1×` since fab links don't carry `&speed=`).
- Manually edit URL to `#wire-20x30&speed=foo` → page loads, speed defaults to `1×`, no console errors.

- [ ] **Step 8: Commit**

```bash
git add index.html
git commit -m "feat(web): speed picker pill bar with live multiplier"
```

---

### Task 5: Verify against the design

**Files:**
- (no source changes)

- [ ] **Step 1: Cross-check against the spec's manual-check list**

Re-read `docs/superpowers/specs/2026-05-24-speed-picker-design.md`'s "Testing" section. The eight manual checks listed there were largely covered in Task 4 Step 7 — make sure each one passed:

1. Default `1×` pill, sim identical to current main.
2. `10×` → chains accelerate, FPS still 60 on 600-bead fab.
3. `1000×` → FPS drops, tab doesn't hang. (Be ready to reload to escape.)
4. `0.1×` → chains crawl, FPS stays 60.
5. Switch fab via lower picker → page reloads, speed from URL is honored.
6. Reload directly on `#wire-20x30&speed=30` → speed pill `30×` is active.
7. Bare `#wire-20x30` → speed defaults to `1×`.
8. Garbage `&speed=foo` → speed defaults to `1×`, no console errors.

If any fails, return to the relevant task and fix; do not paper over.

- [ ] **Step 2: Run verify-web.py against a local build**

Run: `trunk build --release` then `python scripts/verify-web.py http://localhost:<port>/` (after starting a static server on `dist/`, or just `trunk serve --release` in one shell and `python scripts/verify-web.py http://127.0.0.1:8080/` in another).

Expected: no new console errors, screenshot looks normal, WebGPU available.

- [ ] **Step 3: Final cargo test sweep**

Run: `cargo test --lib`
Expected: all tests pass, including the 11 new ones in `speed::tests`.

- [ ] **Step 4: Final commit (if anything was tweaked) and push**

If steps 1–3 surfaced any small fixes, commit them with a descriptive message. Otherwise nothing to do.

The web deploy is push-to-`web`-branch per `memory/jigglefab-deploy.md`; do not push to `web` as part of this plan unless explicitly asked.

---

## Self-Review

- **Spec coverage:** Walked the spec section by section.
  - UX (pill bar above fab picker, discrete values, default 1×, click semantics) → Task 4.
  - URL hash format (`#fab[&speed=N]`, unknown keys ignored, missing/invalid → 1) → Tasks 1 (parser) and 4 (JS).
  - Semantics (`N = max(round(BASE × speed), 1)`, no upper cap) → Task 1 (`substeps_for_speed`).
  - JS↔WASM plumbing (`AtomicU32` fixed-point ×1000, exported setter) → Tasks 1, 3.
  - Internal control flow diagram → Tasks 2, 3.
  - `index.html` changes (SPEEDS, paintSpeedPicker, click handler, hash parsing, styling, no reload on speed change) → Task 4.
  - Testing (8 manual checks) → Task 5 Step 1.
  - Out of scope items (hybrid mode, per-fab defaults, sliders, localStorage) → not implemented. ✓
- **Placeholder scan:** No "TBD"/"TODO"/"handle errors". The one fuzzy estimate (`bottom: 56px` clears a single-row picker but may overlap on wrap) is called out as acceptable for P1.
- **Type consistency:** `set_speed`/`current_speed`/`current_substeps`/`parse_speed_from_hash` names match across Tasks 1, 2, 3. `__jigglefabSetSpeed` name matches between Task 3 (Rust install) and Task 4 (JS call). `SPEEDS` JS constant uses the same strings as `ALLOWED_SPEED_STRINGS` Rust constant (Task 1 comment + Task 4 Step 3 comment cross-reference each other).
