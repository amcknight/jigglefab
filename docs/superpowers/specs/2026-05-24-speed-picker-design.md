# Speed picker for the web demo

## Goal

Let viewers of the web demo dial sim speed up and down without leaving the page or sacrificing physics fidelity. The intent is "watch these guys jiggle faster" — not "stress-test CCD with huge dt." Speed is purely a per-frame work multiplier; substep dt stays at `1/60 s`. When the device can't keep up, FPS drops honestly and the in-page HUD shows it.

## UX

Add a second pill bar **above** the existing fab picker, same visual style. Pills are discrete:

```
0.1×  0.3×  1×  3×  10×  30×  100×  300×  1000×
```

Default is `1×` (active styling). Speed is orthogonal to fab — any speed pairs with any fab.

Clicking a speed pill:
- Updates the live multiplier (no reload).
- Rewrites the hash via `history.replaceState` to `#<current-fab>&speed=<value>` (omit the `&speed=…` segment when value is `1`, to keep canonical URLs clean).
- Repaints active pill styling.

The existing HUD already shows FPS and frame ms — no HUD changes needed. Users will read those values to decide when they've gone too far.

## URL hash format

Today the hash is just the fab key (`#wire-20x30`). Extend to allow an optional `&speed=<N>` suffix:

```
#wire-20x30
#wire-20x30&speed=10
```

Parsing rule: split on `&`, first segment is the fab key, subsequent segments are `key=value` pairs. Only `speed` is recognized; unknown keys are ignored. If `speed` is missing or not one of the allowed values, default to `1`.

Allowed speed values (must match a pill): `0.1, 0.3, 1, 3, 10, 30, 100, 300, 1000`.

Changing the **fab** still triggers a reload (current behavior — WASM reads the fab once at startup). Changing the **speed** does not.

## Semantics

Per rendered frame, the substep loop runs `N` steps of `FRAME_DT = 1/60 s`:

```
N = max(round(BASE_SUBSTEPS * speed), 1)
```

- `BASE_SUBSTEPS = 10` (current `SUBSTEPS` constant).
- At `speed = 0.1`, `N = 1` (not 0 — we always advance at least one substep so motion doesn't stop).
- No upper cap. At the top of the dial (`1000×` on a big fab) frame time can stretch into multiple seconds, which makes the page sluggish to click off — that's the user-visible cost of picking 1000×, and the FPS HUD signals it. If a user gets stuck, they can reload to a different `&speed=` value in the URL.

Physics fidelity is identical to today at every setting; only the amount of sim time advanced per render frame changes.

## JS ↔ WASM plumbing

One new exported function on the WASM side:

```rust
#[wasm_bindgen]
pub fn set_speed(multiplier: f32) { ... }
```

It writes to a shared `AtomicU32` that encodes `multiplier × 1000` as a fixed-point integer (so `1.0` → `1000`, `0.1` → `100`, `1000.0` → `1_000_000`). `AtomicF32` isn't stable, and this encoding avoids the dependency. The substep loop reads the atomic each frame, decodes back to `f32`, and computes `N`.

Storage location: a `static AtomicU32` in `src/app.rs` (or a small new `src/speed.rs` module if it grows). Initial value set from the URL hash at startup, alongside the existing fab lookup.

The exported `set_speed` is called from JS whenever:
1. A pill is clicked (live update).
2. Startup (so the JS-parsed hash value flows into WASM after the WASM module is ready).

## Internal control flow

```
src/app.rs
├── const BASE_SUBSTEPS: u32 = 10
├── static SPEED_FIXED: AtomicU32 = AtomicU32::new(1000)  // 1.0×
├── fn current_substeps() -> u32   // reads atomic, returns max(round(BASE*speed), 1)
├── #[wasm_bindgen] pub fn set_speed(m: f32)
└── window_event::RedrawRequested
        for _ in 0..current_substeps() { scheduler.step(sim, FRAME_DT); }
```

Native build: `current_substeps()` returns `BASE_SUBSTEPS` (no atomic, no exported fn). Speed picker is web-only.

## index.html changes

- Add a `SPEEDS` constant analogous to `FABS`: `[0.1, 0.3, 1, 3, 10, 30, 100, 300, 1000]`.
- Add a `<nav id="speed-picker">` element above the existing `#picker`.
- `paintSpeedPicker()` analogous to `paintPicker()`.
- Click handler: prevent default, update hash via `history.replaceState`, repaint, and call `wasm_bindgen` exported `set_speed`.
- Parse speed out of the hash at startup, paint the active pill.
- Style: reuse the existing `#picker` CSS (rename to a shared class or duplicate — duplicate is fine, only ~20 lines).

The `hashchange → location.reload()` listener stays as-is. Speed clicks use `replaceState`, which does not fire `hashchange`, so no reload.

## Testing

This is web-only UI plumbing — automated tests are limited to what `scripts/verify-web.py` covers (page loads, no console errors, WASM boots).

Manual checks before merging:

1. Load page, default speed pill is `1×`, sim looks identical to current main.
2. Click `10×`, chains visibly accelerate, FPS still 60 (on a 600-bead fab).
3. Click `1000×`, FPS drops; chains move so fast they blur — confirm tab doesn't hang.
4. Click `0.1×`, chains crawl, FPS stays 60.
5. Switch fab via the lower picker → page reloads, speed pill from URL is honored.
6. Reload directly on `#wire-20x30&speed=30` → speed picker shows `30×` active and sim runs at that speed.
7. Bare `#wire-20x30` (no speed segment) → speed defaults to `1×`.
8. Garbage `&speed=foo` → speed defaults to `1×`, no console errors.

## Out of scope

- Hybrid mode (more substeps + bigger dt). Rejected: defeats the "physics stays faithful" property the user explicitly chose.
- Per-fab default speeds. Speed is global and resets to `1×` when fab changes via the picker (unless a `&speed=` is in the destination hash).
- Slider control. Discrete pills match the existing picker idiom.
- "Hyperfast jiggle" preset combo (e.g. 40×30 + 10×). Subsumed by orthogonal pickers — set both pills.
- Animating the transition between speeds. Just snaps.
- Persisting speed in localStorage. URL hash is the persistence layer.
