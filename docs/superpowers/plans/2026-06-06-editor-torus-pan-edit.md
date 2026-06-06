# Editor Torus-Native Pan + Edit Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the editor torus-native: pan wraps seamlessly (no wall/void), all edit gestures work across the seam, and a faint seam-line grid keeps you oriented.

**Architecture:** Remove the camera's domain clamp (pan wraps the center mod `world_size`; `screen_to_world` returns raw continuous coords). Move wrapping to bead-commit time and make the editor gestures min-image/ghost-aware. Add a per-vertex `shade` to the overlay pipeline and draw the dim world-boundary grid through it.

**Tech Stack:** Rust, `glam` (Vec2/Mat4), `winit`, `wgpu`/WGSL. The torus primitives `Grid::wrap_pos` and `Grid::min_image` already exist ([src/grid.rs:160](../../../src/grid.rs#L160), [:217](../../../src/grid.rs#L217)).

**Build/test commands** (cargo is not on the default Bash PATH):
```bash
export PATH="$PATH:/c/Users/thedo/.cargo/bin"
cd /c/Users/thedo/git/jigglefab
cargo test --lib                                   # unit tests (debug)
cargo check --lib                                  # native type-check (debug full BUILD link-fails on a known GNU quirk — use check)
cargo check --lib --target wasm32-unknown-unknown  # web type-check
```

---

## File Structure

- **`src/camera.rs`** — pan/zoom wrap the center instead of clamping; `screen_to_world` returns raw; new `visible_world_rect` and pure `seam_segments`. (Pure math; unit-tested.)
- **`src/editor.rs`** — wrap on commit (`place`, `append_chain_bead`, `translate_selection`); min-image in `chain_extend`; 9-ghost `any_ghost` for `select_rect`/`select_lasso`. (Pure; unit-tested.)
- **`shaders/overlay.wgsl`** + **`src/render.rs`** — overlay vertex carries a `shade` alpha multiplier.
- **`src/app.rs`** — overlay builder composes dim seam lines + bright drag lines; `hit_selected` min-image; drop the resize re-clamp; `__jigglefabGetCenterX/Y` bridge.

---

## Task 1: Camera — wrap pan, raw `screen_to_world`, `visible_world_rect`

Drop the clamp. Pan/zoom wrap the center mod `world_size`; `screen_to_world` returns continuous coords; add `visible_world_rect`. Several existing clamp-era tests are rewritten.

**Files:**
- Modify: `src/camera.rs`
- Test: `src/camera.rs` (`#[cfg(test)]`)

- [ ] **Step 1: Update the camera methods**

In `src/camera.rs`:

(a) Replace `screen_to_world_raw` + the clamped `screen_to_world` (the two methods at lines 33–53) with a single raw method:
```rust
    /// Convert a screen pixel to a continuous world point. On the torus the
    /// result may lie outside `[0, world_size]`; callers wrap when committing a
    /// position to the scene.
    pub fn screen_to_world(&self, cursor: (f64, f64), viewport: (u32, u32), world_size: f32) -> Vec2 {
        let vw = viewport.0.max(1) as f32;
        let vh = viewport.1.max(1) as f32;
        let vis = self.visible_extent(viewport, world_size);
        let fx = cursor.0 as f32 / vw;
        let fy = cursor.1 as f32 / vh;
        Vec2::new(
            self.center.x - vis.x * 0.5 + fx * vis.x,
            self.center.y + vis.y * 0.5 - fy * vis.y,
        )
    }
```

(b) In `world_to_screen`, change the doc comment `/// Inverse of `screen_to_world_raw`, ...` to `/// Inverse of `screen_to_world`, for tests/overlay math.`

(c) In `zoom_at`, change `let anchor = self.screen_to_world_raw(cursor, viewport, world_size);` to `let anchor = self.screen_to_world(cursor, viewport, world_size);`, and replace the final `self.clamp_pan(viewport, world_size);` with:
```rust
        self.center.x = self.center.x.rem_euclid(world_size);
        self.center.y = self.center.y.rem_euclid(world_size);
```

(d) Delete the entire `clamp_pan` method (lines 87–99).

(e) Replace `pan_by`'s body so it wraps instead of clamping:
```rust
    /// Pan by a cursor screen-delta in physical pixels (content follows the
    /// cursor). The center wraps mod `world_size` — the world is a torus, so
    /// panning never hits a wall; the renderer's ghost ring keeps a full
    /// neighborhood visible.
    pub fn pan_by(&mut self, screen_delta: (f32, f32), viewport: (u32, u32), world_size: f32) {
        let vw = viewport.0.max(1) as f32;
        let vh = viewport.1.max(1) as f32;
        let vis = self.visible_extent(viewport, world_size);
        let cx = self.center.x - screen_delta.0 * vis.x / vw;
        let cy = self.center.y + screen_delta.1 * vis.y / vh;
        self.center = Vec2::new(cx.rem_euclid(world_size), cy.rem_euclid(world_size));
    }
```

(f) Add `visible_world_rect` (after `view_proj`, inside `impl Camera`):
```rust
    /// The raw world-space rectangle currently visible: (min corner, max corner).
    /// Corners may lie outside `[0, world_size]` (the view can straddle the seam).
    pub fn visible_world_rect(&self, viewport: (u32, u32), world_size: f32) -> (Vec2, Vec2) {
        let vis = self.visible_extent(viewport, world_size);
        (self.center - vis * 0.5, self.center + vis * 0.5)
    }
```

- [ ] **Step 2: Rewrite the clamp-era tests and add wrap tests**

In `src/camera.rs`'s `mod tests`:

Delete these now-obsolete tests: `pan_clamp_keeps_world_in_view`, `pan_letterboxed_axis_pins_to_center`, `wide_viewport_clamps_x_outside_world`.

Add a small modular-equality helper at the top of `mod tests` (next to `approx`):
```rust
    /// True if `a` equals `b` per-axis modulo `ws` (torus equivalence).
    fn approx_mod(a: Vec2, b: Vec2, ws: f32) -> bool {
        let dx = (a.x - b.x).rem_euclid(ws);
        let dy = (a.y - b.y).rem_euclid(ws);
        let near = |v: f32| v < 1e-2 || (ws - v) < 1e-2;
        near(dx) && near(dy)
    }
```

Change `zoom_at_keeps_cursor_world_point_fixed`'s final assertion from `assert!(approx(before, after), ...)` to:
```rust
    assert!(approx_mod(before, after, WS), "anchor moved: {before:?} -> {after:?}");
```

Add these new tests:
```rust
    #[test]
    fn pan_wraps_center_into_domain() {
        let mut cam = Camera::fit(WS); // center (64,64), zoom 1
        let viewport = (800, 800);
        // A pan far larger than the world must wrap, not clamp or run away.
        cam.pan_by((100_000.0, 0.0), viewport, WS);
        assert!(cam.center.x >= 0.0 && cam.center.x < WS, "center.x not wrapped: {}", cam.center.x);
    }

    #[test]
    fn pan_full_world_returns_to_start() {
        // Panning by exactly one world width brings the center back (seamless).
        let mut cam = Camera { zoom: 2.0, center: Vec2::new(30.0, 40.0) };
        let viewport = (800, 800); // square ⇒ vis = WS/zoom = 64; world-per-px = 64/800
        let start = cam.center;
        // screen_delta that moves center by exactly -WS on x: dx * vis.x/vw = WS
        // ⇒ dx = WS * vw / vis.x = 128 * 800 / 64 = 1600.
        cam.pan_by((1600.0, 0.0), viewport, WS);
        assert!(approx_mod(cam.center, start, WS), "not seamless: {:?} vs {:?}", cam.center, start);
    }

    #[test]
    fn screen_to_world_is_unclamped() {
        // A far-left pixel on a wide view maps to negative x (no edge clamp).
        let cam = Camera::fit(WS);
        let w = cam.screen_to_world((0.0, 400.0), (1600, 800), WS); // aspect 2 ⇒ vis.x = 2*WS
        assert!(w.x < 0.0, "expected raw negative x, got {}", w.x);
    }

    #[test]
    fn visible_world_rect_matches_extent() {
        let cam = Camera { zoom: 2.0, center: Vec2::new(70.0, 60.0) };
        let viewport = (800, 800);
        let (min, max) = cam.visible_world_rect(viewport, WS);
        let half = WS / (2.0 * 2.0); // square, vis = WS/zoom = 64 ⇒ half = 32
        assert!(approx(min, Vec2::new(70.0 - half, 60.0 - half)), "min {min:?}");
        assert!(approx(max, Vec2::new(70.0 + half, 60.0 + half)), "max {max:?}");
    }
```

Keep `pan_moves_center_opposite_to_cursor_x`, `zoom_at_clamps_to_max`, `zoom_at_clamps_to_min_and_anchor_exact_at_clamp`, and all fit/round-trip/view_proj tests unchanged (small pans stay in-domain; the zoom-clamp tests use the early-return path, which is unchanged).

- [ ] **Step 3: Build and test**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cd /c/Users/thedo/git/jigglefab && cargo test --lib camera && cargo check --lib && cargo check --lib --target wasm32-unknown-unknown`
Expected: camera tests pass; both `check`s clean. (`app.rs` still compiles: it calls `screen_to_world` and `pan_by`, whose signatures are unchanged; the `Resized` arm's `pan_by((0.0,0.0),...)` is now a harmless re-wrap — removed in Task 5.)

- [ ] **Step 4: Commit**
```bash
git add src/camera.rs
git commit -m "feat(camera): torus pan (wrap center), raw screen_to_world, visible_world_rect"
```

---

## Task 2: Editor — wrap on commit + min-image gestures

Wrap positions when they become bead data; make chain spacing and selection seam-aware.

**Files:**
- Modify: `src/editor.rs`
- Test: `src/editor.rs` (`#[cfg(test)]`)

- [ ] **Step 1: Write the failing tests**

Add to `src/editor.rs`'s `mod tests` (it already has helpers for building scenes; mirror the existing test style — read a couple of nearby tests like `place_appends_with_chosen_state` and `translate_selection_clamps_to_world` first to match the scene-construction pattern). Use the existing `CHAIN_STEP` const and `Grid` for min-image distance:

```rust
    #[test]
    fn place_wraps_out_of_domain_position() {
        let mut scene = test_scene(128.0); // helper used by other tests; world 128
        // Place at x just past the right edge → wraps to near 0.
        let idx = scene.place(Vec2::new(128.0 + 2.0, 10.0));
        let p = Vec2::from(scene.beads[idx as usize].pos);
        assert!(p.x >= 0.0 && p.x < 128.0, "x not wrapped: {}", p.x);
        assert!((p.x - 2.0).abs() < 1e-4, "expected ~2.0, got {}", p.x);
    }

    #[test]
    fn chain_extend_across_seam_keeps_step_spacing() {
        let mut scene = test_scene(128.0);
        // First bead near the right edge.
        let a = scene.place(Vec2::new(127.8, 10.0));
        // Cursor just past the seam in RAW coords (continuous): 128.8.
        let b = scene.chain_extend(a, Vec2::new(128.8, 10.0));
        assert_ne!(a, b, "a chain bead should have been placed across the seam");
        let grid = crate::grid::Grid::new(128.0);
        let pa = Vec2::from(scene.beads[a as usize].pos);
        let pb = Vec2::from(scene.beads[b as usize].pos);
        let d = grid.min_image(pa, pb).length();
        assert!((d - CHAIN_STEP).abs() < 1e-3, "min-image spacing {d} != CHAIN_STEP {CHAIN_STEP}");
    }

    #[test]
    fn select_rect_across_seam_selects_far_side_bead() {
        let mut scene = test_scene(128.0);
        // Bead near the left edge of the domain.
        let i = scene.place(Vec2::new(1.0, 64.0));
        // A rect drawn in RAW coords straddling the seam: x from 126 to 130
        // (i.e. wrapping past 128). The bead's ghost at x = 1+128 = 129 is inside.
        scene.select_rect(Vec2::new(126.0, 60.0), Vec2::new(130.0, 68.0));
        assert!(scene.selection.contains(&i), "ghost-in-rect bead not selected");
    }

    #[test]
    fn translate_selection_wraps_at_boundary() {
        let mut scene = test_scene(128.0);
        let i = scene.place(Vec2::new(127.0, 50.0));
        scene.selection.insert(i);
        scene.translate_selection(Vec2::new(3.0, 0.0)); // 127 + 3 = 130 → wraps to 2
        let p = Vec2::from(scene.beads[i as usize].pos);
        assert!((p.x - 2.0).abs() < 1e-4, "x not wrapped: {}", p.x);
    }
```

If a `test_scene(world_size)` helper does not already exist in the test module, add one that builds a minimal `Scene` the same way the existing tests do (grab the construction from an existing test such as `place_appends_with_chosen_state` and factor it into `fn test_scene(world_size: f32) -> Scene`). Also DELETE the obsolete `translate_selection_clamps_to_world` test (clamping is replaced by wrapping; the new `translate_selection_wraps_at_boundary` supersedes it).

- [ ] **Step 2: Run tests to verify they fail**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cd /c/Users/thedo/git/jigglefab && cargo test --lib editor`
Expected: the four new tests FAIL (positions clamped/un-wrapped, chain spacing wrong across seam, far-side bead not selected).

- [ ] **Step 3: Implement**

In `src/editor.rs`:

(a) `place` — wrap on entry. At the top of `place` (after the signature, before pushing), build the grid and wrap:
```rust
    pub fn place(&mut self, pos: Vec2) -> u32 {
        let grid = crate::grid::Grid::new(self.world_size);
        let pos = grid.wrap_pos(pos);
        let state_name = self.chemistry.states[self.next_state_idx as usize].clone();
        // ... unchanged body, but REMOVE the later `let grid = crate::grid::Grid::new(self.world_size);`
        //     line in the bond loop and reuse the `grid` bound above.
```
(The existing bond-derivation loop already uses a `grid`; collapse to the single binding so there's one `Grid`.)

(b) `append_chain_bead` — wrap on entry:
```rust
    pub fn append_chain_bead(&mut self, pos: Vec2, prev_idx: u32) -> u32 {
        let pos = crate::grid::Grid::new(self.world_size).wrap_pos(pos);
        let state_name = self.chemistry.states[self.next_state_idx as usize].clone();
        // ... rest unchanged, using the wrapped `pos`
```

(c) `chain_extend` — step along the min-image direction so a seam-crossing drag takes the short path:
```rust
    pub fn chain_extend(&mut self, last_idx: u32, cursor: Vec2) -> u32 {
        let grid = crate::grid::Grid::new(self.world_size);
        let mut last = last_idx;
        loop {
            let last_pos = Vec2::from(self.beads[last as usize].pos);
            let to_cursor = grid.min_image(last_pos, cursor);
            let dist = to_cursor.length();
            if dist < CHAIN_STEP {
                break;
            }
            let dir = to_cursor / dist;
            let new_pos = last_pos + dir * CHAIN_STEP;
            last = self.append_chain_bead(new_pos, last);
        }
        last
    }
```

(d) Add the 9-ghost helper (a free fn near `point_in_rect`/`point_in_polygon`):
```rust
/// True if any torus ghost copy of `pos` (offset by `±world_size` per axis)
/// satisfies `inside`. Used so selections work when the view straddles the seam.
pub fn any_ghost<F: Fn(Vec2) -> bool>(pos: Vec2, world_size: f32, inside: F) -> bool {
    for ky in -1..=1 {
        for kx in -1..=1 {
            let g = Vec2::new(pos.x + kx as f32 * world_size, pos.y + ky as f32 * world_size);
            if inside(g) {
                return true;
            }
        }
    }
    false
}
```

(e) `select_rect` / `select_lasso` — test via ghosts:
```rust
    pub fn select_rect(&mut self, a: Vec2, b: Vec2) {
        self.selection.clear();
        let ws = self.world_size;
        for (i, bead) in self.beads.iter().enumerate() {
            if any_ghost(Vec2::from(bead.pos), ws, |g| point_in_rect(g, a, b)) {
                self.selection.insert(i as u32);
            }
        }
    }

    pub fn select_lasso(&mut self, poly: &[Vec2]) {
        self.selection.clear();
        let ws = self.world_size;
        for (i, bead) in self.beads.iter().enumerate() {
            if any_ghost(Vec2::from(bead.pos), ws, |g| point_in_polygon(g, poly)) {
                self.selection.insert(i as u32);
            }
        }
    }
```

(f) `translate_selection` — wrap instead of clamp:
```rust
    pub fn translate_selection(&mut self, delta: Vec2) {
        let grid = crate::grid::Grid::new(self.world_size);
        for &idx in &self.selection {
            let b = &mut self.beads[idx as usize];
            let p = grid.wrap_pos(Vec2::new(b.pos[0] + delta.x, b.pos[1] + delta.y));
            b.pos = [p.x, p.y];
        }
    }
```
(Update its doc comment: replace "then clamp each component to `[0, world_size]`" with "then wrap each component into `[0, world_size)` (torus).")

- [ ] **Step 4: Run tests to verify they pass**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cd /c/Users/thedo/git/jigglefab && cargo test --lib editor`
Expected: PASS, including the four new tests; existing editor tests still green (the non-seam `select_rect_replaces_selection`, `select_lasso_concave_polygon`, `translate_selection_shifts_only_selected_beads`, `place_derives_bond_to_nearby_bead`, etc. are unaffected because in-domain coords have their ghost-0 copy in the region and `wrap_pos` is identity inside the domain).

- [ ] **Step 5: Commit**
```bash
git add src/editor.rs
git commit -m "feat(editor): wrap on commit + min-image/ghost gestures (torus edit)"
```

---

## Task 3: Overlay vertex carries a `shade` (render + shader plumbing)

Add a per-vertex alpha multiplier to the thin-line overlay so seam lines (Task 4) can render dim while the drag overlay stays bright. Behavior-preserving (everything still bright at `shade = 1.0`).

**Files:**
- Modify: `shaders/overlay.wgsl`
- Modify: `src/render.rs`
- Modify: `src/app.rs` (overlay builder return type + the bright drag verts)

- [ ] **Step 1: Update the shader**

Replace `shaders/overlay.wgsl` `VsIn`/`VsOut`/`vs_main`/`fs_main` so a `shade` flows through:
```wgsl
struct VsIn {
    @location(0) world: vec2<f32>,
    @location(1) shade: f32,
};

struct VsOut {
    @builtin(position) clip: vec4<f32>,
    @location(0) shade: f32,
};

@vertex
fn vs_main(in: VsIn) -> VsOut {
    var out: VsOut;
    out.clip = camera.view_proj * vec4<f32>(in.world, 0.0, 1.0);
    out.shade = in.shade;
    return out;
}

@fragment
fn fs_main(in: VsOut) -> @location(0) vec4<f32> {
    return vec4<f32>(1.0, 1.0, 1.0, 0.7 * in.shade);
}
```
(Keep the `Camera` struct/binding block above it unchanged.)

- [ ] **Step 2: Update the renderer**

In `src/render.rs`:

(a) Add a public vertex type near `BeadGpu`:
```rust
#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
pub struct OverlayVertex {
    pub pos: [f32; 2],
    pub shade: f32,
}
```

(b) Where `overlay_buf` is created (line ~195), size it by the new stride and bump capacity:
```rust
        let overlay_capacity: usize = 512;
        let overlay_buf = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("overlay"),
            size: (overlay_capacity * std::mem::size_of::<OverlayVertex>()) as u64,
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
```
(Match the existing field names in the descriptor — only `size` and the capacity literal change.)

(c) In the overlay pipeline's `VertexBufferLayout` (line ~239), change stride and attributes:
```rust
                buffers: &[wgpu::VertexBufferLayout {
                    array_stride: std::mem::size_of::<OverlayVertex>() as u64,
                    step_mode: wgpu::VertexStepMode::Vertex,
                    attributes: &[
                        wgpu::VertexAttribute { offset: 0, shader_location: 0, format: wgpu::VertexFormat::Float32x2 },
                        wgpu::VertexAttribute { offset: 8, shader_location: 1, format: wgpu::VertexFormat::Float32 },
                    ],
                }],
```

(d) Change `update_overlay` to take the new type:
```rust
    pub fn update_overlay(&mut self, segments: &[OverlayVertex]) {
        let count = segments.len().min(self.overlay_capacity) as u32;
        self.overlay_vertex_count = count;
        if count == 0 { return; }
        self.queue.write_buffer(
            &self.overlay_buf,
            0,
            bytemuck::cast_slice(&segments[..count as usize]),
        );
    }
```
(Drop the old `debug_assert!(segments.len() % 2 == 0, ...)` if present, or keep it — LineList still needs even counts; keeping it is fine.)

- [ ] **Step 3: Update the app overlay builder to emit `OverlayVertex` (bright)**

In `src/app.rs`, change `overlay_segments` to return `Vec<crate::render::OverlayVertex>` with `shade: 1.0` for the drag overlay (no seam lines yet — Task 4). Replace the function:
```rust
    /// Overlay line segments for this frame (LineList: consecutive pairs = one
    /// segment). Drag overlay (rect/lasso) renders bright; Task 4 adds dim seam
    /// lines.
    fn overlay_segments(&self) -> Vec<crate::render::OverlayVertex> {
        use crate::render::OverlayVertex;
        let bright = |p: [f32; 2]| OverlayVertex { pos: p, shade: 1.0 };
        match &self.drag {
            crate::editor::DragState::Rect { anchor, current, .. } => {
                let (a, b) = (*anchor, *current);
                let (xmin, xmax) = if a.x <= b.x { (a.x, b.x) } else { (b.x, a.x) };
                let (ymin, ymax) = if a.y <= b.y { (a.y, b.y) } else { (b.y, a.y) };
                vec![
                    [xmin, ymin], [xmax, ymin],
                    [xmax, ymin], [xmax, ymax],
                    [xmax, ymax], [xmin, ymax],
                    [xmin, ymax], [xmin, ymin],
                ].into_iter().map(bright).collect()
            }
            crate::editor::DragState::Lasso { points } => {
                if points.len() < 2 { return Vec::new(); }
                let mut segs: Vec<OverlayVertex> = Vec::with_capacity((points.len() + 1) * 2);
                for w in points.windows(2) {
                    segs.push(bright([w[0].x, w[0].y]));
                    segs.push(bright([w[1].x, w[1].y]));
                }
                let last = points[points.len() - 1];
                let first = points[0];
                segs.push(bright([last.x, last.y]));
                segs.push(bright([first.x, first.y]));
                segs
            }
            _ => Vec::new(),
        }
    }
```
The two `renderer.update_overlay(&overlay)` call sites need no change (they pass `&overlay`, now `&[OverlayVertex]`).

- [ ] **Step 4: Build-check both targets**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cd /c/Users/thedo/git/jigglefab && cargo check --lib && cargo check --lib --target wasm32-unknown-unknown && cargo test --lib`
Expected: clean; 144-ish tests pass (no new unit tests here — this is render plumbing; the drag overlay still renders identically at shade 1.0).

- [ ] **Step 5: Commit**
```bash
git add shaders/overlay.wgsl src/render.rs src/app.rs
git commit -m "feat(render): per-vertex shade on the overlay line pipeline"
```

---

## Task 4: Seam-line grid

Add the pure `seam_segments` generator and have the app draw the dim world-boundary grid every frame.

**Files:**
- Modify: `src/camera.rs` (pure `seam_segments` + test)
- Modify: `src/app.rs` (compose seam lines into the overlay)

- [ ] **Step 1: Write the failing test for `seam_segments`**

In `src/camera.rs`'s `mod tests`:
```rust
    #[test]
    fn seam_segments_covers_boundaries_in_view() {
        // A view rect spanning a bit beyond one full world on both axes should
        // emit boundary lines at x=0 and x=128 (and y=0, y=128).
        let segs = seam_segments(Vec2::new(-10.0, -10.0), Vec2::new(138.0, 138.0), WS);
        // 2 vertical + 2 horizontal lines × 2 verts each = 8 vertices.
        assert_eq!(segs.len(), 8, "got {:?}", segs);
        // x=0 vertical line present (a pair with both x == 0).
        assert!(segs.windows(2).any(|w| w[0][0] == 0.0 && w[1][0] == 0.0));
        // x=128 vertical line present.
        assert!(segs.windows(2).any(|w| w[0][0] == 128.0 && w[1][0] == 128.0));
    }

    #[test]
    fn seam_segments_fit_view_shows_outer_box() {
        // At fit (view exactly [0,WS]²) the boundaries at 0 and WS show.
        let segs = seam_segments(Vec2::new(0.0, 0.0), Vec2::new(WS, WS), WS);
        assert_eq!(segs.len(), 8);
    }
```

- [ ] **Step 2: Run it to verify it fails**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cd /c/Users/thedo/git/jigglefab && cargo test --lib camera::tests::seam`
Expected: FAIL — `cannot find function seam_segments`.

- [ ] **Step 3: Implement `seam_segments`**

In `src/camera.rs`, add a free function (outside `impl Camera`, e.g. just below it):
```rust
/// World-boundary grid lines (at integer multiples of `world_size`) that
/// intersect the visible rect `[min, max]`. Returns LineList vertex pairs in
/// world space — purely cosmetic seam markers for orientation on the torus.
pub fn seam_segments(min: Vec2, max: Vec2, world_size: f32) -> Vec<[f32; 2]> {
    let mut segs = Vec::new();
    let first = (min.x / world_size).ceil() as i32;
    let last = (max.x / world_size).floor() as i32;
    for m in first..=last {
        let x = m as f32 * world_size;
        segs.push([x, min.y]);
        segs.push([x, max.y]);
    }
    let first = (min.y / world_size).ceil() as i32;
    let last = (max.y / world_size).floor() as i32;
    for m in first..=last {
        let y = m as f32 * world_size;
        segs.push([min.x, y]);
        segs.push([max.x, y]);
    }
    segs
}
```

- [ ] **Step 4: Run it to verify it passes**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cd /c/Users/thedo/git/jigglefab && cargo test --lib camera::tests::seam`
Expected: PASS (both seam tests).

- [ ] **Step 5: Compose seam lines into the app overlay**

In `src/app.rs`, add an associated constant inside the `impl App` block (so `Self::SEAM_SHADE` resolves):
```rust
    /// Alpha multiplier for the faint domain-boundary seam grid.
    const SEAM_SHADE: f32 = 0.25;
```
Then, in `overlay_segments`, prepend the dim seam grid before the drag overlay. Change the start of the function:
```rust
    fn overlay_segments(&self) -> Vec<crate::render::OverlayVertex> {
        use crate::render::OverlayVertex;
        let bright = |p: [f32; 2]| OverlayVertex { pos: p, shade: 1.0 };
        let mut out: Vec<OverlayVertex> = Vec::new();
        // Faint seam grid for orientation (shown in both Run and Edit).
        if let Some(viewport) = self.viewport() {
            let ws = self.world_size();
            let (min, max) = self.camera.visible_world_rect(viewport, ws);
            for p in crate::camera::seam_segments(min, max, ws) {
                out.push(OverlayVertex { pos: p, shade: Self::SEAM_SHADE });
            }
        }
        // Drag overlay (bright).
        match &self.drag {
            crate::editor::DragState::Rect { anchor, current, .. } => {
                let (a, b) = (*anchor, *current);
                let (xmin, xmax) = if a.x <= b.x { (a.x, b.x) } else { (b.x, a.x) };
                let (ymin, ymax) = if a.y <= b.y { (a.y, b.y) } else { (b.y, a.y) };
                for p in [
                    [xmin, ymin], [xmax, ymin],
                    [xmax, ymin], [xmax, ymax],
                    [xmax, ymax], [xmin, ymax],
                    [xmin, ymax], [xmin, ymin],
                ] {
                    out.push(bright(p));
                }
            }
            crate::editor::DragState::Lasso { points } => {
                if points.len() >= 2 {
                    for w in points.windows(2) {
                        out.push(bright([w[0].x, w[0].y]));
                        out.push(bright([w[1].x, w[1].y]));
                    }
                    let last = points[points.len() - 1];
                    let first = points[0];
                    out.push(bright([last.x, last.y]));
                    out.push(bright([first.x, first.y]));
                }
            }
            _ => {}
        }
        out
    }
```

- [ ] **Step 6: Build-check both targets and run a local visual smoke**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cd /c/Users/thedo/git/jigglefab && cargo check --lib && cargo check --lib --target wasm32-unknown-unknown && cargo test --lib`
Expected: clean; all unit tests pass (camera seam tests included).

Local visual check (if `trunk` + WebGPU available; otherwise note it was skipped): `trunk serve --release --port 8080 &` then `python scripts/verify-web.py http://localhost:8080/ --editor --headed` and confirm in `scripts/verify-out/` screenshots that faint boundary lines are visible and the editor smoke still passes. If headless/no-GPU, say so and rely on the unit tests + Task 5's smoke.

- [ ] **Step 7: Commit**
```bash
git add src/camera.rs src/app.rs
git commit -m "feat(editor): faint seam-line grid for torus orientation"
```

---

## Task 5: App gesture wiring + torus browser smoke

Finish the app-side torus behavior (`hit_selected` min-image, drop the dead resize re-clamp), add a center bridge, and add a smoke assertion that pan wraps.

**Files:**
- Modify: `src/app.rs`
- Modify: `scripts/verify-web.py`

- [ ] **Step 1: `hit_selected` uses min-image**

In `src/app.rs`, replace the body of `hit_selected` so a selected bead can be grabbed by whichever ghost is under the cursor:
```rust
    fn hit_selected(scene: &crate::editor::Scene, world_pos: glam::Vec2) -> bool {
        let grid = crate::grid::Grid::new(scene.world_size);
        scene.selection.iter().any(|&idx| {
            let p = glam::Vec2::from(scene.beads[idx as usize].pos);
            grid.min_image(world_pos, p).length() <= crate::ccd::RADIUS
        })
    }
```

- [ ] **Step 2: Drop the resize re-clamp**

In the `WindowEvent::Resized(size)` arm, delete the now-dead re-clamp line and its comment (the camera center is always valid under wrapping). The arm becomes:
```rust
            WindowEvent::Resized(size) => {
                let Some(renderer) = &mut self.renderer else { return };
                let Some(sim) = &mut self.sim else { return };
                renderer.resize(size);
                renderer.update_camera(&self.camera, sim.world_size(), &sim.palette());
            }
```

- [ ] **Step 3: Add `__jigglefabGetCenterX/Y` bridge**

In `src/app.rs`:

(a) Add two `f32` fields to `web_bridge::Snapshot` (after `zoom`): `pub center_x: f32,` and `pub center_y: f32,`.

(b) In the `RedrawRequested` wasm snapshot-write struct literal (where `zoom: self.camera.zoom` is set), add `center_x: self.camera.center.x,` and `center_y: self.camera.center.y,`.

(c) Add two installers mirroring `install_window_get_zoom`:
```rust
#[cfg(target_arch = "wasm32")]
fn install_window_get_center_x() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> f32 {
        web_bridge::SNAPSHOT.with(|s| s.borrow().center_x)
    }) as Box<dyn Fn() -> f32>);
    expose_to_window!("__jigglefabGetCenterX", cb);
}

#[cfg(target_arch = "wasm32")]
fn install_window_get_center_y() {
    use wasm_bindgen::closure::Closure;
    let cb = Closure::wrap(Box::new(|| -> f32 {
        web_bridge::SNAPSHOT.with(|s| s.borrow().center_y)
    }) as Box<dyn Fn() -> f32>);
    expose_to_window!("__jigglefabGetCenterY", cb);
}
```

(d) Call both in the wasm init block, next to `install_window_get_zoom();`:
```rust
            install_window_get_center_x();
            install_window_get_center_y();
```

- [ ] **Step 4: Build-check both targets**

Run: `export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cd /c/Users/thedo/git/jigglefab && cargo check --lib && cargo check --lib --target wasm32-unknown-unknown && cargo test --lib`
Expected: clean; tests pass.

- [ ] **Step 5: Add the torus smoke assertion**

In `scripts/verify-web.py`, inside the `--editor` block (after the zoom smoke from the previous feature, before `console_lines.append("[editor] extended smoke test passed")`), add a pan-wrap check using middle-drag:
```python
            # --- Torus pan: a huge pan wraps the center into the domain. ---
            await page.keyboard.press("0")  # reset view to fit
            await page.wait_for_timeout(100)
            cx_before = await page.evaluate("window.__jigglefabGetCenterX()")
            await page.mouse.move(cx, cy)
            await page.mouse.down(button="middle")
            await page.mouse.move(cx + 4000, cy)  # pan far right (many world-widths)
            await page.mouse.up(button="middle")
            await page.wait_for_timeout(150)
            cx_after = await page.evaluate("window.__jigglefabGetCenterX()")
            assert cx_after != cx_before, f"pan did not move center: {cx_before} -> {cx_after}"
            # Wrapped (small, finite) — NOT clamped at an edge and NOT run away to thousands.
            assert 0.0 <= cx_after < 1000.0, f"center not wrapped into domain: {cx_after}"
            console_lines.append(f"[editor] torus pan OK: center.x {cx_before} -> {cx_after}")
```

- [ ] **Step 6: Run the local browser smoke (don't fake it)**

```bash
export PATH="$PATH:/c/Users/thedo/.cargo/bin" && cd /c/Users/thedo/git/jigglefab
trunk serve --release --port 8080 &
# wait for the build + server, then:
python scripts/verify-web.py http://localhost:8080/ --editor
```
Expected: prints `[editor] torus pan OK: ...` with a wrapped `center.x`, and `[editor] extended smoke test passed`, exit 0. Stop the `trunk serve` afterward. If `trunk`/WebGPU is unavailable here, report exactly what you ran and what failed; rely on the unit tests + `cargo check` and flag the smoke as not executed.

- [ ] **Step 7: Commit**
```bash
git add src/app.rs scripts/verify-web.py
git commit -m "feat(editor): min-image hit-test, drop resize clamp, torus pan smoke"
```

---

## Self-Review notes

- **Spec coverage:** pan wraps (T1) · raw screen_to_world (T1) · place/chain/translate wrap + min-image (T2) · ghost selection (T2) · move hit-test min-image (T5) · seam line via overlay shade (T3) + grid geometry (T4) · faint/dim (T3 shade, T4 SEAM_SHADE) · `visible_world_rect` (T1). Spec §1–§4 all covered; §5 file list matches T1–T5.
- **Deferred (spec §Out), absent:** zoom-range change, minimap/tile labels, seam snapping, camera-in-saved-scene.
- **Intermediate states compile:** T1 leaves app.rs compiling (signatures stable; resize re-clamp harmlessly re-wraps until removed in T5). T2 is editor-internal. T3 changes the overlay return type + both `update_overlay` call sites accept it. T4 adds seam lines. T5 finishes app wiring.
- **Type consistency:** `OverlayVertex { pos: [f32;2], shade: f32 }` defined in T3, used in T3/T4; `seam_segments(min,max,ws) -> Vec<[f32;2]>` defined in T4 used by app in T4; `any_ghost`/`visible_world_rect` defined and used within their tasks.
- **Open verification gap:** the seam-line *visual* (faint, correct placement) is only machine-checked by the `seam_segments` unit test + "smoke didn't break"; its dimness/appearance needs the manual screenshot in T4 Step 6 (flag if environment can't run it).
