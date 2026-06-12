# Bead Render Modes Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add six runtime-selectable bead render modes (Disc, Voronoi, Soft Voronoi, Metaball-blend, Metaball-argmax, Worley) sharing one expanded bead buffer.

**Architecture:** Two GPU pipelines — Disc (existing, unchanged) and Field (new, full-screen-quad fragment shader branching on a mode uniform). The bead buffer gains `vel` (wired but unread by v1) and `component_id` (CPU-computed connected component over the bond graph) so metaball modes can fuse only bonded beads. UI exposes a chip-group picker + R/Shift+R cycle + localStorage persistence.

**Tech Stack:** Rust + wgpu + WGSL shaders, vanilla HTML/JS UI via `window.__jigglefab*` bridge functions, existing winit keyboard handler in `src/app.rs`.

**Spec:** [docs/superpowers/specs/2026-06-11-bead-render-modes-design.md](docs/superpowers/specs/2026-06-11-bead-render-modes-design.md)

---

## Phase 1 — Foundation (CPU types & data plumbing)

### Task 1: `RenderMode` enum

**Files:**
- Create: `src/render_mode.rs`
- Modify: `src/lib.rs` (add `pub mod render_mode;`)
- Test: inline `#[cfg(test)]` in `src/render_mode.rs`

- [ ] **Step 1: Write failing tests**

Create `src/render_mode.rs`:

```rust
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum RenderMode {
    Disc,
    Voronoi,
    SoftVoronoi,
    Worley,
    MetaballBlend,
    MetaballArgmax,
}

impl RenderMode {
    pub const ALL: [RenderMode; 6] = [
        RenderMode::Disc,
        RenderMode::Voronoi,
        RenderMode::SoftVoronoi,
        RenderMode::Worley,
        RenderMode::MetaballBlend,
        RenderMode::MetaballArgmax,
    ];

    /// Numeric discriminant uploaded to the shader as a `u32`.
    /// Disc never reaches the field shader, but giving it id 0 makes the
    /// enum-to-id mapping uniform.
    pub fn shader_id(self) -> u32 {
        match self {
            RenderMode::Disc => 0,
            RenderMode::Voronoi => 0,
            RenderMode::SoftVoronoi => 1,
            RenderMode::Worley => 2,
            RenderMode::MetaballBlend => 3,
            RenderMode::MetaballArgmax => 4,
        }
    }

    pub fn is_field(self) -> bool {
        !matches!(self, RenderMode::Disc)
    }

    pub fn cycle(self, forward: bool) -> RenderMode {
        let idx = Self::ALL.iter().position(|m| *m == self).unwrap();
        let next = if forward {
            (idx + 1) % Self::ALL.len()
        } else {
            (idx + Self::ALL.len() - 1) % Self::ALL.len()
        };
        Self::ALL[next]
    }

    pub fn label(self) -> &'static str {
        match self {
            RenderMode::Disc => "Disc",
            RenderMode::Voronoi => "Voronoi",
            RenderMode::SoftVoronoi => "Soft Voronoi",
            RenderMode::Worley => "Worley",
            RenderMode::MetaballBlend => "Metaball Blend",
            RenderMode::MetaballArgmax => "Metaball Argmax",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cycle_forward_wraps() {
        let last = RenderMode::ALL[RenderMode::ALL.len() - 1];
        assert_eq!(last.cycle(true), RenderMode::ALL[0]);
    }

    #[test]
    fn cycle_back_wraps() {
        assert_eq!(RenderMode::ALL[0].cycle(false),
                   RenderMode::ALL[RenderMode::ALL.len() - 1]);
    }

    #[test]
    fn serde_kebab_case_roundtrip() {
        let json = serde_json::to_string(&RenderMode::MetaballBlend).unwrap();
        assert_eq!(json, "\"metaball-blend\"");
        let back: RenderMode = serde_json::from_str(&json).unwrap();
        assert_eq!(back, RenderMode::MetaballBlend);
    }

    #[test]
    fn is_field_only_false_for_disc() {
        assert!(!RenderMode::Disc.is_field());
        for m in RenderMode::ALL.iter().filter(|m| **m != RenderMode::Disc) {
            assert!(m.is_field(), "{:?} should be a field mode", m);
        }
    }
}
```

Add to `src/lib.rs`: `pub mod render_mode;`

- [ ] **Step 2: Run tests, expect compile fail then pass**

Run: `cargo test render_mode --lib`
Expected: tests pass.

- [ ] **Step 3: Commit**

```bash
git add src/render_mode.rs src/lib.rs
git commit -m "feat(render): add RenderMode enum with cycle/serde"
```

---

### Task 2: Connected components from bond graph

**Files:**
- Create: `src/component.rs`
- Modify: `src/lib.rs` (add `pub mod component;`)
- Test: inline `#[cfg(test)]` in `src/component.rs`

- [ ] **Step 1: Write failing tests**

Create `src/component.rs`:

```rust
use crate::bond::BondPair;

/// Assign each bead a connected-component id over the bond graph.
/// Unbonded beads each get a unique id. Result length equals `bead_count`.
///
/// Uses union-find with path compression. O(N + bonds·α(N)).
pub fn compute_component_ids(bead_count: usize, bonds: &[BondPair]) -> Vec<u32> {
    let mut parent: Vec<u32> = (0..bead_count as u32).collect();

    fn find(parent: &mut [u32], mut x: u32) -> u32 {
        while parent[x as usize] != x {
            let p = parent[x as usize];
            parent[x as usize] = parent[p as usize];
            x = parent[x as usize];
        }
        x
    }

    for b in bonds {
        let a = find(&mut parent, b.lo());
        let c = find(&mut parent, b.hi());
        if a != c {
            parent[a as usize] = c;
        }
    }

    // Flatten so every bead points at its root.
    for i in 0..bead_count {
        parent[i] = find(&mut parent, i as u32);
    }
    parent
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unbonded_beads_each_get_unique_id() {
        let ids = compute_component_ids(3, &[]);
        let set: std::collections::HashSet<_> = ids.iter().collect();
        assert_eq!(set.len(), 3);
    }

    #[test]
    fn two_bonded_beads_share_id() {
        let bonds = vec![BondPair::new(0, 1)];
        let ids = compute_component_ids(3, &bonds);
        assert_eq!(ids[0], ids[1]);
        assert_ne!(ids[0], ids[2]);
    }

    #[test]
    fn three_bead_chain_shares_id() {
        let bonds = vec![BondPair::new(0, 1), BondPair::new(1, 2)];
        let ids = compute_component_ids(4, &bonds);
        assert_eq!(ids[0], ids[1]);
        assert_eq!(ids[1], ids[2]);
        assert_ne!(ids[0], ids[3]);
    }

    #[test]
    fn triangle_shares_id() {
        let bonds = vec![
            BondPair::new(0, 1),
            BondPair::new(1, 2),
            BondPair::new(2, 0),
        ];
        let ids = compute_component_ids(3, &bonds);
        assert_eq!(ids[0], ids[1]);
        assert_eq!(ids[1], ids[2]);
    }

    #[test]
    fn empty_bead_list_returns_empty() {
        let ids = compute_component_ids(0, &[]);
        assert!(ids.is_empty());
    }
}
```

Add to `src/lib.rs`: `pub mod component;`

- [ ] **Step 2: Run tests**

Run: `cargo test component --lib`
Expected: PASS.

- [ ] **Step 3: Commit**

```bash
git add src/component.rs src/lib.rs
git commit -m "feat(component): connected-component union-find over bond graph"
```

---

### Task 3: Expand `BeadGpu` and `CameraUbo`

**Files:**
- Modify: `src/render.rs:10-37` (BeadGpu and CameraUbo structs)

- [ ] **Step 1: Add a layout test**

Add to the bottom of `src/render.rs` (above `impl Renderer`):

```rust
#[cfg(test)]
mod gpu_layout_tests {
    use super::*;

    #[test]
    fn beadgpu_size_is_32() {
        assert_eq!(std::mem::size_of::<BeadGpu>(), 32);
    }

    #[test]
    fn beadgpu_roundtrips_through_bytemuck() {
        let b = BeadGpu {
            pos: [1.5, -2.5],
            vel: [0.1, 0.2],
            state: 3,
            selected: 1,
            component_id: 7,
            _pad: 0,
        };
        let bytes = bytemuck::bytes_of(&b);
        let back: BeadGpu = *bytemuck::from_bytes(bytes);
        assert_eq!(back.pos, b.pos);
        assert_eq!(back.vel, b.vel);
        assert_eq!(back.state, b.state);
        assert_eq!(back.selected, b.selected);
        assert_eq!(back.component_id, b.component_id);
    }
}
```

- [ ] **Step 2: Modify `BeadGpu`**

Replace the existing `BeadGpu` struct (lines 8-14) with:

```rust
#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
struct BeadGpu {
    pos: [f32; 2],
    vel: [f32; 2],
    state: u32,
    selected: u32,
    component_id: u32,
    _pad: u32,
}
```

- [ ] **Step 3: Modify `CameraUbo`**

Replace `CameraUbo` (lines 28-37) with:

```rust
#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
struct CameraUbo {
    view_proj: [[f32; 4]; 4],
    inv_view_proj: [[f32; 4]; 4],
    radius: f32,
    world_size: f32,
    bead_count: u32,
    mode: u32,
    state_colors: [[f32; 4]; MAX_STATES],
}
```

`inv_view_proj` is the inverse of `view_proj`. The field shader's vertex stage
uses it to recover world coords from clip coords for a full-screen triangle.
Disc shader ignores it. Computed CPU-side in `update_camera` (next task).

- [ ] **Step 4: Update existing `beads.wgsl` shader to match the new `Bead` layout**

Modify `shaders/beads.wgsl` lines 1-14, replace the `Bead` struct with:

```wgsl
struct Bead {
    pos: vec2<f32>,
    vel: vec2<f32>,
    state: u32,
    selected: u32,
    component_id: u32,
    _pad: u32,
};

struct Camera {
    view_proj: mat4x4<f32>,
    inv_view_proj: mat4x4<f32>,
    radius: f32,
    world_size: f32,
    bead_count: u32,
    mode: u32,
    state_colors: array<vec4<f32>, 8>,
};
```

The disc fragment shader ignores `inv_view_proj`, `vel`, `component_id`, `bead_count`, `mode` — they're available but unused.

- [ ] **Step 5: Run tests**

Run: `cargo test --lib gpu_layout_tests`
Expected: PASS. (Other tests may break temporarily — Task 4 fixes them.)

- [ ] **Step 6: Commit**

```bash
git add src/render.rs shaders/beads.wgsl
git commit -m "feat(render): expand BeadGpu with vel+component_id; CameraUbo with bead_count+mode"
```

---

### Task 4: Wire vel + component_id through `update_beads` and `update_camera`

**Files:**
- Modify: `src/render.rs:296-341` (update_beads, update_camera)
- Modify: `src/app.rs:1253` (Run-mode call site)
- Modify: `src/app.rs:1271` (Edit-mode call site)

- [ ] **Step 1: Change `update_beads` signature in `render.rs`**

Replace lines 296-320 with:

```rust
pub fn update_beads(
    &mut self,
    positions: &[Vec2],
    velocities: &[Vec2],
    states: &[u32],
    selected: &[u32],
    component_ids: &[u32],
) {
    debug_assert_eq!(positions.len(), velocities.len());
    debug_assert_eq!(positions.len(), states.len());
    debug_assert_eq!(positions.len(), selected.len());
    debug_assert_eq!(positions.len(), component_ids.len());
    if positions.len() > self.bead_capacity {
        self.bead_capacity = positions.len().next_power_of_two();
        self.bead_buf = self.device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("beads"),
            size: (self.bead_capacity * std::mem::size_of::<BeadGpu>()) as u64,
            usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        self.bind_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("beads bg"),
            layout: &self.bind_layout,
            entries: &[
                wgpu::BindGroupEntry { binding: 0, resource: self.camera_buf.as_entire_binding() },
                wgpu::BindGroupEntry { binding: 1, resource: self.bead_buf.as_entire_binding() },
            ],
        });
    }
    let gpu_beads: Vec<BeadGpu> = (0..positions.len())
        .map(|i| BeadGpu {
            pos: [positions[i].x, positions[i].y],
            vel: [velocities[i].x, velocities[i].y],
            state: states[i],
            selected: selected[i],
            component_id: component_ids[i],
            _pad: 0,
        })
        .collect();
    self.queue.write_buffer(&self.bead_buf, 0, bytemuck::cast_slice(&gpu_beads));
}
```

- [ ] **Step 2: Change `update_camera` signature in `render.rs`**

Replace lines 322-341 with:

```rust
pub fn update_camera(
    &mut self,
    camera: &crate::camera::Camera,
    world_size: f32,
    palette: &[[f32; 3]],
    bead_count: u32,
    mode: crate::render_mode::RenderMode,
) {
    let vp = camera.view_proj((self.size.width, self.size.height), world_size);
    let mut state_colors = [[0.0f32, 0.0, 0.0, 1.0]; MAX_STATES];
    for (i, slot) in state_colors.iter_mut().enumerate() {
        if !palette.is_empty() {
            let c = palette[i % palette.len()];
            *slot = [c[0], c[1], c[2], 1.0];
        }
    }
    let inv = vp.inverse();
    let ubo = CameraUbo {
        view_proj: vp.to_cols_array_2d(),
        inv_view_proj: inv.to_cols_array_2d(),
        radius: crate::ccd::RADIUS,
        world_size,
        bead_count,
        mode: mode.shader_id(),
        state_colors,
    };
    self.queue.write_buffer(&self.camera_buf, 0, bytemuck::bytes_of(&ubo));
}
```

- [ ] **Step 3: Update Run-mode call site in `src/app.rs:1253`**

Find the line `renderer.update_beads(&sim.positions, &sim.states, &selected)` and replace with:

```rust
let comp_ids = jigglefab::component::compute_component_ids(sim.positions.len(), &sim.bonds);
renderer.update_beads(&sim.positions, &sim.velocities, &sim.states, &selected, &comp_ids);
```

If `sim.velocities` does not exist, locate it on the sim struct (search `pub velocities` or the Bead struct in `src/parallel/state.rs`) and expose a slice — `sim` likely owns parallel state; expose a getter if needed. If neither exists, pass `&vec![Vec2::ZERO; sim.positions.len()]` and add a TODO comment in `src/sim.rs` to expose velocities (acceptable v1 fallback; v1 shaders don't read vel).

Also update the `update_camera` call near it to pass `sim.positions.len() as u32` and the current `render_mode` (stored on `App` per Task 14):

```rust
renderer.update_camera(&camera, world_size, &palette, sim.positions.len() as u32, app.render_mode);
```

- [ ] **Step 4: Update Edit-mode call site in `src/app.rs:1271`**

Find `renderer.update_beads(&positions, &states, &selected)` and replace with:

```rust
let velocities: Vec<Vec2> = vec![Vec2::ZERO; positions.len()];
let comp_ids = jigglefab::component::compute_component_ids(positions.len(), &scene.bonds);
renderer.update_beads(&positions, &velocities, &states, &selected, &comp_ids);
```

If the local `scene` binding has a different name, adapt — search the surrounding 20 lines for the Scene-like struct.

Update the nearby `update_camera` call the same way as Step 3.

- [ ] **Step 5: Build**

Run: `cargo build --lib`
Expected: clean build.

Run: `cargo build --target wasm32-unknown-unknown` (only if cargo-wasm is set up; otherwise skip)
Expected: clean build.

- [ ] **Step 6: Commit**

```bash
git add src/render.rs src/app.rs src/sim.rs
git commit -m "feat(render): wire vel and component_id through update_beads/camera"
```

---

## Phase 2 — Field pipeline scaffolding

### Task 5: Field shader skeleton (vertex full-screen tri + BG fragment)

**Files:**
- Create: `shaders/field.wgsl`

- [ ] **Step 1: Create the shader**

```wgsl
struct Bead {
    pos: vec2<f32>,
    vel: vec2<f32>,
    state: u32,
    selected: u32,
    component_id: u32,
    _pad: u32,
};

struct Camera {
    view_proj: mat4x4<f32>,
    inv_view_proj: mat4x4<f32>,
    radius: f32,
    world_size: f32,
    bead_count: u32,
    mode: u32,
    state_colors: array<vec4<f32>, 8>,
};

@group(0) @binding(0) var<uniform> camera: Camera;
@group(0) @binding(1) var<storage, read> beads: array<Bead>;

const BG: vec3<f32> = vec3<f32>(0.05, 0.05, 0.07);
const ISO: f32 = 0.5;

struct VsOut {
    @builtin(position) clip: vec4<f32>,
    @location(0) world: vec2<f32>,
};

// Full-screen triangle covering the whole screen. World coords are recovered
// by multiplying clip-space by the camera's pre-computed inverse view_proj
// (carried in the camera UBO).
@vertex
fn vs_main(@builtin(vertex_index) vi: u32) -> VsOut {
    var pos = array<vec2<f32>, 3>(
        vec2<f32>(-1.0, -1.0),
        vec2<f32>( 3.0, -1.0),
        vec2<f32>(-1.0,  3.0),
    );
    let clip = vec4<f32>(pos[vi], 0.0, 1.0);
    let w4 = camera.inv_view_proj * clip;
    var out: VsOut;
    out.clip = clip;
    out.world = w4.xy / w4.w;
    return out;
}

@fragment
fn fs_main(in: VsOut) -> @location(0) vec4<f32> {
    // Placeholder: just emit background. Mode dispatch comes in later tasks.
    return vec4<f32>(BG, 1.0);
}
```

- [ ] **Step 2: Verify shader compiles standalone**

Run: `cargo build --lib`
Expected: clean (the shader is only compiled when the pipeline using it is built — Task 6).

- [ ] **Step 3: Commit**

```bash
git add shaders/field.wgsl
git commit -m "feat(render): field shader skeleton (BG-only fragment)"
```

---

### Task 6: Build Field pipeline and dispatch in `render()`

**Files:**
- Modify: `src/render.rs` (`Renderer` struct, `new`, `render`)

- [ ] **Step 1: Add `field_pipeline` to `Renderer` struct**

Add to the `Renderer` struct (around line 56 alongside `overlay_pipeline`):

```rust
    field_pipeline: wgpu::RenderPipeline,
    mode: crate::render_mode::RenderMode,
```

- [ ] **Step 2: Build the Field pipeline in `Renderer::new`**

After the existing `pipeline` build (around line 199, before the overlay pipeline section), add:

```rust
let field_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
    label: Some("field"),
    source: wgpu::ShaderSource::Wgsl(include_str!("../shaders/field.wgsl").into()),
});
let field_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
    label: Some("field layout"),
    bind_group_layouts: &[&bind_layout],
    push_constant_ranges: &[],
});
let field_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
    label: Some("field pipeline"),
    layout: Some(&field_pipeline_layout),
    vertex: wgpu::VertexState {
        module: &field_shader,
        entry_point: Some("vs_main"),
        buffers: &[],
        compilation_options: Default::default(),
    },
    fragment: Some(wgpu::FragmentState {
        module: &field_shader,
        entry_point: Some("fs_main"),
        targets: &[Some(wgpu::ColorTargetState {
            format,
            blend: Some(wgpu::BlendState::ALPHA_BLENDING),
            write_mask: wgpu::ColorWrites::ALL,
        })],
        compilation_options: Default::default(),
    }),
    primitive: wgpu::PrimitiveState::default(),
    depth_stencil: None,
    multisample: wgpu::MultisampleState::default(),
    multiview: None,
    cache: None,
});
```

Add `field_pipeline` and `mode: crate::render_mode::RenderMode::Disc` to the `Renderer { ... }` initializer at the bottom of `new`.

- [ ] **Step 3: Add public `set_mode` getter/setter**

Add to `impl Renderer`:

```rust
pub fn mode(&self) -> crate::render_mode::RenderMode {
    self.mode
}

pub fn set_mode(&mut self, mode: crate::render_mode::RenderMode) {
    self.mode = mode;
}
```

- [ ] **Step 4: Dispatch in `render()`**

Replace the `pass.set_pipeline(&self.pipeline); ...; pass.draw(...)` block at lines 382-389 with:

```rust
if self.mode.is_field() {
    pass.set_pipeline(&self.field_pipeline);
    pass.set_bind_group(0, &self.bind_group, &[]);
    pass.draw(0..3, 0..1);  // 1 full-screen triangle
} else {
    pass.set_pipeline(&self.pipeline);
    pass.set_bind_group(0, &self.bind_group, &[]);
    pass.set_vertex_buffer(0, self.quad_vbuf.slice(..));
    pass.draw(0..6, 0..(bead_count * 9) as u32);
}
```

- [ ] **Step 5: Build and smoke**

Run: `cargo build --lib`
Expected: clean.

Run: `cargo run --bin jigglefab -- --foreground` (if a native binary exists), or open the web build per `scripts/verify-web.py`. Default mode is Disc, so visuals should be unchanged.

- [ ] **Step 6: Manual mode-switch sanity**

Temporarily set `renderer.set_mode(RenderMode::Voronoi)` near the renderer construction site in `src/app.rs`. Reload — you should see solid background (Voronoi color function is not yet implemented). Revert the temp line.

- [ ] **Step 7: Commit**

```bash
git add src/render.rs
git commit -m "feat(render): Field pipeline scaffold + mode dispatch in render()"
```

---

## Phase 3 — Field shader modes

### Task 7: Field accumulator in WGSL

**Files:**
- Modify: `shaders/field.wgsl`

- [ ] **Step 1: Add `FieldAccum` struct + `falloff` + `accumulate_field` to `shaders/field.wgsl`**

Insert after the `const ISO` line, before `VsOut`:

```wgsl
struct FieldAccum {
    nearest_idx: u32,
    nearest_d: f32,
    second_d: f32,
    argmax_idx: u32,
    argmax_f: f32,
    total_f_in_comp: f32,
};

fn falloff(d: f32, R: f32) -> f32 {
    if (d >= R) { return 0.0; }
    let t = 1.0 - (d * d) / (R * R);
    return t * t;
}

fn accumulate_field(p: vec2<f32>) -> FieldAccum {
    var acc: FieldAccum;
    acc.nearest_idx = 0u;
    acc.nearest_d = 1e30;
    acc.second_d = 1e30;
    acc.argmax_idx = 0u;
    acc.argmax_f = 0.0;
    acc.total_f_in_comp = 0.0;
    let R = camera.radius;
    let WS = camera.world_size;
    // Pass 1: nearest, second-nearest, argmax (with 3x3 toroidal ghosts).
    for (var i: u32 = 0u; i < camera.bead_count; i = i + 1u) {
        let b = beads[i];
        for (var gy: i32 = -1; gy <= 1; gy = gy + 1) {
            for (var gx: i32 = -1; gx <= 1; gx = gx + 1) {
                let center = b.pos + vec2<f32>(f32(gx), f32(gy)) * WS;
                let d = distance(p, center);
                let f = falloff(d, R);
                if (d < acc.nearest_d) {
                    acc.second_d = acc.nearest_d;
                    acc.nearest_d = d;
                    acc.nearest_idx = i;
                } else if (d < acc.second_d) {
                    acc.second_d = d;
                }
                if (f > acc.argmax_f) {
                    acc.argmax_f = f;
                    acc.argmax_idx = i;
                }
            }
        }
    }
    // Pass 2: in-component field sum (only needed by metaball modes).
    // mode 3 = metaball-blend, mode 4 = metaball-argmax.
    if (camera.mode >= 3u && acc.argmax_f > 0.0) {
        let target_comp = beads[acc.argmax_idx].component_id;
        for (var i: u32 = 0u; i < camera.bead_count; i = i + 1u) {
            let b = beads[i];
            if (b.component_id != target_comp) { continue; }
            for (var gy: i32 = -1; gy <= 1; gy = gy + 1) {
                for (var gx: i32 = -1; gx <= 1; gx = gx + 1) {
                    let center = b.pos + vec2<f32>(f32(gx), f32(gy)) * WS;
                    let d = distance(p, center);
                    acc.total_f_in_comp = acc.total_f_in_comp + falloff(d, R);
                }
            }
        }
    }
    return acc;
}
```

- [ ] **Step 2: Wire the accumulator into the fragment for debug visualisation**

Replace the `fs_main` body with:

```wgsl
@fragment
fn fs_main(in: VsOut) -> @location(0) vec4<f32> {
    let acc = accumulate_field(in.world);
    // Debug: show nearest-distance falloff so we can verify the accumulator
    // is finding beads correctly. Later tasks replace this with mode dispatch.
    let t = clamp(1.0 - acc.nearest_d / (camera.radius * 1.5), 0.0, 1.0);
    return vec4<f32>(vec3<f32>(t), 1.0);
}
```

- [ ] **Step 3: Build & visually verify**

Run: `cargo build --lib && trunk build` (or however the web build is invoked — see `Trunk.toml`).

Open the web build, temporarily set mode to Voronoi in `app.rs`, reload. You should see white-ish glowing dots where beads are. If beads look offset, the world-space recovery in `mat_inverse_2d` is wrong — verify against `src/camera.rs::view_proj`.

- [ ] **Step 4: Commit**

```bash
git add shaders/field.wgsl
git commit -m "feat(field): FieldAccum + accumulate_field (debug visualization)"
```

---

### Task 8: Voronoi mode

**Files:**
- Modify: `shaders/field.wgsl`

- [ ] **Step 1: Add `voronoi_color` function**

Above `fs_main`, add:

```wgsl
fn voronoi_color(acc: FieldAccum) -> vec3<f32> {
    if (acc.nearest_d > camera.radius * 1.5) {
        return BG;
    }
    let s = beads[acc.nearest_idx].state;
    return camera.state_colors[s].rgb;
}
```

- [ ] **Step 2: Wire mode 0 in `fs_main`**

Replace `fs_main` with:

```wgsl
@fragment
fn fs_main(in: VsOut) -> @location(0) vec4<f32> {
    let acc = accumulate_field(in.world);
    var color: vec3<f32> = BG;
    switch (camera.mode) {
        case 0u: { color = voronoi_color(acc); }
        default: { color = BG; }
    }
    return vec4<f32>(color, 1.0);
}
```

- [ ] **Step 3: Build, run, verify**

Run: web build + open page. Switch (manually via temporary `set_mode` in `app.rs`) to Voronoi. Expected: hard cell polygons in bead colors, background everywhere beyond `1.5 × R` from any bead.

- [ ] **Step 4: Commit**

```bash
git add shaders/field.wgsl
git commit -m "feat(field): Voronoi mode (mode 0)"
```

---

### Task 9: Soft Voronoi mode

**Files:**
- Modify: `shaders/field.wgsl`

- [ ] **Step 1: Add `soft_voronoi_color` function**

Above `fs_main`, add:

```wgsl
// Cosmetic seam-softening. Re-finds second-nearest by pos-only distance
// (no ghost wrap) — exact toroidal correctness for second_idx isn't
// visually important.
fn soft_voronoi_color(acc: FieldAccum) -> vec3<f32> {
    if (acc.nearest_d > camera.radius * 1.5) {
        return BG;
    }
    let s1 = beads[acc.nearest_idx].state;
    let c1 = camera.state_colors[s1].rgb;
    let seam_width = camera.radius * 0.04;
    let contest = 1.0 - clamp((acc.second_d - acc.nearest_d) / seam_width, 0.0, 1.0);
    let t = contest * 0.5;  // never fully fade ownership
    var second_idx: u32 = acc.nearest_idx;
    var sd: f32 = 1e30;
    let anchor = beads[acc.nearest_idx].pos;
    for (var i: u32 = 0u; i < camera.bead_count; i = i + 1u) {
        if (i == acc.nearest_idx) { continue; }
        let d = distance(beads[i].pos, anchor);
        if (d < sd) { sd = d; second_idx = i; }
    }
    let s2 = beads[second_idx].state;
    let c2 = camera.state_colors[s2].rgb;
    return mix(c1, c2, t);
}
```

NOTE on the re-find loop: tracking `second_idx` in the accumulator would be cleaner but bloats `FieldAccum`. The seam blur is cosmetic and exact second-bead identity across the toroidal wrap matters less than the smooth-color-blend behaviour. If perf shows up here, add `second_idx` to `FieldAccum` in a follow-up.

- [ ] **Step 2: Wire mode 1**

Update the `fs_main` switch:

```wgsl
switch (camera.mode) {
    case 0u: { color = voronoi_color(acc); }
    case 1u: { color = soft_voronoi_color(acc); }
    default: { color = BG; }
}
```

- [ ] **Step 3: Build, run, verify**

Switch to Soft Voronoi. Expected: same hard cells as Voronoi, but with a fuzzy fade along boundaries between same-coloured cells (which won't be visible) and a more obvious fade along boundaries between different-coloured cells.

- [ ] **Step 4: Commit**

```bash
git add shaders/field.wgsl
git commit -m "feat(field): Soft Voronoi mode (mode 1)"
```

---

### Task 10: Worley mode

**Files:**
- Modify: `shaders/field.wgsl`

- [ ] **Step 1: Add `worley_color` function**

```wgsl
fn worley_color(acc: FieldAccum) -> vec3<f32> {
    if (acc.nearest_d > camera.radius * 1.5) {
        return BG;
    }
    let intensity = clamp((acc.second_d - acc.nearest_d) * 4.0, 0.0, 1.0);
    let s = beads[acc.nearest_idx].state;
    let c = camera.state_colors[s].rgb;
    return c * intensity + BG * (1.0 - intensity);
}
```

- [ ] **Step 2: Wire mode 2**

```wgsl
case 2u: { color = worley_color(acc); }
```

- [ ] **Step 3: Build, run, verify**

Switch to Worley. Expected: bead-coloured regions with dark "cracks" on the cell boundaries.

- [ ] **Step 4: Commit**

```bash
git add shaders/field.wgsl
git commit -m "feat(field): Worley mode (mode 2)"
```

---

### Task 11: Metaball-argmax mode

**Files:**
- Modify: `shaders/field.wgsl`

- [ ] **Step 1: Add `metaball_argmax_color`**

```wgsl
fn metaball_argmax_color(acc: FieldAccum) -> vec3<f32> {
    if (acc.total_f_in_comp < ISO) {
        return BG;
    }
    let s = beads[acc.argmax_idx].state;
    let c = camera.state_colors[s].rgb;
    // Soft edge as total_f approaches ISO from above.
    let edge = clamp((acc.total_f_in_comp - ISO) * 6.0, 0.0, 1.0);
    return c * edge + BG * (1.0 - edge);
}
```

- [ ] **Step 2: Wire mode 4**

```wgsl
case 4u: { color = metaball_argmax_color(acc); }
```

- [ ] **Step 3: Build, run, verify**

Switch to Metaball Argmax. Expected:
- Isolated bead: single round blob in its state colour.
- Bonded pair (same colour): peanut shape, one colour.
- Bonded pair (different colours): peanut shape, crisp color seam at the field-equal line.
- Unbonded beads close together: each renders as its own blob, no fusing.

- [ ] **Step 4: Commit**

```bash
git add shaders/field.wgsl
git commit -m "feat(field): Metaball Argmax mode (mode 4)"
```

---

### Task 12: Metaball-blend mode

**Files:**
- Modify: `shaders/field.wgsl`

- [ ] **Step 1: Add `metaball_blend_color`**

Mode 3 needs the field-weighted color average over in-component beads. This walks the bead buffer a third time. Add this helper:

```wgsl
fn metaball_blend_color(p: vec2<f32>, acc: FieldAccum) -> vec3<f32> {
    if (acc.total_f_in_comp < ISO) {
        return BG;
    }
    let target_comp = beads[acc.argmax_idx].component_id;
    let R = camera.radius;
    let WS = camera.world_size;
    var weighted = vec3<f32>(0.0);
    var total_w = 0.0;
    for (var i: u32 = 0u; i < camera.bead_count; i = i + 1u) {
        let b = beads[i];
        if (b.component_id != target_comp) { continue; }
        let s = b.state;
        let col = camera.state_colors[s].rgb;
        for (var gy: i32 = -1; gy <= 1; gy = gy + 1) {
            for (var gx: i32 = -1; gx <= 1; gx = gx + 1) {
                let center = b.pos + vec2<f32>(f32(gx), f32(gy)) * WS;
                let f = falloff(distance(p, center), R);
                weighted = weighted + col * f;
                total_w = total_w + f;
            }
        }
    }
    let c = weighted / max(total_w, 1e-6);
    let edge = clamp((acc.total_f_in_comp - ISO) * 6.0, 0.0, 1.0);
    return c * edge + BG * (1.0 - edge);
}
```

- [ ] **Step 2: Wire mode 3**

```wgsl
case 3u: { color = metaball_blend_color(in.world, acc); }
```

- [ ] **Step 3: Build, run, verify**

Switch to Metaball Blend. Expected:
- Single bead: solid colour blob.
- Bonded same-colour pair: peanut, one colour.
- Bonded red-blue pair: smooth red→purple→blue gradient along the bond axis.
- 3-bead triangle: three-way smooth color blend in the centre.
- Unbonded beads close together: each blob renders separately, no colour bleed between them.

- [ ] **Step 4: Commit**

```bash
git add shaders/field.wgsl
git commit -m "feat(field): Metaball Blend mode (mode 3)"
```

---

## Phase 4 — Selection rings post-pass

### Task 13: Selection ring shader, pipeline, dispatch

**Files:**
- Create: `shaders/selection_ring.wgsl`
- Modify: `src/render.rs` (build the post-pass pipeline; dispatch after Field pass)

- [ ] **Step 1: Create `shaders/selection_ring.wgsl`**

```wgsl
struct Bead {
    pos: vec2<f32>,
    vel: vec2<f32>,
    state: u32,
    selected: u32,
    component_id: u32,
    _pad: u32,
};

struct Camera {
    view_proj: mat4x4<f32>,
    radius: f32,
    world_size: f32,
    bead_count: u32,
    mode: u32,
    state_colors: array<vec4<f32>, 8>,
};

@group(0) @binding(0) var<uniform> camera: Camera;
@group(0) @binding(1) var<storage, read> beads: array<Bead>;

struct VsIn {
    @location(0) quad_uv: vec2<f32>,
    @builtin(instance_index) inst: u32,
};

struct VsOut {
    @builtin(position) clip: vec4<f32>,
    @location(0) local: vec2<f32>,
    @location(1) @interpolate(flat) selected: u32,
};

@vertex
fn vs_main(in: VsIn) -> VsOut {
    // Same instancing/ghost-wrap shape as the disc pipeline.
    let bead_idx = in.inst / 9u;
    let ghost = in.inst % 9u;
    let gx = f32(i32(ghost % 3u) - 1);
    let gy = f32(i32(ghost / 3u) - 1);
    let bead = beads[bead_idx];
    let center = bead.pos + vec2<f32>(gx, gy) * camera.world_size;
    let world = center + in.quad_uv * camera.radius;
    var out: VsOut;
    out.clip = camera.view_proj * vec4<f32>(world, 0.0, 1.0);
    out.local = in.quad_uv;
    out.selected = bead.selected;
    return out;
}

@fragment
fn fs_main(in: VsOut) -> @location(0) vec4<f32> {
    if (in.selected == 0u) { discard; }
    let d = length(in.local);
    let ring = smoothstep(0.83, 0.88, d) * (1.0 - smoothstep(0.95, 1.0, d));
    if (ring < 0.05) { discard; }
    return vec4<f32>(1.0, 1.0, 1.0, ring);
}
```

- [ ] **Step 2: Build selection-ring pipeline in `Renderer::new`**

After the field_pipeline build, add:

```rust
let ring_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
    label: Some("selection ring"),
    source: wgpu::ShaderSource::Wgsl(include_str!("../shaders/selection_ring.wgsl").into()),
});
let ring_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
    label: Some("selection ring pipeline"),
    layout: Some(&pipeline_layout),  // same layout as disc — same bindings
    vertex: wgpu::VertexState {
        module: &ring_shader,
        entry_point: Some("vs_main"),
        buffers: &[wgpu::VertexBufferLayout {
            array_stride: 8,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &[wgpu::VertexAttribute {
                offset: 0,
                shader_location: 0,
                format: wgpu::VertexFormat::Float32x2,
            }],
        }],
        compilation_options: Default::default(),
    },
    fragment: Some(wgpu::FragmentState {
        module: &ring_shader,
        entry_point: Some("fs_main"),
        targets: &[Some(wgpu::ColorTargetState {
            format,
            blend: Some(wgpu::BlendState::ALPHA_BLENDING),
            write_mask: wgpu::ColorWrites::ALL,
        })],
        compilation_options: Default::default(),
    }),
    primitive: wgpu::PrimitiveState::default(),
    depth_stencil: None,
    multisample: wgpu::MultisampleState::default(),
    multiview: None,
    cache: None,
});
```

Add `ring_pipeline: wgpu::RenderPipeline` to the `Renderer` struct and to the `Self { ... }` initializer.

- [ ] **Step 3: Dispatch ring pass in `render()`**

After the field/disc dispatch, before the overlay block, add:

```rust
if self.mode.is_field() {
    let mut ring_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
        label: Some("ring pass"),
        color_attachments: &[Some(wgpu::RenderPassColorAttachment {
            view: &view,
            resolve_target: None,
            ops: wgpu::Operations {
                load: wgpu::LoadOp::Load,
                store: wgpu::StoreOp::Store,
            },
        })],
        depth_stencil_attachment: None,
        timestamp_writes: None,
        occlusion_query_set: None,
    });
    ring_pass.set_pipeline(&self.ring_pipeline);
    ring_pass.set_bind_group(0, &self.bind_group, &[]);
    ring_pass.set_vertex_buffer(0, self.quad_vbuf.slice(..));
    ring_pass.draw(0..6, 0..(bead_count * 9) as u32);
}
```

- [ ] **Step 4: Build, run, verify**

In Voronoi (or any field) mode with a bead selected, you should see a white ring around the selected bead. Move the bead — ring follows. Deselect — ring vanishes.

- [ ] **Step 5: Commit**

```bash
git add shaders/selection_ring.wgsl src/render.rs
git commit -m "feat(render): selection-ring post-pass for field modes"
```

---

## Phase 5 — UI

### Task 14: JS bridge functions for render mode

**Files:**
- Modify: `src/app.rs` (add `App.render_mode`, add `__jigglefabGetRenderMode` / `__jigglefabSetRenderMode` exports, hook into the existing `web_bridge` module around lines 17-78)

- [ ] **Step 1: Add `render_mode: RenderMode` field to `App`**

Find the `App` struct in `src/app.rs` (search for `struct App`). Add field `render_mode: crate::render_mode::RenderMode`. Initialize in `App::new` to `RenderMode::Disc`.

- [ ] **Step 2: Add bridge functions**

In the `web_bridge` module (lines 17-78), add (using the same `#[wasm_bindgen]` or `extern` pattern as existing bridge fns — look at `__jigglefabGetLibraryJson` as the template):

```rust
#[wasm_bindgen]
pub fn __jigglefab_set_render_mode(name: &str) {
    if let Ok(mode) = serde_json::from_str::<crate::render_mode::RenderMode>(
        &format!("\"{}\"", name)
    ) {
        APP.with(|a| {
            if let Some(app) = a.borrow_mut().as_mut() {
                app.render_mode = mode;
                app.renderer.set_mode(mode);
            }
        });
    }
}

#[wasm_bindgen]
pub fn __jigglefab_get_render_mode() -> String {
    APP.with(|a| {
        a.borrow()
            .as_ref()
            .map(|app| {
                serde_json::to_string(&app.render_mode)
                    .unwrap_or_else(|_| "\"disc\"".to_string())
                    .trim_matches('"')
                    .to_string()
            })
            .unwrap_or_else(|| "disc".to_string())
    })
}
```

If the existing pattern uses a different style (no `APP.with`, direct fn pointers, etc.), follow it instead. The shape is: read/write `app.render_mode` and call `renderer.set_mode`.

- [ ] **Step 3: Build**

Run: `cargo build --target wasm32-unknown-unknown` (or whatever the project's web build is).
Expected: clean.

- [ ] **Step 4: Commit**

```bash
git add src/app.rs
git commit -m "feat(web): bridge fns for render mode get/set"
```

---

### Task 15: HTML picker + localStorage persistence

**Files:**
- Modify: `index.html` (add render-mode chip group; JS handler; localStorage read/write)

- [ ] **Step 1: Add HTML chip group**

In `index.html`, find the existing `#state-pills` block (around line 252-257). Add a sibling block:

```html
<nav id="render-pills" class="pill-group">
  <span class="pill-label">Render:</span>
  <a href="#" data-mode="disc">Disc</a>
  <a href="#" data-mode="voronoi">Voronoi</a>
  <a href="#" data-mode="soft-voronoi">Soft Voronoi</a>
  <a href="#" data-mode="worley">Worley</a>
  <a href="#" data-mode="metaball-blend">Metaball Blend</a>
  <a href="#" data-mode="metaball-argmax">Metaball Argmax</a>
</nav>
```

- [ ] **Step 2: Add JS init + handlers**

In the JS init block (alongside the existing chemistry/state init around lines 448-505), add:

```javascript
const RENDER_MODE_KEY = 'jigglefab:render-mode';

function applyRenderMode(mode) {
  if (window.__jigglefab_set_render_mode) {
    window.__jigglefab_set_render_mode(mode);
  }
  document.querySelectorAll('#render-pills a').forEach(a => {
    a.classList.toggle('selected', a.dataset.mode === mode);
  });
  localStorage.setItem(RENDER_MODE_KEY, mode);
}

document.querySelectorAll('#render-pills a').forEach(a => {
  a.addEventListener('click', e => {
    e.preventDefault();
    applyRenderMode(a.dataset.mode);
  });
});

// Restore on load. Default to 'disc' if no stored value.
const stored = localStorage.getItem(RENDER_MODE_KEY) || 'disc';
// Defer until the wasm bridge fn exists.
function tryRestoreRenderMode() {
  if (window.__jigglefab_set_render_mode) {
    applyRenderMode(stored);
  } else {
    setTimeout(tryRestoreRenderMode, 100);
  }
}
tryRestoreRenderMode();
```

- [ ] **Step 3: Manual smoke**

`trunk serve` (or whatever local-dev command runs the web build). Open the page. Click each Render pill — the rendered scene should switch modes. Reload the page — the mode you last selected should be active.

- [ ] **Step 4: Commit**

```bash
git add index.html
git commit -m "feat(web): render-mode chip group + localStorage persistence"
```

---

### Task 16: `R` / `Shift+R` keybinds

**Files:**
- Modify: `src/app.rs:1424-1460` (keyboard handler)

- [ ] **Step 1: Add R-key arm**

Inside the keyboard handler match block (search for `Key::Character(` in `src/app.rs:1424-1460`), add:

```rust
Key::Character(s) if s.as_str().eq_ignore_ascii_case("r") => {
    let forward = !self.shift_held;  // adapt to the existing modifier tracking
    let new_mode = self.render_mode.cycle(forward);
    self.render_mode = new_mode;
    self.renderer.set_mode(new_mode);

    // Mirror to JS so the picker chip + localStorage stay in sync.
    // Dispatch a CustomEvent the JS init in Task 15 subscribes to.
    #[cfg(target_arch = "wasm32")]
    {
        use wasm_bindgen::JsValue;
        if let Some(w) = web_sys::window() {
            let detail = JsValue::from_str(new_mode.label_kebab());
            let init = web_sys::CustomEventInit::new();
            init.set_detail(&detail);
            if let Ok(ev) = web_sys::CustomEvent::new_with_event_init_dict(
                "jigglefab:render-mode-changed",
                &init,
            ) {
                let _ = w.dispatch_event(&ev);
            }
        }
    }
}
```

Add a `label_kebab` helper to `RenderMode` (in `src/render_mode.rs`):

```rust
pub fn label_kebab(self) -> &'static str {
    match self {
        RenderMode::Disc => "disc",
        RenderMode::Voronoi => "voronoi",
        RenderMode::SoftVoronoi => "soft-voronoi",
        RenderMode::Worley => "worley",
        RenderMode::MetaballBlend => "metaball-blend",
        RenderMode::MetaballArgmax => "metaball-argmax",
    }
}
```

If the existing keyboard handler doesn't track `shift_held` as a field on `App`, follow the pattern it does use (search the surrounding code for shift handling — there's at least a `Key::Named(NamedKey::Shift)` arm in the handler).

- [ ] **Step 2: Subscribe to the custom event in `index.html` JS**

In the JS init block from Task 15, add (after the existing `applyRenderMode` definition):

```javascript
window.addEventListener('jigglefab:render-mode-changed', e => {
  applyRenderMode(e.detail);
});
```

- [ ] **Step 3: Manual smoke**

Press `R` repeatedly. The render mode should cycle forward through the six options. `Shift+R` cycles backward. The picker chip in the HTML should update its `.selected` class as you cycle.

- [ ] **Step 4: Commit**

```bash
git add src/app.rs src/render_mode.rs index.html
git commit -m "feat(web): R/Shift+R cycles render modes; mirrors to picker"
```

---

## Phase 6 — Tests

### Task 17: Per-mode color-ends-locally smoke in `verify-web.py`

**Files:**
- Modify: `scripts/verify-web.py`

- [ ] **Step 1: Read the existing structure of `verify-web.py`**

Look for the existing per-feature test functions (the device-library smoke is referenced in the project memory as a working example). Mirror the pattern.

- [ ] **Step 2: Add a per-mode test function**

```python
def test_render_mode(page, mode_name: str):
    # Place a known cluster: one isolated + one bonded pair + one bonded triple.
    # (Reuse the editor's place-and-bond helpers used in the editor smoke test
    # — search for `def _place_beads` or similar.)
    _place_known_cluster(page)

    page.click(f"#render-pills a[data-mode='{mode_name}']")
    page.wait_for_timeout(200)  # allow one frame for the mode switch

    screenshot = page.screenshot()
    img = Image.open(io.BytesIO(screenshot))

    # Assert: pixels far from any bead must be background colour.
    # Background is (0.05, 0.05, 0.07) in linear → roughly (13, 13, 18) in sRGB.
    BG = (13, 13, 18)
    TOL = 6
    far_pixels = _sample_pixels_far_from_beads(img)
    for px in far_pixels:
        for ch in range(3):
            assert abs(px[ch] - BG[ch]) <= TOL, (
                f"mode={mode_name}: pixel {px} far from beads is not background"
            )

    # Assert: some pixels in the bead region are non-background (mode draws).
    near_pixels = _sample_pixels_near_beads(img)
    drawn = sum(1 for px in near_pixels
                if any(abs(px[ch] - BG[ch]) > TOL for ch in range(3)))
    assert drawn > len(near_pixels) // 4, (
        f"mode={mode_name}: too few non-background pixels in bead region"
    )
```

Add helpers `_place_known_cluster`, `_sample_pixels_far_from_beads`, `_sample_pixels_near_beads`. The "near" / "far" definitions: near = within `1.0 × R_screen` of any placed bead's screen position; far = beyond `2.5 × R_screen` from every bead.

- [ ] **Step 3: Add a loop over all six modes in the main test sequence**

```python
for mode in ["disc", "voronoi", "soft-voronoi", "worley", "metaball-blend", "metaball-argmax"]:
    test_render_mode(page, mode)
```

- [ ] **Step 4: Run the smoke test**

Run: `python scripts/verify-web.py`
Expected: PASS for all six modes.

- [ ] **Step 5: Commit**

```bash
git add scripts/verify-web.py
git commit -m "test(web): per-mode color-ends-locally smoke"
```

---

### Task 18: Offscreen shader-golden test harness

**Files:**
- Create: `src/bin/render_golden.rs`
- Create: `tests/render_modes_golden.rs`
- Create: `tests/golden/render-modes/.gitkeep`

- [ ] **Step 1: Write the offscreen renderer binary**

`src/bin/render_golden.rs`:

```rust
//! Renders a fixed bead scene through the Renderer into an offscreen 256×256
//! RGBA texture and writes a PNG. Used to (re-)generate golden images.
//!
//! Usage: cargo run --bin render_golden -- <mode> <out.png>

use anyhow::{Context, Result};
use glam::Vec2;
use jigglefab::bond::BondPair;
use jigglefab::component::compute_component_ids;
use jigglefab::render_mode::RenderMode;

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 3 {
        anyhow::bail!("usage: render_golden <mode-kebab> <out.png>");
    }
    let mode: RenderMode = serde_json::from_str(&format!("\"{}\"", args[1]))?;
    let out_path = &args[2];

    let positions = vec![
        Vec2::new(-0.45,  0.0),
        Vec2::new(-0.12,  0.15),
        Vec2::new( 0.20,  0.05),
        Vec2::new( 0.55,  0.30),
        Vec2::new(-0.05, -0.45),
    ];
    let velocities = vec![Vec2::ZERO; positions.len()];
    let states = vec![0u32, 0, 1, 1, 0];
    let selected = vec![0u32; positions.len()];
    let bonds = vec![
        BondPair::new(0, 1),
        BondPair::new(1, 2),
        BondPair::new(3, 4),
    ];
    let component_ids = compute_component_ids(positions.len(), &bonds);

    // Build a headless renderer that writes to an offscreen target. The
    // simplest path: reuse the existing Renderer with a dummy window via the
    // wgpu offscreen-only path. If this is too entangled with winit, write a
    // sibling `OffscreenRenderer` in `src/render_offscreen.rs` that mirrors
    // Renderer's GPU work but renders to a texture instead of a Surface.
    let pixels = jigglefab::render_offscreen::render_scene(
        256, 256, mode, &positions, &velocities, &states, &selected, &component_ids,
    )?;

    image::save_buffer(
        out_path,
        &pixels,
        256,
        256,
        image::ColorType::Rgba8,
    ).context("write PNG")?;
    Ok(())
}
```

(`OffscreenRenderer` / `render_offscreen` is a small adapter; create it as part of this task. It is essentially `Renderer::new` without a surface, plus a final `texture.copy_to_buffer` + readback. Cite [`https://github.com/gfx-rs/wgpu/blob/trunk/examples/src/hello_compute/mod.rs`] as a reference if needed.)

- [ ] **Step 2: Write the golden-comparison test**

`tests/render_modes_golden.rs`:

```rust
use std::path::PathBuf;

const MODES: &[&str] = &[
    "voronoi", "soft-voronoi", "worley", "metaball-blend", "metaball-argmax",
];
const TOLERANCE: u8 = 8;
const GOLDEN_DIR: &str = "tests/golden/render-modes";

fn render(mode: &str) -> image::RgbaImage {
    use glam::Vec2;
    use jigglefab::bond::BondPair;
    use jigglefab::component::compute_component_ids;
    use jigglefab::render_mode::RenderMode;
    let positions = vec![
        Vec2::new(-0.45,  0.0),
        Vec2::new(-0.12,  0.15),
        Vec2::new( 0.20,  0.05),
        Vec2::new( 0.55,  0.30),
        Vec2::new(-0.05, -0.45),
    ];
    let velocities = vec![Vec2::ZERO; positions.len()];
    let states = vec![0u32, 0, 1, 1, 0];
    let selected = vec![0u32; positions.len()];
    let bonds = vec![
        BondPair::new(0, 1), BondPair::new(1, 2), BondPair::new(3, 4),
    ];
    let component_ids = compute_component_ids(positions.len(), &bonds);
    let mode_enum: RenderMode = serde_json::from_str(&format!("\"{}\"", mode)).unwrap();
    let pixels = jigglefab::render_offscreen::render_scene(
        256, 256, mode_enum, &positions, &velocities, &states, &selected, &component_ids,
    ).unwrap();
    image::RgbaImage::from_vec(256, 256, pixels).unwrap()
}

fn compare(a: &image::RgbaImage, b: &image::RgbaImage, tol: u8) -> bool {
    if a.dimensions() != b.dimensions() { return false; }
    let mut bad = 0usize;
    for (pa, pb) in a.pixels().zip(b.pixels()) {
        for c in 0..3 {
            if pa.0[c].abs_diff(pb.0[c]) > tol { bad += 1; break; }
        }
    }
    let total = (a.width() * a.height()) as usize;
    (bad as f32) / (total as f32) < 0.005  // <0.5% pixels above tolerance
}

#[test]
fn render_modes_match_goldens() {
    for mode in MODES {
        let golden_path = PathBuf::from(GOLDEN_DIR).join(format!("{}.png", mode));
        let current = render(mode);
        if !golden_path.exists() {
            panic!(
                "missing golden for {mode}: rerun with `cargo test -- --ignored regenerate_goldens`"
            );
        }
        let golden = image::open(&golden_path).unwrap().to_rgba8();
        assert!(compare(&current, &golden, TOLERANCE),
            "render-mode {mode} mismatches golden at {}", golden_path.display());
    }
}

#[test]
#[ignore]
fn regenerate_goldens() {
    std::fs::create_dir_all(GOLDEN_DIR).unwrap();
    for mode in MODES {
        let current = render(mode);
        let golden_path = PathBuf::from(GOLDEN_DIR).join(format!("{}.png", mode));
        current.save(&golden_path).unwrap();
    }
    println!("regenerated {} goldens", MODES.len());
}
```

- [ ] **Step 3: Create the offscreen renderer adapter**

In a new `src/render_offscreen.rs`, export:

```rust
pub fn render_scene(
    w: u32, h: u32,
    mode: crate::render_mode::RenderMode,
    positions: &[glam::Vec2],
    velocities: &[glam::Vec2],
    states: &[u32],
    selected: &[u32],
    component_ids: &[u32],
) -> anyhow::Result<Vec<u8>> { ... }
```

The implementation mirrors `Renderer::new` but creates a `wgpu::Texture` with `TextureUsages::RENDER_ATTACHMENT | COPY_SRC` instead of a `Surface`. After rendering, copy the texture into a `Buffer` and `map_read` to get RGBA bytes. wgpu has a well-known example for this pattern; transcribe it directly.

Add `pub mod render_offscreen;` to `src/lib.rs`.

- [ ] **Step 4: Generate the goldens (first time)**

Run: `cargo test --test render_modes_golden -- --ignored regenerate_goldens`

Eyeball each PNG in `tests/golden/render-modes/`. They should look like the descriptions in Phase 3 tasks (Voronoi = hard cells; Worley = cracks; Metaball Blend = smooth blob; etc.). If any look wrong, the bug is in the shader, not the test — fix the shader.

- [ ] **Step 5: Run goldens normally**

Run: `cargo test --test render_modes_golden`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add src/render_offscreen.rs src/bin/render_golden.rs src/lib.rs \
        tests/render_modes_golden.rs tests/golden/render-modes/
git commit -m "test(render): offscreen golden harness + per-mode goldens"
```

---

## Self-Review

After all tasks complete, verify against the spec:

- [ ] All six render modes selectable and visually distinct (manual web smoke).
- [ ] Color-ends-locally invariant holds in every mode (Task 17).
- [ ] Bond-aware merging: unbonded close beads do NOT fuse in metaball modes (manual visual check).
- [ ] localStorage persistence across reloads.
- [ ] `R` / `Shift+R` cycle in both directions, wrap at endpoints.
- [ ] Default on first load is Disc (no surprise to existing users).
- [ ] Selection rings visible in field modes.
- [ ] `cargo test` passes.
- [ ] `scripts/verify-web.py` passes.
- [ ] Bonds-change → component_ids re-uploaded (dirty-flag gate; verify by manually adding a bond at runtime and confirming metaball-blend re-fuses).

## Out-of-Scope Reminders

These were explicitly deferred in the spec and must NOT slip into this PR:
- Anisotropic-velocity modes.
- Per-mode live sliders.
- 3D rendering.
- Spatial-grid acceleration (A → C swap).
- Bond-only metaballs (rather than whole-component).

If perf shows up as bad at 5k beads with bonds, file a follow-up plan to implement the A → C swap. Do not bundle it here.
