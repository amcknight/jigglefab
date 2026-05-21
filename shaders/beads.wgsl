struct Bead {
    pos: vec2<f32>,
    _pad: vec2<f32>,  // forces 16-byte stride to match Rust-side BeadGpu
};

struct Camera {
    view_proj: mat4x4<f32>,
    radius: f32,
    world_size: f32,
    _pad0: f32,
    _pad1: f32,
};

@group(0) @binding(0) var<uniform> camera: Camera;
@group(0) @binding(1) var<storage, read> beads: array<Bead>;

struct VsIn {
    @location(0) quad_uv: vec2<f32>, // unit-quad corner in [-1, 1]
    @builtin(instance_index) inst: u32,
};

struct VsOut {
    @builtin(position) clip: vec4<f32>,
    @location(0) local: vec2<f32>,
};

@vertex
fn vs_main(in: VsIn) -> VsOut {
    // Per-instance encoding: 9 copies of each bead — one at the bead's actual
    // position and 8 wrap-ghosts at ±world_size offsets in each axis. The
    // rasterizer discards offscreen ghosts for free; the on-screen ones make
    // bonds across the torus seam visible.
    let bead_idx = in.inst / 9u;
    let ghost = in.inst % 9u;
    let gx = f32(i32(ghost % 3u) - 1);
    let gy = f32(i32(ghost / 3u) - 1);
    let center = beads[bead_idx].pos + vec2<f32>(gx, gy) * camera.world_size;
    let world = center + in.quad_uv * camera.radius;
    var out: VsOut;
    out.clip = camera.view_proj * vec4<f32>(world, 0.0, 1.0);
    out.local = in.quad_uv;
    return out;
}

@fragment
fn fs_main(in: VsOut) -> @location(0) vec4<f32> {
    let d = length(in.local);
    if (d > 1.0) {
        discard;
    }
    // Soft edge so the disc looks like a disc, not a polygon.
    let a = smoothstep(1.0, 0.95, d);
    return vec4<f32>(0.78, 0.78, 0.80, a);
}
