struct Bead {
    pos: vec2<f32>,
};

struct Camera {
    view_proj: mat4x4<f32>,
    radius: f32,
    _pad0: f32,
    _pad1: f32,
    _pad2: f32,
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
    let center = beads[in.inst].pos;
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
