struct Bead {
    pos: vec2<f32>,
    state: u32,
    selected: u32,
};

struct Camera {
    view_proj: mat4x4<f32>,
    radius: f32,
    world_size: f32,
    _pad0: f32,
    _pad1: f32,
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
    @location(1) @interpolate(flat) state: u32,
    @location(2) @interpolate(flat) selected: u32,
};

@vertex
fn vs_main(in: VsIn) -> VsOut {
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
    out.state = bead.state;
    out.selected = bead.selected;
    return out;
}

@fragment
fn fs_main(in: VsOut) -> @location(0) vec4<f32> {
    let d = length(in.local);
    if (d > 1.0) {
        discard;
    }
    let body = smoothstep(1.0, 0.95, d);
    let c = camera.state_colors[in.state].rgb;
    var color = c;
    var alpha = body;
    if (in.selected != 0u) {
        let ring = smoothstep(0.83, 0.88, d) * (1.0 - smoothstep(0.95, 1.0, d));
        color = mix(color, vec3<f32>(1.0, 1.0, 1.0), ring);
        alpha = max(alpha, ring);
    }
    return vec4<f32>(color, alpha);
}
