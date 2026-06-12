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
