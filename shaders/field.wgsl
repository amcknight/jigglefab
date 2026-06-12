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
