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
    let acc = accumulate_field(in.world);
    // Debug: show nearest-distance falloff so we can verify the accumulator
    // is finding beads correctly. Later tasks replace this with mode dispatch.
    let t = clamp(1.0 - acc.nearest_d / (camera.radius * 1.5), 0.0, 1.0);
    return vec4<f32>(vec3<f32>(t), 1.0);
}
