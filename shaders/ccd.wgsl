// Per-pair CCD: port of next_contact() from ccd.rs.
// Dispatch with ceil(max_pairs / 64) workgroups.
// pair_count is read from iter_state[2] (set by grid_pairs).

struct Params {
    n_beads: u32, cells_per_axis: u32, max_per_cell: u32, n_states: u32,
    world_size: f32, max_pairs: u32, n_bonds: u32, _pad: u32,
}
struct Pair    { a: u32, b: u32, bonded: u32, _pad: u32, }
struct Contact { t: f32, a: u32, b: u32, flags: u32, }

const INF_T: f32 = 1e30;
const RADIUS: f32 = 1.0;

fn min_image(d: vec2f, ws: f32) -> vec2f {
    return d - ws * round(d / ws);
}

@group(0) @binding(0) var<uniform> params: Params;
@group(0) @binding(1) var<storage, read> positions: array<vec2f>;
@group(0) @binding(2) var<storage, read> velocities: array<vec2f>;
@group(0) @binding(3) var<storage, read> iter_state: array<u32>;  // [2] = pair_count
@group(0) @binding(4) var<storage, read> pairs: array<Pair>;
@group(0) @binding(5) var<storage, read_write> contacts: array<Contact>;

@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) gid: vec3u) {
    let idx = gid.x;
    let pair_count = iter_state[2];
    if idx >= pair_count {
        if idx < params.max_pairs {
            contacts[idx] = Contact(INF_T, 0u, 0u, 0u);
        }
        return;
    }

    let pair = pairs[idx];
    let a = pair.a;
    let b = pair.b;
    let pa = positions[a];
    let pb_raw = positions[b];
    let d = min_image(pb_raw - pa, params.world_size);
    let dv = velocities[b] - velocities[a];
    let dt_remaining = bitcast<f32>(iter_state[1]);

    // Quadratic: |d + dv*t|^2 = R^2
    // (dv·dv)t^2 + 2(d·dv)t + (d·d - R^2) = 0
    // Expanded to avoid FMA in dot() which could break tiebreaking.
    let a_c = dv.x * dv.x + dv.y * dv.y;
    let b_c = 2.0 * (d.x * dv.x + d.y * dv.y);
    let c_c = d.x * d.x + d.y * d.y - RADIUS * RADIUS;

    if a_c < 1e-12 {
        contacts[idx] = Contact(INF_T, a, b, pair.bonded << 1u);
        return;
    }
    let disc = b_c * b_c - 4.0 * a_c * c_c;
    if disc < 0.0 {
        contacts[idx] = Contact(INF_T, a, b, pair.bonded << 1u);
        return;
    }
    let sqrt_disc = sqrt(disc);
    let t_early = (-b_c - sqrt_disc) / (2.0 * a_c);
    let t_late  = (-b_c + sqrt_disc) / (2.0 * a_c);

    // Smallest non-negative root within [0, dt_remaining].
    var best: f32 = INF_T;
    if t_early >= 0.0 && t_early <= dt_remaining { best = t_early; }
    if t_late  >= 0.0 && t_late  <= dt_remaining && t_late < best { best = t_late; }

    if best >= INF_T {
        contacts[idx] = Contact(INF_T, a, b, pair.bonded << 1u);
        return;
    }

    let d_at_t = d + dv * best;
    let exiting = u32(d_at_t.x * dv.x + d_at_t.y * dv.y > 0.0);
    // flags: bit 0 = exiting, bit 1 = bonded
    let flags = exiting | (pair.bonded << 1u);
    contacts[idx] = Contact(best, a, b, flags);
}
