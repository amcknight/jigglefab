struct Params {
    n_beads: u32, cells_per_axis: u32, max_per_cell: u32, n_states: u32,
    world_size: f32, max_pairs: u32, n_bonds: u32, _pad: u32,
}
struct Contact { t: f32, a: u32, b: u32, flags: u32, }

const INF_T: f32 = 1e30;
const RADIUS: f32 = 1.0;
const BOUNDARY_EPS: f32 = 1e-5;

// Actions: 0=Reflect, 1=Pass, 2=ReflectSwap
const ACTION_REFLECT:      u32 = 0u;
const ACTION_PASS:         u32 = 1u;
const ACTION_REFLECT_SWAP: u32 = 2u;

fn min_image(d: vec2f, ws: f32) -> vec2f {
    return d - ws * round(d / ws);
}

fn wrap_pos(p: vec2f, ws: f32) -> vec2f {
    return ((p % ws) + ws) % ws;
}

@group(0) @binding(0) var<uniform> params: Params;
@group(0) @binding(1) var<storage, read_write> positions: array<vec2f>;
@group(0) @binding(2) var<storage, read_write> velocities: array<vec2f>;
@group(0) @binding(3) var<storage, read_write> states: array<u32>;
@group(0) @binding(4) var<storage, read> contacts: array<Contact>;  // [0] = winner
@group(0) @binding(5) var<storage, read_write> iter_state: array<atomic<u32>>;
@group(0) @binding(6) var<storage, read> chemistry: array<u32>;

// ── advance_beads ─────────────────────────────────────────────────────────────
// Advance every bead by t_advance = min(contacts[0].t, dt_remaining).
// Dispatch with ceil(n_beads / 64) workgroups.
@compute @workgroup_size(64)
fn advance_beads(@builtin(global_invocation_id) gid: vec3u) {
    let i = gid.x;
    if i >= params.n_beads { return; }

    let dt_remaining = bitcast<f32>(atomicLoad(&iter_state[1]));
    let best_t = contacts[0].t;
    let t_advance = select(dt_remaining, best_t, best_t < INF_T);

    positions[i] = wrap_pos(positions[i] + velocities[i] * t_advance, params.world_size);
}

// ── resolve_contact ───────────────────────────────────────────────────────────
// Single thread: apply chemistry to the winning contact, snap positions, update iter_state.
// Dispatch with 1 thread (workgroup_size=1, 1 workgroup).
@compute @workgroup_size(1)
fn resolve_contact() {
    let best = contacts[0];
    let dt_remaining = bitcast<f32>(atomicLoad(&iter_state[1]));

    if best.t >= INF_T {
        // No contact — all beads advanced by dt_remaining; we're done.
        atomicOr(&iter_state[0], 1u);  // done flag
        return;
    }

    let new_dt = dt_remaining - best.t;
    atomicStore(&iter_state[1], bitcast<u32>(new_dt));

    let a = best.a;
    let b = best.b;
    let exiting = (best.flags & 1u) != 0u;
    let bonded  = (best.flags & 2u) != 0u;

    // Drift correction: if topology disagrees with geometry, pass through.
    // bonded == exiting means they're in their "natural" state.
    let action = select(
        ACTION_PASS,
        chemistry[states[a] * params.n_states * 2u + states[b] * 2u + u32(bonded)],
        bonded == exiting
    );

    let pa = positions[a];
    let pb_raw = positions[b];
    let d = min_image(pb_raw - pa, params.world_size);
    let dist = length(d);

    if action == ACTION_REFLECT || action == ACTION_REFLECT_SWAP {
        // Reflect: swap normal-component velocities.
        if dist > 1e-12 {
            let n = d / dist;
            let va = velocities[a];
            let vb = velocities[b];
            let va_n = dot(va, n) * n;
            let vb_n = dot(vb, n) * n;
            velocities[a] = va + (vb_n - va_n);
            velocities[b] = vb + (va_n - vb_n);
        }
    }

    if action == ACTION_REFLECT_SWAP {
        let sa = states[a];
        states[a] = states[b];
        states[b] = sa;
    }

    // Snap positions onto the correct side of R after this action.
    let post_inside = select(!exiting, exiting,
        action == ACTION_REFLECT || action == ACTION_REFLECT_SWAP);
    if dist > 1e-12 {
        let n = d / dist;
        let snap_dist = select(RADIUS + BOUNDARY_EPS, RADIUS - BOUNDARY_EPS, post_inside);
        let correction = (snap_dist - dist) * 0.5;
        positions[a] = wrap_pos(pa - n * correction, params.world_size);
        positions[b] = wrap_pos(pb_raw + n * correction, params.world_size);
    }
}
