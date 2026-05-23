// Two-pass parallel reduction to find contact with minimum (t, a, b).
// Pass 1 (reduce_local): dispatch ceil(max_pairs / 256) workgroups of 256 threads.
//   Each workgroup reduces its 256 contacts into reduce_scratch[workgroup_id].
// Pass 2 (reduce_global): dispatch 1 workgroup of 256 threads.
//   Reduces reduce_scratch into contacts[0].

struct Params {
    n_beads: u32, cells_per_axis: u32, max_per_cell: u32, n_states: u32,
    world_size: f32, max_pairs: u32, n_bonds: u32, _pad: u32,
}
struct Contact { t: f32, a: u32, b: u32, flags: u32, }

const INF_T: f32 = 1e30;

var<workgroup> wg_contacts: array<Contact, 256>;

fn contact_lt(x: Contact, y: Contact) -> bool {
    if x.t < y.t { return true; }
    if x.t > y.t { return false; }
    if x.a < y.a { return true; }
    if x.a > y.a { return false; }
    return x.b < y.b;
}

fn sentinel() -> Contact { return Contact(INF_T, 0xFFFFFFFFu, 0xFFFFFFFFu, 0u); }

@group(0) @binding(0) var<uniform> params: Params;
@group(0) @binding(1) var<storage, read> iter_state: array<u32>;   // [2] = pair_count
@group(0) @binding(2) var<storage, read_write> contacts: array<Contact>;
@group(0) @binding(3) var<storage, read_write> reduce_scratch: array<Contact>;

@compute @workgroup_size(256)
fn reduce_local(
    @builtin(global_invocation_id) gid: vec3u,
    @builtin(local_invocation_id)  lid: vec3u,
    @builtin(workgroup_id)         wid: vec3u,
) {
    let pair_count = iter_state[2];
    let idx = gid.x;
    wg_contacts[lid.x] = select(sentinel(), contacts[idx], idx < pair_count);
    workgroupBarrier();

    for (var stride: u32 = 128u; stride > 0u; stride >>= 1u) {
        if lid.x < stride {
            let other = wg_contacts[lid.x + stride];
            if contact_lt(other, wg_contacts[lid.x]) {
                wg_contacts[lid.x] = other;
            }
        }
        workgroupBarrier();
    }

    if lid.x == 0u {
        reduce_scratch[wid.x] = wg_contacts[0];
    }
}

@compute @workgroup_size(256)
fn reduce_global(
    @builtin(local_invocation_id) lid: vec3u,
) {
    // scratch has at most ceil(max_pairs/256) entries; pad with sentinels.
    let scratch_count = (params.max_pairs + 255u) / 256u;
    wg_contacts[lid.x] = select(sentinel(), reduce_scratch[lid.x], lid.x < scratch_count);
    workgroupBarrier();

    for (var stride: u32 = 128u; stride > 0u; stride >>= 1u) {
        if lid.x < stride {
            let other = wg_contacts[lid.x + stride];
            if contact_lt(other, wg_contacts[lid.x]) {
                wg_contacts[lid.x] = other;
            }
        }
        workgroupBarrier();
    }

    if lid.x == 0u {
        contacts[0] = wg_contacts[0];
    }
}
