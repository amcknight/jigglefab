// Each bead i scans 9 neighbour cells and emits unique pairs (i < j).
// Dispatch with ceil(n_beads / 64) workgroups.

struct Params {
    n_beads: u32, cells_per_axis: u32, max_per_cell: u32, n_states: u32,
    world_size: f32, max_pairs: u32, n_bonds: u32, _pad: u32,
}

struct Pair { a: u32, b: u32, bonded: u32, _pad: u32, }

@group(0) @binding(0) var<uniform> params: Params;
@group(0) @binding(1) var<storage, read> positions: array<vec2f>;
@group(0) @binding(2) var<storage, read> grid_counts: array<u32>;
@group(0) @binding(3) var<storage, read> grid_beads: array<u32>;
@group(0) @binding(4) var<storage, read> bonds: array<vec2u>;  // sorted (a,b) with a<b
@group(0) @binding(5) var<storage, read_write> iter_state: array<atomic<u32>>;
@group(0) @binding(6) var<storage, read_write> pairs: array<Pair>;

fn cell_for(pos: vec2f) -> vec2i {
    let cell_size = params.world_size / f32(params.cells_per_axis);
    let cx = i32(pos.x / cell_size);
    let cy = i32(pos.y / cell_size);
    return vec2i(cx, cy);
}

fn is_bonded(a: u32, b: u32) -> bool {
    // Binary search in sorted bonds array (always a < b).
    var lo: u32 = 0u;
    var hi: u32 = params.n_bonds;
    loop {
        if lo >= hi { return false; }
        let mid = lo + (hi - lo) / 2u;
        let bond = bonds[mid];
        if bond.x == a && bond.y == b { return true; }
        if bond.x < a || (bond.x == a && bond.y < b) {
            lo = mid + 1u;
        } else {
            hi = mid;
        }
    }
    return false;
}

@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) gid: vec3u) {
    let i = gid.x;
    if i >= params.n_beads { return; }

    let cpa = i32(params.cells_per_axis);
    let pos_i = positions[i];
    let ci = cell_for(pos_i);

    for (var dy: i32 = -1; dy <= 1; dy++) {
        for (var dx: i32 = -1; dx <= 1; dx++) {
            // Torus-wrap neighbour cell coordinates.
            let nx = ((ci.x + dx) % cpa + cpa) % cpa;
            let ny = ((ci.y + dy) % cpa + cpa) % cpa;
            let ncell = u32(ny * cpa + nx);
            let count = min(grid_counts[ncell], params.max_per_cell);
            for (var s: u32 = 0u; s < count; s++) {
                let j = grid_beads[ncell * params.max_per_cell + s];
                if j <= i { continue; }  // emit each pair once (i < j)

                // Claim a slot.
                let slot = atomicAdd(&iter_state[2], 1u);
                if slot >= params.max_pairs {
                    atomicOr(&iter_state[0], 2u);  // overflow
                    return;
                }

                let bonded = u32(is_bonded(i, j));
                pairs[slot] = Pair(i, j, bonded, 0u);
            }
        }
    }
}
