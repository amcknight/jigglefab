// Each bead claims a slot in its grid cell via atomicAdd.
// Dispatch with ceil(n_beads / 64) workgroups.

struct Params {
    n_beads: u32, cells_per_axis: u32, max_per_cell: u32, n_states: u32,
    world_size: f32, max_pairs: u32, n_bonds: u32, _pad: u32,
}

@group(0) @binding(0) var<uniform> params: Params;
@group(0) @binding(1) var<storage, read> positions: array<vec2f>;
@group(0) @binding(2) var<storage, read_write> grid_counts: array<atomic<u32>>;
@group(0) @binding(3) var<storage, read_write> grid_beads: array<u32>;
@group(0) @binding(4) var<storage, read_write> iter_state: array<atomic<u32>>;

fn cell_for(pos: vec2f) -> u32 {
    let cell_size = params.world_size / f32(params.cells_per_axis);
    let cx = u32(pos.x / cell_size) % params.cells_per_axis;
    let cy = u32(pos.y / cell_size) % params.cells_per_axis;
    return cy * params.cells_per_axis + cx;
}

@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) gid: vec3u) {
    let i = gid.x;
    if i >= params.n_beads { return; }

    let cell = cell_for(positions[i]);
    let slot = atomicAdd(&grid_counts[cell], 1u);
    if slot >= params.max_per_cell {
        // Signal overflow — CPU falls back to CpuSequential this frame.
        atomicOr(&iter_state[0], 2u);
        return;
    }
    grid_beads[cell * params.max_per_cell + slot] = i;
}
