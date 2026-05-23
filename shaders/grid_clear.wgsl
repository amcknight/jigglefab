// Clears grid_counts to zero. Dispatch with ceil(n_cells / 64) workgroups.

struct Params {
    n_beads: u32, cells_per_axis: u32, max_per_cell: u32, n_states: u32,
    world_size: f32, max_pairs: u32, n_bonds: u32, _pad: u32,
}

@group(0) @binding(0) var<uniform> params: Params;
@group(0) @binding(1) var<storage, read_write> grid_counts: array<atomic<u32>>;
@group(0) @binding(2) var<storage, read_write> iter_state: array<atomic<u32>>;

@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) gid: vec3u) {
    let n_cells = params.cells_per_axis * params.cells_per_axis;
    let i = gid.x;
    if i < n_cells {
        atomicStore(&grid_counts[i], 0u);
    }
    // Thread 0 also clears pair_count in iter_state[2].
    if i == 0u {
        atomicStore(&iter_state[2], 0u);
    }
}
