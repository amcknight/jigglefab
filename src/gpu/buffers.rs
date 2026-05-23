use bytemuck::{Pod, Zeroable};
use glam::Vec2;

#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable, Debug)]
pub struct GpuParams {
    pub n_beads: u32,
    pub cells_per_axis: u32,
    pub max_per_cell: u32,
    pub n_states: u32,
    pub world_size: f32,
    pub max_pairs: u32,
    pub n_bonds: u32,
    pub _pad: u32,
}

/// Estimate max candidate pairs: each bead scans 9 cells × max_per_cell neighbours / 2 for uniqueness.
/// Capped at 256*256=65536 so reduce_local emits at most 256 scratch entries, which reduce_global (1 wg of 256) can handle.
pub fn max_pairs_for(n_beads: u32, max_per_cell: u32) -> u32 {
    let uncapped = n_beads * max_per_cell * 9 / 2 + 64;
    uncapped.min(256 * 256)  // reduce_global can handle at most 256 scratch entries
}

pub struct GpuBuffers {
    pub params: wgpu::Buffer,
    pub positions: wgpu::Buffer,
    pub velocities: wgpu::Buffer,
    pub states: wgpu::Buffer,
    pub bonds: wgpu::Buffer,
    pub grid_counts: wgpu::Buffer,
    pub grid_beads: wgpu::Buffer,
    pub pairs: wgpu::Buffer,
    pub contacts: wgpu::Buffer,
    pub reduce_scratch: wgpu::Buffer,
    pub chemistry: wgpu::Buffer,
    pub iter_state: wgpu::Buffer,
    pub readback: wgpu::Buffer,
    pub n_beads: u32,
    pub max_pairs: u32,
    pub cells_per_axis: u32,
    pub max_per_cell: u32,
}

impl GpuBuffers {
    pub fn new(
        device: &wgpu::Device,
        n_beads: usize,
        n_bonds: usize,
        world_size: f32,
        _n_states: usize,
        chemistry_table: &[u32],
    ) -> Self {
        use wgpu::BufferUsages as U;
        let n = n_beads as u32;
        // Cell size = 2.0 (= 2 × RADIUS). Each cell is 2×2 world units.
        let cells_per_axis = (world_size / 2.0).ceil() as u32;
        let n_cells = (cells_per_axis * cells_per_axis) as usize;
        let max_per_cell: u32 = 32;
        let max_pairs = max_pairs_for(n, max_per_cell);

        let mk = |size: u64, usage: U| {
            device.create_buffer(&wgpu::BufferDescriptor {
                label: None,
                size,
                usage,
                mapped_at_creation: false,
            })
        };

        let beads_bytes = (n as u64) * 8;        // vec2f = 8 bytes
        let states_bytes = (n as u64) * 4;        // u32 = 4 bytes
        let bonds_bytes = (n_bonds as u64) * 8;   // [u32; 2] = 8 bytes
        let cells_bytes = (n_cells as u64) * 4;   // u32 per cell
        let grid_beads_bytes = (n_cells as u64) * (max_per_cell as u64) * 4;
        // Pair struct: 4 × u32 = 16 bytes; Contact struct: 4 × u32 = 16 bytes
        let pairs_bytes = (max_pairs as u64) * 16;
        let contacts_bytes = (max_pairs as u64) * 16;
        let scratch_bytes = ((max_pairs / 256 + 1) as u64) * 16;
        let chem_bytes = (chemistry_table.len() as u64) * 4;
        // Readback staging: positions + velocities + states + iter_state(12 bytes)
        let readback_bytes = beads_bytes * 2 + states_bytes + 12;

        let storage_rw = U::STORAGE | U::COPY_DST | U::COPY_SRC;
        let storage_ro = U::STORAGE | U::COPY_DST;

        let params = mk(
            std::mem::size_of::<GpuParams>() as u64,
            U::UNIFORM | U::COPY_DST,
        );
        let positions      = mk(beads_bytes.max(4),      storage_rw);
        let velocities     = mk(beads_bytes.max(4),      storage_rw);
        let states         = mk(states_bytes.max(4),     storage_rw);
        let bonds          = mk(bonds_bytes.max(4),      storage_ro);
        let grid_counts    = mk(cells_bytes.max(4),      U::STORAGE);
        let grid_beads     = mk(grid_beads_bytes.max(4), U::STORAGE);
        let pairs          = mk(pairs_bytes.max(4),      U::STORAGE);
        let contacts       = mk(contacts_bytes.max(4),   U::STORAGE);
        let reduce_scratch = mk(scratch_bytes.max(4),    U::STORAGE);
        let chemistry      = mk(chem_bytes.max(4),       storage_ro);
        let iter_state     = mk(12,                      U::STORAGE | U::COPY_SRC | U::COPY_DST);
        let readback       = mk(readback_bytes.max(4),   U::MAP_READ | U::COPY_DST);

        GpuBuffers {
            params, positions, velocities, states, bonds,
            grid_counts, grid_beads, pairs, contacts, reduce_scratch,
            chemistry, iter_state, readback,
            n_beads: n, max_pairs, cells_per_axis, max_per_cell,
        }
    }

    /// Upload per-frame CPU data to GPU. Call once per frame before dispatch.
    pub fn upload_frame(
        &self,
        queue: &wgpu::Queue,
        positions: &[Vec2],
        velocities: &[Vec2],
        states: &[u32],
        bonds_sorted: &[[u32; 2]],
        params: GpuParams,
        frame_dt: f32,
    ) {
        queue.write_buffer(&self.params, 0, bytemuck::bytes_of(&params));
        queue.write_buffer(&self.positions, 0, bytemuck::cast_slice(positions));
        queue.write_buffer(&self.velocities, 0, bytemuck::cast_slice(velocities));
        queue.write_buffer(&self.states, 0, bytemuck::cast_slice(states));
        if !bonds_sorted.is_empty() {
            queue.write_buffer(&self.bonds, 0, bytemuck::cast_slice(bonds_sorted));
        }
        // iter_state: [flags=0, dt_remaining as f32 bits, pair_count=0]
        let dt_bits = frame_dt.to_bits();
        queue.write_buffer(&self.iter_state, 0,
            bytemuck::cast_slice(&[0u32, dt_bits, 0u32]));
    }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;
    use crate::gpu::context::GpuContext;

    #[test]
    #[ignore = "requires GPU"]
    fn positions_upload_readback() {
        let ctx = GpuContext::new_headless().unwrap();
        let positions = vec![Vec2::new(1.0, 2.0), Vec2::new(3.0, 4.0)];
        let velocities = vec![Vec2::new(1.0, 0.0), Vec2::new(-1.0, 0.0)];
        let states = vec![0u32, 0u32];
        let chem_table = vec![0u32; 2]; // 1-state chemistry, 2 entries
        let bufs = GpuBuffers::new(&ctx.device, 2, 0, 30.0, 1, &chem_table);
        let params = GpuParams {
            n_beads: 2, cells_per_axis: bufs.cells_per_axis,
            max_per_cell: bufs.max_per_cell, n_states: 1,
            world_size: 30.0, max_pairs: bufs.max_pairs,
            n_bonds: 0, _pad: 0,
        };
        bufs.upload_frame(&ctx.queue, &positions, &velocities, &states, &[], params, 1.0/60.0);
        ctx.queue.submit([]);

        // Copy positions to readback and verify
        let mut enc = ctx.device.create_command_encoder(&Default::default());
        enc.copy_buffer_to_buffer(&bufs.positions, 0, &bufs.readback, 0, 16);
        ctx.queue.submit([enc.finish()]);

        let slice = bufs.readback.slice(..16);
        slice.map_async(wgpu::MapMode::Read, |_| {});
        ctx.device.poll(wgpu::Maintain::Wait);
        let result: Vec<Vec2> = {
            let view = slice.get_mapped_range();
            bytemuck::cast_slice::<u8, Vec2>(&view).to_vec()
        };
        bufs.readback.unmap();

        assert!((result[0] - positions[0]).length() < 1e-6);
        assert!((result[1] - positions[1]).length() < 1e-6);
    }
}
