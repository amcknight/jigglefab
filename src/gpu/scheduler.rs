use glam::Vec2;

use crate::gpu::buffers::{GpuBuffers, GpuParams};
use crate::gpu::context::GpuContext;
use crate::gpu::pipelines::GpuPipelines;
use crate::scheduler::Scheduler;
use crate::sim::{Sim, StepMetrics};

const BATCH_SIZE: u32 = 64;

pub struct GpuEventLoop {
    ctx: GpuContext,
    pipelines: GpuPipelines,
    buffers: GpuBuffers,
    bonds_sorted: Vec<[u32; 2]>,
    _chemistry_table: Vec<u32>,
}

impl GpuEventLoop {
    pub fn new(ctx: GpuContext, sim: &Sim) -> Self {
        let n = sim.positions.len();
        let world_size = sim.world_size();
        let n_states = sim.chemistry.states.len();
        let chemistry_table = sim.chemistry.action_table_flat();
        let bonds_sorted = Self::sort_bonds(sim);
        let n_bonds = bonds_sorted.len();
        let pipelines = GpuPipelines::new(&ctx.device);
        let buffers = GpuBuffers::new(
            &ctx.device, n, n_bonds, world_size, n_states, &chemistry_table,
        );

        // Upload chemistry once (static for life of scheduler)
        ctx.queue.write_buffer(
            &buffers.chemistry, 0,
            bytemuck::cast_slice(&chemistry_table),
        );

        GpuEventLoop { ctx, pipelines, buffers, bonds_sorted, _chemistry_table: chemistry_table }
    }

    fn sort_bonds(sim: &Sim) -> Vec<[u32; 2]> {
        let mut v: Vec<[u32; 2]> = sim.bonds.iter()
            .map(|bp| bp.as_array())
            .collect();
        v.sort_unstable();
        v
    }

    fn make_params(&self, sim: &Sim, _frame_dt: f32) -> GpuParams {
        GpuParams {
            n_beads: sim.positions.len() as u32,
            cells_per_axis: self.buffers.cells_per_axis,
            max_per_cell: self.buffers.max_per_cell,
            n_states: sim.chemistry.states.len() as u32,
            world_size: sim.world_size(),
            max_pairs: self.buffers.max_pairs,
            n_bonds: self.bonds_sorted.len() as u32,
            _pad: 0,
        }
    }

    fn encode_iteration(&self, enc: &mut wgpu::CommandEncoder, params: &GpuParams) {
        let n = params.n_beads;
        let n_cells = params.cells_per_axis * params.cells_per_axis;
        let max_pairs = params.max_pairs;

        let bufs = &self.buffers;
        let pl = &self.pipelines;
        let device = &self.ctx.device;

        macro_rules! e {
            ($binding:expr, $buf:expr) => {
                wgpu::BindGroupEntry {
                    binding: $binding,
                    resource: $buf.as_entire_binding(),
                }
            };
        }

        macro_rules! bg {
            ($layout:expr, [$($entry:expr),* $(,)?]) => {
                device.create_bind_group(&wgpu::BindGroupDescriptor {
                    label: None,
                    layout: $layout,
                    entries: &[$($entry),*],
                })
            };
        }

        macro_rules! dispatch {
            ($enc:expr, $pipeline:expr, $bind_group:expr, $threads:expr, $wg_size:expr) => {{
                let mut pass = $enc.begin_compute_pass(&wgpu::ComputePassDescriptor {
                    label: None,
                    timestamp_writes: None,
                });
                pass.set_pipeline($pipeline);
                pass.set_bind_group(0, &$bind_group, &[]);
                let wg_count = ($threads + $wg_size - 1) / $wg_size;
                pass.dispatch_workgroups(wg_count, 1, 1);
            }};
        }

        // 1. grid_clear
        let bg1 = bg!(&pl.bgl_grid_clear, [
            e!(0, bufs.params), e!(1, bufs.grid_counts), e!(2, bufs.iter_state),
        ]);
        dispatch!(enc, &pl.grid_clear, bg1, n_cells, 64);

        // 2. grid_fill
        let bg2 = bg!(&pl.bgl_grid_fill, [
            e!(0, bufs.params), e!(1, bufs.positions), e!(2, bufs.grid_counts),
            e!(3, bufs.grid_beads), e!(4, bufs.iter_state),
        ]);
        dispatch!(enc, &pl.grid_fill, bg2, n, 64);

        // 3. grid_pairs
        let bg3 = bg!(&pl.bgl_grid_pairs, [
            e!(0, bufs.params), e!(1, bufs.positions), e!(2, bufs.grid_counts),
            e!(3, bufs.grid_beads), e!(4, bufs.bonds), e!(5, bufs.iter_state),
            e!(6, bufs.pairs),
        ]);
        dispatch!(enc, &pl.grid_pairs, bg3, n, 64);

        // 4. ccd
        let bg4 = bg!(&pl.bgl_ccd, [
            e!(0, bufs.params), e!(1, bufs.positions), e!(2, bufs.velocities),
            e!(3, bufs.iter_state), e!(4, bufs.pairs), e!(5, bufs.contacts),
        ]);
        dispatch!(enc, &pl.ccd, bg4, max_pairs, 64);

        // 5. reduce_local
        let bg5 = bg!(&pl.bgl_reduce, [
            e!(0, bufs.params), e!(1, bufs.iter_state),
            e!(2, bufs.contacts), e!(3, bufs.reduce_scratch),
        ]);
        dispatch!(enc, &pl.reduce_local, bg5, max_pairs, 256);

        // 6. reduce_global (always 1 workgroup of 256)
        let bg6 = bg!(&pl.bgl_reduce, [
            e!(0, bufs.params), e!(1, bufs.iter_state),
            e!(2, bufs.contacts), e!(3, bufs.reduce_scratch),
        ]);
        {
            let mut pass = enc.begin_compute_pass(&wgpu::ComputePassDescriptor {
                label: None,
                timestamp_writes: None,
            });
            pass.set_pipeline(&pl.reduce_global);
            pass.set_bind_group(0, &bg6, &[]);
            pass.dispatch_workgroups(1, 1, 1);
        }

        // 7. advance_beads
        let bg7 = bg!(&pl.bgl_advance, [
            e!(0, bufs.params), e!(1, bufs.positions), e!(2, bufs.velocities),
            e!(3, bufs.states), e!(4, bufs.contacts), e!(5, bufs.iter_state),
        ]);
        dispatch!(enc, &pl.advance_beads, bg7, n, 64);

        // 8. resolve_contact (1 thread / 1 workgroup)
        let bg8 = bg!(&pl.bgl_resolve, [
            e!(0, bufs.params), e!(1, bufs.positions), e!(2, bufs.velocities),
            e!(3, bufs.states), e!(4, bufs.contacts),
            e!(5, bufs.iter_state), e!(6, bufs.chemistry),
        ]);
        {
            let mut pass = enc.begin_compute_pass(&wgpu::ComputePassDescriptor {
                label: None,
                timestamp_writes: None,
            });
            pass.set_pipeline(&pl.resolve_contact);
            pass.set_bind_group(0, &bg8, &[]);
            pass.dispatch_workgroups(1, 1, 1);
        }
    }
}

impl Scheduler for GpuEventLoop {
    fn step(&mut self, sim: &mut Sim, dt: f32) -> StepMetrics {
        let n = sim.positions.len() as u32;

        // Sync bond set from sim — upload happens in upload_frame regardless.
        self.bonds_sorted = Self::sort_bonds(sim);

        let params = self.make_params(sim, dt);

        // Upload per-frame data.
        self.buffers.upload_frame(
            &self.ctx.queue,
            &sim.positions, &sim.velocities, &sim.states,
            &self.bonds_sorted, params, dt,
        );

        // Batch dispatch loop.
        let mut contacts_resolved: u32 = 0;
        let mut iter_cap_hit = false;
        let iter_cap = (sim.positions.len() as u32) * 64;

        loop {
            let mut enc = self.ctx.device.create_command_encoder(&Default::default());

            for _ in 0..BATCH_SIZE {
                self.encode_iteration(&mut enc, &params);
            }

            // Copy iter_state to readback buffer for status check.
            enc.copy_buffer_to_buffer(
                &self.buffers.iter_state, 0,
                &self.buffers.readback, 0,
                12,
            );

            self.ctx.queue.submit([enc.finish()]);

            // Read status.
            let status = {
                let slice = self.buffers.readback.slice(..12);
                slice.map_async(wgpu::MapMode::Read, |_| {});
                self.ctx.device.poll(wgpu::Maintain::Wait);
                let view = slice.get_mapped_range();
                let words: [u32; 3] = bytemuck::pod_read_unaligned(&*view);
                drop(view);
                self.buffers.readback.unmap();
                words
            };

            contacts_resolved += BATCH_SIZE;
            let flags = status[0];

            if flags & 1 != 0 { break; }          // done flag
            if flags & 2 != 0 { iter_cap_hit = true; break; }  // overflow
            if contacts_resolved >= iter_cap { iter_cap_hit = true; break; }
        }

        // Final readback: positions, velocities, states.
        let n_bytes = (n as u64) * 8;   // vec2f = 8 bytes
        let s_bytes = (n as u64) * 4;   // u32 = 4 bytes

        let mut enc = self.ctx.device.create_command_encoder(&Default::default());
        enc.copy_buffer_to_buffer(&self.buffers.positions, 0, &self.buffers.readback, 0, n_bytes);
        enc.copy_buffer_to_buffer(&self.buffers.velocities, 0, &self.buffers.readback, n_bytes, n_bytes);
        enc.copy_buffer_to_buffer(&self.buffers.states, 0, &self.buffers.readback, n_bytes * 2, s_bytes);
        self.ctx.queue.submit([enc.finish()]);

        let readback_size = n_bytes * 2 + s_bytes;
        {
            let slice = self.buffers.readback.slice(..readback_size);
            slice.map_async(wgpu::MapMode::Read, |_| {});
            self.ctx.device.poll(wgpu::Maintain::Wait);
            let view = slice.get_mapped_range();
            let bytes = &*view;
            let pos: &[Vec2] = bytemuck::cast_slice(&bytes[..n_bytes as usize]);
            let vel: &[Vec2] = bytemuck::cast_slice(&bytes[n_bytes as usize..(n_bytes * 2) as usize]);
            let sts: &[u32]  = bytemuck::cast_slice(&bytes[(n_bytes * 2) as usize..]);
            sim.positions.copy_from_slice(pos);
            sim.velocities.copy_from_slice(vel);
            sim.states.copy_from_slice(sts);
            drop(view);
            self.buffers.readback.unmap();
        }

        // Run enforce_bonds on CPU after readback (handles bond topology drift).
        sim.enforce_bonds();

        StepMetrics {
            contacts_resolved,
            candidate_pairs: 0,
            iter_cap_hit,
        }
    }
}
