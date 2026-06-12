//! Offscreen rendering adapter for golden tests.
//!
//! Mirrors the GPU work of the on-screen `Renderer` but writes to a 256×256
//! RGBA texture and reads back the pixels. No window, no event loop.
//!
//! This is intentionally separate from `Renderer` so the production renderer
//! stays focused. The shared code is the WGSL — we read the same shader
//! files via `include_str!`.

use anyhow::{Context, Result};
use glam::{Mat4, Vec2};
use wgpu::util::DeviceExt;

use crate::render::{BeadGpu, CameraUbo};
use crate::render_mode::RenderMode;

const MAX_STATES: usize = 8;

pub fn render_scene(
    w: u32,
    h: u32,
    mode: RenderMode,
    positions: &[Vec2],
    velocities: &[Vec2],
    states: &[u32],
    selected: &[u32],
    component_ids: &[u32],
) -> Result<Vec<u8>> {
    pollster::block_on(render_scene_async(
        w, h, mode, positions, velocities, states, selected, component_ids,
    ))
}

async fn render_scene_async(
    w: u32,
    h: u32,
    mode: RenderMode,
    positions: &[Vec2],
    velocities: &[Vec2],
    states: &[u32],
    selected: &[u32],
    component_ids: &[u32],
) -> Result<Vec<u8>> {
    let instance = wgpu::Instance::default();
    let adapter = instance
        .request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            compatible_surface: None,
            force_fallback_adapter: false,
        })
        .await
        .ok_or_else(|| anyhow::anyhow!("no adapter"))?;

    let (device, queue) = adapter
        .request_device(
            &wgpu::DeviceDescriptor {
                label: Some("offscreen device"),
                required_features: wgpu::Features::empty(),
                required_limits: wgpu::Limits::default(),
                memory_hints: wgpu::MemoryHints::Performance,
            },
            None,
        )
        .await
        .map_err(|e| anyhow::anyhow!("request_device: {e:?}"))?;

    let format = wgpu::TextureFormat::Rgba8UnormSrgb;
    let target = device.create_texture(&wgpu::TextureDescriptor {
        label: Some("offscreen target"),
        size: wgpu::Extent3d { width: w, height: h, depth_or_array_layers: 1 },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format,
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
        view_formats: &[],
    });
    let target_view = target.create_view(&Default::default());

    // Build bead + camera buffers.
    let gpu_beads: Vec<BeadGpu> = (0..positions.len())
        .map(|i| BeadGpu {
            pos: [positions[i].x, positions[i].y],
            vel: [velocities[i].x, velocities[i].y],
            state: states[i],
            selected: selected[i],
            component_id: component_ids[i],
            _pad: 0,
        })
        .collect();
    let bead_buf = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: Some("beads"),
        contents: bytemuck::cast_slice(&gpu_beads),
        usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_DST,
    });

    // Simple ortho camera: world [-1, 1] × [-h/w, h/w] maps to clip.
    let aspect = h as f32 / w as f32;
    let view_proj = Mat4::orthographic_rh(-1.0, 1.0, -aspect, aspect, -1.0, 1.0);
    let inv_view_proj = view_proj.inverse();
    let palette = [
        [0.92f32, 0.32, 0.32],
        [0.32, 0.55, 0.92],
        [0.32, 0.92, 0.55],
        [0.92, 0.92, 0.32],
        [0.55, 0.32, 0.92],
        [0.92, 0.55, 0.32],
        [0.32, 0.92, 0.92],
        [0.92, 0.32, 0.92],
    ];
    let mut state_colors = [[0.0f32; 4]; MAX_STATES];
    for i in 0..MAX_STATES {
        state_colors[i] = [palette[i][0], palette[i][1], palette[i][2], 1.0];
    }
    let camera_ubo = CameraUbo {
        view_proj: view_proj.to_cols_array_2d(),
        inv_view_proj: inv_view_proj.to_cols_array_2d(),
        radius: 0.22,  // matches the design's R for the canonical test scene
        world_size: 2.0,
        bead_count: positions.len() as u32,
        mode: mode.shader_id(),
        state_colors,
    };
    let camera_buf = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: Some("camera"),
        contents: bytemuck::bytes_of(&camera_ubo),
        usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
    });

    let bind_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
        label: Some("offscreen bind"),
        entries: &[
            wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStages::VERTEX | wgpu::ShaderStages::FRAGMENT,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Uniform,
                    has_dynamic_offset: false,
                    min_binding_size: None,
                },
                count: None,
            },
            wgpu::BindGroupLayoutEntry {
                binding: 1,
                visibility: wgpu::ShaderStages::VERTEX | wgpu::ShaderStages::FRAGMENT,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Storage { read_only: true },
                    has_dynamic_offset: false,
                    min_binding_size: None,
                },
                count: None,
            },
        ],
    });
    let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: Some("offscreen bg"),
        layout: &bind_layout,
        entries: &[
            wgpu::BindGroupEntry { binding: 0, resource: camera_buf.as_entire_binding() },
            wgpu::BindGroupEntry { binding: 1, resource: bead_buf.as_entire_binding() },
        ],
    });

    // Pick the right shader for the mode.
    let shader_src = if mode.is_field() {
        include_str!("../shaders/field.wgsl")
    } else {
        include_str!("../shaders/beads.wgsl")
    };
    let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: Some("offscreen shader"),
        source: wgpu::ShaderSource::Wgsl(shader_src.into()),
    });
    let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
        label: Some("offscreen pipeline layout"),
        bind_group_layouts: &[&bind_layout],
        push_constant_ranges: &[],
    });

    // For the disc shader we also need the unit quad vertex buffer.
    let quad: [[f32; 2]; 6] = [
        [-1.0, -1.0], [ 1.0, -1.0], [ 1.0,  1.0],
        [-1.0, -1.0], [ 1.0,  1.0], [-1.0,  1.0],
    ];
    let quad_vbuf = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: Some("quad"),
        contents: bytemuck::cast_slice(&quad),
        usage: wgpu::BufferUsages::VERTEX,
    });

    let pipeline = if mode.is_field() {
        device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("offscreen field pipeline"),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                buffers: &[],
                compilation_options: Default::default(),
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader,
                entry_point: Some("fs_main"),
                targets: &[Some(wgpu::ColorTargetState {
                    format,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
                compilation_options: Default::default(),
            }),
            primitive: wgpu::PrimitiveState::default(),
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
            multiview: None,
            cache: None,
        })
    } else {
        device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("offscreen disc pipeline"),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                buffers: &[wgpu::VertexBufferLayout {
                    array_stride: 8,
                    step_mode: wgpu::VertexStepMode::Vertex,
                    attributes: &[wgpu::VertexAttribute {
                        offset: 0,
                        shader_location: 0,
                        format: wgpu::VertexFormat::Float32x2,
                    }],
                }],
                compilation_options: Default::default(),
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader,
                entry_point: Some("fs_main"),
                targets: &[Some(wgpu::ColorTargetState {
                    format,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
                compilation_options: Default::default(),
            }),
            primitive: wgpu::PrimitiveState::default(),
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
            multiview: None,
            cache: None,
        })
    };

    // Readback buffer. wgpu requires 256-byte row alignment.
    let bytes_per_pixel: u32 = 4;
    let unpadded_bytes_per_row = w * bytes_per_pixel;
    let align = 256;
    let padded_bytes_per_row = ((unpadded_bytes_per_row + align - 1) / align) * align;
    let buffer_size = (padded_bytes_per_row * h) as u64;
    let readback = device.create_buffer(&wgpu::BufferDescriptor {
        label: Some("readback"),
        size: buffer_size,
        usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
        mapped_at_creation: false,
    });

    // Encode and submit.
    let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
        label: Some("offscreen encoder"),
    });
    {
        let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some("offscreen pass"),
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view: &target_view,
                resolve_target: None,
                ops: wgpu::Operations {
                    load: wgpu::LoadOp::Clear(wgpu::Color { r: 0.05, g: 0.05, b: 0.07, a: 1.0 }),
                    store: wgpu::StoreOp::Store,
                },
            })],
            depth_stencil_attachment: None,
            timestamp_writes: None,
            occlusion_query_set: None,
        });
        pass.set_pipeline(&pipeline);
        pass.set_bind_group(0, &bind_group, &[]);
        if mode.is_field() {
            pass.draw(0..3, 0..1);
        } else {
            pass.set_vertex_buffer(0, quad_vbuf.slice(..));
            pass.draw(0..6, 0..(positions.len() * 9) as u32);
        }
    }
    encoder.copy_texture_to_buffer(
        wgpu::ImageCopyTexture {
            texture: &target,
            mip_level: 0,
            origin: wgpu::Origin3d::ZERO,
            aspect: wgpu::TextureAspect::All,
        },
        wgpu::ImageCopyBuffer {
            buffer: &readback,
            layout: wgpu::ImageDataLayout {
                offset: 0,
                bytes_per_row: Some(padded_bytes_per_row),
                rows_per_image: Some(h),
            },
        },
        wgpu::Extent3d { width: w, height: h, depth_or_array_layers: 1 },
    );
    queue.submit(std::iter::once(encoder.finish()));

    // Map and read.
    let slice = readback.slice(..);
    let (tx, rx) = futures_intrusive::channel::shared::oneshot_channel();
    slice.map_async(wgpu::MapMode::Read, move |r| { let _ = tx.send(r); });
    device.poll(wgpu::Maintain::Wait);
    rx.receive().await.context("readback channel closed")?.context("readback map failed")?;
    let raw = slice.get_mapped_range();

    // Strip row padding.
    let mut out = Vec::with_capacity((w * h * 4) as usize);
    for row in 0..h {
        let start = (row * padded_bytes_per_row) as usize;
        let end = start + (w * 4) as usize;
        out.extend_from_slice(&raw[start..end]);
    }
    Ok(out)
}

/// Canonical 5-bead scene used by the golden tests and render_golden binary.
/// Returns `(positions, velocities, states, selected, component_ids)`.
pub fn canonical_scene() -> (Vec<Vec2>, Vec<Vec2>, Vec<u32>, Vec<u32>, Vec<u32>) {
    use crate::bond::BondPair;
    use crate::component::compute_component_ids;
    let positions = vec![
        Vec2::new(-0.45,  0.0),
        Vec2::new(-0.12,  0.15),
        Vec2::new( 0.20,  0.05),
        Vec2::new( 0.55,  0.30),
        Vec2::new(-0.05, -0.45),
    ];
    let velocities = vec![Vec2::ZERO; positions.len()];
    let states = vec![0u32, 0, 1, 1, 0];
    let selected = vec![0u32; positions.len()];
    let bonds = vec![
        BondPair::new(0, 1), BondPair::new(1, 2), BondPair::new(3, 4),
    ];
    let component_ids = compute_component_ids(positions.len(), &bonds);
    (positions, velocities, states, selected, component_ids)
}
