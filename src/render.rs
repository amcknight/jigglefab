use std::sync::Arc;
use winit::window::Window;
use anyhow::Result;
use bytemuck::{Pod, Zeroable};
use glam::{Mat4, Vec2};
use wgpu::util::DeviceExt;

#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
struct BeadGpu {
    pos: [f32; 2],
    _pad: [f32; 2],
}

#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
struct CameraUbo {
    view_proj: [[f32; 4]; 4],
    radius: f32,
    _pad: [f32; 3],
}

pub struct Renderer {
    pub surface: wgpu::Surface<'static>,
    pub device: wgpu::Device,
    pub queue: wgpu::Queue,
    pub config: wgpu::SurfaceConfiguration,
    pub size: winit::dpi::PhysicalSize<u32>,
    pipeline: wgpu::RenderPipeline,
    quad_vbuf: wgpu::Buffer,
    bead_buf: wgpu::Buffer,
    bead_capacity: usize,
    camera_buf: wgpu::Buffer,
    bind_group: wgpu::BindGroup,
    bind_layout: wgpu::BindGroupLayout,
}

impl Renderer {
    pub async fn new(window: Arc<Window>, initial_bead_count: usize) -> Result<Self> {
        let size = window.inner_size();
        let instance = wgpu::Instance::default();
        let surface = instance.create_surface(window.clone())?;
        let adapter = instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            compatible_surface: Some(&surface),
            force_fallback_adapter: false,
        }).await.ok_or_else(|| anyhow::anyhow!("no adapter found"))?;

        let (device, queue) = adapter.request_device(&wgpu::DeviceDescriptor {
            label: Some("jigglefab device"),
            required_features: wgpu::Features::empty(),
            required_limits: wgpu::Limits::default(),
            memory_hints: wgpu::MemoryHints::Performance,
        }, None).await.map_err(|e| anyhow::anyhow!("request_device failed: {e:?}"))?;

        let surface_caps = surface.get_capabilities(&adapter);
        let format = surface_caps.formats.iter().copied()
            .find(|f| f.is_srgb()).unwrap_or(surface_caps.formats[0]);

        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format,
            width: size.width.max(1),
            height: size.height.max(1),
            present_mode: wgpu::PresentMode::Fifo,
            alpha_mode: surface_caps.alpha_modes[0],
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        };
        surface.configure(&device, &config);

        // Unit quad as 6 vertices (two triangles).
        let quad: [[f32; 2]; 6] = [
            [-1.0, -1.0], [ 1.0, -1.0], [ 1.0,  1.0],
            [-1.0, -1.0], [ 1.0,  1.0], [-1.0,  1.0],
        ];
        let quad_vbuf = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("quad"),
            contents: bytemuck::cast_slice(&quad),
            usage: wgpu::BufferUsages::VERTEX,
        });

        let bead_capacity = initial_bead_count.max(1);
        let bead_buf = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("beads"),
            size: (bead_capacity * std::mem::size_of::<BeadGpu>()) as u64,
            usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        let camera_buf = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("camera"),
            size: std::mem::size_of::<CameraUbo>() as u64,
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        let bind_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("beads bind"),
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
                    visibility: wgpu::ShaderStages::VERTEX,
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
            label: Some("beads bg"),
            layout: &bind_layout,
            entries: &[
                wgpu::BindGroupEntry { binding: 0, resource: camera_buf.as_entire_binding() },
                wgpu::BindGroupEntry { binding: 1, resource: bead_buf.as_entire_binding() },
            ],
        });

        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("beads"),
            source: wgpu::ShaderSource::Wgsl(include_str!("../shaders/beads.wgsl").into()),
        });

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("beads layout"),
            bind_group_layouts: &[&bind_layout],
            push_constant_ranges: &[],
        });

        let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("beads pipeline"),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: "vs_main",
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
                entry_point: "fs_main",
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
        });

        Ok(Self {
            surface, device, queue, config, size,
            pipeline, quad_vbuf, bead_buf, bead_capacity, camera_buf, bind_group, bind_layout,
        })
    }

    pub fn resize(&mut self, new_size: winit::dpi::PhysicalSize<u32>) {
        if new_size.width > 0 && new_size.height > 0 {
            self.size = new_size;
            self.config.width = new_size.width;
            self.config.height = new_size.height;
            self.surface.configure(&self.device, &self.config);
        }
    }

    pub fn update_beads(&mut self, positions: &[Vec2]) {
        // Re-allocate the storage buffer if it's too small.
        if positions.len() > self.bead_capacity {
            self.bead_capacity = positions.len().next_power_of_two();
            self.bead_buf = self.device.create_buffer(&wgpu::BufferDescriptor {
                label: Some("beads"),
                size: (self.bead_capacity * std::mem::size_of::<BeadGpu>()) as u64,
                usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_DST,
                mapped_at_creation: false,
            });
            self.bind_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: Some("beads bg"),
                layout: &self.bind_layout,
                entries: &[
                    wgpu::BindGroupEntry { binding: 0, resource: self.camera_buf.as_entire_binding() },
                    wgpu::BindGroupEntry { binding: 1, resource: self.bead_buf.as_entire_binding() },
                ],
            });
        }
        let gpu_beads: Vec<BeadGpu> = positions.iter()
            .map(|p| BeadGpu { pos: [p.x, p.y], _pad: [0.0; 2] })
            .collect();
        self.queue.write_buffer(&self.bead_buf, 0, bytemuck::cast_slice(&gpu_beads));
    }

    pub fn update_camera(&mut self, world_size: f32) {
        // Orthographic projection covering the whole world, square, centered.
        let aspect = self.size.width as f32 / self.size.height as f32;
        let (w, h) = if aspect >= 1.0 {
            (world_size * aspect, world_size)
        } else {
            (world_size, world_size / aspect)
        };
        let proj = Mat4::orthographic_rh(0.0, w, 0.0, h, -1.0, 1.0);
        // Center the world inside the view if aspect > 1.
        let offset_x = (w - world_size) * 0.5;
        let offset_y = (h - world_size) * 0.5;
        let view = Mat4::from_translation(glam::Vec3::new(offset_x, offset_y, 0.0));
        let vp = proj * view;
        let ubo = CameraUbo {
            view_proj: vp.to_cols_array_2d(),
            radius: crate::ccd::RADIUS,
            _pad: [0.0; 3],
        };
        self.queue.write_buffer(&self.camera_buf, 0, bytemuck::bytes_of(&ubo));
    }

    pub fn render(&self, bead_count: usize) -> Result<()> {
        let frame = self.surface.get_current_texture()?;
        let view = frame.texture.create_view(&Default::default());
        let mut encoder = self.device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: Some("bead encoder"),
        });
        {
            let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("bead pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &view,
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
            pass.set_pipeline(&self.pipeline);
            pass.set_bind_group(0, &self.bind_group, &[]);
            pass.set_vertex_buffer(0, self.quad_vbuf.slice(..));
            pass.draw(0..6, 0..bead_count as u32);
        }
        self.queue.submit(std::iter::once(encoder.finish()));
        frame.present();
        Ok(())
    }
}
