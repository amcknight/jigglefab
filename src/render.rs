use std::sync::Arc;
use winit::window::Window;
use anyhow::Result;
use bytemuck::{Pod, Zeroable};
use glam::Vec2;
use wgpu::util::DeviceExt;

#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
struct BeadGpu {
    pos: [f32; 2],
    state: u32,
    selected: u32,
}

#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
pub struct OverlayVertex {
    pub pos: [f32; 2],
    pub shade: f32,
}

// Maximum number of chemistry states the renderer can colour. Today's
// chemistries (grey: 1, wire: 2) use a tiny fraction; the upper bound is fixed
// here so the UBO has a stable layout the shader can index.
const MAX_STATES: usize = 8;

#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
struct CameraUbo {
    view_proj: [[f32; 4]; 4],
    radius: f32,
    world_size: f32,
    _pad: [f32; 2],
    // vec4 per state for std140 alignment. .rgb is the colour; .a unused.
    state_colors: [[f32; 4]; MAX_STATES],
}

pub struct Renderer {
    pub surface: wgpu::Surface<'static>,
    pub device: Arc<wgpu::Device>,
    pub queue: Arc<wgpu::Queue>,
    pub config: wgpu::SurfaceConfiguration,
    pub size: winit::dpi::PhysicalSize<u32>,
    pipeline: wgpu::RenderPipeline,
    quad_vbuf: wgpu::Buffer,
    bead_buf: wgpu::Buffer,
    bead_capacity: usize,
    camera_buf: wgpu::Buffer,
    bind_group: wgpu::BindGroup,
    bind_layout: wgpu::BindGroupLayout,
    overlay_pipeline: wgpu::RenderPipeline,
    overlay_buf: wgpu::Buffer,
    overlay_capacity: usize,
    overlay_vertex_count: u32,
    overlay_bind_group: wgpu::BindGroup,
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

        let (device_raw, queue_raw) = adapter.request_device(&wgpu::DeviceDescriptor {
            label: Some("jigglefab device"),
            required_features: wgpu::Features::empty(),
            required_limits: wgpu::Limits::default(),
            memory_hints: wgpu::MemoryHints::Performance,
        }, None).await.map_err(|e| anyhow::anyhow!("request_device failed: {e:?}"))?;
        let device = Arc::new(device_raw);
        let queue = Arc::new(queue_raw);

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
        });

        let overlay_capacity: usize = 1024;
        let overlay_buf = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("overlay verts"),
            size: (overlay_capacity * std::mem::size_of::<OverlayVertex>()) as u64,
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        let overlay_bind_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("overlay bind"),
            entries: &[
                wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::VERTEX,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
            ],
        });
        let overlay_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("overlay bg"),
            layout: &overlay_bind_layout,
            entries: &[
                wgpu::BindGroupEntry { binding: 0, resource: camera_buf.as_entire_binding() },
            ],
        });
        let overlay_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("overlay"),
            source: wgpu::ShaderSource::Wgsl(include_str!("../shaders/overlay.wgsl").into()),
        });
        let overlay_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("overlay layout"),
            bind_group_layouts: &[&overlay_bind_layout],
            push_constant_ranges: &[],
        });
        let overlay_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("overlay pipeline"),
            layout: Some(&overlay_pipeline_layout),
            vertex: wgpu::VertexState {
                module: &overlay_shader,
                entry_point: Some("vs_main"),
                buffers: &[wgpu::VertexBufferLayout {
                    array_stride: std::mem::size_of::<OverlayVertex>() as u64,
                    step_mode: wgpu::VertexStepMode::Vertex,
                    attributes: &[
                        wgpu::VertexAttribute { offset: 0, shader_location: 0, format: wgpu::VertexFormat::Float32x2 },
                        wgpu::VertexAttribute { offset: 8, shader_location: 1, format: wgpu::VertexFormat::Float32 },
                    ],
                }],
                compilation_options: Default::default(),
            },
            fragment: Some(wgpu::FragmentState {
                module: &overlay_shader,
                entry_point: Some("fs_main"),
                targets: &[Some(wgpu::ColorTargetState {
                    format,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
                compilation_options: Default::default(),
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::LineList,
                ..Default::default()
            },
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
            multiview: None,
            cache: None,
        });

        Ok(Self {
            surface, device, queue, config, size,
            pipeline, quad_vbuf, bead_buf, bead_capacity, camera_buf, bind_group, bind_layout,
            overlay_pipeline,
            overlay_buf,
            overlay_capacity,
            overlay_vertex_count: 0,
            overlay_bind_group,
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

    pub fn update_beads(&mut self, positions: &[Vec2], states: &[u32], selected: &[u32]) {
        debug_assert_eq!(positions.len(), states.len());
        debug_assert_eq!(positions.len(), selected.len());
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
        let gpu_beads: Vec<BeadGpu> = positions.iter().zip(states.iter()).zip(selected.iter())
            .map(|((p, &s), &sel)| BeadGpu { pos: [p.x, p.y], state: s, selected: sel })
            .collect();
        self.queue.write_buffer(&self.bead_buf, 0, bytemuck::cast_slice(&gpu_beads));
    }

    pub fn update_camera(&mut self, camera: &crate::camera::Camera, world_size: f32, palette: &[[f32; 3]]) {
        let vp = camera.view_proj((self.size.width, self.size.height), world_size);
        let mut state_colors = [[0.0f32, 0.0, 0.0, 1.0]; MAX_STATES];
        for (i, slot) in state_colors.iter_mut().enumerate() {
            // Cycle through the palette if there are more states than entries,
            // but in practice we expect `palette.len() <= MAX_STATES`.
            if !palette.is_empty() {
                let c = palette[i % palette.len()];
                *slot = [c[0], c[1], c[2], 1.0];
            }
        }
        let ubo = CameraUbo {
            view_proj: vp.to_cols_array_2d(),
            radius: crate::ccd::RADIUS,
            world_size,
            _pad: [0.0; 2],
            state_colors,
        };
        self.queue.write_buffer(&self.camera_buf, 0, bytemuck::bytes_of(&ubo));
    }

    pub fn gpu_context(&self) -> (Arc<wgpu::Device>, Arc<wgpu::Queue>) {
        (self.device.clone(), self.queue.clone())
    }

    /// Upload a polyline of world-space vertex pairs. Each consecutive pair of
    /// vertices defines one line segment (LineList topology). Pass an empty
    /// slice to hide the overlay this frame.
    pub fn update_overlay(&mut self, segments: &[OverlayVertex]) {
        let count = segments.len().min(self.overlay_capacity) as u32;
        self.overlay_vertex_count = count;
        if count == 0 { return; }
        self.queue.write_buffer(
            &self.overlay_buf,
            0,
            bytemuck::cast_slice(&segments[..count as usize]),
        );
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
            // Each bead is drawn 9 times: once at its position and 8 wrap-ghost
            // copies at ±world_size offsets. Off-screen ghosts get clipped by
            // the rasterizer for free. This makes bonds across the torus seam
            // visible — without it, a chain straddling x=0 looks broken.
            pass.draw(0..6, 0..(bead_count * 9) as u32);
        }
        if self.overlay_vertex_count > 0 {
            let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("overlay pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Load,
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });
            pass.set_pipeline(&self.overlay_pipeline);
            pass.set_bind_group(0, &self.overlay_bind_group, &[]);
            pass.set_vertex_buffer(0, self.overlay_buf.slice(..));
            pass.draw(0..self.overlay_vertex_count, 0..1);
        }
        self.queue.submit(std::iter::once(encoder.finish()));
        frame.present();
        Ok(())
    }
}
