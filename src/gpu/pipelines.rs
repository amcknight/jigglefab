use wgpu::*;

pub struct GpuPipelines {
    pub grid_clear:      ComputePipeline,
    pub grid_fill:       ComputePipeline,
    pub grid_pairs:      ComputePipeline,
    pub ccd:             ComputePipeline,
    pub reduce_local:    ComputePipeline,
    pub reduce_global:   ComputePipeline,
    pub advance_beads:   ComputePipeline,
    pub resolve_contact: ComputePipeline,

    pub bgl_grid_clear:  BindGroupLayout,
    pub bgl_grid_fill:   BindGroupLayout,
    pub bgl_grid_pairs:  BindGroupLayout,
    pub bgl_ccd:         BindGroupLayout,
    pub bgl_reduce:      BindGroupLayout,
    pub bgl_advance:     BindGroupLayout,
    pub bgl_resolve:     BindGroupLayout,
}

fn storage_rw() -> BindingType {
    BindingType::Buffer {
        ty: BufferBindingType::Storage { read_only: false },
        has_dynamic_offset: false,
        min_binding_size: None,
    }
}

fn storage_ro() -> BindingType {
    BindingType::Buffer {
        ty: BufferBindingType::Storage { read_only: true },
        has_dynamic_offset: false,
        min_binding_size: None,
    }
}

fn uniform_binding() -> BindingType {
    BindingType::Buffer {
        ty: BufferBindingType::Uniform,
        has_dynamic_offset: false,
        min_binding_size: None,
    }
}

fn make_bgl(device: &Device, label: &str, entries: &[(u32, BindingType)]) -> BindGroupLayout {
    let entries: Vec<_> = entries
        .iter()
        .map(|&(binding, ref ty)| BindGroupLayoutEntry {
            binding,
            visibility: ShaderStages::COMPUTE,
            ty: ty.clone(),
            count: None,
        })
        .collect();
    device.create_bind_group_layout(&BindGroupLayoutDescriptor {
        label: Some(label),
        entries: &entries,
    })
}

fn make_pipeline(
    device: &Device,
    label: &str,
    bgl: &BindGroupLayout,
    module: &ShaderModule,
    entry_point: &str,
) -> ComputePipeline {
    let layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
        label: None,
        bind_group_layouts: &[bgl],
        push_constant_ranges: &[],
    });
    device.create_compute_pipeline(&ComputePipelineDescriptor {
        label: Some(label),
        layout: Some(&layout),
        module,
        entry_point: Some(entry_point),
        compilation_options: Default::default(),
        cache: None,
    })
}

impl GpuPipelines {
    pub fn new(device: &Device) -> Self {
        // ── Bind group layouts ────────────────────────────────────────────────
        // grid_clear: params(uniform), grid_counts(rw), iter_state(rw)
        let bgl_grid_clear = make_bgl(device, "grid_clear", &[
            (0, uniform_binding()), (1, storage_rw()), (2, storage_rw()),
        ]);
        // grid_fill: params, positions(ro), grid_counts(rw), grid_beads(rw), iter_state(rw)
        let bgl_grid_fill = make_bgl(device, "grid_fill", &[
            (0, uniform_binding()), (1, storage_ro()), (2, storage_rw()),
            (3, storage_rw()), (4, storage_rw()),
        ]);
        // grid_pairs: params, positions(ro), grid_counts(ro), grid_beads(ro),
        //             bonds(ro), iter_state(rw), pairs(rw)
        let bgl_grid_pairs = make_bgl(device, "grid_pairs", &[
            (0, uniform_binding()), (1, storage_ro()), (2, storage_ro()),
            (3, storage_ro()), (4, storage_ro()), (5, storage_rw()), (6, storage_rw()),
        ]);
        // ccd: params, positions(ro), velocities(ro), iter_state(ro), pairs(ro), contacts(rw)
        let bgl_ccd = make_bgl(device, "ccd", &[
            (0, uniform_binding()), (1, storage_ro()), (2, storage_ro()),
            (3, storage_ro()), (4, storage_ro()), (5, storage_rw()),
        ]);
        // reduce: params, iter_state(ro), contacts(rw), reduce_scratch(rw)
        let bgl_reduce = make_bgl(device, "reduce", &[
            (0, uniform_binding()), (1, storage_ro()),
            (2, storage_rw()), (3, storage_rw()),
        ]);
        // advance_beads: params, positions(rw), velocities(ro), states(ro),
        //                contacts(ro), iter_state(ro)
        let bgl_advance = make_bgl(device, "advance_beads", &[
            (0, uniform_binding()), (1, storage_rw()), (2, storage_ro()),
            (3, storage_ro()), (4, storage_ro()), (5, storage_ro()),
        ]);
        // resolve_contact: params, positions(rw), velocities(rw), states(rw),
        //                  contacts(ro), iter_state(rw), chemistry(ro)
        let bgl_resolve = make_bgl(device, "resolve_contact", &[
            (0, uniform_binding()), (1, storage_rw()), (2, storage_rw()),
            (3, storage_rw()), (4, storage_ro()), (5, storage_rw()), (6, storage_ro()),
        ]);

        // ── Shaders ───────────────────────────────────────────────────────────
        let load = |label: &str, src: &str| {
            device.create_shader_module(ShaderModuleDescriptor {
                label: Some(label),
                source: ShaderSource::Wgsl(src.into()),
            })
        };

        let sh_grid_clear      = load("grid_clear",      include_str!("../../shaders/grid_clear.wgsl"));
        let sh_grid_fill       = load("grid_fill",       include_str!("../../shaders/grid_fill.wgsl"));
        let sh_grid_pairs      = load("grid_pairs",      include_str!("../../shaders/grid_pairs.wgsl"));
        let sh_ccd             = load("ccd",             include_str!("../../shaders/ccd.wgsl"));
        let sh_reduce          = load("reduce",          include_str!("../../shaders/reduce.wgsl"));
        let sh_advance_resolve = load("advance_resolve", include_str!("../../shaders/advance_resolve.wgsl"));

        // ── Pipelines ─────────────────────────────────────────────────────────
        let grid_clear      = make_pipeline(device, "grid_clear",      &bgl_grid_clear,  &sh_grid_clear,      "main");
        let grid_fill       = make_pipeline(device, "grid_fill",       &bgl_grid_fill,   &sh_grid_fill,       "main");
        let grid_pairs      = make_pipeline(device, "grid_pairs",      &bgl_grid_pairs,  &sh_grid_pairs,      "main");
        let ccd             = make_pipeline(device, "ccd",             &bgl_ccd,         &sh_ccd,             "main");
        let reduce_local    = make_pipeline(device, "reduce_local",    &bgl_reduce,      &sh_reduce,          "reduce_local");
        let reduce_global   = make_pipeline(device, "reduce_global",   &bgl_reduce,      &sh_reduce,          "reduce_global");
        let advance_beads   = make_pipeline(device, "advance_beads",   &bgl_advance,     &sh_advance_resolve, "advance_beads");
        let resolve_contact = make_pipeline(device, "resolve_contact", &bgl_resolve,     &sh_advance_resolve, "resolve_contact");

        Self {
            grid_clear, grid_fill, grid_pairs, ccd,
            reduce_local, reduce_global, advance_beads, resolve_contact,
            bgl_grid_clear, bgl_grid_fill, bgl_grid_pairs, bgl_ccd,
            bgl_reduce, bgl_advance, bgl_resolve,
        }
    }
}
