use std::sync::Arc;

pub struct GpuContext {
    pub device: Arc<wgpu::Device>,
    pub queue: Arc<wgpu::Queue>,
}

impl GpuContext {
    pub fn from_renderer(device: Arc<wgpu::Device>, queue: Arc<wgpu::Queue>) -> Self {
        Self { device, queue }
    }

    /// Create a headless GPU context (no surface). Used by bench and tests.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn new_headless() -> anyhow::Result<Self> {
        let instance = wgpu::Instance::default();
        let adapter = pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            compatible_surface: None,
            force_fallback_adapter: false,
        })).ok_or_else(|| anyhow::anyhow!("no GPU adapter found"))?;

        let (device, queue) = pollster::block_on(adapter.request_device(
            &wgpu::DeviceDescriptor {
                label: Some("jigglefab-headless"),
                required_features: wgpu::Features::empty(),
                required_limits: wgpu::Limits::default(),
                memory_hints: wgpu::MemoryHints::Performance,
            },
            None,
        )).map_err(|e| anyhow::anyhow!("request_device: {e}"))?;

        Ok(Self { device: Arc::new(device), queue: Arc::new(queue) })
    }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;

    #[test]
    #[ignore = "requires GPU"]
    fn headless_context_creates() {
        GpuContext::new_headless().expect("should get a GPU context");
    }
}
