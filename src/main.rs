#[cfg(not(target_arch = "wasm32"))]
fn main() -> anyhow::Result<()> {
    jigglefab::run()
}

#[cfg(target_arch = "wasm32")]
fn main() {}
