pub mod state;
pub use state::{Bead, STACK_CAP};

pub mod pool;
pub use pool::BeadPool;

pub mod coloring;
pub use coloring::{color_pairs, Pair};

pub mod substep;
pub use substep::compute_active_contacts;

pub mod resolve;
pub use resolve::{resolve_pair, ResolveContext};

pub mod scheduler;
pub use scheduler::{CpuParallel, DEFAULT_DT_SUB};

#[cfg(not(target_arch = "wasm32"))]
pub mod scheduler_mt;
#[cfg(not(target_arch = "wasm32"))]
pub use scheduler_mt::{CpuParallelMt, DEFAULT_DT_SUB as DEFAULT_DT_SUB_MT};
