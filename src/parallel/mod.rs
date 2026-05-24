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
