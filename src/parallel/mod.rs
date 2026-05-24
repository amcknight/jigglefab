pub mod state;
pub use state::{Bead, STACK_CAP};

pub mod pool;
pub use pool::BeadPool;

pub mod coloring;
pub use coloring::{color_pairs, Pair};
