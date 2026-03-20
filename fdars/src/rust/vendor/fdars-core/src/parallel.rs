//! Parallel iteration abstraction for WASM compatibility.
//!
//! This module provides conditional parallel/sequential iteration based on
//! the `parallel` feature flag. On native targets with the `parallel` feature,
//! uses rayon for multi-threaded execution. On WASM or without the feature,
//! falls back to sequential iteration.
//!
//! # Usage
//!
//! Use the `iter_maybe_parallel!` macro to conditionally parallelize iteration:
//!
//! ```ignore
//! use crate::parallel::iter_maybe_parallel;
//!
//! let results: Vec<_> = iter_maybe_parallel!((0..n))
//!     .map(|i| expensive_computation(i))
//!     .collect();
//! ```

/// Macro for conditionally parallel iteration over ranges.
///
/// When the `parallel` feature is enabled, uses `into_par_iter()`.
/// Otherwise, uses `into_iter()` for sequential execution.
///
/// # Examples
///
/// ```ignore
/// use crate::iter_maybe_parallel;
///
/// // Range iteration
/// let results: Vec<_> = iter_maybe_parallel!((0..100))
///     .map(|i| i * 2)
///     .collect();
///
/// // Vec iteration (consuming)
/// let vec = vec![1, 2, 3];
/// let results: Vec<_> = iter_maybe_parallel!(vec)
///     .map(|x| x * 2)
///     .collect();
/// ```
#[macro_export]
macro_rules! iter_maybe_parallel {
    ($expr:expr) => {{
        #[cfg(feature = "parallel")]
        {
            use rayon::iter::IntoParallelIterator;

            IntoParallelIterator::into_par_iter($expr)
        }
        #[cfg(not(feature = "parallel"))]
        {
            IntoIterator::into_iter($expr)
        }
    }};
}

/// Macro for conditionally parallel reference iteration over slices.
///
/// When the `parallel` feature is enabled, uses `par_iter()`.
/// Otherwise, uses `iter()` for sequential execution.
#[macro_export]
macro_rules! slice_maybe_parallel {
    ($expr:expr) => {{
        #[cfg(feature = "parallel")]
        {
            use rayon::prelude::*;
            $expr.par_iter()
        }
        #[cfg(not(feature = "parallel"))]
        {
            $expr.iter()
        }
    }};
}

/// Macro for conditionally parallel mutable iteration over slices.
///
/// When the `parallel` feature is enabled, uses `par_iter_mut()`.
/// Otherwise, uses `iter_mut()` for sequential execution.
#[macro_export]
macro_rules! slice_maybe_parallel_mut {
    ($expr:expr) => {{
        #[cfg(feature = "parallel")]
        {
            use rayon::prelude::*;
            $expr.par_iter_mut()
        }
        #[cfg(not(feature = "parallel"))]
        {
            $expr.iter_mut()
        }
    }};
}

/// Macro for parallel/sequential chunks iteration on mutable slices.
///
/// # Example
/// ```ignore
/// use crate::maybe_par_chunks_mut;
///
/// maybe_par_chunks_mut!(data, chunk_size, |chunk| {
///     // process chunk
/// });
/// ```
#[macro_export]
macro_rules! maybe_par_chunks_mut {
    ($slice:expr, $chunk_size:expr, $closure:expr) => {{
        #[cfg(feature = "parallel")]
        {
            use rayon::prelude::*;
            $slice.par_chunks_mut($chunk_size).for_each($closure);
        }
        #[cfg(not(feature = "parallel"))]
        {
            $slice.chunks_mut($chunk_size).for_each($closure);
        }
    }};
}

/// Macro for enumerated parallel/sequential chunks iteration.
///
/// # Example
/// ```ignore
/// use crate::maybe_par_chunks_mut_enumerate;
///
/// maybe_par_chunks_mut_enumerate!(data, chunk_size, |(idx, chunk)| {
///     // process chunk at index idx
/// });
/// ```
#[macro_export]
macro_rules! maybe_par_chunks_mut_enumerate {
    ($slice:expr, $chunk_size:expr, $closure:expr) => {{
        #[cfg(feature = "parallel")]
        {
            use rayon::prelude::*;
            $slice
                .par_chunks_mut($chunk_size)
                .enumerate()
                .for_each($closure);
        }
        #[cfg(not(feature = "parallel"))]
        {
            $slice
                .chunks_mut($chunk_size)
                .enumerate()
                .for_each($closure);
        }
    }};
}

// Re-export macros at module level
pub use iter_maybe_parallel;
pub use maybe_par_chunks_mut;
pub use maybe_par_chunks_mut_enumerate;
pub use slice_maybe_parallel;
pub use slice_maybe_parallel_mut;
