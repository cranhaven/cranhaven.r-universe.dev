//! # fdars-core
//!
//! Core algorithms for Functional Data Analysis in Rust.
//!
//! This crate provides pure Rust implementations of various FDA methods including:
//! - Functional data operations (mean, derivatives, norms)
//! - Depth measures (Fraiman-Muniz, modal, band, random projection, etc.)
//! - Distance metrics (Lp, Hausdorff, DTW, Fourier, etc.)
//! - Basis representations (B-splines, P-splines, Fourier)
//! - Clustering (k-means, fuzzy c-means)
//! - Smoothing (Nadaraya-Watson, local linear/polynomial regression)
//! - Outlier detection
//! - Regression (PCA, PLS, ridge)
//! - Seasonal analysis (period estimation, peak detection, seasonal strength)
//! - Detrending and decomposition for non-stationary data
//!
//! ## Data Layout
//!
//! Functional data is represented using the [`FdMatrix`] type, a column-major matrix
//! wrapping a flat `Vec<f64>` with safe `(i, j)` indexing and dimension tracking:
//! - For n observations with m evaluation points: `data[(i, j)]` gives observation i at point j
//! - 2D surfaces (n observations, m1 x m2 grid): stored as n x (m1*m2) matrices
//! - Zero-copy column access via `data.column(j)`, row gather via `data.row(i)`
//! - nalgebra interop via `to_dmatrix()` / `from_dmatrix()` for SVD operations

#![allow(clippy::needless_range_loop)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::type_complexity)]

pub mod matrix;
pub mod parallel;

pub mod basis;
pub mod clustering;
pub mod depth;
pub mod detrend;
pub mod fdata;
pub mod helpers;
pub mod irreg_fdata;
pub mod metric;
pub mod outliers;
pub mod regression;
pub mod seasonal;
pub mod simulation;
pub mod smoothing;
pub mod streaming_depth;
pub mod utility;

// Re-export matrix type
pub use matrix::FdMatrix;

// Re-export commonly used items
pub use helpers::{
    extract_curves, l2_distance, simpsons_weights, simpsons_weights_2d, DEFAULT_CONVERGENCE_TOL,
    NUMERICAL_EPS,
};

// Re-export seasonal analysis types
pub use seasonal::{
    autoperiod, autoperiod_fdata, cfd_autoperiod, cfd_autoperiod_fdata, hilbert_transform, sazed,
    sazed_fdata, AutoperiodCandidate, AutoperiodResult, CfdAutoperiodResult, ChangeDetectionResult,
    ChangePoint, ChangeType, DetectedPeriod, InstantaneousPeriod, Peak, PeakDetectionResult,
    PeriodEstimate, SazedComponents, SazedResult, StrengthMethod,
};

// Re-export detrending types
pub use detrend::{DecomposeResult, TrendResult};

// Re-export simulation types
pub use simulation::{EFunType, EValType};

// Re-export irregular fdata types
pub use irreg_fdata::IrregFdata;

// Re-export streaming depth types
pub use streaming_depth::{
    FullReferenceState, RollingReference, SortedReferenceState, StreamingBd, StreamingDepth,
    StreamingFraimanMuniz, StreamingMbd,
};
