//! Basis representation functions for functional data.
//!
//! This module provides B-spline and Fourier basis expansions for representing
//! functional data in a finite-dimensional basis.

use crate::iter_maybe_parallel;
use crate::matrix::FdMatrix;
use nalgebra::{DMatrix, DVector, SVD};
#[cfg(feature = "parallel")]
use rayon::iter::ParallelIterator;
use std::f64::consts::PI;

/// Compute pseudoinverse of a symmetric matrix via SVD.
///
/// Uses singular value decomposition with threshold-based truncation
/// for numerical stability with near-singular matrices.
fn svd_pseudoinverse(mat: &DMatrix<f64>) -> Option<DMatrix<f64>> {
    let n = mat.nrows();
    let svd = SVD::new(mat.clone(), true, true);
    let max_sv = svd.singular_values.iter().cloned().fold(0.0_f64, f64::max);
    let eps = 1e-10 * max_sv;

    let u = svd.u.as_ref()?;
    let v_t = svd.v_t.as_ref()?;

    let s_inv: Vec<f64> = svd
        .singular_values
        .iter()
        .map(|&s| if s > eps { 1.0 / s } else { 0.0 })
        .collect();

    let mut result = DMatrix::zeros(n, n);
    for i in 0..n {
        for j in 0..n {
            let mut sum = 0.0;
            for k in 0..n.min(s_inv.len()) {
                sum += v_t[(k, i)] * s_inv[k] * u[(j, k)];
            }
            result[(i, j)] = sum;
        }
    }

    Some(result)
}

/// Compute model selection criterion (GCV, AIC, or BIC).
///
/// # Arguments
/// * `rss` - Residual sum of squares
/// * `n_points` - Number of data points
/// * `edf` - Effective degrees of freedom
/// * `criterion` - 0=GCV, 1=AIC, 2=BIC
fn compute_model_criterion(rss: f64, n_points: f64, edf: f64, criterion: i32) -> f64 {
    match criterion {
        0 => {
            let gcv_denom = 1.0 - edf / n_points;
            if gcv_denom.abs() > 1e-10 {
                (rss / n_points) / (gcv_denom * gcv_denom)
            } else {
                f64::INFINITY
            }
        }
        1 => {
            let mse = rss / n_points;
            n_points * mse.ln() + 2.0 * edf
        }
        _ => {
            let mse = rss / n_points;
            n_points * mse.ln() + n_points.ln() * edf
        }
    }
}

/// Construct B-spline knot vector with extended boundary knots.
fn construct_bspline_knots(t_min: f64, t_max: f64, nknots: usize, order: usize) -> Vec<f64> {
    let dt = (t_max - t_min) / (nknots - 1) as f64;
    let mut knots = Vec::with_capacity(nknots + 2 * order);
    for i in 0..order {
        knots.push(t_min - (order - i) as f64 * dt);
    }
    for i in 0..nknots {
        knots.push(t_min + i as f64 * dt);
    }
    for i in 1..=order {
        knots.push(t_max + i as f64 * dt);
    }
    knots
}

/// Evaluate order-zero B-spline basis at a single point.
fn evaluate_order_zero(t_val: f64, knots: &[f64], t_max_knot_idx: usize) -> Vec<f64> {
    let mut b0 = vec![0.0; knots.len() - 1];
    for j in 0..(knots.len() - 1) {
        let in_interval = if j == t_max_knot_idx - 1 {
            t_val >= knots[j] && t_val <= knots[j + 1]
        } else {
            t_val >= knots[j] && t_val < knots[j + 1]
        };
        if in_interval {
            b0[j] = 1.0;
            break;
        }
    }
    b0
}

/// Compute one order of B-spline recurrence from the previous order.
fn bspline_recurrence_step(b: &[f64], knots: &[f64], t_val: f64, k: usize) -> Vec<f64> {
    (0..(knots.len() - k))
        .map(|j| {
            let d1 = knots[j + k - 1] - knots[j];
            let d2 = knots[j + k] - knots[j + 1];
            let left = if d1.abs() > 1e-10 {
                (t_val - knots[j]) / d1 * b[j]
            } else {
                0.0
            };
            let right = if d2.abs() > 1e-10 {
                (knots[j + k] - t_val) / d2 * b[j + 1]
            } else {
                0.0
            };
            left + right
        })
        .collect()
}

/// Compute B-spline basis matrix for given knots and grid points.
///
/// Creates a B-spline basis with uniformly spaced knots extended beyond the data range.
/// For order k and nknots interior knots, produces nknots + order basis functions.
pub fn bspline_basis(t: &[f64], nknots: usize, order: usize) -> Vec<f64> {
    let n = t.len();
    let nbasis = nknots + order;

    let t_min = t.iter().cloned().fold(f64::INFINITY, f64::min);
    let t_max = t.iter().cloned().fold(f64::NEG_INFINITY, f64::max);

    let knots = construct_bspline_knots(t_min, t_max, nknots, order);
    let t_max_knot_idx = order + nknots - 1;

    let mut basis = vec![0.0; n * nbasis];

    for (ti, &t_val) in t.iter().enumerate() {
        let mut b = evaluate_order_zero(t_val, &knots, t_max_knot_idx);

        for k in 2..=order {
            b = bspline_recurrence_step(&b, &knots, t_val, k);
        }

        for j in 0..nbasis {
            basis[ti + j * n] = b[j];
        }
    }

    basis
}

/// Compute Fourier basis matrix.
///
/// The period is automatically set to the range of evaluation points (t_max - t_min).
/// For explicit period control, use `fourier_basis_with_period`.
pub fn fourier_basis(t: &[f64], nbasis: usize) -> Vec<f64> {
    let t_min = t.iter().cloned().fold(f64::INFINITY, f64::min);
    let t_max = t.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    let period = t_max - t_min;
    fourier_basis_with_period(t, nbasis, period)
}

/// Compute Fourier basis matrix with explicit period.
///
/// This function creates a Fourier basis expansion where the period can be specified
/// independently of the evaluation range. This is essential for seasonal analysis
/// where the seasonal period may differ from the observation window.
///
/// # Arguments
/// * `t` - Evaluation points
/// * `nbasis` - Number of basis functions (1 constant + pairs of sin/cos)
/// * `period` - The period for the Fourier basis
///
/// # Returns
/// Column-major matrix (n_points x nbasis) stored as flat vector
pub fn fourier_basis_with_period(t: &[f64], nbasis: usize, period: f64) -> Vec<f64> {
    let n = t.len();
    let t_min = t.iter().cloned().fold(f64::INFINITY, f64::min);

    let mut basis = vec![0.0; n * nbasis];

    for (i, &ti) in t.iter().enumerate() {
        let x = 2.0 * PI * (ti - t_min) / period;

        basis[i] = 1.0;

        let mut k = 1;
        let mut freq = 1;
        while k < nbasis {
            if k < nbasis {
                basis[i + k * n] = (freq as f64 * x).sin();
                k += 1;
            }
            if k < nbasis {
                basis[i + k * n] = (freq as f64 * x).cos();
                k += 1;
            }
            freq += 1;
        }
    }

    basis
}

/// Compute difference matrix for P-spline penalty.
pub fn difference_matrix(n: usize, order: usize) -> DMatrix<f64> {
    if order == 0 {
        return DMatrix::identity(n, n);
    }

    let mut d = DMatrix::zeros(n - 1, n);
    for i in 0..(n - 1) {
        d[(i, i)] = -1.0;
        d[(i, i + 1)] = 1.0;
    }

    let mut result = d;
    for _ in 1..order {
        if result.nrows() <= 1 {
            break;
        }
        let rows = result.nrows() - 1;
        let cols = result.ncols();
        let mut d_next = DMatrix::zeros(rows, cols);
        for i in 0..rows {
            for j in 0..cols {
                d_next[(i, j)] = -result[(i, j)] + result[(i + 1, j)];
            }
        }
        result = d_next;
    }

    result
}

/// Result of basis projection.
pub struct BasisProjectionResult {
    /// Coefficient matrix (n_samples x n_basis)
    pub coefficients: FdMatrix,
    /// Number of basis functions used
    pub n_basis: usize,
}

/// Project functional data to basis coefficients.
///
/// # Arguments
/// * `data` - Column-major FdMatrix (n x m)
/// * `argvals` - Evaluation points
/// * `nbasis` - Number of basis functions
/// * `basis_type` - 0 = B-spline, 1 = Fourier
pub fn fdata_to_basis_1d(
    data: &FdMatrix,
    argvals: &[f64],
    nbasis: usize,
    basis_type: i32,
) -> Option<BasisProjectionResult> {
    let n = data.nrows();
    let m = data.ncols();
    if n == 0 || m == 0 || argvals.len() != m || nbasis < 2 {
        return None;
    }

    let basis = if basis_type == 1 {
        fourier_basis(argvals, nbasis)
    } else {
        // For order 4 B-splines: nbasis = nknots + order, so nknots = nbasis - 4
        bspline_basis(argvals, nbasis.saturating_sub(4).max(2), 4)
    };

    let actual_nbasis = basis.len() / m;
    let b_mat = DMatrix::from_column_slice(m, actual_nbasis, &basis);

    let btb = &b_mat.transpose() * &b_mat;
    let btb_inv = svd_pseudoinverse(&btb)?;
    let proj = btb_inv * b_mat.transpose();

    let coefs: Vec<f64> = iter_maybe_parallel!(0..n)
        .flat_map(|i| {
            let curve: Vec<f64> = (0..m).map(|j| data[(i, j)]).collect();
            (0..actual_nbasis)
                .map(|k| {
                    let mut sum = 0.0;
                    for j in 0..m {
                        sum += proj[(k, j)] * curve[j];
                    }
                    sum
                })
                .collect::<Vec<_>>()
        })
        .collect();

    Some(BasisProjectionResult {
        coefficients: FdMatrix::from_column_major(coefs, n, actual_nbasis).unwrap(),
        n_basis: actual_nbasis,
    })
}

/// Reconstruct functional data from basis coefficients.
pub fn basis_to_fdata_1d(
    coefs: &FdMatrix,
    argvals: &[f64],
    nbasis: usize,
    basis_type: i32,
) -> FdMatrix {
    let n = coefs.nrows();
    let coefs_ncols = coefs.ncols();
    let m = argvals.len();
    if n == 0 || m == 0 || nbasis < 2 {
        return FdMatrix::zeros(0, 0);
    }

    let basis = if basis_type == 1 {
        fourier_basis(argvals, nbasis)
    } else {
        // For order 4 B-splines: nbasis = nknots + order, so nknots = nbasis - 4
        bspline_basis(argvals, nbasis.saturating_sub(4).max(2), 4)
    };

    let actual_nbasis = basis.len() / m;

    let flat: Vec<f64> = iter_maybe_parallel!(0..n)
        .flat_map(|i| {
            (0..m)
                .map(|j| {
                    let mut sum = 0.0;
                    for k in 0..actual_nbasis.min(coefs_ncols) {
                        sum += coefs[(i, k)] * basis[j + k * m];
                    }
                    sum
                })
                .collect::<Vec<_>>()
        })
        .collect();

    FdMatrix::from_column_major(flat, n, m).unwrap()
}

/// Result of P-spline fitting.
pub struct PsplineFitResult {
    /// Coefficient matrix (n x nbasis)
    pub coefficients: FdMatrix,
    /// Fitted values (n x m)
    pub fitted: FdMatrix,
    /// Effective degrees of freedom
    pub edf: f64,
    /// Residual sum of squares
    pub rss: f64,
    /// GCV score
    pub gcv: f64,
    /// AIC
    pub aic: f64,
    /// BIC
    pub bic: f64,
    /// Number of basis functions
    pub n_basis: usize,
}

/// Fit P-splines to functional data.
pub fn pspline_fit_1d(
    data: &FdMatrix,
    argvals: &[f64],
    nbasis: usize,
    lambda: f64,
    order: usize,
) -> Option<PsplineFitResult> {
    let n = data.nrows();
    let m = data.ncols();
    if n == 0 || m == 0 || nbasis < 2 || argvals.len() != m {
        return None;
    }

    // For order 4 B-splines: nbasis = nknots + order, so nknots = nbasis - 4
    let basis = bspline_basis(argvals, nbasis.saturating_sub(4).max(2), 4);
    let actual_nbasis = basis.len() / m;
    let b_mat = DMatrix::from_column_slice(m, actual_nbasis, &basis);

    let d = difference_matrix(actual_nbasis, order);
    let penalty = &d.transpose() * &d;

    let btb = &b_mat.transpose() * &b_mat;
    let btb_penalized = &btb + lambda * &penalty;

    let btb_inv = svd_pseudoinverse(&btb_penalized)?;
    let proj = &btb_inv * b_mat.transpose();
    let h_mat = &b_mat * &proj;
    let edf: f64 = (0..m).map(|i| h_mat[(i, i)]).sum();

    let mut all_coefs = FdMatrix::zeros(n, actual_nbasis);
    let mut all_fitted = FdMatrix::zeros(n, m);
    let mut total_rss = 0.0;

    for i in 0..n {
        let curve: Vec<f64> = (0..m).map(|j| data[(i, j)]).collect();
        let curve_vec = DVector::from_vec(curve.clone());

        let bt_y = b_mat.transpose() * &curve_vec;
        let coefs = &btb_inv * bt_y;

        for k in 0..actual_nbasis {
            all_coefs[(i, k)] = coefs[k];
        }

        let fitted = &b_mat * &coefs;
        for j in 0..m {
            all_fitted[(i, j)] = fitted[j];
            let resid = curve[j] - fitted[j];
            total_rss += resid * resid;
        }
    }

    let total_points = (n * m) as f64;

    let gcv_denom = 1.0 - edf / m as f64;
    let gcv = if gcv_denom.abs() > 1e-10 {
        (total_rss / total_points) / (gcv_denom * gcv_denom)
    } else {
        f64::INFINITY
    };

    let mse = total_rss / total_points;
    let aic = total_points * mse.ln() + 2.0 * edf;
    let bic = total_points * mse.ln() + total_points.ln() * edf;

    Some(PsplineFitResult {
        coefficients: all_coefs,
        fitted: all_fitted,
        edf,
        rss: total_rss,
        gcv,
        aic,
        bic,
        n_basis: actual_nbasis,
    })
}

/// Result of Fourier basis fitting.
pub struct FourierFitResult {
    /// Coefficient matrix (n x nbasis)
    pub coefficients: FdMatrix,
    /// Fitted values (n x m)
    pub fitted: FdMatrix,
    /// Effective degrees of freedom (equals nbasis for unpenalized fit)
    pub edf: f64,
    /// Residual sum of squares
    pub rss: f64,
    /// GCV score
    pub gcv: f64,
    /// AIC
    pub aic: f64,
    /// BIC
    pub bic: f64,
    /// Number of basis functions
    pub n_basis: usize,
}

/// Compute GCV, AIC, and BIC model selection criteria.
fn compute_fit_criteria(total_rss: f64, total_points: f64, edf: f64, m: usize) -> (f64, f64, f64) {
    let gcv_denom = 1.0 - edf / m as f64;
    let gcv = if gcv_denom.abs() > 1e-10 {
        (total_rss / total_points) / (gcv_denom * gcv_denom)
    } else {
        f64::INFINITY
    };
    let mse = total_rss / total_points;
    let aic = total_points * mse.ln() + 2.0 * edf;
    let bic = total_points * mse.ln() + total_points.ln() * edf;
    (gcv, aic, bic)
}

/// Fit Fourier basis to functional data using least squares.
///
/// Projects data onto Fourier basis and reconstructs fitted values.
/// Unlike P-splines, this uses unpenalized least squares projection.
///
/// # Arguments
/// * `data` - Column-major FdMatrix (n x m)
/// * `argvals` - Evaluation points
/// * `nbasis` - Number of Fourier basis functions (should be odd: 1 constant + pairs of sin/cos)
///
/// # Returns
/// FourierFitResult with coefficients, fitted values, and model selection criteria
pub fn fourier_fit_1d(data: &FdMatrix, argvals: &[f64], nbasis: usize) -> Option<FourierFitResult> {
    let n = data.nrows();
    let m = data.ncols();
    if n == 0 || m == 0 || nbasis < 3 || argvals.len() != m {
        return None;
    }

    // Ensure nbasis is odd (1 constant + pairs of sin/cos)
    let nbasis = if nbasis % 2 == 0 { nbasis + 1 } else { nbasis };

    let basis = fourier_basis(argvals, nbasis);
    let actual_nbasis = basis.len() / m;
    let b_mat = DMatrix::from_column_slice(m, actual_nbasis, &basis);

    let btb = &b_mat.transpose() * &b_mat;
    let btb_inv = svd_pseudoinverse(&btb)?;
    let proj = &btb_inv * b_mat.transpose();
    let h_mat = &b_mat * &proj;
    let edf: f64 = (0..m).map(|i| h_mat[(i, i)]).sum();

    let mut all_coefs = FdMatrix::zeros(n, actual_nbasis);
    let mut all_fitted = FdMatrix::zeros(n, m);
    let mut total_rss = 0.0;

    for i in 0..n {
        let curve: Vec<f64> = (0..m).map(|j| data[(i, j)]).collect();
        let curve_vec = DVector::from_vec(curve.clone());

        let bt_y = b_mat.transpose() * &curve_vec;
        let coefs = &btb_inv * bt_y;

        for k in 0..actual_nbasis {
            all_coefs[(i, k)] = coefs[k];
        }

        let fitted = &b_mat * &coefs;
        for j in 0..m {
            all_fitted[(i, j)] = fitted[j];
            let resid = curve[j] - fitted[j];
            total_rss += resid * resid;
        }
    }

    let total_points = (n * m) as f64;
    let (gcv, aic, bic) = compute_fit_criteria(total_rss, total_points, edf, m);

    Some(FourierFitResult {
        coefficients: all_coefs,
        fitted: all_fitted,
        edf,
        rss: total_rss,
        gcv,
        aic,
        bic,
        n_basis: actual_nbasis,
    })
}

/// Select optimal number of Fourier basis functions using GCV.
///
/// Performs grid search over nbasis values and returns the one with minimum GCV.
///
/// # Arguments
/// * `data` - Column-major FdMatrix (n x m)
/// * `argvals` - Evaluation points
/// * `min_nbasis` - Minimum number of basis functions to try
/// * `max_nbasis` - Maximum number of basis functions to try
///
/// # Returns
/// Optimal number of basis functions
pub fn select_fourier_nbasis_gcv(
    data: &FdMatrix,
    argvals: &[f64],
    min_nbasis: usize,
    max_nbasis: usize,
) -> usize {
    let m = data.ncols();
    let min_nb = min_nbasis.max(3);
    // Ensure max doesn't exceed m (can't have more parameters than data points)
    let max_nb = max_nbasis.min(m / 2);

    if max_nb <= min_nb {
        return min_nb;
    }

    let mut best_nbasis = min_nb;
    let mut best_gcv = f64::INFINITY;

    // Test odd values only (1 constant + pairs of sin/cos)
    let mut nbasis = if min_nb % 2 == 0 { min_nb + 1 } else { min_nb };
    while nbasis <= max_nb {
        if let Some(result) = fourier_fit_1d(data, argvals, nbasis) {
            if result.gcv < best_gcv && result.gcv.is_finite() {
                best_gcv = result.gcv;
                best_nbasis = nbasis;
            }
        }
        nbasis += 2;
    }

    best_nbasis
}

/// Result of automatic basis selection for a single curve.
#[derive(Clone)]
pub struct SingleCurveSelection {
    /// Selected basis type: 0 = P-spline, 1 = Fourier
    pub basis_type: i32,
    /// Selected number of basis functions
    pub nbasis: usize,
    /// Best criterion score (GCV, AIC, or BIC)
    pub score: f64,
    /// Coefficients for the selected basis
    pub coefficients: Vec<f64>,
    /// Fitted values
    pub fitted: Vec<f64>,
    /// Effective degrees of freedom
    pub edf: f64,
    /// Whether seasonal pattern was detected (if use_seasonal_hint)
    pub seasonal_detected: bool,
    /// Lambda value (for P-spline, NaN for Fourier)
    pub lambda: f64,
}

/// Result of automatic basis selection for all curves.
pub struct BasisAutoSelectionResult {
    /// Per-curve selection results
    pub selections: Vec<SingleCurveSelection>,
    /// Criterion used (0=GCV, 1=AIC, 2=BIC)
    pub criterion: i32,
}

/// Detect if a curve has seasonal/periodic pattern using FFT.
///
/// Returns true if the peak power in the periodogram is significantly
/// above the mean power level.
fn detect_seasonality_fft(curve: &[f64]) -> bool {
    use rustfft::{num_complex::Complex, FftPlanner};

    let n = curve.len();
    if n < 8 {
        return false;
    }

    // Remove mean
    let mean: f64 = curve.iter().sum::<f64>() / n as f64;
    let mut input: Vec<Complex<f64>> = curve.iter().map(|&x| Complex::new(x - mean, 0.0)).collect();

    let mut planner = FftPlanner::new();
    let fft = planner.plan_fft_forward(n);
    fft.process(&mut input);

    // Compute power spectrum (skip DC component and Nyquist)
    let powers: Vec<f64> = input[1..n / 2].iter().map(|c| c.norm_sqr()).collect();

    if powers.is_empty() {
        return false;
    }

    let max_power = powers.iter().cloned().fold(0.0_f64, f64::max);
    let mean_power = powers.iter().sum::<f64>() / powers.len() as f64;

    // Seasonal if peak is significantly above mean
    max_power > 2.0 * mean_power
}

/// Fit a single curve with Fourier basis and compute criterion score.
fn fit_curve_fourier(
    curve: &[f64],
    m: usize,
    argvals: &[f64],
    nbasis: usize,
    criterion: i32,
) -> Option<(f64, Vec<f64>, Vec<f64>, f64)> {
    let nbasis = if nbasis % 2 == 0 { nbasis + 1 } else { nbasis };

    let basis = fourier_basis(argvals, nbasis);
    let actual_nbasis = basis.len() / m;
    let b_mat = DMatrix::from_column_slice(m, actual_nbasis, &basis);

    let btb = &b_mat.transpose() * &b_mat;
    let btb_inv = svd_pseudoinverse(&btb)?;
    let proj = &btb_inv * b_mat.transpose();
    let h_mat = &b_mat * &proj;
    let edf: f64 = (0..m).map(|i| h_mat[(i, i)]).sum();

    let curve_vec = DVector::from_column_slice(curve);
    let coefs = &btb_inv * (b_mat.transpose() * &curve_vec);
    let fitted = &b_mat * &coefs;

    let rss: f64 = (0..m).map(|j| (curve[j] - fitted[j]).powi(2)).sum();
    let score = compute_model_criterion(rss, m as f64, edf, criterion);

    let coef_vec: Vec<f64> = (0..actual_nbasis).map(|k| coefs[k]).collect();
    let fitted_vec: Vec<f64> = (0..m).map(|j| fitted[j]).collect();

    Some((score, coef_vec, fitted_vec, edf))
}

/// Fit a single curve with P-spline basis and compute criterion score.
fn fit_curve_pspline(
    curve: &[f64],
    m: usize,
    argvals: &[f64],
    nbasis: usize,
    lambda: f64,
    order: usize,
    criterion: i32,
) -> Option<(f64, Vec<f64>, Vec<f64>, f64)> {
    let basis = bspline_basis(argvals, nbasis.saturating_sub(4).max(2), 4);
    let actual_nbasis = basis.len() / m;
    let b_mat = DMatrix::from_column_slice(m, actual_nbasis, &basis);

    let d = difference_matrix(actual_nbasis, order);
    let penalty = &d.transpose() * &d;
    let btb = &b_mat.transpose() * &b_mat;
    let btb_penalized = &btb + lambda * &penalty;

    let btb_inv = svd_pseudoinverse(&btb_penalized)?;
    let proj = &btb_inv * b_mat.transpose();
    let h_mat = &b_mat * &proj;
    let edf: f64 = (0..m).map(|i| h_mat[(i, i)]).sum();

    let curve_vec = DVector::from_column_slice(curve);
    let coefs = &btb_inv * (b_mat.transpose() * &curve_vec);
    let fitted = &b_mat * &coefs;

    let rss: f64 = (0..m).map(|j| (curve[j] - fitted[j]).powi(2)).sum();
    let score = compute_model_criterion(rss, m as f64, edf, criterion);

    let coef_vec: Vec<f64> = (0..actual_nbasis).map(|k| coefs[k]).collect();
    let fitted_vec: Vec<f64> = (0..m).map(|j| fitted[j]).collect();

    Some((score, coef_vec, fitted_vec, edf))
}

/// Result of a basis search for a single curve.
struct BasisSearchResult {
    score: f64,
    nbasis: usize,
    coefs: Vec<f64>,
    fitted: Vec<f64>,
    edf: f64,
    lambda: f64,
}

/// Search over Fourier basis sizes for the best fit.
fn search_fourier_basis(
    curve: &[f64],
    m: usize,
    argvals: &[f64],
    fourier_min: usize,
    fourier_max: usize,
    seasonal: bool,
    criterion: i32,
) -> Option<BasisSearchResult> {
    let fourier_start = if seasonal {
        fourier_min.max(5)
    } else {
        fourier_min
    };
    let mut nb = if fourier_start % 2 == 0 {
        fourier_start + 1
    } else {
        fourier_start
    };

    let mut best: Option<BasisSearchResult> = None;
    while nb <= fourier_max {
        if let Some((score, coefs, fitted, edf)) =
            fit_curve_fourier(curve, m, argvals, nb, criterion)
        {
            if score.is_finite() && best.as_ref().map_or(true, |b| score < b.score) {
                best = Some(BasisSearchResult {
                    score,
                    nbasis: nb,
                    coefs,
                    fitted,
                    edf,
                    lambda: f64::NAN,
                });
            }
        }
        nb += 2;
    }
    best
}

/// Try a single P-spline fit and update best if it improves the score.
fn try_pspline_fit_update(
    curve: &[f64],
    m: usize,
    argvals: &[f64],
    nb: usize,
    lam: f64,
    criterion: i32,
    best: &mut Option<BasisSearchResult>,
) {
    if let Some((score, coefs, fitted, edf)) =
        fit_curve_pspline(curve, m, argvals, nb, lam, 2, criterion)
    {
        if score.is_finite() && best.as_ref().map_or(true, |b| score < b.score) {
            *best = Some(BasisSearchResult {
                score,
                nbasis: nb,
                coefs,
                fitted,
                edf,
                lambda: lam,
            });
        }
    }
}

/// Search over P-spline basis sizes (and optionally lambda) for the best fit.
fn search_pspline_basis(
    curve: &[f64],
    m: usize,
    argvals: &[f64],
    pspline_min: usize,
    pspline_max: usize,
    lambda_grid: &[f64],
    auto_lambda: bool,
    lambda: f64,
    criterion: i32,
) -> Option<BasisSearchResult> {
    let mut best: Option<BasisSearchResult> = None;
    for nb in pspline_min..=pspline_max {
        let lambdas: Box<dyn Iterator<Item = f64>> = if auto_lambda {
            Box::new(lambda_grid.iter().copied())
        } else {
            Box::new(std::iter::once(lambda))
        };
        for lam in lambdas {
            try_pspline_fit_update(curve, m, argvals, nb, lam, criterion, &mut best);
        }
    }
    best
}

/// Select optimal basis type and parameters for each curve individually.
///
/// This function compares Fourier and P-spline bases for each curve,
/// selecting the optimal basis type and number of basis functions using
/// model selection criteria (GCV, AIC, or BIC).
///
/// # Arguments
/// * `data` - Column-major FdMatrix (n x m)
/// * `argvals` - Evaluation points
/// * `criterion` - Model selection criterion: 0=GCV, 1=AIC, 2=BIC
/// * `nbasis_min` - Minimum number of basis functions (0 for auto)
/// * `nbasis_max` - Maximum number of basis functions (0 for auto)
/// * `lambda_pspline` - Smoothing parameter for P-spline (negative for auto-select)
/// * `use_seasonal_hint` - Whether to use FFT to detect seasonality
///
/// # Returns
/// BasisAutoSelectionResult with per-curve selections
pub fn select_basis_auto_1d(
    data: &FdMatrix,
    argvals: &[f64],
    criterion: i32,
    nbasis_min: usize,
    nbasis_max: usize,
    lambda_pspline: f64,
    use_seasonal_hint: bool,
) -> BasisAutoSelectionResult {
    let n = data.nrows();
    let m = data.ncols();
    let fourier_min = if nbasis_min > 0 { nbasis_min.max(3) } else { 3 };
    let fourier_max = if nbasis_max > 0 {
        nbasis_max.min(m / 3).min(25)
    } else {
        (m / 3).min(25)
    };
    let pspline_min = if nbasis_min > 0 { nbasis_min.max(6) } else { 6 };
    let pspline_max = if nbasis_max > 0 {
        nbasis_max.min(m / 2).min(40)
    } else {
        (m / 2).min(40)
    };

    let lambda_grid = [0.001, 0.01, 0.1, 1.0, 10.0, 100.0];
    let auto_lambda = lambda_pspline < 0.0;

    let selections: Vec<SingleCurveSelection> = iter_maybe_parallel!(0..n)
        .map(|i| {
            let curve: Vec<f64> = (0..m).map(|j| data[(i, j)]).collect();
            let seasonal_detected = if use_seasonal_hint {
                detect_seasonality_fft(&curve)
            } else {
                false
            };

            let fourier_best = search_fourier_basis(
                &curve,
                m,
                argvals,
                fourier_min,
                fourier_max,
                seasonal_detected,
                criterion,
            );
            let pspline_best = search_pspline_basis(
                &curve,
                m,
                argvals,
                pspline_min,
                pspline_max,
                &lambda_grid,
                auto_lambda,
                lambda_pspline,
                criterion,
            );

            // Pick the best overall result
            let (basis_type, result) = match (fourier_best, pspline_best) {
                (Some(f), Some(p)) => {
                    if f.score <= p.score {
                        (1i32, f)
                    } else {
                        (0i32, p)
                    }
                }
                (Some(f), None) => (1, f),
                (None, Some(p)) => (0, p),
                (None, None) => {
                    return SingleCurveSelection {
                        basis_type: 0,
                        nbasis: pspline_min,
                        score: f64::INFINITY,
                        coefficients: Vec::new(),
                        fitted: Vec::new(),
                        edf: 0.0,
                        seasonal_detected,
                        lambda: f64::NAN,
                    };
                }
            };

            SingleCurveSelection {
                basis_type,
                nbasis: result.nbasis,
                score: result.score,
                coefficients: result.coefs,
                fitted: result.fitted,
                edf: result.edf,
                seasonal_detected,
                lambda: result.lambda,
            }
        })
        .collect();

    BasisAutoSelectionResult {
        selections,
        criterion,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::f64::consts::PI;

    /// Generate a uniform grid of points
    fn uniform_grid(n: usize) -> Vec<f64> {
        (0..n).map(|i| i as f64 / (n - 1) as f64).collect()
    }

    /// Generate sine wave data
    fn sine_wave(t: &[f64], freq: f64) -> Vec<f64> {
        t.iter().map(|&ti| (2.0 * PI * freq * ti).sin()).collect()
    }

    // ============== B-spline basis tests ==============

    #[test]
    fn test_bspline_basis_dimensions() {
        let t = uniform_grid(50);
        let nknots = 10;
        let order = 4;
        let basis = bspline_basis(&t, nknots, order);

        let expected_nbasis = nknots + order;
        assert_eq!(basis.len(), t.len() * expected_nbasis);
    }

    #[test]
    fn test_bspline_basis_partition_of_unity() {
        // B-splines should sum to 1 at each point (partition of unity)
        let t = uniform_grid(50);
        let nknots = 8;
        let order = 4;
        let basis = bspline_basis(&t, nknots, order);

        let nbasis = nknots + order;
        for i in 0..t.len() {
            let sum: f64 = (0..nbasis).map(|j| basis[i + j * t.len()]).sum();
            assert!(
                (sum - 1.0).abs() < 1e-10,
                "B-spline partition of unity failed at point {}: sum = {}",
                i,
                sum
            );
        }
    }

    #[test]
    fn test_bspline_basis_non_negative() {
        let t = uniform_grid(50);
        let basis = bspline_basis(&t, 8, 4);

        for &val in &basis {
            assert!(val >= -1e-10, "B-spline values should be non-negative");
        }
    }

    #[test]
    fn test_bspline_basis_boundary() {
        // Test that basis functions work at boundary points
        let t = vec![0.0, 0.5, 1.0];
        let basis = bspline_basis(&t, 5, 4);

        // Should have valid output (no NaN or Inf)
        for &val in &basis {
            assert!(val.is_finite(), "B-spline should produce finite values");
        }
    }

    // ============== Fourier basis tests ==============

    #[test]
    fn test_fourier_basis_dimensions() {
        let t = uniform_grid(50);
        let nbasis = 7;
        let basis = fourier_basis(&t, nbasis);

        assert_eq!(basis.len(), t.len() * nbasis);
    }

    #[test]
    fn test_fourier_basis_constant_first_column() {
        let t = uniform_grid(50);
        let nbasis = 7;
        let basis = fourier_basis(&t, nbasis);

        // First column should be constant (DC component = 1)
        let first_val = basis[0];
        for i in 0..t.len() {
            assert!(
                (basis[i] - first_val).abs() < 1e-10,
                "First Fourier column should be constant"
            );
        }
    }

    #[test]
    fn test_fourier_basis_sin_cos_range() {
        let t = uniform_grid(100);
        let nbasis = 11;
        let basis = fourier_basis(&t, nbasis);

        // Sin and cos values should be in [-1, 1]
        for &val in &basis {
            assert!((-1.0 - 1e-10..=1.0 + 1e-10).contains(&val));
        }
    }

    #[test]
    fn test_fourier_basis_with_period() {
        let t = uniform_grid(100);
        let nbasis = 5;
        let period = 0.5;
        let basis = fourier_basis_with_period(&t, nbasis, period);

        assert_eq!(basis.len(), t.len() * nbasis);
        // Verify first column is constant
        let first_val = basis[0];
        for i in 0..t.len() {
            assert!((basis[i] - first_val).abs() < 1e-10);
        }
    }

    #[test]
    fn test_fourier_basis_period_affects_frequency() {
        let t = uniform_grid(100);
        let nbasis = 5;

        let basis1 = fourier_basis_with_period(&t, nbasis, 1.0);
        let basis2 = fourier_basis_with_period(&t, nbasis, 0.5);

        // Different periods should give different basis matrices
        let n = t.len();
        let mut any_different = false;
        for i in 0..n {
            // Compare second column (first sin term)
            if (basis1[i + n] - basis2[i + n]).abs() > 1e-10 {
                any_different = true;
                break;
            }
        }
        assert!(
            any_different,
            "Different periods should produce different bases"
        );
    }

    // ============== Difference matrix tests ==============

    #[test]
    fn test_difference_matrix_order_zero() {
        let d = difference_matrix(5, 0);
        assert_eq!(d.nrows(), 5);
        assert_eq!(d.ncols(), 5);

        // Should be identity matrix
        for i in 0..5 {
            for j in 0..5 {
                let expected = if i == j { 1.0 } else { 0.0 };
                assert!((d[(i, j)] - expected).abs() < 1e-10);
            }
        }
    }

    #[test]
    fn test_difference_matrix_first_order() {
        let d = difference_matrix(5, 1);
        assert_eq!(d.nrows(), 4);
        assert_eq!(d.ncols(), 5);

        // First order differences: D * [1,2,3,4,5] = [1,1,1,1]
        let x = DVector::from_vec(vec![1.0, 2.0, 3.0, 4.0, 5.0]);
        let dx = &d * x;
        for i in 0..4 {
            assert!((dx[i] - 1.0).abs() < 1e-10);
        }
    }

    #[test]
    fn test_difference_matrix_second_order() {
        let d = difference_matrix(5, 2);
        assert_eq!(d.nrows(), 3);
        assert_eq!(d.ncols(), 5);

        // Second order differences of linear: D^2 * [1,2,3,4,5] = [0,0,0]
        let x = DVector::from_vec(vec![1.0, 2.0, 3.0, 4.0, 5.0]);
        let dx = &d * x;
        for i in 0..3 {
            assert!(dx[i].abs() < 1e-10, "Second diff of linear should be zero");
        }
    }

    #[test]
    fn test_difference_matrix_quadratic() {
        let d = difference_matrix(5, 2);

        // Second order differences of quadratic: D^2 * [1,4,9,16,25] = [2,2,2]
        let x = DVector::from_vec(vec![1.0, 4.0, 9.0, 16.0, 25.0]);
        let dx = &d * x;
        for i in 0..3 {
            assert!(
                (dx[i] - 2.0).abs() < 1e-10,
                "Second diff of squares should be 2"
            );
        }
    }

    // ============== Basis projection tests ==============

    /// Create an FdMatrix from per-curve data (each curve is one row).
    /// The input flat data is in "row of rows" order: all values for curve 0, then curve 1, etc.
    /// We need to convert to column-major layout.
    fn make_matrix(flat_row_major: &[f64], n: usize, m: usize) -> FdMatrix {
        let mut col_major = vec![0.0; n * m];
        for i in 0..n {
            for j in 0..m {
                col_major[i + j * n] = flat_row_major[i * m + j];
            }
        }
        FdMatrix::from_column_major(col_major, n, m).unwrap()
    }

    #[test]
    fn test_fdata_to_basis_1d_bspline() {
        let t = uniform_grid(50);
        let n = 5;
        let m = t.len();

        // Create simple data (n curves, each shifted linear)
        let flat: Vec<f64> = (0..n)
            .flat_map(|i| t.iter().map(move |&ti| ti + i as f64 * 0.1))
            .collect();
        let data = make_matrix(&flat, n, m);

        let result = fdata_to_basis_1d(&data, &t, 10, 0);
        assert!(result.is_some());

        let res = result.unwrap();
        assert!(res.n_basis > 0);
        assert_eq!(res.coefficients.nrows(), n);
        assert_eq!(res.coefficients.ncols(), res.n_basis);
    }

    #[test]
    fn test_fdata_to_basis_1d_fourier() {
        let t = uniform_grid(50);
        let n = 5;
        let m = t.len();

        // Create sine wave data
        let flat: Vec<f64> = (0..n).flat_map(|_| sine_wave(&t, 2.0)).collect();
        let data = make_matrix(&flat, n, m);

        let result = fdata_to_basis_1d(&data, &t, 7, 1);
        assert!(result.is_some());

        let res = result.unwrap();
        assert_eq!(res.n_basis, 7);
    }

    #[test]
    fn test_fdata_to_basis_1d_invalid_input() {
        let t = uniform_grid(50);

        // Empty data
        let empty = FdMatrix::zeros(0, 50);
        let result = fdata_to_basis_1d(&empty, &t, 10, 0);
        assert!(result.is_none());

        // nbasis too small
        let data = FdMatrix::zeros(1, 50);
        let result = fdata_to_basis_1d(&data, &t, 1, 0);
        assert!(result.is_none());
    }

    #[test]
    fn test_basis_roundtrip() {
        let t = uniform_grid(100);
        let n = 1;
        let m = t.len();

        // Create smooth sine wave data (Fourier basis should represent exactly)
        let raw = sine_wave(&t, 1.0);
        let data = FdMatrix::from_column_major(raw.clone(), n, m).unwrap();

        // Project to Fourier basis with enough terms
        let proj = fdata_to_basis_1d(&data, &t, 5, 1).unwrap();

        // Reconstruct
        let reconstructed = basis_to_fdata_1d(&proj.coefficients, &t, proj.n_basis, 1);

        // Should approximately match original for a simple sine wave
        let mut max_error = 0.0;
        for j in 0..m {
            let err = (raw[j] - reconstructed[(0, j)]).abs();
            if err > max_error {
                max_error = err;
            }
        }
        assert!(max_error < 0.5, "Roundtrip error too large: {}", max_error);
    }

    #[test]
    fn test_basis_to_fdata_empty_input() {
        let empty = FdMatrix::zeros(0, 0);
        let result = basis_to_fdata_1d(&empty, &[], 5, 0);
        assert!(result.is_empty());
    }

    // ============== P-spline fitting tests ==============

    #[test]
    fn test_pspline_fit_1d_basic() {
        let t = uniform_grid(50);
        let n = 3;
        let m = t.len();

        // Create noisy data
        let flat: Vec<f64> = (0..n)
            .flat_map(|i| {
                t.iter()
                    .enumerate()
                    .map(move |(j, &ti)| (2.0 * PI * ti).sin() + 0.1 * (i * j) as f64 % 1.0)
            })
            .collect();
        let data = make_matrix(&flat, n, m);

        let result = pspline_fit_1d(&data, &t, 15, 1.0, 2);
        assert!(result.is_some());

        let res = result.unwrap();
        assert!(res.n_basis > 0);
        assert_eq!(res.fitted.nrows(), n);
        assert_eq!(res.fitted.ncols(), m);
        assert!(res.rss >= 0.0);
        assert!(res.edf > 0.0);
        assert!(res.gcv.is_finite());
    }

    #[test]
    fn test_pspline_fit_1d_smoothness() {
        let t = uniform_grid(50);
        let n = 1;
        let m = t.len();

        // Create noisy sine wave
        let raw: Vec<f64> = t
            .iter()
            .enumerate()
            .map(|(i, &ti)| (2.0 * PI * ti).sin() + 0.3 * ((i * 17) % 100) as f64 / 100.0)
            .collect();
        let data = FdMatrix::from_column_major(raw, n, m).unwrap();

        let low_lambda = pspline_fit_1d(&data, &t, 15, 0.01, 2).unwrap();
        let high_lambda = pspline_fit_1d(&data, &t, 15, 100.0, 2).unwrap();

        // Higher lambda should give lower edf (more smoothing)
        assert!(high_lambda.edf < low_lambda.edf);
    }

    #[test]
    fn test_pspline_fit_1d_invalid_input() {
        let t = uniform_grid(50);
        let empty = FdMatrix::zeros(0, 50);
        let result = pspline_fit_1d(&empty, &t, 15, 1.0, 2);
        assert!(result.is_none());
    }

    // ============== Fourier fitting tests ==============

    #[test]
    fn test_fourier_fit_1d_sine_wave() {
        let t = uniform_grid(100);
        let n = 1;
        let m = t.len();

        // Create pure sine wave
        let raw = sine_wave(&t, 2.0);
        let data = FdMatrix::from_column_major(raw, n, m).unwrap();

        let result = fourier_fit_1d(&data, &t, 11);
        assert!(result.is_some());

        let res = result.unwrap();
        assert!(res.rss < 1e-6, "Pure sine should have near-zero RSS");
    }

    #[test]
    fn test_fourier_fit_1d_makes_nbasis_odd() {
        let t = uniform_grid(50);
        let raw = sine_wave(&t, 1.0);
        let data = FdMatrix::from_column_major(raw, 1, t.len()).unwrap();

        // Pass even nbasis
        let result = fourier_fit_1d(&data, &t, 6);
        assert!(result.is_some());

        // Should have been adjusted to odd
        let res = result.unwrap();
        assert!(res.n_basis % 2 == 1);
    }

    #[test]
    fn test_fourier_fit_1d_criteria() {
        let t = uniform_grid(50);
        let raw = sine_wave(&t, 2.0);
        let data = FdMatrix::from_column_major(raw, 1, t.len()).unwrap();

        let result = fourier_fit_1d(&data, &t, 9).unwrap();

        // All criteria should be finite
        assert!(result.gcv.is_finite());
        assert!(result.aic.is_finite());
        assert!(result.bic.is_finite());
    }

    #[test]
    fn test_fourier_fit_1d_invalid_nbasis() {
        let t = uniform_grid(50);
        let raw = sine_wave(&t, 1.0);
        let data = FdMatrix::from_column_major(raw, 1, t.len()).unwrap();

        // nbasis < 3 should return None
        let result = fourier_fit_1d(&data, &t, 2);
        assert!(result.is_none());
    }

    // ============== GCV selection tests ==============

    #[test]
    fn test_select_fourier_nbasis_gcv_range() {
        let t = uniform_grid(100);
        let raw = sine_wave(&t, 3.0);
        let data = FdMatrix::from_column_major(raw, 1, t.len()).unwrap();

        let best = select_fourier_nbasis_gcv(&data, &t, 3, 15);

        assert!((3..=15).contains(&best));
        assert!(best % 2 == 1, "Selected nbasis should be odd");
    }

    #[test]
    fn test_select_fourier_nbasis_gcv_respects_min() {
        let t = uniform_grid(50);
        let raw = sine_wave(&t, 1.0);
        let data = FdMatrix::from_column_major(raw, 1, t.len()).unwrap();

        let best = select_fourier_nbasis_gcv(&data, &t, 7, 15);
        assert!(best >= 7);
    }

    // ============== Auto selection tests ==============

    #[test]
    fn test_select_basis_auto_1d_returns_results() {
        let t = uniform_grid(50);
        let n = 3;
        let m = t.len();

        let flat: Vec<f64> = (0..n).flat_map(|i| sine_wave(&t, 1.0 + i as f64)).collect();
        let data = make_matrix(&flat, n, m);

        let result = select_basis_auto_1d(&data, &t, 0, 5, 15, 1.0, false);

        assert_eq!(result.selections.len(), n);
        for sel in &result.selections {
            assert!(sel.nbasis >= 3);
            assert!(!sel.coefficients.is_empty());
            assert_eq!(sel.fitted.len(), m);
        }
    }

    #[test]
    fn test_select_basis_auto_1d_seasonal_hint() {
        let t = uniform_grid(100);
        let n = 1;
        let m = t.len();

        // Strong seasonal pattern
        let raw = sine_wave(&t, 5.0);
        let data = FdMatrix::from_column_major(raw, n, m).unwrap();

        let result = select_basis_auto_1d(&data, &t, 0, 0, 0, -1.0, true);

        assert_eq!(result.selections.len(), 1);
        assert!(result.selections[0].seasonal_detected);
    }

    #[test]
    fn test_select_basis_auto_1d_non_seasonal() {
        let t = uniform_grid(50);
        let n = 1;
        let m = t.len();

        // Constant data (definitely not seasonal)
        let raw: Vec<f64> = vec![1.0; m];
        let data = FdMatrix::from_column_major(raw, n, m).unwrap();

        let result = select_basis_auto_1d(&data, &t, 0, 0, 0, -1.0, true);

        // Constant data shouldn't be detected as seasonal
        assert!(!result.selections[0].seasonal_detected);
    }

    #[test]
    fn test_select_basis_auto_1d_criterion_options() {
        let t = uniform_grid(50);
        let raw = sine_wave(&t, 2.0);
        let data = FdMatrix::from_column_major(raw, 1, t.len()).unwrap();

        // Test all three criteria
        let gcv_result = select_basis_auto_1d(&data, &t, 0, 0, 0, 1.0, false);
        let aic_result = select_basis_auto_1d(&data, &t, 1, 0, 0, 1.0, false);
        let bic_result = select_basis_auto_1d(&data, &t, 2, 0, 0, 1.0, false);

        assert_eq!(gcv_result.criterion, 0);
        assert_eq!(aic_result.criterion, 1);
        assert_eq!(bic_result.criterion, 2);
    }
}
