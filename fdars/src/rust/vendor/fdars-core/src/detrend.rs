//! Detrending and decomposition functions for non-stationary functional data.
//!
//! This module provides methods for removing trends from functional data
//! to enable more accurate seasonal analysis. It includes:
//! - Linear detrending (least squares)
//! - Polynomial detrending (QR decomposition)
//! - Differencing (first and second order)
//! - LOESS detrending (local polynomial regression)
//! - Spline detrending (P-splines)
//! - Automatic method selection via AIC

use crate::iter_maybe_parallel;
use crate::matrix::FdMatrix;
use crate::smoothing::local_polynomial;
use nalgebra::{DMatrix, DVector, Dyn, SVD};
#[cfg(feature = "parallel")]
use rayon::iter::ParallelIterator;

/// Result of detrending operation.
#[derive(Debug, Clone)]
pub struct TrendResult {
    /// Estimated trend values (n x m)
    pub trend: FdMatrix,
    /// Detrended data (n x m)
    pub detrended: FdMatrix,
    /// Method used for detrending
    pub method: String,
    /// Polynomial coefficients (for polynomial methods, per sample)
    /// For n samples with polynomial degree d: n x (d+1)
    pub coefficients: Option<FdMatrix>,
    /// Residual sum of squares for each sample
    pub rss: Vec<f64>,
    /// Number of parameters (for AIC calculation)
    pub n_params: usize,
}

/// Result of seasonal decomposition.
#[derive(Debug, Clone)]
pub struct DecomposeResult {
    /// Trend component (n x m)
    pub trend: FdMatrix,
    /// Seasonal component (n x m)
    pub seasonal: FdMatrix,
    /// Remainder/residual component (n x m)
    pub remainder: FdMatrix,
    /// Period used for decomposition
    pub period: f64,
    /// Decomposition method ("additive" or "multiplicative")
    pub method: String,
}

/// Remove linear trend from functional data using least squares.
///
/// # Arguments
/// * `data` - Matrix (n x m): n samples, m evaluation points
/// * `argvals` - Time/argument values of length m
///
/// # Returns
/// TrendResult with trend, detrended data, and coefficients (intercept, slope)
pub fn detrend_linear(data: &FdMatrix, argvals: &[f64]) -> TrendResult {
    let (n, m) = data.shape();
    if n == 0 || m < 2 || argvals.len() != m {
        return TrendResult {
            trend: FdMatrix::zeros(n, m),
            detrended: FdMatrix::from_slice(data.as_slice(), n, m)
                .unwrap_or_else(|| FdMatrix::zeros(n, m)),
            method: "linear".to_string(),
            coefficients: None,
            rss: vec![0.0; n],
            n_params: 2,
        };
    }

    let mean_t: f64 = argvals.iter().sum::<f64>() / m as f64;
    let ss_t: f64 = argvals.iter().map(|&t| (t - mean_t).powi(2)).sum();

    let results: Vec<(Vec<f64>, Vec<f64>, f64, f64, f64)> = iter_maybe_parallel!(0..n)
        .map(|i| {
            let curve: Vec<f64> = (0..m).map(|j| data[(i, j)]).collect();
            let mean_y: f64 = curve.iter().sum::<f64>() / m as f64;
            let mut sp = 0.0;
            for j in 0..m {
                sp += (argvals[j] - mean_t) * (curve[j] - mean_y);
            }
            let slope = if ss_t.abs() > 1e-15 { sp / ss_t } else { 0.0 };
            let intercept = mean_y - slope * mean_t;
            let mut trend = vec![0.0; m];
            let mut detrended = vec![0.0; m];
            let mut rss = 0.0;
            for j in 0..m {
                trend[j] = intercept + slope * argvals[j];
                detrended[j] = curve[j] - trend[j];
                rss += detrended[j].powi(2);
            }
            (trend, detrended, intercept, slope, rss)
        })
        .collect();

    let mut trend = FdMatrix::zeros(n, m);
    let mut detrended = FdMatrix::zeros(n, m);
    let mut coefficients = FdMatrix::zeros(n, 2);
    let mut rss = vec![0.0; n];

    for (i, (t, d, intercept, slope, r)) in results.into_iter().enumerate() {
        for j in 0..m {
            trend[(i, j)] = t[j];
            detrended[(i, j)] = d[j];
        }
        coefficients[(i, 0)] = intercept;
        coefficients[(i, 1)] = slope;
        rss[i] = r;
    }

    TrendResult {
        trend,
        detrended,
        method: "linear".to_string(),
        coefficients: Some(coefficients),
        rss,
        n_params: 2,
    }
}

fn build_vandermonde_matrix(t_norm: &[f64], m: usize, n_coef: usize) -> DMatrix<f64> {
    let mut design = DMatrix::zeros(m, n_coef);
    for j in 0..m {
        let t = t_norm[j];
        let mut power = 1.0;
        for k in 0..n_coef {
            design[(j, k)] = power;
            power *= t;
        }
    }
    design
}

fn fit_polynomial_single_curve(
    curve: &[f64],
    svd: &SVD<f64, Dyn, Dyn>,
    design: &DMatrix<f64>,
    n_coef: usize,
    m: usize,
) -> (Vec<f64>, Vec<f64>, Vec<f64>, f64) {
    let y = DVector::from_row_slice(curve);
    let beta = svd
        .solve(&y, 1e-10)
        .unwrap_or_else(|_| DVector::zeros(n_coef));
    let fitted = design * &beta;
    let mut trend = vec![0.0; m];
    let mut detrended = vec![0.0; m];
    let mut rss = 0.0;
    for j in 0..m {
        trend[j] = fitted[j];
        detrended[j] = curve[j] - fitted[j];
        rss += detrended[j].powi(2);
    }
    let coefs: Vec<f64> = beta.iter().cloned().collect();
    (trend, detrended, coefs, rss)
}

fn diff_single_curve(curve: &[f64], m: usize, order: usize) -> (Vec<f64>, Vec<f64>, Vec<f64>, f64) {
    let diff1: Vec<f64> = (0..m - 1).map(|j| curve[j + 1] - curve[j]).collect();
    let detrended = if order == 2 {
        (0..diff1.len() - 1)
            .map(|j| diff1[j + 1] - diff1[j])
            .collect()
    } else {
        diff1.clone()
    };
    let initial_values = if order == 2 {
        vec![curve[0], curve[1]]
    } else {
        vec![curve[0]]
    };
    let rss: f64 = detrended.iter().map(|&x| x.powi(2)).sum();
    let new_m = m - order;
    let mut trend = vec![0.0; m];
    trend[0] = curve[0];
    if order == 1 {
        for j in 1..m {
            trend[j] = curve[j] - if j <= new_m { detrended[j - 1] } else { 0.0 };
        }
    } else {
        trend = curve.to_vec();
    }
    let mut det_full = vec![0.0; m];
    det_full[..new_m].copy_from_slice(&detrended[..new_m]);
    (trend, det_full, initial_values, rss)
}

fn reassemble_polynomial_results(
    results: Vec<(Vec<f64>, Vec<f64>, Vec<f64>, f64)>,
    n: usize,
    m: usize,
    n_coef: usize,
) -> (FdMatrix, FdMatrix, FdMatrix, Vec<f64>) {
    let mut trend = FdMatrix::zeros(n, m);
    let mut detrended = FdMatrix::zeros(n, m);
    let mut coefficients = FdMatrix::zeros(n, n_coef);
    let mut rss = vec![0.0; n];
    for (i, (t, d, coefs, r)) in results.into_iter().enumerate() {
        for j in 0..m {
            trend[(i, j)] = t[j];
            detrended[(i, j)] = d[j];
        }
        for k in 0..n_coef {
            coefficients[(i, k)] = coefs[k];
        }
        rss[i] = r;
    }
    (trend, detrended, coefficients, rss)
}

/// Remove polynomial trend from functional data using QR decomposition.
pub fn detrend_polynomial(data: &FdMatrix, argvals: &[f64], degree: usize) -> TrendResult {
    let (n, m) = data.shape();
    if n == 0 || m < degree + 1 || argvals.len() != m || degree == 0 {
        return TrendResult {
            trend: FdMatrix::zeros(n, m),
            detrended: FdMatrix::from_slice(data.as_slice(), n, m)
                .unwrap_or_else(|| FdMatrix::zeros(n, m)),
            method: format!("polynomial({})", degree),
            coefficients: None,
            rss: vec![0.0; n],
            n_params: degree + 1,
        };
    }
    if degree == 1 {
        let mut result = detrend_linear(data, argvals);
        result.method = "polynomial(1)".to_string();
        return result;
    }
    let n_coef = degree + 1;
    let t_min = argvals.iter().cloned().fold(f64::INFINITY, f64::min);
    let t_max = argvals.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    let t_range = if (t_max - t_min).abs() > 1e-15 {
        t_max - t_min
    } else {
        1.0
    };
    let t_norm: Vec<f64> = argvals.iter().map(|&t| (t - t_min) / t_range).collect();
    let design = build_vandermonde_matrix(&t_norm, m, n_coef);
    let svd = design.clone().svd(true, true);
    let results: Vec<(Vec<f64>, Vec<f64>, Vec<f64>, f64)> = iter_maybe_parallel!(0..n)
        .map(|i| {
            let curve: Vec<f64> = (0..m).map(|j| data[(i, j)]).collect();
            fit_polynomial_single_curve(&curve, &svd, &design, n_coef, m)
        })
        .collect();
    let (trend, detrended, coefficients, rss) =
        reassemble_polynomial_results(results, n, m, n_coef);
    TrendResult {
        trend,
        detrended,
        method: format!("polynomial({})", degree),
        coefficients: Some(coefficients),
        rss,
        n_params: n_coef,
    }
}

/// Remove trend by differencing.
pub fn detrend_diff(data: &FdMatrix, order: usize) -> TrendResult {
    let (n, m) = data.shape();
    if n == 0 || m <= order || order == 0 || order > 2 {
        return TrendResult {
            trend: FdMatrix::zeros(n, m),
            detrended: FdMatrix::from_slice(data.as_slice(), n, m)
                .unwrap_or_else(|| FdMatrix::zeros(n, m)),
            method: format!("diff{}", order),
            coefficients: None,
            rss: vec![0.0; n],
            n_params: order,
        };
    }
    let results: Vec<(Vec<f64>, Vec<f64>, Vec<f64>, f64)> = iter_maybe_parallel!(0..n)
        .map(|i| {
            let curve: Vec<f64> = (0..m).map(|j| data[(i, j)]).collect();
            diff_single_curve(&curve, m, order)
        })
        .collect();
    let mut trend = FdMatrix::zeros(n, m);
    let mut detrended = FdMatrix::zeros(n, m);
    let mut coefficients = FdMatrix::zeros(n, order);
    let mut rss = vec![0.0; n];
    for (i, (t, d, init, r)) in results.into_iter().enumerate() {
        for j in 0..m {
            trend[(i, j)] = t[j];
            detrended[(i, j)] = d[j];
        }
        for k in 0..order {
            coefficients[(i, k)] = init[k];
        }
        rss[i] = r;
    }
    TrendResult {
        trend,
        detrended,
        method: format!("diff{}", order),
        coefficients: Some(coefficients),
        rss,
        n_params: order,
    }
}

/// Remove trend using LOESS (local polynomial regression).
pub fn detrend_loess(
    data: &FdMatrix,
    argvals: &[f64],
    bandwidth: f64,
    degree: usize,
) -> TrendResult {
    let (n, m) = data.shape();
    if n == 0 || m < 3 || argvals.len() != m || bandwidth <= 0.0 {
        return TrendResult {
            trend: FdMatrix::zeros(n, m),
            detrended: FdMatrix::from_slice(data.as_slice(), n, m)
                .unwrap_or_else(|| FdMatrix::zeros(n, m)),
            method: "loess".to_string(),
            coefficients: None,
            rss: vec![0.0; n],
            n_params: (m as f64 * bandwidth).ceil() as usize,
        };
    }
    let t_min = argvals.iter().cloned().fold(f64::INFINITY, f64::min);
    let t_max = argvals.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    let abs_bandwidth = (t_max - t_min) * bandwidth;
    let results: Vec<(Vec<f64>, Vec<f64>, f64)> = iter_maybe_parallel!(0..n)
        .map(|i| {
            let curve: Vec<f64> = (0..m).map(|j| data[(i, j)]).collect();
            let trend =
                local_polynomial(argvals, &curve, argvals, abs_bandwidth, degree, "gaussian");
            let mut detrended = vec![0.0; m];
            let mut rss = 0.0;
            for j in 0..m {
                detrended[j] = curve[j] - trend[j];
                rss += detrended[j].powi(2);
            }
            (trend, detrended, rss)
        })
        .collect();
    let mut trend = FdMatrix::zeros(n, m);
    let mut detrended = FdMatrix::zeros(n, m);
    let mut rss = vec![0.0; n];
    for (i, (t, d, r)) in results.into_iter().enumerate() {
        for j in 0..m {
            trend[(i, j)] = t[j];
            detrended[(i, j)] = d[j];
        }
        rss[i] = r;
    }
    let n_params = (m as f64 * bandwidth).ceil() as usize;
    TrendResult {
        trend,
        detrended,
        method: "loess".to_string(),
        coefficients: None,
        rss,
        n_params,
    }
}

/// Automatically select the best detrending method using AIC.
pub fn auto_detrend(data: &FdMatrix, argvals: &[f64]) -> TrendResult {
    let (n, m) = data.shape();
    if n == 0 || m < 4 || argvals.len() != m {
        return TrendResult {
            trend: FdMatrix::zeros(n, m),
            detrended: FdMatrix::from_slice(data.as_slice(), n, m)
                .unwrap_or_else(|| FdMatrix::zeros(n, m)),
            method: "auto(none)".to_string(),
            coefficients: None,
            rss: vec![0.0; n],
            n_params: 0,
        };
    }
    let compute_aic = |result: &TrendResult| -> f64 {
        let mut total_aic = 0.0;
        for i in 0..n {
            let rss = result.rss[i];
            let k = result.n_params as f64;
            let aic = if rss > 1e-15 {
                m as f64 * (rss / m as f64).ln() + 2.0 * k
            } else {
                f64::NEG_INFINITY
            };
            total_aic += aic;
        }
        total_aic / n as f64
    };
    let linear = detrend_linear(data, argvals);
    let poly2 = detrend_polynomial(data, argvals, 2);
    let poly3 = detrend_polynomial(data, argvals, 3);
    let loess = detrend_loess(data, argvals, 0.3, 2);
    let aic_linear = compute_aic(&linear);
    let aic_poly2 = compute_aic(&poly2);
    let aic_poly3 = compute_aic(&poly3);
    let aic_loess = compute_aic(&loess);
    let methods = [
        (aic_linear, "linear", linear),
        (aic_poly2, "polynomial(2)", poly2),
        (aic_poly3, "polynomial(3)", poly3),
        (aic_loess, "loess", loess),
    ];
    let (_, best_name, mut best_result) = methods
        .into_iter()
        .min_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal))
        .unwrap();
    best_result.method = format!("auto({})", best_name);
    best_result
}

fn fit_fourier_seasonal(
    detrended_i: &[f64],
    argvals: &[f64],
    omega: f64,
    n_harm: usize,
    m: usize,
) -> (Vec<f64>, Vec<f64>) {
    let n_coef = 2 * n_harm;
    let mut design = DMatrix::zeros(m, n_coef);
    for j in 0..m {
        let t = argvals[j];
        for k in 0..n_harm {
            let freq = (k + 1) as f64 * omega;
            design[(j, 2 * k)] = (freq * t).cos();
            design[(j, 2 * k + 1)] = (freq * t).sin();
        }
    }
    let y = DVector::from_row_slice(detrended_i);
    let svd = design.clone().svd(true, true);
    let coef = svd
        .solve(&y, 1e-10)
        .unwrap_or_else(|_| DVector::zeros(n_coef));
    let fitted = &design * &coef;
    let seasonal: Vec<f64> = fitted.iter().cloned().collect();
    let remainder: Vec<f64> = (0..m).map(|j| detrended_i[j] - seasonal[j]).collect();
    (seasonal, remainder)
}

/// Additive seasonal decomposition: data = trend + seasonal + remainder
pub fn decompose_additive(
    data: &FdMatrix,
    argvals: &[f64],
    period: f64,
    trend_method: &str,
    bandwidth: f64,
    n_harmonics: usize,
) -> DecomposeResult {
    let (n, m) = data.shape();
    if n == 0 || m < 4 || argvals.len() != m || period <= 0.0 {
        return DecomposeResult {
            trend: FdMatrix::zeros(n, m),
            seasonal: FdMatrix::zeros(n, m),
            remainder: FdMatrix::from_slice(data.as_slice(), n, m)
                .unwrap_or_else(|| FdMatrix::zeros(n, m)),
            period,
            method: "additive".to_string(),
        };
    }
    let _ = trend_method;
    let trend_result = detrend_loess(data, argvals, bandwidth.max(0.3), 2);
    let n_harm = n_harmonics.max(1).min(m / 4);
    let omega = 2.0 * std::f64::consts::PI / period;
    let results: Vec<(Vec<f64>, Vec<f64>, Vec<f64>)> = iter_maybe_parallel!(0..n)
        .map(|i| {
            let trend_i: Vec<f64> = (0..m).map(|j| trend_result.trend[(i, j)]).collect();
            let detrended_i: Vec<f64> = (0..m).map(|j| trend_result.detrended[(i, j)]).collect();
            let (seasonal, remainder) =
                fit_fourier_seasonal(&detrended_i, argvals, omega, n_harm, m);
            (trend_i, seasonal, remainder)
        })
        .collect();
    let mut trend = FdMatrix::zeros(n, m);
    let mut seasonal = FdMatrix::zeros(n, m);
    let mut remainder = FdMatrix::zeros(n, m);
    for (i, (t, s, r)) in results.into_iter().enumerate() {
        for j in 0..m {
            trend[(i, j)] = t[j];
            seasonal[(i, j)] = s[j];
            remainder[(i, j)] = r[j];
        }
    }
    DecomposeResult {
        trend,
        seasonal,
        remainder,
        period,
        method: "additive".to_string(),
    }
}

/// Multiplicative seasonal decomposition: data = trend * seasonal * remainder
pub fn decompose_multiplicative(
    data: &FdMatrix,
    argvals: &[f64],
    period: f64,
    trend_method: &str,
    bandwidth: f64,
    n_harmonics: usize,
) -> DecomposeResult {
    let (n, m) = data.shape();
    if n == 0 || m < 4 || argvals.len() != m || period <= 0.0 {
        return DecomposeResult {
            trend: FdMatrix::zeros(n, m),
            seasonal: FdMatrix::zeros(n, m),
            remainder: FdMatrix::from_slice(data.as_slice(), n, m)
                .unwrap_or_else(|| FdMatrix::zeros(n, m)),
            period,
            method: "multiplicative".to_string(),
        };
    }
    let min_val = data
        .as_slice()
        .iter()
        .cloned()
        .fold(f64::INFINITY, f64::min);
    let shift = if min_val <= 0.0 { -min_val + 1.0 } else { 0.0 };
    let log_data_vec: Vec<f64> = data.as_slice().iter().map(|&x| (x + shift).ln()).collect();
    let log_data = FdMatrix::from_column_major(log_data_vec, n, m).unwrap();
    let additive_result = decompose_additive(
        &log_data,
        argvals,
        period,
        trend_method,
        bandwidth,
        n_harmonics,
    );
    let mut trend = FdMatrix::zeros(n, m);
    let mut seasonal = FdMatrix::zeros(n, m);
    let mut remainder = FdMatrix::zeros(n, m);
    for i in 0..n {
        for j in 0..m {
            trend[(i, j)] = additive_result.trend[(i, j)].exp() - shift;
            seasonal[(i, j)] = additive_result.seasonal[(i, j)].exp();
            remainder[(i, j)] = additive_result.remainder[(i, j)].exp();
        }
    }
    DecomposeResult {
        trend,
        seasonal,
        remainder,
        period,
        method: "multiplicative".to_string(),
    }
}

// ============================================================================
// STL Decomposition (Cleveland et al., 1990)
// ============================================================================

/// Result of STL decomposition including robustness weights.
#[derive(Debug, Clone)]
pub struct StlResult {
    /// Trend component (n x m)
    pub trend: FdMatrix,
    /// Seasonal component (n x m)
    pub seasonal: FdMatrix,
    /// Remainder/residual component (n x m)
    pub remainder: FdMatrix,
    /// Robustness weights per point (n x m)
    pub weights: FdMatrix,
    /// Period used for decomposition
    pub period: usize,
    /// Seasonal smoothing window
    pub s_window: usize,
    /// Trend smoothing window
    pub t_window: usize,
    /// Number of inner loop iterations performed
    pub inner_iterations: usize,
    /// Number of outer loop iterations performed
    pub outer_iterations: usize,
}

/// STL Decomposition: Seasonal and Trend decomposition using LOESS
pub fn stl_decompose(
    data: &FdMatrix,
    period: usize,
    s_window: Option<usize>,
    t_window: Option<usize>,
    l_window: Option<usize>,
    robust: bool,
    inner_iterations: Option<usize>,
    outer_iterations: Option<usize>,
) -> StlResult {
    let (n, m) = data.shape();
    if n == 0 || m < 2 * period || period < 2 {
        return StlResult {
            trend: FdMatrix::zeros(n, m),
            seasonal: FdMatrix::zeros(n, m),
            remainder: FdMatrix::from_slice(data.as_slice(), n, m)
                .unwrap_or_else(|| FdMatrix::zeros(n, m)),
            weights: FdMatrix::from_column_major(vec![1.0; n * m], n, m)
                .unwrap_or_else(|| FdMatrix::zeros(n, m)),
            period,
            s_window: 0,
            t_window: 0,
            inner_iterations: 0,
            outer_iterations: 0,
        };
    }
    let s_win = s_window.unwrap_or(7).max(3) | 1;
    let t_win = t_window.unwrap_or_else(|| {
        let ratio = 1.5 * period as f64 / (1.0 - 1.5 / s_win as f64);
        let val = ratio.ceil() as usize;
        val.max(3) | 1
    });
    let l_win = l_window.unwrap_or(period) | 1;
    let n_inner = inner_iterations.unwrap_or(2);
    let n_outer = outer_iterations.unwrap_or(if robust { 15 } else { 1 });
    let results: Vec<(Vec<f64>, Vec<f64>, Vec<f64>, Vec<f64>)> = iter_maybe_parallel!(0..n)
        .map(|i| {
            let curve: Vec<f64> = (0..m).map(|j| data[(i, j)]).collect();
            stl_single_series(
                &curve, period, s_win, t_win, l_win, robust, n_inner, n_outer,
            )
        })
        .collect();
    let mut trend = FdMatrix::zeros(n, m);
    let mut seasonal = FdMatrix::zeros(n, m);
    let mut remainder = FdMatrix::zeros(n, m);
    let mut weights = FdMatrix::from_column_major(vec![1.0; n * m], n, m).unwrap();
    for (i, (t, s, r, w)) in results.into_iter().enumerate() {
        for j in 0..m {
            trend[(i, j)] = t[j];
            seasonal[(i, j)] = s[j];
            remainder[(i, j)] = r[j];
            weights[(i, j)] = w[j];
        }
    }
    StlResult {
        trend,
        seasonal,
        remainder,
        weights,
        period,
        s_window: s_win,
        t_window: t_win,
        inner_iterations: n_inner,
        outer_iterations: n_outer,
    }
}

fn stl_single_series(
    data: &[f64],
    period: usize,
    s_window: usize,
    t_window: usize,
    l_window: usize,
    robust: bool,
    n_inner: usize,
    n_outer: usize,
) -> (Vec<f64>, Vec<f64>, Vec<f64>, Vec<f64>) {
    let m = data.len();
    let mut trend = vec![0.0; m];
    let mut seasonal = vec![0.0; m];
    let mut weights = vec![1.0; m];
    for _outer in 0..n_outer {
        for _inner in 0..n_inner {
            let detrended: Vec<f64> = data
                .iter()
                .zip(trend.iter())
                .map(|(&y, &t)| y - t)
                .collect();
            let cycle_smoothed = smooth_cycle_subseries(&detrended, period, s_window, &weights);
            let low_pass = stl_lowpass_filter(&cycle_smoothed, period, l_window);
            seasonal = cycle_smoothed
                .iter()
                .zip(low_pass.iter())
                .map(|(&c, &l)| c - l)
                .collect();
            let deseasonalized: Vec<f64> = data
                .iter()
                .zip(seasonal.iter())
                .map(|(&y, &s)| y - s)
                .collect();
            trend = weighted_loess(&deseasonalized, t_window, &weights);
        }
        if robust && _outer < n_outer - 1 {
            let remainder: Vec<f64> = data
                .iter()
                .zip(trend.iter())
                .zip(seasonal.iter())
                .map(|((&y, &t), &s)| y - t - s)
                .collect();
            weights = compute_robustness_weights(&remainder);
        }
    }
    let remainder: Vec<f64> = data
        .iter()
        .zip(trend.iter())
        .zip(seasonal.iter())
        .map(|((&y, &t), &s)| y - t - s)
        .collect();
    (trend, seasonal, remainder, weights)
}

fn smooth_cycle_subseries(
    data: &[f64],
    period: usize,
    s_window: usize,
    weights: &[f64],
) -> Vec<f64> {
    let m = data.len();
    let n_cycles = m.div_ceil(period);
    let mut result = vec![0.0; m];
    for pos in 0..period {
        let mut subseries_idx: Vec<usize> = Vec::new();
        let mut subseries_vals: Vec<f64> = Vec::new();
        let mut subseries_weights: Vec<f64> = Vec::new();
        for cycle in 0..n_cycles {
            let idx = cycle * period + pos;
            if idx < m {
                subseries_idx.push(idx);
                subseries_vals.push(data[idx]);
                subseries_weights.push(weights[idx]);
            }
        }
        if subseries_vals.is_empty() {
            continue;
        }
        let smoothed = weighted_loess(&subseries_vals, s_window, &subseries_weights);
        for (i, &idx) in subseries_idx.iter().enumerate() {
            result[idx] = smoothed[i];
        }
    }
    result
}

fn stl_lowpass_filter(data: &[f64], period: usize, _l_window: usize) -> Vec<f64> {
    let ma1 = moving_average(data, period);
    let ma2 = moving_average(&ma1, period);
    moving_average(&ma2, 3)
}

fn moving_average(data: &[f64], window: usize) -> Vec<f64> {
    let m = data.len();
    if m == 0 || window == 0 {
        return data.to_vec();
    }
    let half = window / 2;
    let mut result = vec![0.0; m];
    for i in 0..m {
        let start = i.saturating_sub(half);
        let end = (i + half + 1).min(m);
        let sum: f64 = data[start..end].iter().sum();
        let count = (end - start) as f64;
        result[i] = sum / count;
    }
    result
}

fn weighted_loess(data: &[f64], window: usize, weights: &[f64]) -> Vec<f64> {
    let m = data.len();
    if m == 0 {
        return vec![];
    }
    let half = window / 2;
    let mut result = vec![0.0; m];
    for i in 0..m {
        let start = i.saturating_sub(half);
        let end = (i + half + 1).min(m);
        let mut sum_w = 0.0;
        let mut sum_wx = 0.0;
        let mut sum_wy = 0.0;
        let mut sum_wxx = 0.0;
        let mut sum_wxy = 0.0;
        for j in start..end {
            let dist = (j as f64 - i as f64).abs() / (half.max(1) as f64);
            let tricube = if dist < 1.0 {
                (1.0 - dist.powi(3)).powi(3)
            } else {
                0.0
            };
            let w = tricube * weights[j];
            let x = j as f64;
            let y = data[j];
            sum_w += w;
            sum_wx += w * x;
            sum_wy += w * y;
            sum_wxx += w * x * x;
            sum_wxy += w * x * y;
        }
        if sum_w > 1e-10 {
            let denom = sum_w * sum_wxx - sum_wx * sum_wx;
            if denom.abs() > 1e-10 {
                let intercept = (sum_wxx * sum_wy - sum_wx * sum_wxy) / denom;
                let slope = (sum_w * sum_wxy - sum_wx * sum_wy) / denom;
                result[i] = intercept + slope * i as f64;
            } else {
                result[i] = sum_wy / sum_w;
            }
        } else {
            result[i] = data[i];
        }
    }
    result
}

fn compute_robustness_weights(residuals: &[f64]) -> Vec<f64> {
    let m = residuals.len();
    if m == 0 {
        return vec![];
    }
    let mut abs_residuals: Vec<f64> = residuals.iter().map(|&r| r.abs()).collect();
    abs_residuals.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let median_idx = m / 2;
    let mad = if m % 2 == 0 {
        (abs_residuals[median_idx - 1] + abs_residuals[median_idx]) / 2.0
    } else {
        abs_residuals[median_idx]
    };
    let h = 6.0 * mad.max(1e-10);
    residuals
        .iter()
        .map(|&r| {
            let u = r.abs() / h;
            if u < 1.0 {
                (1.0 - u * u).powi(2)
            } else {
                0.0
            }
        })
        .collect()
}

/// Wrapper function for functional data STL decomposition.
pub fn stl_fdata(
    data: &FdMatrix,
    _argvals: &[f64],
    period: usize,
    s_window: Option<usize>,
    t_window: Option<usize>,
    robust: bool,
) -> StlResult {
    stl_decompose(data, period, s_window, t_window, None, robust, None, None)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::f64::consts::PI;

    #[test]
    fn test_detrend_linear_removes_linear_trend() {
        let m = 100;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64 * 10.0).collect();
        let data_vec: Vec<f64> = argvals
            .iter()
            .map(|&t| 2.0 + 0.5 * t + (2.0 * PI * t / 2.0).sin())
            .collect();
        let data = FdMatrix::from_column_major(data_vec, 1, m).unwrap();
        let result = detrend_linear(&data, &argvals);
        let expected: Vec<f64> = argvals
            .iter()
            .map(|&t| (2.0 * PI * t / 2.0).sin())
            .collect();
        let mut max_diff = 0.0f64;
        for j in 0..m {
            let diff = (result.detrended[(0, j)] - expected[j]).abs();
            max_diff = max_diff.max(diff);
        }
        assert!(max_diff < 0.2, "Max difference: {}", max_diff);
    }

    #[test]
    fn test_detrend_polynomial_removes_quadratic_trend() {
        let m = 100;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64 * 10.0).collect();
        let data_vec: Vec<f64> = argvals
            .iter()
            .map(|&t| 1.0 + 0.5 * t - 0.1 * t * t + (2.0 * PI * t / 2.0).sin())
            .collect();
        let data = FdMatrix::from_column_major(data_vec, 1, m).unwrap();
        let result = detrend_polynomial(&data, &argvals, 2);
        let expected: Vec<f64> = argvals
            .iter()
            .map(|&t| (2.0 * PI * t / 2.0).sin())
            .collect();
        let detrended_vec: Vec<f64> = (0..m).map(|j| result.detrended[(0, j)]).collect();
        let mean_det: f64 = detrended_vec.iter().sum::<f64>() / m as f64;
        let mean_exp: f64 = expected.iter().sum::<f64>() / m as f64;
        let mut num = 0.0;
        let mut den_det = 0.0;
        let mut den_exp = 0.0;
        for j in 0..m {
            num += (detrended_vec[j] - mean_det) * (expected[j] - mean_exp);
            den_det += (detrended_vec[j] - mean_det).powi(2);
            den_exp += (expected[j] - mean_exp).powi(2);
        }
        let corr = num / (den_det.sqrt() * den_exp.sqrt());
        assert!(corr > 0.95, "Correlation: {}", corr);
    }

    #[test]
    fn test_detrend_diff1() {
        let m = 100;
        let data_vec: Vec<f64> = {
            let mut v = vec![0.0; m];
            v[0] = 1.0;
            for i in 1..m {
                v[i] = v[i - 1] + 0.1 * (i as f64).sin();
            }
            v
        };
        let data = FdMatrix::from_column_major(data_vec.clone(), 1, m).unwrap();
        let result = detrend_diff(&data, 1);
        for j in 0..m - 1 {
            let expected = data_vec[j + 1] - data_vec[j];
            assert!(
                (result.detrended[(0, j)] - expected).abs() < 1e-10,
                "Mismatch at {}: {} vs {}",
                j,
                result.detrended[(0, j)],
                expected
            );
        }
    }

    #[test]
    fn test_auto_detrend_selects_linear_for_linear_data() {
        let m = 100;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64).collect();
        let data_vec: Vec<f64> = argvals.iter().map(|&t| 2.0 + 0.5 * t).collect();
        let data = FdMatrix::from_column_major(data_vec, 1, m).unwrap();
        let result = auto_detrend(&data, &argvals);
        assert!(
            result.method.contains("linear") || result.method.contains("polynomial"),
            "Method: {}",
            result.method
        );
    }

    #[test]
    fn test_detrend_loess_removes_linear_trend() {
        let m = 100;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64 * 10.0).collect();
        let data_vec: Vec<f64> = argvals
            .iter()
            .map(|&t| 2.0 + 0.5 * t + (2.0 * PI * t / 2.0).sin())
            .collect();
        let data = FdMatrix::from_column_major(data_vec, 1, m).unwrap();
        let result = detrend_loess(&data, &argvals, 0.3, 1);
        let expected: Vec<f64> = argvals
            .iter()
            .map(|&t| (2.0 * PI * t / 2.0).sin())
            .collect();
        let detrended_vec: Vec<f64> = (0..m).map(|j| result.detrended[(0, j)]).collect();
        let mean_det: f64 = detrended_vec.iter().sum::<f64>() / m as f64;
        let mean_exp: f64 = expected.iter().sum::<f64>() / m as f64;
        let mut num = 0.0;
        let mut den_det = 0.0;
        let mut den_exp = 0.0;
        for j in 0..m {
            num += (detrended_vec[j] - mean_det) * (expected[j] - mean_exp);
            den_det += (detrended_vec[j] - mean_det).powi(2);
            den_exp += (expected[j] - mean_exp).powi(2);
        }
        let corr = num / (den_det.sqrt() * den_exp.sqrt());
        assert!(corr > 0.9, "Correlation: {}", corr);
        assert_eq!(result.method, "loess");
    }

    #[test]
    fn test_detrend_loess_removes_quadratic_trend() {
        let m = 100;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64 * 10.0).collect();
        let data_vec: Vec<f64> = argvals
            .iter()
            .map(|&t| 1.0 + 0.3 * t - 0.05 * t * t + (2.0 * PI * t / 2.0).sin())
            .collect();
        let data = FdMatrix::from_column_major(data_vec, 1, m).unwrap();
        let result = detrend_loess(&data, &argvals, 0.3, 2);
        assert_eq!(result.trend.ncols(), m);
        assert_eq!(result.detrended.ncols(), m);
        assert!(result.rss[0] > 0.0);
    }

    #[test]
    fn test_detrend_loess_different_bandwidths() {
        let m = 100;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64 * 10.0).collect();
        let data_vec: Vec<f64> = argvals
            .iter()
            .enumerate()
            .map(|(i, &t)| (2.0 * PI * t / 2.0).sin() + 0.1 * ((i * 17) % 100) as f64 / 100.0)
            .collect();
        let data = FdMatrix::from_column_major(data_vec, 1, m).unwrap();
        let result_small = detrend_loess(&data, &argvals, 0.1, 1);
        let result_large = detrend_loess(&data, &argvals, 0.5, 1);
        assert_eq!(result_small.trend.ncols(), m);
        assert_eq!(result_large.trend.ncols(), m);
        assert!(result_large.n_params >= result_small.n_params);
    }

    #[test]
    fn test_detrend_loess_short_series() {
        let m = 10;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64).collect();
        let data_vec: Vec<f64> = argvals.iter().map(|&t| t * 2.0).collect();
        let data = FdMatrix::from_column_major(data_vec, 1, m).unwrap();
        let result = detrend_loess(&data, &argvals, 0.3, 1);
        assert_eq!(result.trend.ncols(), m);
        assert_eq!(result.detrended.ncols(), m);
    }

    #[test]
    fn test_decompose_additive_separates_components() {
        let m = 200;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64 * 10.0).collect();
        let data_vec: Vec<f64> = argvals
            .iter()
            .map(|&t| 2.0 + 0.5 * t + (2.0 * PI * t / period).sin())
            .collect();
        let data = FdMatrix::from_column_major(data_vec.clone(), 1, m).unwrap();
        let result = decompose_additive(&data, &argvals, period, "loess", 0.3, 3);
        assert_eq!(result.trend.ncols(), m);
        assert_eq!(result.seasonal.ncols(), m);
        assert_eq!(result.remainder.ncols(), m);
        assert_eq!(result.method, "additive");
        assert_eq!(result.period, period);
        for j in 0..m {
            let reconstructed =
                result.trend[(0, j)] + result.seasonal[(0, j)] + result.remainder[(0, j)];
            assert!(
                (reconstructed - data_vec[j]).abs() < 0.5,
                "Reconstruction error at {}: {} vs {}",
                j,
                reconstructed,
                data_vec[j]
            );
        }
    }

    #[test]
    fn test_decompose_additive_different_harmonics() {
        let m = 200;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64 * 10.0).collect();
        let data_vec: Vec<f64> = argvals
            .iter()
            .map(|&t| 1.0 + (2.0 * PI * t / period).sin())
            .collect();
        let data = FdMatrix::from_column_major(data_vec, 1, m).unwrap();
        let result1 = decompose_additive(&data, &argvals, period, "loess", 0.3, 1);
        let result5 = decompose_additive(&data, &argvals, period, "loess", 0.3, 5);
        assert_eq!(result1.seasonal.ncols(), m);
        assert_eq!(result5.seasonal.ncols(), m);
    }

    #[test]
    fn test_decompose_additive_residual_properties() {
        let m = 200;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64 * 10.0).collect();
        let data_vec: Vec<f64> = argvals
            .iter()
            .map(|&t| 2.0 + 0.3 * t + (2.0 * PI * t / period).sin())
            .collect();
        let data = FdMatrix::from_column_major(data_vec.clone(), 1, m).unwrap();
        let result = decompose_additive(&data, &argvals, period, "loess", 0.3, 3);
        let remainder_vec: Vec<f64> = (0..m).map(|j| result.remainder[(0, j)]).collect();
        let mean_rem: f64 = remainder_vec.iter().sum::<f64>() / m as f64;
        assert!(mean_rem.abs() < 0.5, "Remainder mean: {}", mean_rem);
        let data_mean: f64 = data_vec.iter().sum::<f64>() / m as f64;
        let var_data: f64 = data_vec
            .iter()
            .map(|&x| (x - data_mean).powi(2))
            .sum::<f64>()
            / m as f64;
        let var_rem: f64 = remainder_vec
            .iter()
            .map(|&x| (x - mean_rem).powi(2))
            .sum::<f64>()
            / m as f64;
        assert!(
            var_rem < var_data,
            "Remainder variance {} should be < data variance {}",
            var_rem,
            var_data
        );
    }

    #[test]
    fn test_decompose_additive_multi_sample() {
        let n = 3;
        let m = 100;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64 * 10.0).collect();
        let mut data = FdMatrix::zeros(n, m);
        for i in 0..n {
            let amp = (i + 1) as f64;
            for j in 0..m {
                data[(i, j)] =
                    1.0 + 0.1 * argvals[j] + amp * (2.0 * PI * argvals[j] / period).sin();
            }
        }
        let result = decompose_additive(&data, &argvals, period, "loess", 0.3, 2);
        assert_eq!(result.trend.shape(), (n, m));
        assert_eq!(result.seasonal.shape(), (n, m));
        assert_eq!(result.remainder.shape(), (n, m));
    }

    #[test]
    fn test_decompose_multiplicative_basic() {
        let m = 200;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64 * 10.0).collect();
        let data_vec: Vec<f64> = argvals
            .iter()
            .map(|&t| (2.0 + 0.1 * t) * (1.0 + 0.3 * (2.0 * PI * t / period).sin()))
            .collect();
        let data = FdMatrix::from_column_major(data_vec, 1, m).unwrap();
        let result = decompose_multiplicative(&data, &argvals, period, "loess", 0.3, 3);
        assert_eq!(result.trend.ncols(), m);
        assert_eq!(result.seasonal.ncols(), m);
        assert_eq!(result.remainder.ncols(), m);
        assert_eq!(result.method, "multiplicative");
        let seasonal_vec: Vec<f64> = (0..m).map(|j| result.seasonal[(0, j)]).collect();
        let mean_seasonal: f64 = seasonal_vec.iter().sum::<f64>() / m as f64;
        assert!(
            (mean_seasonal - 1.0).abs() < 0.5,
            "Mean seasonal factor: {}",
            mean_seasonal
        );
    }

    #[test]
    fn test_decompose_multiplicative_non_positive_data() {
        let m = 100;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64 * 10.0).collect();
        let data_vec: Vec<f64> = argvals
            .iter()
            .map(|&t| -1.0 + (2.0 * PI * t / period).sin())
            .collect();
        let data = FdMatrix::from_column_major(data_vec, 1, m).unwrap();
        let result = decompose_multiplicative(&data, &argvals, period, "loess", 0.3, 2);
        assert_eq!(result.trend.ncols(), m);
        assert_eq!(result.seasonal.ncols(), m);
        for j in 0..m {
            let s = result.seasonal[(0, j)];
            assert!(s.is_finite(), "Seasonal should be finite");
        }
    }

    #[test]
    fn test_decompose_multiplicative_vs_additive() {
        let m = 200;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64 * 10.0).collect();
        let data_vec: Vec<f64> = argvals
            .iter()
            .map(|&t| 5.0 + (2.0 * PI * t / period).sin())
            .collect();
        let data = FdMatrix::from_column_major(data_vec, 1, m).unwrap();
        let add_result = decompose_additive(&data, &argvals, period, "loess", 0.3, 3);
        let mult_result = decompose_multiplicative(&data, &argvals, period, "loess", 0.3, 3);
        assert_eq!(add_result.seasonal.ncols(), m);
        assert_eq!(mult_result.seasonal.ncols(), m);
        let add_seasonal_vec: Vec<f64> = (0..m).map(|j| add_result.seasonal[(0, j)]).collect();
        let add_mean: f64 = add_seasonal_vec.iter().sum::<f64>() / m as f64;
        let mult_seasonal_vec: Vec<f64> = (0..m).map(|j| mult_result.seasonal[(0, j)]).collect();
        let mult_mean: f64 = mult_seasonal_vec.iter().sum::<f64>() / m as f64;
        assert!(
            add_mean.abs() < mult_mean,
            "Additive mean {} vs mult mean {}",
            add_mean,
            mult_mean
        );
    }

    #[test]
    fn test_decompose_multiplicative_edge_cases() {
        let empty = FdMatrix::zeros(0, 0);
        let result = decompose_multiplicative(&empty, &[], 2.0, "loess", 0.3, 2);
        assert_eq!(result.trend.len(), 0);
        let m = 5;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64).collect();
        let data = FdMatrix::from_column_major(vec![1.0, 2.0, 3.0, 2.0, 1.0], 1, m).unwrap();
        let result = decompose_multiplicative(&data, &argvals, 2.0, "loess", 0.3, 1);
        assert_eq!(result.remainder.ncols(), m);
    }

    #[test]
    fn test_stl_decompose_basic() {
        let period = 12;
        let n_cycles = 10;
        let m = period * n_cycles;
        let data_vec: Vec<f64> = (0..m)
            .map(|i| {
                let t = i as f64;
                0.01 * t + (2.0 * PI * t / period as f64).sin()
            })
            .collect();
        let data = FdMatrix::from_column_major(data_vec.clone(), 1, m).unwrap();
        let result = stl_decompose(&data, period, None, None, None, false, None, None);
        assert_eq!(result.trend.ncols(), m);
        assert_eq!(result.seasonal.ncols(), m);
        assert_eq!(result.remainder.ncols(), m);
        assert_eq!(result.period, period);
        for j in 0..m {
            let reconstructed =
                result.trend[(0, j)] + result.seasonal[(0, j)] + result.remainder[(0, j)];
            assert!(
                (reconstructed - data_vec[j]).abs() < 1e-8,
                "Reconstruction error at {}: {} vs {}",
                j,
                reconstructed,
                data_vec[j]
            );
        }
    }

    #[test]
    fn test_stl_decompose_robust() {
        let period = 12;
        let n_cycles = 10;
        let m = period * n_cycles;
        let mut data_vec: Vec<f64> = (0..m)
            .map(|i| {
                let t = i as f64;
                0.01 * t + (2.0 * PI * t / period as f64).sin()
            })
            .collect();
        data_vec[30] += 10.0;
        data_vec[60] += 10.0;
        let data = FdMatrix::from_column_major(data_vec, 1, m).unwrap();
        let result = stl_decompose(&data, period, None, None, None, true, None, Some(5));
        assert!(
            result.weights[(0, 30)] < 1.0,
            "Weight at outlier should be < 1.0: {}",
            result.weights[(0, 30)]
        );
        assert!(
            result.weights[(0, 60)] < 1.0,
            "Weight at outlier should be < 1.0: {}",
            result.weights[(0, 60)]
        );
        let non_outlier_weight = result.weights[(0, 15)];
        assert!(
            non_outlier_weight > result.weights[(0, 30)],
            "Non-outlier weight {} should be > outlier weight {}",
            non_outlier_weight,
            result.weights[(0, 30)]
        );
    }

    #[test]
    fn test_stl_decompose_default_params() {
        let period = 10;
        let m = period * 8;
        let data_vec: Vec<f64> = (0..m)
            .map(|i| (2.0 * PI * i as f64 / period as f64).sin())
            .collect();
        let data = FdMatrix::from_column_major(data_vec, 1, m).unwrap();
        let result = stl_decompose(&data, period, None, None, None, false, None, None);
        assert_eq!(result.trend.ncols(), m);
        assert_eq!(result.seasonal.ncols(), m);
        assert!(result.s_window >= 3);
        assert!(result.t_window >= 3);
        assert_eq!(result.inner_iterations, 2);
        assert_eq!(result.outer_iterations, 1);
    }

    #[test]
    fn test_stl_decompose_invalid() {
        let data = FdMatrix::from_column_major(vec![1.0, 2.0], 1, 2).unwrap();
        let result = stl_decompose(&data, 1, None, None, None, false, None, None);
        assert_eq!(result.s_window, 0);
        let data = FdMatrix::from_column_major(vec![1.0, 2.0, 3.0], 1, 3).unwrap();
        let result = stl_decompose(&data, 5, None, None, None, false, None, None);
        assert_eq!(result.s_window, 0);
        let data = FdMatrix::zeros(0, 0);
        let result = stl_decompose(&data, 10, None, None, None, false, None, None);
        assert_eq!(result.trend.len(), 0);
    }

    #[test]
    fn test_stl_fdata() {
        let n = 3;
        let period = 10;
        let m = period * 5;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64).collect();
        let mut data = FdMatrix::zeros(n, m);
        for i in 0..n {
            let amp = (i + 1) as f64;
            for j in 0..m {
                data[(i, j)] = amp * (2.0 * PI * argvals[j] / period as f64).sin();
            }
        }
        let result = stl_fdata(&data, &argvals, period, None, None, false);
        assert_eq!(result.trend.shape(), (n, m));
        assert_eq!(result.seasonal.shape(), (n, m));
        assert_eq!(result.remainder.shape(), (n, m));
        for i in 0..n {
            for j in 0..m {
                let reconstructed =
                    result.trend[(i, j)] + result.seasonal[(i, j)] + result.remainder[(i, j)];
                assert!(
                    (reconstructed - data[(i, j)]).abs() < 1e-8,
                    "Reconstruction error for sample {} at {}: {} vs {}",
                    i,
                    j,
                    reconstructed,
                    data[(i, j)]
                );
            }
        }
    }

    #[test]
    fn test_stl_decompose_multi_sample() {
        let n = 5;
        let period = 10;
        let m = period * 6;
        let mut data = FdMatrix::zeros(n, m);
        for i in 0..n {
            let offset = i as f64 * 0.5;
            for j in 0..m {
                data[(i, j)] =
                    offset + 0.01 * j as f64 + (2.0 * PI * j as f64 / period as f64).sin();
            }
        }
        let result = stl_decompose(&data, period, None, None, None, false, None, None);
        assert_eq!(result.trend.shape(), (n, m));
        assert_eq!(result.seasonal.shape(), (n, m));
        assert_eq!(result.remainder.shape(), (n, m));
        assert_eq!(result.weights.shape(), (n, m));
    }

    #[test]
    fn test_detrend_diff_order2() {
        let m = 50;
        let data_vec: Vec<f64> = (0..m).map(|i| (i as f64).powi(2)).collect();
        let data = FdMatrix::from_column_major(data_vec, 1, m).unwrap();
        let result = detrend_diff(&data, 2);
        for j in 0..m - 2 {
            assert!(
                (result.detrended[(0, j)] - 2.0).abs() < 1e-10,
                "Second diff at {}: expected 2.0, got {}",
                j,
                result.detrended[(0, j)]
            );
        }
    }

    #[test]
    fn test_detrend_polynomial_degree3() {
        let m = 100;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64 * 5.0).collect();
        let data_vec: Vec<f64> = argvals
            .iter()
            .map(|&t| 1.0 + 2.0 * t - 0.5 * t * t + 0.1 * t * t * t)
            .collect();
        let data = FdMatrix::from_column_major(data_vec, 1, m).unwrap();
        let result = detrend_polynomial(&data, &argvals, 3);
        assert_eq!(result.method, "polynomial(3)");
        assert!(result.coefficients.is_some());
        let max_detrend: f64 = (0..m)
            .map(|j| result.detrended[(0, j)].abs())
            .fold(0.0, f64::max);
        assert!(
            max_detrend < 0.1,
            "Pure cubic should be nearly zero after degree-3 detrend: {}",
            max_detrend
        );
    }

    #[test]
    fn test_detrend_loess_invalid() {
        let data = FdMatrix::from_column_major(vec![1.0, 2.0, 3.0, 4.0, 5.0], 1, 5).unwrap();
        let argvals = vec![0.0, 1.0, 2.0, 3.0, 4.0];
        let result = detrend_loess(&data, &argvals, -0.1, 1);
        assert_eq!(result.detrended.as_slice(), data.as_slice());
        let data2 = FdMatrix::from_column_major(vec![1.0, 2.0], 1, 2).unwrap();
        let result = detrend_loess(&data2, &[0.0, 1.0], 0.3, 1);
        assert_eq!(result.detrended.as_slice(), &[1.0, 2.0]);
    }
}
