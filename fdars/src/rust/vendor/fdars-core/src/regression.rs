//! Regression functions for functional data.
//!
//! This module provides functional PCA, PLS, and ridge regression.

use crate::iter_maybe_parallel;
use crate::matrix::FdMatrix;
#[cfg(feature = "linalg")]
use anofox_regression::solvers::RidgeRegressor;
#[cfg(feature = "linalg")]
use anofox_regression::{FittedRegressor, Regressor};
use nalgebra::SVD;
#[cfg(feature = "parallel")]
use rayon::iter::ParallelIterator;

/// Result of functional PCA.
pub struct FpcaResult {
    /// Singular values
    pub singular_values: Vec<f64>,
    /// Rotation matrix (loadings), m x ncomp
    pub rotation: FdMatrix,
    /// Scores matrix, n x ncomp
    pub scores: FdMatrix,
    /// Mean function
    pub mean: Vec<f64>,
    /// Centered data, n x m
    pub centered: FdMatrix,
}

/// Center columns of a matrix and return (centered_matrix, column_means).
fn center_columns(data: &FdMatrix) -> (FdMatrix, Vec<f64>) {
    let (n, m) = data.shape();
    let means: Vec<f64> = iter_maybe_parallel!(0..m)
        .map(|j| {
            let col = data.column(j);
            let sum: f64 = col.iter().sum();
            sum / n as f64
        })
        .collect();

    let mut centered = FdMatrix::zeros(n, m);
    for j in 0..m {
        for i in 0..n {
            centered[(i, j)] = data[(i, j)] - means[j];
        }
    }
    (centered, means)
}

/// Extract rotation (V) and scores (U*S) from SVD results.
fn extract_pc_components(
    svd: &SVD<f64, nalgebra::Dyn, nalgebra::Dyn>,
    n: usize,
    m: usize,
    ncomp: usize,
) -> Option<(Vec<f64>, FdMatrix, FdMatrix)> {
    let singular_values: Vec<f64> = svd.singular_values.iter().take(ncomp).cloned().collect();

    let v_t = svd.v_t.as_ref()?;
    let mut rotation = FdMatrix::zeros(m, ncomp);
    for k in 0..ncomp {
        for j in 0..m {
            rotation[(j, k)] = v_t[(k, j)];
        }
    }

    let u = svd.u.as_ref()?;
    let mut scores = FdMatrix::zeros(n, ncomp);
    for k in 0..ncomp {
        let sv_k = singular_values[k];
        for i in 0..n {
            scores[(i, k)] = u[(i, k)] * sv_k;
        }
    }

    Some((singular_values, rotation, scores))
}

/// Perform functional PCA via SVD on centered data.
///
/// # Arguments
/// * `data` - Matrix (n x m): n observations, m evaluation points
/// * `ncomp` - Number of components to extract
pub fn fdata_to_pc_1d(data: &FdMatrix, ncomp: usize) -> Option<FpcaResult> {
    let (n, m) = data.shape();
    if n == 0 || m == 0 || ncomp < 1 {
        return None;
    }

    let ncomp = ncomp.min(n).min(m);
    let (centered, means) = center_columns(data);
    let svd = SVD::new(centered.to_dmatrix(), true, true);
    let (singular_values, rotation, scores) = extract_pc_components(&svd, n, m, ncomp)?;

    Some(FpcaResult {
        singular_values,
        rotation,
        scores,
        mean: means,
        centered,
    })
}

/// Result of PLS regression.
pub struct PlsResult {
    /// Weight vectors, m x ncomp
    pub weights: FdMatrix,
    /// Score vectors, n x ncomp
    pub scores: FdMatrix,
    /// Loading vectors, m x ncomp
    pub loadings: FdMatrix,
}

/// Compute PLS weight vector: w = X'y / ||X'y||
fn pls_compute_weights(x_cen: &FdMatrix, y_cen: &[f64]) -> Vec<f64> {
    let (n, m) = x_cen.shape();
    let mut w: Vec<f64> = (0..m)
        .map(|j| {
            let mut sum = 0.0;
            for i in 0..n {
                sum += x_cen[(i, j)] * y_cen[i];
            }
            sum
        })
        .collect();

    let w_norm: f64 = w.iter().map(|&wi| wi * wi).sum::<f64>().sqrt();
    if w_norm > 1e-10 {
        for wi in &mut w {
            *wi /= w_norm;
        }
    }
    w
}

/// Compute PLS scores: t = Xw
fn pls_compute_scores(x_cen: &FdMatrix, w: &[f64]) -> Vec<f64> {
    let (n, m) = x_cen.shape();
    (0..n)
        .map(|i| {
            let mut sum = 0.0;
            for j in 0..m {
                sum += x_cen[(i, j)] * w[j];
            }
            sum
        })
        .collect()
}

/// Compute PLS loadings: p = X't / (t't)
fn pls_compute_loadings(x_cen: &FdMatrix, t: &[f64], t_norm_sq: f64) -> Vec<f64> {
    let (n, m) = x_cen.shape();
    (0..m)
        .map(|j| {
            let mut sum = 0.0;
            for i in 0..n {
                sum += x_cen[(i, j)] * t[i];
            }
            sum / t_norm_sq.max(1e-10)
        })
        .collect()
}

/// Deflate X by removing the rank-1 component t * p'
fn pls_deflate_x(x_cen: &mut FdMatrix, t: &[f64], p: &[f64]) {
    let (n, m) = x_cen.shape();
    for j in 0..m {
        for i in 0..n {
            x_cen[(i, j)] -= t[i] * p[j];
        }
    }
}

/// Execute one NIPALS step: compute weights/scores/loadings and deflate X and y.
fn pls_nipals_step(
    k: usize,
    x_cen: &mut FdMatrix,
    y_cen: &mut [f64],
    weights: &mut FdMatrix,
    scores: &mut FdMatrix,
    loadings: &mut FdMatrix,
) {
    let n = x_cen.nrows();
    let m = x_cen.ncols();

    let w = pls_compute_weights(x_cen, y_cen);
    let t = pls_compute_scores(x_cen, &w);
    let t_norm_sq: f64 = t.iter().map(|&ti| ti * ti).sum();
    let p = pls_compute_loadings(x_cen, &t, t_norm_sq);

    for j in 0..m {
        weights[(j, k)] = w[j];
        loadings[(j, k)] = p[j];
    }
    for i in 0..n {
        scores[(i, k)] = t[i];
    }

    pls_deflate_x(x_cen, &t, &p);
    let t_y: f64 = t.iter().zip(y_cen.iter()).map(|(&ti, &yi)| ti * yi).sum();
    let q = t_y / t_norm_sq.max(1e-10);
    for i in 0..n {
        y_cen[i] -= t[i] * q;
    }
}

/// Perform PLS via NIPALS algorithm.
///
/// # Arguments
/// * `data` - Matrix (n x m): n observations, m evaluation points
/// * `y` - Response vector (length n)
/// * `ncomp` - Number of components to extract
pub fn fdata_to_pls_1d(data: &FdMatrix, y: &[f64], ncomp: usize) -> Option<PlsResult> {
    let (n, m) = data.shape();
    if n == 0 || m == 0 || y.len() != n || ncomp < 1 {
        return None;
    }

    let ncomp = ncomp.min(n).min(m);

    // Center X and y
    let x_means: Vec<f64> = (0..m)
        .map(|j| {
            let col = data.column(j);
            let sum: f64 = col.iter().sum();
            sum / n as f64
        })
        .collect();

    let y_mean: f64 = y.iter().sum::<f64>() / n as f64;

    let mut x_cen = FdMatrix::zeros(n, m);
    for j in 0..m {
        for i in 0..n {
            x_cen[(i, j)] = data[(i, j)] - x_means[j];
        }
    }

    let mut y_cen: Vec<f64> = y.iter().map(|&yi| yi - y_mean).collect();

    let mut weights = FdMatrix::zeros(m, ncomp);
    let mut scores = FdMatrix::zeros(n, ncomp);
    let mut loadings = FdMatrix::zeros(m, ncomp);

    // NIPALS algorithm
    for k in 0..ncomp {
        pls_nipals_step(
            k,
            &mut x_cen,
            &mut y_cen,
            &mut weights,
            &mut scores,
            &mut loadings,
        );
    }

    Some(PlsResult {
        weights,
        scores,
        loadings,
    })
}

/// Result of ridge regression fit.
#[cfg(feature = "linalg")]
pub struct RidgeResult {
    /// Coefficients
    pub coefficients: Vec<f64>,
    /// Intercept
    pub intercept: f64,
    /// Fitted values
    pub fitted_values: Vec<f64>,
    /// Residuals
    pub residuals: Vec<f64>,
    /// R-squared
    pub r_squared: f64,
    /// Lambda used
    pub lambda: f64,
    /// Error message if any
    pub error: Option<String>,
}

/// Fit ridge regression.
///
/// # Arguments
/// * `x` - Predictor matrix (n x m)
/// * `y` - Response vector
/// * `lambda` - Regularization parameter
/// * `with_intercept` - Whether to include intercept
#[cfg(feature = "linalg")]
pub fn ridge_regression_fit(
    x: &FdMatrix,
    y: &[f64],
    lambda: f64,
    with_intercept: bool,
) -> RidgeResult {
    let (n, m) = x.shape();
    if n == 0 || m == 0 || y.len() != n {
        return RidgeResult {
            coefficients: Vec::new(),
            intercept: 0.0,
            fitted_values: Vec::new(),
            residuals: Vec::new(),
            r_squared: 0.0,
            lambda,
            error: Some("Invalid input dimensions".to_string()),
        };
    }

    // Convert to faer Mat format
    let x_faer = faer::Mat::from_fn(n, m, |i, j| x[(i, j)]);
    let y_faer = faer::Col::from_fn(n, |i| y[i]);

    // Build and fit the ridge regressor
    let regressor = RidgeRegressor::builder()
        .with_intercept(with_intercept)
        .lambda(lambda)
        .build();

    let fitted = match regressor.fit(&x_faer, &y_faer) {
        Ok(f) => f,
        Err(e) => {
            return RidgeResult {
                coefficients: Vec::new(),
                intercept: 0.0,
                fitted_values: Vec::new(),
                residuals: Vec::new(),
                r_squared: 0.0,
                lambda,
                error: Some(format!("Fit failed: {:?}", e)),
            }
        }
    };

    // Extract coefficients
    let coefs = fitted.coefficients();
    let coefficients: Vec<f64> = (0..coefs.nrows()).map(|i| coefs[i]).collect();

    // Get intercept
    let intercept = fitted.intercept().unwrap_or(0.0);

    // Compute fitted values
    let mut fitted_values = vec![0.0; n];
    for i in 0..n {
        let mut pred = intercept;
        for j in 0..m {
            pred += x[(i, j)] * coefficients[j];
        }
        fitted_values[i] = pred;
    }

    // Compute residuals
    let residuals: Vec<f64> = y
        .iter()
        .zip(fitted_values.iter())
        .map(|(&yi, &yhat)| yi - yhat)
        .collect();

    // Compute R-squared
    let y_mean: f64 = y.iter().sum::<f64>() / n as f64;
    let ss_tot: f64 = y.iter().map(|&yi| (yi - y_mean).powi(2)).sum();
    let ss_res: f64 = residuals.iter().map(|&r| r.powi(2)).sum();
    let r_squared = if ss_tot > 0.0 {
        1.0 - ss_res / ss_tot
    } else {
        0.0
    };

    RidgeResult {
        coefficients,
        intercept,
        fitted_values,
        residuals,
        r_squared,
        lambda,
        error: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::f64::consts::PI;

    /// Generate functional data with known structure for testing
    fn generate_test_fdata(n: usize, m: usize) -> (FdMatrix, Vec<f64>) {
        let t: Vec<f64> = (0..m).map(|j| j as f64 / (m - 1) as f64).collect();

        // Create n curves: sine waves with varying phase
        let mut data = FdMatrix::zeros(n, m);
        for i in 0..n {
            let phase = (i as f64 / n as f64) * PI;
            for j in 0..m {
                data[(i, j)] = (2.0 * PI * t[j] + phase).sin();
            }
        }

        (data, t)
    }

    // ============== FPCA tests ==============

    #[test]
    fn test_fdata_to_pc_1d_basic() {
        let n = 20;
        let m = 50;
        let ncomp = 3;
        let (data, _) = generate_test_fdata(n, m);

        let result = fdata_to_pc_1d(&data, ncomp);
        assert!(result.is_some());

        let fpca = result.unwrap();
        assert_eq!(fpca.singular_values.len(), ncomp);
        assert_eq!(fpca.rotation.shape(), (m, ncomp));
        assert_eq!(fpca.scores.shape(), (n, ncomp));
        assert_eq!(fpca.mean.len(), m);
        assert_eq!(fpca.centered.shape(), (n, m));
    }

    #[test]
    fn test_fdata_to_pc_1d_singular_values_decreasing() {
        let n = 20;
        let m = 50;
        let ncomp = 5;
        let (data, _) = generate_test_fdata(n, m);

        let fpca = fdata_to_pc_1d(&data, ncomp).unwrap();

        // Singular values should be in decreasing order
        for i in 1..fpca.singular_values.len() {
            assert!(
                fpca.singular_values[i] <= fpca.singular_values[i - 1] + 1e-10,
                "Singular values should be decreasing"
            );
        }
    }

    #[test]
    fn test_fdata_to_pc_1d_centered_has_zero_mean() {
        let n = 20;
        let m = 50;
        let (data, _) = generate_test_fdata(n, m);

        let fpca = fdata_to_pc_1d(&data, 3).unwrap();

        // Column means of centered data should be zero
        for j in 0..m {
            let col_mean: f64 = (0..n).map(|i| fpca.centered[(i, j)]).sum::<f64>() / n as f64;
            assert!(
                col_mean.abs() < 1e-10,
                "Centered data should have zero column mean"
            );
        }
    }

    #[test]
    fn test_fdata_to_pc_1d_ncomp_limits() {
        let n = 10;
        let m = 50;
        let (data, _) = generate_test_fdata(n, m);

        // Request more components than n - should cap at n
        let fpca = fdata_to_pc_1d(&data, 20).unwrap();
        assert!(fpca.singular_values.len() <= n);
    }

    #[test]
    fn test_fdata_to_pc_1d_invalid_input() {
        // Empty data
        let empty = FdMatrix::zeros(0, 50);
        let result = fdata_to_pc_1d(&empty, 3);
        assert!(result.is_none());

        // Zero components
        let (data, _) = generate_test_fdata(10, 50);
        let result = fdata_to_pc_1d(&data, 0);
        assert!(result.is_none());
    }

    #[test]
    fn test_fdata_to_pc_1d_reconstruction() {
        let n = 10;
        let m = 30;
        let (data, _) = generate_test_fdata(n, m);

        // Use all components for perfect reconstruction
        let ncomp = n.min(m);
        let fpca = fdata_to_pc_1d(&data, ncomp).unwrap();

        // Reconstruct: X_centered = scores * rotation^T
        for i in 0..n {
            for j in 0..m {
                let mut reconstructed = 0.0;
                for k in 0..ncomp {
                    let score = fpca.scores[(i, k)];
                    let loading = fpca.rotation[(j, k)];
                    reconstructed += score * loading;
                }
                let original_centered = fpca.centered[(i, j)];
                assert!(
                    (reconstructed - original_centered).abs() < 0.1,
                    "Reconstruction error at ({}, {}): {} vs {}",
                    i,
                    j,
                    reconstructed,
                    original_centered
                );
            }
        }
    }

    // ============== PLS tests ==============

    #[test]
    fn test_fdata_to_pls_1d_basic() {
        let n = 20;
        let m = 30;
        let ncomp = 3;
        let (x, _) = generate_test_fdata(n, m);

        // Create y with some relationship to x
        let y: Vec<f64> = (0..n).map(|i| (i as f64 / n as f64) + 0.1).collect();

        let result = fdata_to_pls_1d(&x, &y, ncomp);
        assert!(result.is_some());

        let pls = result.unwrap();
        assert_eq!(pls.weights.shape(), (m, ncomp));
        assert_eq!(pls.scores.shape(), (n, ncomp));
        assert_eq!(pls.loadings.shape(), (m, ncomp));
    }

    #[test]
    fn test_fdata_to_pls_1d_weights_normalized() {
        let n = 20;
        let m = 30;
        let ncomp = 2;
        let (x, _) = generate_test_fdata(n, m);
        let y: Vec<f64> = (0..n).map(|i| i as f64).collect();

        let pls = fdata_to_pls_1d(&x, &y, ncomp).unwrap();

        // Weight vectors should be approximately unit norm
        for k in 0..ncomp {
            let norm: f64 = (0..m)
                .map(|j| pls.weights[(j, k)].powi(2))
                .sum::<f64>()
                .sqrt();
            assert!(
                (norm - 1.0).abs() < 0.1,
                "Weight vector {} should be unit norm, got {}",
                k,
                norm
            );
        }
    }

    #[test]
    fn test_fdata_to_pls_1d_invalid_input() {
        let (x, _) = generate_test_fdata(10, 30);

        // Wrong y length
        let result = fdata_to_pls_1d(&x, &[0.0; 5], 2);
        assert!(result.is_none());

        // Zero components
        let y = vec![0.0; 10];
        let result = fdata_to_pls_1d(&x, &y, 0);
        assert!(result.is_none());
    }

    // ============== Ridge regression tests ==============

    #[cfg(feature = "linalg")]
    #[test]
    fn test_ridge_regression_fit_basic() {
        let n = 50;
        let m = 5;

        // Create X with known structure
        let mut x = FdMatrix::zeros(n, m);
        for i in 0..n {
            for j in 0..m {
                x[(i, j)] = (i as f64 + j as f64) / (n + m) as f64;
            }
        }

        // Create y = sum of x columns + noise
        let y: Vec<f64> = (0..n)
            .map(|i| {
                let mut sum = 0.0;
                for j in 0..m {
                    sum += x[(i, j)];
                }
                sum + 0.01 * (i as f64 % 10.0)
            })
            .collect();

        let result = ridge_regression_fit(&x, &y, 0.1, true);

        assert!(result.error.is_none(), "Ridge should fit without error");
        assert_eq!(result.coefficients.len(), m);
        assert_eq!(result.fitted_values.len(), n);
        assert_eq!(result.residuals.len(), n);
    }

    #[cfg(feature = "linalg")]
    #[test]
    fn test_ridge_regression_fit_r_squared() {
        let n = 50;
        let m = 3;

        let x = FdMatrix::from_column_major(
            (0..n * m).map(|i| i as f64 / (n * m) as f64).collect(),
            n,
            m,
        )
        .unwrap();
        let y: Vec<f64> = (0..n).map(|i| i as f64 / n as f64).collect();

        let result = ridge_regression_fit(&x, &y, 0.01, true);

        assert!(
            result.r_squared > 0.5,
            "R-squared should be high, got {}",
            result.r_squared
        );
        assert!(result.r_squared <= 1.0 + 1e-10, "R-squared should be <= 1");
    }

    #[cfg(feature = "linalg")]
    #[test]
    fn test_ridge_regression_fit_regularization() {
        let n = 30;
        let m = 10;

        let x = FdMatrix::from_column_major(
            (0..n * m)
                .map(|i| ((i * 17) % 100) as f64 / 100.0)
                .collect(),
            n,
            m,
        )
        .unwrap();
        let y: Vec<f64> = (0..n).map(|i| (i as f64).sin()).collect();

        let low_lambda = ridge_regression_fit(&x, &y, 0.001, true);
        let high_lambda = ridge_regression_fit(&x, &y, 100.0, true);

        let norm_low: f64 = low_lambda
            .coefficients
            .iter()
            .map(|c| c.powi(2))
            .sum::<f64>()
            .sqrt();
        let norm_high: f64 = high_lambda
            .coefficients
            .iter()
            .map(|c| c.powi(2))
            .sum::<f64>()
            .sqrt();

        assert!(
            norm_high <= norm_low + 1e-6,
            "Higher lambda should shrink coefficients: {} vs {}",
            norm_high,
            norm_low
        );
    }

    #[cfg(feature = "linalg")]
    #[test]
    fn test_ridge_regression_fit_residuals() {
        let n = 20;
        let m = 3;

        let x = FdMatrix::from_column_major(
            (0..n * m).map(|i| i as f64 / (n * m) as f64).collect(),
            n,
            m,
        )
        .unwrap();
        let y: Vec<f64> = (0..n).map(|i| i as f64 / n as f64).collect();

        let result = ridge_regression_fit(&x, &y, 0.1, true);

        for i in 0..n {
            let expected_resid = y[i] - result.fitted_values[i];
            assert!(
                (result.residuals[i] - expected_resid).abs() < 1e-10,
                "Residual mismatch at {}",
                i
            );
        }
    }

    #[cfg(feature = "linalg")]
    #[test]
    fn test_ridge_regression_fit_no_intercept() {
        let n = 30;
        let m = 5;

        let x = FdMatrix::from_column_major(
            (0..n * m).map(|i| i as f64 / (n * m) as f64).collect(),
            n,
            m,
        )
        .unwrap();
        let y: Vec<f64> = (0..n).map(|i| i as f64 / n as f64).collect();

        let result = ridge_regression_fit(&x, &y, 0.1, false);

        assert!(result.error.is_none());
        assert!(
            result.intercept.abs() < 1e-10,
            "Intercept should be 0, got {}",
            result.intercept
        );
    }

    #[cfg(feature = "linalg")]
    #[test]
    fn test_ridge_regression_fit_invalid_input() {
        let empty = FdMatrix::zeros(0, 5);
        let result = ridge_regression_fit(&empty, &[], 0.1, true);
        assert!(result.error.is_some());

        let x = FdMatrix::zeros(10, 10);
        let y = vec![0.0; 5];
        let result = ridge_regression_fit(&x, &y, 0.1, true);
        assert!(result.error.is_some());
    }
}
