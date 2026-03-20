//! Functional data operations: mean, center, derivatives, norms, and geometric median.

use crate::helpers::{simpsons_weights, simpsons_weights_2d, NUMERICAL_EPS};
use crate::iter_maybe_parallel;
use crate::matrix::FdMatrix;
#[cfg(feature = "parallel")]
use rayon::iter::ParallelIterator;

/// Compute finite difference for a 1D function at a given index.
///
/// Uses forward difference at left boundary, backward difference at right boundary,
/// and central difference for interior points.
fn finite_diff_1d(
    values: impl Fn(usize) -> f64,
    idx: usize,
    n_points: usize,
    step_sizes: &[f64],
) -> f64 {
    if idx == 0 {
        (values(1) - values(0)) / step_sizes[0]
    } else if idx == n_points - 1 {
        (values(n_points - 1) - values(n_points - 2)) / step_sizes[n_points - 1]
    } else {
        (values(idx + 1) - values(idx - 1)) / step_sizes[idx]
    }
}

/// Compute 2D partial derivatives at a single grid point.
///
/// Returns (∂f/∂s, ∂f/∂t, ∂²f/∂s∂t) using finite differences.
fn compute_2d_derivatives(
    get_val: impl Fn(usize, usize) -> f64,
    si: usize,
    ti: usize,
    m1: usize,
    m2: usize,
    hs: &[f64],
    ht: &[f64],
) -> (f64, f64, f64) {
    // ∂f/∂s
    let ds = finite_diff_1d(|s| get_val(s, ti), si, m1, hs);

    // ∂f/∂t
    let dt = finite_diff_1d(|t| get_val(si, t), ti, m2, ht);

    // ∂²f/∂s∂t (mixed partial)
    let denom = hs[si] * ht[ti];

    // Get the appropriate indices for s and t differences
    let (s_lo, s_hi) = if si == 0 {
        (0, 1)
    } else if si == m1 - 1 {
        (m1 - 2, m1 - 1)
    } else {
        (si - 1, si + 1)
    };

    let (t_lo, t_hi) = if ti == 0 {
        (0, 1)
    } else if ti == m2 - 1 {
        (m2 - 2, m2 - 1)
    } else {
        (ti - 1, ti + 1)
    };

    let dsdt = (get_val(s_hi, t_hi) - get_val(s_lo, t_hi) - get_val(s_hi, t_lo)
        + get_val(s_lo, t_lo))
        / denom;

    (ds, dt, dsdt)
}

/// Perform Weiszfeld iteration to compute geometric median.
///
/// This is the core algorithm shared by 1D and 2D geometric median computations.
fn weiszfeld_iteration(data: &FdMatrix, weights: &[f64], max_iter: usize, tol: f64) -> Vec<f64> {
    let (n, m) = data.shape();

    // Initialize with the mean
    let mut median: Vec<f64> = (0..m)
        .map(|j| {
            let col = data.column(j);
            col.iter().sum::<f64>() / n as f64
        })
        .collect();

    for _ in 0..max_iter {
        // Compute distances from current median to all curves
        let distances: Vec<f64> = (0..n)
            .map(|i| {
                let mut dist_sq = 0.0;
                for j in 0..m {
                    let diff = data[(i, j)] - median[j];
                    dist_sq += diff * diff * weights[j];
                }
                dist_sq.sqrt()
            })
            .collect();

        // Compute weights (1/distance), handling zero distances
        let inv_distances: Vec<f64> = distances
            .iter()
            .map(|d| {
                if *d > NUMERICAL_EPS {
                    1.0 / d
                } else {
                    1.0 / NUMERICAL_EPS
                }
            })
            .collect();

        let sum_inv_dist: f64 = inv_distances.iter().sum();

        // Update median using Weiszfeld iteration
        let new_median: Vec<f64> = (0..m)
            .map(|j| {
                let mut weighted_sum = 0.0;
                for i in 0..n {
                    weighted_sum += data[(i, j)] * inv_distances[i];
                }
                weighted_sum / sum_inv_dist
            })
            .collect();

        // Check convergence
        let diff: f64 = median
            .iter()
            .zip(new_median.iter())
            .map(|(a, b)| (a - b).abs())
            .sum::<f64>()
            / m as f64;

        median = new_median;

        if diff < tol {
            break;
        }
    }

    median
}

/// Compute the mean function across all samples (1D).
///
/// # Arguments
/// * `data` - Functional data matrix (n x m)
///
/// # Returns
/// Mean function values at each evaluation point
pub fn mean_1d(data: &FdMatrix) -> Vec<f64> {
    let (n, m) = data.shape();
    if n == 0 || m == 0 {
        return Vec::new();
    }

    iter_maybe_parallel!(0..m)
        .map(|j| {
            let col = data.column(j);
            col.iter().sum::<f64>() / n as f64
        })
        .collect()
}

/// Compute the mean function for 2D surfaces.
///
/// Data is stored as n x (m1*m2) matrix where each row is a flattened surface.
pub fn mean_2d(data: &FdMatrix) -> Vec<f64> {
    // Same computation as 1D - just compute pointwise mean
    mean_1d(data)
}

/// Center functional data by subtracting the mean function.
///
/// # Arguments
/// * `data` - Functional data matrix (n x m)
///
/// # Returns
/// Centered data matrix
pub fn center_1d(data: &FdMatrix) -> FdMatrix {
    let (n, m) = data.shape();
    if n == 0 || m == 0 {
        return FdMatrix::zeros(0, 0);
    }

    // First compute the mean for each column (parallelized)
    let means: Vec<f64> = iter_maybe_parallel!(0..m)
        .map(|j| {
            let col = data.column(j);
            col.iter().sum::<f64>() / n as f64
        })
        .collect();

    // Create centered data
    let mut centered = FdMatrix::zeros(n, m);
    for j in 0..m {
        let col = centered.column_mut(j);
        let src = data.column(j);
        for i in 0..n {
            col[i] = src[i] - means[j];
        }
    }

    centered
}

/// Compute Lp norm for each sample.
///
/// # Arguments
/// * `data` - Functional data matrix (n x m)
/// * `argvals` - Evaluation points for integration
/// * `p` - Order of the norm (e.g., 2.0 for L2)
///
/// # Returns
/// Vector of Lp norms for each sample
pub fn norm_lp_1d(data: &FdMatrix, argvals: &[f64], p: f64) -> Vec<f64> {
    let (n, m) = data.shape();
    if n == 0 || m == 0 || argvals.len() != m {
        return Vec::new();
    }

    let weights = simpsons_weights(argvals);

    iter_maybe_parallel!(0..n)
        .map(|i| {
            let mut integral = 0.0;
            for j in 0..m {
                let val = data[(i, j)].abs().powf(p);
                integral += val * weights[j];
            }
            integral.powf(1.0 / p)
        })
        .collect()
}

/// Compute numerical derivative of functional data (parallelized over rows).
///
/// # Arguments
/// * `data` - Functional data matrix (n x m)
/// * `argvals` - Evaluation points
/// * `nderiv` - Order of derivative
///
/// # Returns
/// Derivative data matrix
pub fn deriv_1d(data: &FdMatrix, argvals: &[f64], nderiv: usize) -> FdMatrix {
    let (n, m) = data.shape();
    if n == 0 || m == 0 || argvals.len() != m || nderiv < 1 {
        return FdMatrix::zeros(n, m);
    }

    let mut current = data.clone();

    // Pre-compute step sizes for central differences
    let h0 = argvals[1] - argvals[0];
    let hn = argvals[m - 1] - argvals[m - 2];
    let h_central: Vec<f64> = (1..(m - 1))
        .map(|j| argvals[j + 1] - argvals[j - 1])
        .collect();

    for _ in 0..nderiv {
        // Compute derivative for each row in parallel
        let deriv: Vec<f64> = iter_maybe_parallel!(0..n)
            .flat_map(|i| {
                let mut row_deriv = vec![0.0; m];

                // Forward difference at left boundary
                row_deriv[0] = (current[(i, 1)] - current[(i, 0)]) / h0;

                // Central differences for interior points
                for j in 1..(m - 1) {
                    row_deriv[j] = (current[(i, j + 1)] - current[(i, j - 1)]) / h_central[j - 1];
                }

                // Backward difference at right boundary
                row_deriv[m - 1] = (current[(i, m - 1)] - current[(i, m - 2)]) / hn;

                row_deriv
            })
            .collect();

        // Reorder from row-major to column-major order
        let mut next = FdMatrix::zeros(n, m);
        for i in 0..n {
            for j in 0..m {
                next[(i, j)] = deriv[i * m + j];
            }
        }
        current = next;
    }

    current
}

/// Result of 2D partial derivatives.
pub struct Deriv2DResult {
    /// Partial derivative with respect to s (∂f/∂s)
    pub ds: FdMatrix,
    /// Partial derivative with respect to t (∂f/∂t)
    pub dt: FdMatrix,
    /// Mixed partial derivative (∂²f/∂s∂t)
    pub dsdt: FdMatrix,
}

/// Compute finite-difference step sizes for a grid.
///
/// Uses forward/backward difference at boundaries and central difference for interior.
fn compute_step_sizes(argvals: &[f64]) -> Vec<f64> {
    let m = argvals.len();
    (0..m)
        .map(|j| {
            if j == 0 {
                argvals[1] - argvals[0]
            } else if j == m - 1 {
                argvals[m - 1] - argvals[m - 2]
            } else {
                argvals[j + 1] - argvals[j - 1]
            }
        })
        .collect()
}

/// Collect per-curve row vectors into a column-major FdMatrix.
fn reassemble_colmajor(rows: &[Vec<f64>], n: usize, ncol: usize) -> FdMatrix {
    let mut mat = FdMatrix::zeros(n, ncol);
    for i in 0..n {
        for j in 0..ncol {
            mat[(i, j)] = rows[i][j];
        }
    }
    mat
}

/// Compute 2D partial derivatives for surface data.
///
/// For a surface f(s,t), computes:
/// - ds: partial derivative with respect to s (∂f/∂s)
/// - dt: partial derivative with respect to t (∂f/∂t)
/// - dsdt: mixed partial derivative (∂²f/∂s∂t)
///
/// # Arguments
/// * `data` - Functional data matrix, n surfaces, each stored as m1*m2 values
/// * `argvals_s` - Grid points in s direction (length m1)
/// * `argvals_t` - Grid points in t direction (length m2)
/// * `m1` - Grid size in s direction
/// * `m2` - Grid size in t direction
pub fn deriv_2d(
    data: &FdMatrix,
    argvals_s: &[f64],
    argvals_t: &[f64],
    m1: usize,
    m2: usize,
) -> Option<Deriv2DResult> {
    let n = data.nrows();
    let ncol = m1 * m2;
    if n == 0 || ncol == 0 || argvals_s.len() != m1 || argvals_t.len() != m2 {
        return None;
    }

    let hs = compute_step_sizes(argvals_s);
    let ht = compute_step_sizes(argvals_t);

    // Compute all derivatives in parallel over surfaces
    let results: Vec<(Vec<f64>, Vec<f64>, Vec<f64>)> = iter_maybe_parallel!(0..n)
        .map(|i| {
            let mut ds = vec![0.0; ncol];
            let mut dt = vec![0.0; ncol];
            let mut dsdt = vec![0.0; ncol];

            let get_val = |si: usize, ti: usize| -> f64 { data[(i, si + ti * m1)] };

            for ti in 0..m2 {
                for si in 0..m1 {
                    let idx = si + ti * m1;
                    let (ds_val, dt_val, dsdt_val) =
                        compute_2d_derivatives(get_val, si, ti, m1, m2, &hs, &ht);
                    ds[idx] = ds_val;
                    dt[idx] = dt_val;
                    dsdt[idx] = dsdt_val;
                }
            }

            (ds, dt, dsdt)
        })
        .collect();

    let ds_vecs: Vec<Vec<f64>> = results.iter().map(|r| r.0.clone()).collect();
    let dt_vecs: Vec<Vec<f64>> = results.iter().map(|r| r.1.clone()).collect();
    let dsdt_vecs: Vec<Vec<f64>> = results.iter().map(|r| r.2.clone()).collect();

    Some(Deriv2DResult {
        ds: reassemble_colmajor(&ds_vecs, n, ncol),
        dt: reassemble_colmajor(&dt_vecs, n, ncol),
        dsdt: reassemble_colmajor(&dsdt_vecs, n, ncol),
    })
}

/// Compute the geometric median (L1 median) of functional data using Weiszfeld's algorithm.
///
/// The geometric median minimizes sum of L2 distances to all curves.
///
/// # Arguments
/// * `data` - Functional data matrix (n x m)
/// * `argvals` - Evaluation points for integration
/// * `max_iter` - Maximum iterations
/// * `tol` - Convergence tolerance
pub fn geometric_median_1d(
    data: &FdMatrix,
    argvals: &[f64],
    max_iter: usize,
    tol: f64,
) -> Vec<f64> {
    let (n, m) = data.shape();
    if n == 0 || m == 0 || argvals.len() != m {
        return Vec::new();
    }

    let weights = simpsons_weights(argvals);
    weiszfeld_iteration(data, &weights, max_iter, tol)
}

/// Compute the geometric median for 2D functional data.
///
/// Data is stored as n x (m1*m2) matrix where each row is a flattened surface.
///
/// # Arguments
/// * `data` - Functional data matrix (n x m) where m = m1*m2
/// * `argvals_s` - Grid points in s direction (length m1)
/// * `argvals_t` - Grid points in t direction (length m2)
/// * `max_iter` - Maximum iterations
/// * `tol` - Convergence tolerance
pub fn geometric_median_2d(
    data: &FdMatrix,
    argvals_s: &[f64],
    argvals_t: &[f64],
    max_iter: usize,
    tol: f64,
) -> Vec<f64> {
    let (n, m) = data.shape();
    let expected_cols = argvals_s.len() * argvals_t.len();
    if n == 0 || m == 0 || m != expected_cols {
        return Vec::new();
    }

    let weights = simpsons_weights_2d(argvals_s, argvals_t);
    weiszfeld_iteration(data, &weights, max_iter, tol)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::f64::consts::PI;

    fn uniform_grid(n: usize) -> Vec<f64> {
        (0..n).map(|i| i as f64 / (n - 1) as f64).collect()
    }

    // ============== Mean tests ==============

    #[test]
    fn test_mean_1d() {
        // 2 samples, 3 points each
        // Sample 1: [1, 2, 3]
        // Sample 2: [3, 4, 5]
        // Mean should be [2, 3, 4]
        let data = vec![1.0, 3.0, 2.0, 4.0, 3.0, 5.0]; // column-major
        let mat = FdMatrix::from_column_major(data, 2, 3).unwrap();
        let mean = mean_1d(&mat);
        assert_eq!(mean, vec![2.0, 3.0, 4.0]);
    }

    #[test]
    fn test_mean_1d_single_sample() {
        let data = vec![1.0, 2.0, 3.0];
        let mat = FdMatrix::from_column_major(data, 1, 3).unwrap();
        let mean = mean_1d(&mat);
        assert_eq!(mean, vec![1.0, 2.0, 3.0]);
    }

    #[test]
    fn test_mean_1d_invalid() {
        assert!(mean_1d(&FdMatrix::zeros(0, 0)).is_empty());
    }

    #[test]
    fn test_mean_2d_delegates() {
        let data = vec![1.0, 3.0, 2.0, 4.0];
        let mat = FdMatrix::from_column_major(data, 2, 2).unwrap();
        let mean1d = mean_1d(&mat);
        let mean2d = mean_2d(&mat);
        assert_eq!(mean1d, mean2d);
    }

    // ============== Center tests ==============

    #[test]
    fn test_center_1d() {
        let data = vec![1.0, 3.0, 2.0, 4.0, 3.0, 5.0]; // column-major
        let mat = FdMatrix::from_column_major(data, 2, 3).unwrap();
        let centered = center_1d(&mat);
        // Mean is [2, 3, 4], so centered should be [-1, 1, -1, 1, -1, 1]
        assert_eq!(centered.as_slice(), &[-1.0, 1.0, -1.0, 1.0, -1.0, 1.0]);
    }

    #[test]
    fn test_center_1d_mean_zero() {
        let data = vec![1.0, 3.0, 2.0, 4.0, 3.0, 5.0];
        let mat = FdMatrix::from_column_major(data, 2, 3).unwrap();
        let centered = center_1d(&mat);
        let centered_mean = mean_1d(&centered);
        for m in centered_mean {
            assert!(m.abs() < 1e-10, "Centered data should have zero mean");
        }
    }

    #[test]
    fn test_center_1d_invalid() {
        let centered = center_1d(&FdMatrix::zeros(0, 0));
        assert!(centered.is_empty());
    }

    // ============== Norm tests ==============

    #[test]
    fn test_norm_lp_1d_constant() {
        // Constant function 2 on [0, 1] has L2 norm = 2
        let argvals = uniform_grid(21);
        let data: Vec<f64> = vec![2.0; 21];
        let mat = FdMatrix::from_column_major(data, 1, 21).unwrap();
        let norms = norm_lp_1d(&mat, &argvals, 2.0);
        assert_eq!(norms.len(), 1);
        assert!(
            (norms[0] - 2.0).abs() < 0.1,
            "L2 norm of constant 2 should be 2"
        );
    }

    #[test]
    fn test_norm_lp_1d_sine() {
        // L2 norm of sin(pi*x) on [0, 1] = sqrt(0.5)
        let argvals = uniform_grid(101);
        let data: Vec<f64> = argvals.iter().map(|&x| (PI * x).sin()).collect();
        let mat = FdMatrix::from_column_major(data, 1, 101).unwrap();
        let norms = norm_lp_1d(&mat, &argvals, 2.0);
        let expected = 0.5_f64.sqrt();
        assert!(
            (norms[0] - expected).abs() < 0.05,
            "Expected {}, got {}",
            expected,
            norms[0]
        );
    }

    #[test]
    fn test_norm_lp_1d_invalid() {
        assert!(norm_lp_1d(&FdMatrix::zeros(0, 0), &[], 2.0).is_empty());
    }

    // ============== Derivative tests ==============

    #[test]
    fn test_deriv_1d_linear() {
        // Derivative of linear function x should be 1
        let argvals = uniform_grid(21);
        let data = argvals.clone();
        let mat = FdMatrix::from_column_major(data, 1, 21).unwrap();
        let deriv = deriv_1d(&mat, &argvals, 1);
        // Interior points should have derivative close to 1
        for j in 2..19 {
            assert!(
                (deriv[(0, j)] - 1.0).abs() < 0.1,
                "Derivative of x should be 1"
            );
        }
    }

    #[test]
    fn test_deriv_1d_quadratic() {
        // Derivative of x^2 should be 2x
        let argvals = uniform_grid(51);
        let data: Vec<f64> = argvals.iter().map(|&x| x * x).collect();
        let mat = FdMatrix::from_column_major(data, 1, 51).unwrap();
        let deriv = deriv_1d(&mat, &argvals, 1);
        // Check interior points
        for j in 5..45 {
            let expected = 2.0 * argvals[j];
            assert!(
                (deriv[(0, j)] - expected).abs() < 0.1,
                "Derivative of x^2 should be 2x"
            );
        }
    }

    #[test]
    fn test_deriv_1d_invalid() {
        let result = deriv_1d(&FdMatrix::zeros(0, 0), &[], 1);
        assert!(result.is_empty() || result.as_slice().iter().all(|&x| x == 0.0));
    }

    // ============== Geometric median tests ==============

    #[test]
    fn test_geometric_median_identical_curves() {
        // All curves identical -> median = that curve
        let argvals = uniform_grid(21);
        let n = 5;
        let m = 21;
        let mut data = vec![0.0; n * m];
        for i in 0..n {
            for j in 0..m {
                data[i + j * n] = (2.0 * PI * argvals[j]).sin();
            }
        }
        let mat = FdMatrix::from_column_major(data, n, m).unwrap();
        let median = geometric_median_1d(&mat, &argvals, 100, 1e-6);
        for j in 0..m {
            let expected = (2.0 * PI * argvals[j]).sin();
            assert!(
                (median[j] - expected).abs() < 0.01,
                "Median should equal all curves"
            );
        }
    }

    #[test]
    fn test_geometric_median_converges() {
        let argvals = uniform_grid(21);
        let n = 10;
        let m = 21;
        let mut data = vec![0.0; n * m];
        for i in 0..n {
            for j in 0..m {
                data[i + j * n] = (i as f64 / n as f64) * argvals[j];
            }
        }
        let mat = FdMatrix::from_column_major(data, n, m).unwrap();
        let median = geometric_median_1d(&mat, &argvals, 100, 1e-6);
        assert_eq!(median.len(), m);
        assert!(median.iter().all(|&x| x.is_finite()));
    }

    #[test]
    fn test_geometric_median_invalid() {
        assert!(geometric_median_1d(&FdMatrix::zeros(0, 0), &[], 100, 1e-6).is_empty());
    }

    // ============== 2D derivative tests ==============

    #[test]
    fn test_deriv_2d_linear_surface() {
        // f(s, t) = 2*s + 3*t
        // ∂f/∂s = 2, ∂f/∂t = 3, ∂²f/∂s∂t = 0
        let m1 = 11;
        let m2 = 11;
        let argvals_s: Vec<f64> = (0..m1).map(|i| i as f64 / (m1 - 1) as f64).collect();
        let argvals_t: Vec<f64> = (0..m2).map(|i| i as f64 / (m2 - 1) as f64).collect();

        let n = 1; // single surface
        let ncol = m1 * m2;
        let mut data = vec![0.0; n * ncol];

        for si in 0..m1 {
            for ti in 0..m2 {
                let s = argvals_s[si];
                let t = argvals_t[ti];
                let idx = si + ti * m1;
                data[idx] = 2.0 * s + 3.0 * t;
            }
        }

        let mat = FdMatrix::from_column_major(data, n, ncol).unwrap();
        let result = deriv_2d(&mat, &argvals_s, &argvals_t, m1, m2).unwrap();

        // Check interior points for ∂f/∂s ≈ 2
        for si in 2..(m1 - 2) {
            for ti in 2..(m2 - 2) {
                let idx = si + ti * m1;
                assert!(
                    (result.ds[(0, idx)] - 2.0).abs() < 0.2,
                    "∂f/∂s at ({}, {}) = {}, expected 2",
                    si,
                    ti,
                    result.ds[(0, idx)]
                );
            }
        }

        // Check interior points for ∂f/∂t ≈ 3
        for si in 2..(m1 - 2) {
            for ti in 2..(m2 - 2) {
                let idx = si + ti * m1;
                assert!(
                    (result.dt[(0, idx)] - 3.0).abs() < 0.2,
                    "∂f/∂t at ({}, {}) = {}, expected 3",
                    si,
                    ti,
                    result.dt[(0, idx)]
                );
            }
        }

        // Check interior points for mixed partial ≈ 0
        for si in 2..(m1 - 2) {
            for ti in 2..(m2 - 2) {
                let idx = si + ti * m1;
                assert!(
                    result.dsdt[(0, idx)].abs() < 0.5,
                    "∂²f/∂s∂t at ({}, {}) = {}, expected 0",
                    si,
                    ti,
                    result.dsdt[(0, idx)]
                );
            }
        }
    }

    #[test]
    fn test_deriv_2d_quadratic_surface() {
        // f(s, t) = s*t
        // ∂f/∂s = t, ∂f/∂t = s, ∂²f/∂s∂t = 1
        let m1 = 21;
        let m2 = 21;
        let argvals_s: Vec<f64> = (0..m1).map(|i| i as f64 / (m1 - 1) as f64).collect();
        let argvals_t: Vec<f64> = (0..m2).map(|i| i as f64 / (m2 - 1) as f64).collect();

        let n = 1;
        let ncol = m1 * m2;
        let mut data = vec![0.0; n * ncol];

        for si in 0..m1 {
            for ti in 0..m2 {
                let s = argvals_s[si];
                let t = argvals_t[ti];
                let idx = si + ti * m1;
                data[idx] = s * t;
            }
        }

        let mat = FdMatrix::from_column_major(data, n, ncol).unwrap();
        let result = deriv_2d(&mat, &argvals_s, &argvals_t, m1, m2).unwrap();

        // Check interior points for ∂f/∂s ≈ t
        for si in 3..(m1 - 3) {
            for ti in 3..(m2 - 3) {
                let idx = si + ti * m1;
                let expected = argvals_t[ti];
                assert!(
                    (result.ds[(0, idx)] - expected).abs() < 0.1,
                    "∂f/∂s at ({}, {}) = {}, expected {}",
                    si,
                    ti,
                    result.ds[(0, idx)],
                    expected
                );
            }
        }

        // Check interior points for ∂f/∂t ≈ s
        for si in 3..(m1 - 3) {
            for ti in 3..(m2 - 3) {
                let idx = si + ti * m1;
                let expected = argvals_s[si];
                assert!(
                    (result.dt[(0, idx)] - expected).abs() < 0.1,
                    "∂f/∂t at ({}, {}) = {}, expected {}",
                    si,
                    ti,
                    result.dt[(0, idx)],
                    expected
                );
            }
        }

        // Check interior points for mixed partial ≈ 1
        for si in 3..(m1 - 3) {
            for ti in 3..(m2 - 3) {
                let idx = si + ti * m1;
                assert!(
                    (result.dsdt[(0, idx)] - 1.0).abs() < 0.3,
                    "∂²f/∂s∂t at ({}, {}) = {}, expected 1",
                    si,
                    ti,
                    result.dsdt[(0, idx)]
                );
            }
        }
    }

    #[test]
    fn test_deriv_2d_invalid_input() {
        // Empty data
        let result = deriv_2d(&FdMatrix::zeros(0, 0), &[], &[], 0, 0);
        assert!(result.is_none());

        // Mismatched dimensions
        let mat = FdMatrix::from_column_major(vec![1.0; 4], 1, 4).unwrap();
        let argvals = vec![0.0, 1.0];
        let result = deriv_2d(&mat, &argvals, &[0.0, 0.5, 1.0], 2, 2);
        assert!(result.is_none());
    }

    // ============== 2D geometric median tests ==============

    #[test]
    fn test_geometric_median_2d_basic() {
        // Three identical surfaces -> median = that surface
        let m1 = 5;
        let m2 = 5;
        let m = m1 * m2;
        let n = 3;
        let argvals_s: Vec<f64> = (0..m1).map(|i| i as f64 / (m1 - 1) as f64).collect();
        let argvals_t: Vec<f64> = (0..m2).map(|i| i as f64 / (m2 - 1) as f64).collect();

        let mut data = vec![0.0; n * m];

        // Create identical surfaces: f(s, t) = s + t
        for i in 0..n {
            for si in 0..m1 {
                for ti in 0..m2 {
                    let idx = si + ti * m1;
                    let s = argvals_s[si];
                    let t = argvals_t[ti];
                    data[i + idx * n] = s + t;
                }
            }
        }

        let mat = FdMatrix::from_column_major(data, n, m).unwrap();
        let median = geometric_median_2d(&mat, &argvals_s, &argvals_t, 100, 1e-6);
        assert_eq!(median.len(), m);

        // Check that median equals the surface
        for si in 0..m1 {
            for ti in 0..m2 {
                let idx = si + ti * m1;
                let expected = argvals_s[si] + argvals_t[ti];
                assert!(
                    (median[idx] - expected).abs() < 0.01,
                    "Median at ({}, {}) = {}, expected {}",
                    si,
                    ti,
                    median[idx],
                    expected
                );
            }
        }
    }
}
