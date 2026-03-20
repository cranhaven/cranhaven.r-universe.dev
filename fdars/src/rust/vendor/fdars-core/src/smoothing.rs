//! Smoothing functions for functional data.
//!
//! This module provides kernel-based smoothing methods including
//! Nadaraya-Watson, local linear, and local polynomial regression.

use crate::slice_maybe_parallel;
#[cfg(feature = "parallel")]
use rayon::iter::ParallelIterator;

/// Gaussian kernel function.
fn gaussian_kernel(u: f64) -> f64 {
    (-0.5 * u * u).exp() / (2.0 * std::f64::consts::PI).sqrt()
}

/// Epanechnikov kernel function.
fn epanechnikov_kernel(u: f64) -> f64 {
    if u.abs() <= 1.0 {
        0.75 * (1.0 - u * u)
    } else {
        0.0
    }
}

/// Get kernel function by name.
fn get_kernel(kernel_type: &str) -> fn(f64) -> f64 {
    match kernel_type.to_lowercase().as_str() {
        "epanechnikov" | "epan" => epanechnikov_kernel,
        _ => gaussian_kernel,
    }
}

/// Nadaraya-Watson kernel smoother.
///
/// # Arguments
/// * `x` - Predictor values
/// * `y` - Response values
/// * `x_new` - Points at which to evaluate the smoother
/// * `bandwidth` - Kernel bandwidth
/// * `kernel` - Kernel type ("gaussian" or "epanechnikov")
///
/// # Returns
/// Smoothed values at x_new
pub fn nadaraya_watson(
    x: &[f64],
    y: &[f64],
    x_new: &[f64],
    bandwidth: f64,
    kernel: &str,
) -> Vec<f64> {
    let n = x.len();
    if n == 0 || y.len() != n || x_new.is_empty() || bandwidth <= 0.0 {
        return vec![0.0; x_new.len()];
    }

    let kernel_fn = get_kernel(kernel);

    slice_maybe_parallel!(x_new)
        .map(|&x0| {
            let mut num = 0.0;
            let mut denom = 0.0;

            for i in 0..n {
                let u = (x[i] - x0) / bandwidth;
                let w = kernel_fn(u);
                num += w * y[i];
                denom += w;
            }

            if denom > 1e-10 {
                num / denom
            } else {
                0.0
            }
        })
        .collect()
}

/// Local linear regression smoother.
///
/// # Arguments
/// * `x` - Predictor values
/// * `y` - Response values
/// * `x_new` - Points at which to evaluate the smoother
/// * `bandwidth` - Kernel bandwidth
/// * `kernel` - Kernel type
///
/// # Returns
/// Smoothed values at x_new
pub fn local_linear(x: &[f64], y: &[f64], x_new: &[f64], bandwidth: f64, kernel: &str) -> Vec<f64> {
    let n = x.len();
    if n == 0 || y.len() != n || x_new.is_empty() || bandwidth <= 0.0 {
        return vec![0.0; x_new.len()];
    }

    let kernel_fn = get_kernel(kernel);

    slice_maybe_parallel!(x_new)
        .map(|&x0| {
            // Compute weighted moments
            let mut s0 = 0.0;
            let mut s1 = 0.0;
            let mut s2 = 0.0;
            let mut t0 = 0.0;
            let mut t1 = 0.0;

            for i in 0..n {
                let u = (x[i] - x0) / bandwidth;
                let w = kernel_fn(u);
                let d = x[i] - x0;

                s0 += w;
                s1 += w * d;
                s2 += w * d * d;
                t0 += w * y[i];
                t1 += w * y[i] * d;
            }

            // Solve local linear regression
            let det = s0 * s2 - s1 * s1;
            if det.abs() > 1e-10 {
                (s2 * t0 - s1 * t1) / det
            } else if s0 > 1e-10 {
                t0 / s0
            } else {
                0.0
            }
        })
        .collect()
}

/// Accumulate weighted normal equations (X'WX and X'Wy) for local polynomial fit.
fn accumulate_weighted_normal_equations(
    x: &[f64],
    y: &[f64],
    x0: f64,
    bandwidth: f64,
    p: usize,
    kernel_fn: impl Fn(f64) -> f64,
) -> (Vec<f64>, Vec<f64>) {
    let n = x.len();
    let mut xtx = vec![0.0; p * p];
    let mut xty = vec![0.0; p];

    for i in 0..n {
        let u = (x[i] - x0) / bandwidth;
        let w = kernel_fn(u);
        let d = x[i] - x0;

        let mut powers = vec![1.0; p];
        for j in 1..p {
            powers[j] = powers[j - 1] * d;
        }

        for j in 0..p {
            for k in 0..p {
                xtx[j * p + k] += w * powers[j] * powers[k];
            }
            xty[j] += w * powers[j] * y[i];
        }
    }

    (xtx, xty)
}

/// Solve a linear system using Gaussian elimination with partial pivoting.
/// Returns the solution vector, or a zero vector if the system is singular.
/// Gaussian elimination with partial pivoting (forward pass).
/// Find the row with the largest absolute value in column `col` at or below the diagonal.
fn find_pivot(a: &[f64], p: usize, col: usize) -> usize {
    let mut max_idx = col;
    for j in (col + 1)..p {
        if a[j * p + col].abs() > a[max_idx * p + col].abs() {
            max_idx = j;
        }
    }
    max_idx
}

/// Swap two rows in both the matrix `a` and the RHS vector `b`.
fn swap_rows(a: &mut [f64], b: &mut [f64], p: usize, row_a: usize, row_b: usize) {
    for k in 0..p {
        a.swap(row_a * p + k, row_b * p + k);
    }
    b.swap(row_a, row_b);
}

/// Subtract a scaled copy of the pivot row from all rows below it.
fn eliminate_below(a: &mut [f64], b: &mut [f64], p: usize, pivot_row: usize) {
    let pivot = a[pivot_row * p + pivot_row];
    for j in (pivot_row + 1)..p {
        let factor = a[j * p + pivot_row] / pivot;
        for k in pivot_row..p {
            a[j * p + k] -= factor * a[pivot_row * p + k];
        }
        b[j] -= factor * b[pivot_row];
    }
}

fn forward_eliminate(a: &mut [f64], b: &mut [f64], p: usize) {
    for i in 0..p {
        let max_idx = find_pivot(a, p, i);
        if max_idx != i {
            swap_rows(a, b, p, i, max_idx);
        }

        if a[i * p + i].abs() < 1e-10 {
            continue;
        }

        eliminate_below(a, b, p, i);
    }
}

/// Back substitution for an upper-triangular system.
fn back_substitute(a: &[f64], b: &[f64], p: usize) -> Vec<f64> {
    let mut coefs = vec![0.0; p];
    for i in (0..p).rev() {
        let mut sum = b[i];
        for j in (i + 1)..p {
            sum -= a[i * p + j] * coefs[j];
        }
        if a[i * p + i].abs() > 1e-10 {
            coefs[i] = sum / a[i * p + i];
        }
    }
    coefs
}

fn solve_gaussian(a: &mut [f64], b: &mut [f64], p: usize) -> Vec<f64> {
    forward_eliminate(a, b, p);
    back_substitute(a, b, p)
}

/// Local polynomial regression smoother.
///
/// # Arguments
/// * `x` - Predictor values
/// * `y` - Response values
/// * `x_new` - Points at which to evaluate the smoother
/// * `bandwidth` - Kernel bandwidth
/// * `degree` - Polynomial degree
/// * `kernel` - Kernel type
///
/// # Returns
/// Smoothed values at x_new
pub fn local_polynomial(
    x: &[f64],
    y: &[f64],
    x_new: &[f64],
    bandwidth: f64,
    degree: usize,
    kernel: &str,
) -> Vec<f64> {
    let n = x.len();
    if n == 0 || y.len() != n || x_new.is_empty() || bandwidth <= 0.0 || degree == 0 {
        return vec![0.0; x_new.len()];
    }

    if degree == 1 {
        return local_linear(x, y, x_new, bandwidth, kernel);
    }

    let kernel_fn = get_kernel(kernel);
    let p = degree + 1; // Number of coefficients

    slice_maybe_parallel!(x_new)
        .map(|&x0| {
            let (mut xtx, mut xty) =
                accumulate_weighted_normal_equations(x, y, x0, bandwidth, p, kernel_fn);
            let coefs = solve_gaussian(&mut xtx, &mut xty, p);
            coefs[0]
        })
        .collect()
}

/// k-Nearest Neighbors smoother.
///
/// # Arguments
/// * `x` - Predictor values
/// * `y` - Response values
/// * `x_new` - Points at which to evaluate the smoother
/// * `k` - Number of neighbors
///
/// # Returns
/// Smoothed values at x_new
pub fn knn_smoother(x: &[f64], y: &[f64], x_new: &[f64], k: usize) -> Vec<f64> {
    let n = x.len();
    if n == 0 || y.len() != n || x_new.is_empty() || k == 0 {
        return vec![0.0; x_new.len()];
    }

    let k = k.min(n);

    slice_maybe_parallel!(x_new)
        .map(|&x0| {
            // Compute distances
            let mut distances: Vec<(usize, f64)> = x
                .iter()
                .enumerate()
                .map(|(i, &xi)| (i, (xi - x0).abs()))
                .collect();

            // Partial sort to get k nearest
            distances.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));

            // Average of k nearest neighbors
            let sum: f64 = distances.iter().take(k).map(|(i, _)| y[*i]).sum();
            sum / k as f64
        })
        .collect()
}

/// Compute smoothing matrix for Nadaraya-Watson.
///
/// Returns the smoother matrix S such that y_hat = S * y.
pub fn smoothing_matrix_nw(x: &[f64], bandwidth: f64, kernel: &str) -> Vec<f64> {
    let n = x.len();
    if n == 0 || bandwidth <= 0.0 {
        return Vec::new();
    }

    let kernel_fn = get_kernel(kernel);
    let mut s = vec![0.0; n * n];

    for i in 0..n {
        let mut row_sum = 0.0;
        for j in 0..n {
            let u = (x[j] - x[i]) / bandwidth;
            s[i + j * n] = kernel_fn(u);
            row_sum += s[i + j * n];
        }
        if row_sum > 1e-10 {
            for j in 0..n {
                s[i + j * n] /= row_sum;
            }
        }
    }

    s
}

#[cfg(test)]
mod tests {
    use super::*;

    fn uniform_grid(n: usize) -> Vec<f64> {
        (0..n).map(|i| i as f64 / (n - 1) as f64).collect()
    }

    // ============== Nadaraya-Watson tests ==============

    #[test]
    fn test_nw_constant_data() {
        let x = uniform_grid(20);
        let y: Vec<f64> = vec![5.0; 20];

        let y_smooth = nadaraya_watson(&x, &y, &x, 0.1, "gaussian");

        // Smoothing constant data should return constant
        for &yi in &y_smooth {
            assert!(
                (yi - 5.0).abs() < 0.1,
                "Constant data should remain constant"
            );
        }
    }

    #[test]
    fn test_nw_linear_data() {
        let x = uniform_grid(50);
        let y: Vec<f64> = x.iter().map(|&xi| 2.0 * xi + 1.0).collect();

        let y_smooth = nadaraya_watson(&x, &y, &x, 0.2, "gaussian");

        // Linear data should be approximately preserved (with some edge effects)
        for i in 10..40 {
            let expected = 2.0 * x[i] + 1.0;
            assert!(
                (y_smooth[i] - expected).abs() < 0.2,
                "Linear trend should be approximately preserved"
            );
        }
    }

    #[test]
    fn test_nw_gaussian_vs_epanechnikov() {
        let x = uniform_grid(30);
        let y: Vec<f64> = x
            .iter()
            .map(|&xi| (2.0 * std::f64::consts::PI * xi).sin())
            .collect();

        let y_gauss = nadaraya_watson(&x, &y, &x, 0.1, "gaussian");
        let y_epan = nadaraya_watson(&x, &y, &x, 0.1, "epanechnikov");

        // Both should produce valid output
        assert_eq!(y_gauss.len(), 30);
        assert_eq!(y_epan.len(), 30);

        // They should be different (different kernels)
        let diff: f64 = y_gauss
            .iter()
            .zip(&y_epan)
            .map(|(a, b)| (a - b).abs())
            .sum();
        assert!(
            diff > 0.0,
            "Different kernels should give different results"
        );
    }

    #[test]
    fn test_nw_invalid_input() {
        // Empty input
        let result = nadaraya_watson(&[], &[], &[0.5], 0.1, "gaussian");
        assert_eq!(result, vec![0.0]);

        // Mismatched lengths
        let result = nadaraya_watson(&[0.0, 1.0], &[1.0], &[0.5], 0.1, "gaussian");
        assert_eq!(result, vec![0.0]);

        // Zero bandwidth
        let result = nadaraya_watson(&[0.0, 1.0], &[1.0, 2.0], &[0.5], 0.0, "gaussian");
        assert_eq!(result, vec![0.0]);
    }

    // ============== Local linear tests ==============

    #[test]
    fn test_ll_constant_data() {
        let x = uniform_grid(20);
        let y: Vec<f64> = vec![3.0; 20];

        let y_smooth = local_linear(&x, &y, &x, 0.15, "gaussian");

        for &yi in &y_smooth {
            assert!((yi - 3.0).abs() < 0.1, "Constant should remain constant");
        }
    }

    #[test]
    fn test_ll_linear_data_exact() {
        let x = uniform_grid(30);
        let y: Vec<f64> = x.iter().map(|&xi| 3.0 * xi + 2.0).collect();

        let y_smooth = local_linear(&x, &y, &x, 0.2, "gaussian");

        // Local linear should fit linear data exactly (in interior)
        for i in 5..25 {
            let expected = 3.0 * x[i] + 2.0;
            assert!(
                (y_smooth[i] - expected).abs() < 0.1,
                "Local linear should fit linear data well"
            );
        }
    }

    #[test]
    fn test_ll_invalid_input() {
        let result = local_linear(&[], &[], &[0.5], 0.1, "gaussian");
        assert_eq!(result, vec![0.0]);

        let result = local_linear(&[0.0, 1.0], &[1.0, 2.0], &[0.5], -0.1, "gaussian");
        assert_eq!(result, vec![0.0]);
    }

    // ============== Local polynomial tests ==============

    #[test]
    fn test_lp_degree1_equals_local_linear() {
        let x = uniform_grid(25);
        let y: Vec<f64> = x.iter().map(|&xi| xi * xi).collect();

        let y_ll = local_linear(&x, &y, &x, 0.15, "gaussian");
        let y_lp = local_polynomial(&x, &y, &x, 0.15, 1, "gaussian");

        for i in 0..25 {
            assert!(
                (y_ll[i] - y_lp[i]).abs() < 1e-10,
                "Degree 1 should equal local linear"
            );
        }
    }

    #[test]
    fn test_lp_quadratic_data() {
        let x = uniform_grid(40);
        let y: Vec<f64> = x.iter().map(|&xi| xi * xi).collect();

        let y_smooth = local_polynomial(&x, &y, &x, 0.15, 2, "gaussian");

        // Local quadratic should fit quadratic data well in interior
        for i in 8..32 {
            let expected = x[i] * x[i];
            assert!(
                (y_smooth[i] - expected).abs() < 0.1,
                "Local quadratic should fit quadratic data"
            );
        }
    }

    #[test]
    fn test_lp_invalid_input() {
        // Zero degree
        let result = local_polynomial(&[0.0, 1.0], &[1.0, 2.0], &[0.5], 0.1, 0, "gaussian");
        assert_eq!(result, vec![0.0]);

        // Empty input
        let result = local_polynomial(&[], &[], &[0.5], 0.1, 2, "gaussian");
        assert_eq!(result, vec![0.0]);
    }

    // ============== KNN smoother tests ==============

    #[test]
    fn test_knn_k1_nearest() {
        let x = vec![0.0, 0.5, 1.0];
        let y = vec![1.0, 2.0, 3.0];

        let result = knn_smoother(&x, &y, &[0.1, 0.6, 0.9], 1);

        // k=1 should return the nearest neighbor's y value
        assert!((result[0] - 1.0).abs() < 1e-10, "0.1 nearest to 0.0 -> 1.0");
        assert!((result[1] - 2.0).abs() < 1e-10, "0.6 nearest to 0.5 -> 2.0");
        assert!((result[2] - 3.0).abs() < 1e-10, "0.9 nearest to 1.0 -> 3.0");
    }

    #[test]
    fn test_knn_k_equals_n_is_mean() {
        let x = vec![0.0, 0.25, 0.5, 0.75, 1.0];
        let y = vec![1.0, 2.0, 3.0, 4.0, 5.0];
        let expected_mean = 3.0;

        let result = knn_smoother(&x, &y, &[0.5], 5);

        assert!(
            (result[0] - expected_mean).abs() < 1e-10,
            "k=n should return mean"
        );
    }

    #[test]
    fn test_knn_invalid_input() {
        let result = knn_smoother(&[], &[], &[0.5], 3);
        assert_eq!(result, vec![0.0]);

        let result = knn_smoother(&[0.0, 1.0], &[1.0, 2.0], &[0.5], 0);
        assert_eq!(result, vec![0.0]);
    }

    // ============== Smoothing matrix tests ==============

    #[test]
    fn test_smoothing_matrix_row_stochastic() {
        let x = uniform_grid(10);
        let s = smoothing_matrix_nw(&x, 0.2, "gaussian");

        assert_eq!(s.len(), 100);

        // Each row should sum to 1 (row stochastic)
        for i in 0..10 {
            let row_sum: f64 = (0..10).map(|j| s[i + j * 10]).sum();
            assert!(
                (row_sum - 1.0).abs() < 1e-10,
                "Row {} should sum to 1, got {}",
                i,
                row_sum
            );
        }
    }

    #[test]
    fn test_smoothing_matrix_invalid_input() {
        let result = smoothing_matrix_nw(&[], 0.1, "gaussian");
        assert!(result.is_empty());

        let result = smoothing_matrix_nw(&[0.0, 1.0], 0.0, "gaussian");
        assert!(result.is_empty());
    }
}
