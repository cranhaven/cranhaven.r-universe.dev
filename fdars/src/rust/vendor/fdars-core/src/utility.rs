//! Utility functions for functional data analysis.

use crate::helpers::simpsons_weights;
use crate::iter_maybe_parallel;
use crate::matrix::FdMatrix;
#[cfg(feature = "parallel")]
use rayon::iter::ParallelIterator;
use std::f64::consts::PI;

/// Compute Simpson's rule integration for a single function.
///
/// # Arguments
/// * `values` - Function values at evaluation points
/// * `argvals` - Evaluation points
pub fn integrate_simpson(values: &[f64], argvals: &[f64]) -> f64 {
    if values.len() != argvals.len() || values.is_empty() {
        return 0.0;
    }

    let weights = simpsons_weights(argvals);
    values
        .iter()
        .zip(weights.iter())
        .map(|(&v, &w)| v * w)
        .sum()
}

/// Compute inner product between two functional data curves.
///
/// # Arguments
/// * `curve1` - First curve values
/// * `curve2` - Second curve values
/// * `argvals` - Evaluation points
pub fn inner_product(curve1: &[f64], curve2: &[f64], argvals: &[f64]) -> f64 {
    if curve1.len() != curve2.len() || curve1.len() != argvals.len() || curve1.is_empty() {
        return 0.0;
    }

    let weights = simpsons_weights(argvals);
    curve1
        .iter()
        .zip(curve2.iter())
        .zip(weights.iter())
        .map(|((&c1, &c2), &w)| c1 * c2 * w)
        .sum()
}

/// Compute inner product matrix for functional data.
///
/// # Arguments
/// * `data` - Matrix of observations (n rows) x evaluation points (m cols)
/// * `argvals` - Evaluation points
///
/// # Returns
/// Symmetric inner product matrix (n x n)
pub fn inner_product_matrix(data: &FdMatrix, argvals: &[f64]) -> FdMatrix {
    let n = data.nrows();
    let m = data.ncols();

    if n == 0 || m == 0 || argvals.len() != m {
        return FdMatrix::zeros(0, 0);
    }

    let weights = simpsons_weights(argvals);

    // Compute upper triangle (parallel when feature enabled)
    let upper_triangle: Vec<(usize, usize, f64)> = iter_maybe_parallel!(0..n)
        .flat_map(|i| {
            (i..n)
                .map(|j| {
                    let mut ip = 0.0;
                    for k in 0..m {
                        ip += data[(i, k)] * data[(j, k)] * weights[k];
                    }
                    (i, j, ip)
                })
                .collect::<Vec<_>>()
        })
        .collect();

    // Build symmetric matrix
    let mut result = FdMatrix::zeros(n, n);
    for (i, j, ip) in upper_triangle {
        result[(i, j)] = ip;
        result[(j, i)] = ip;
    }

    result
}

/// Compute the Adot matrix used in PCvM statistic.
/// Packed symmetric index for 1-based indices in lower-triangular storage.
fn packed_sym_index(a: usize, b: usize) -> usize {
    let (hi, lo) = if a >= b { (a, b) } else { (b, a) };
    hi * (hi - 1) / 2 + lo - 1
}

/// Compute the angular distance sum for a single (i, j) pair over all reference points.
fn adot_pair_sum(inprod: &[f64], n: usize, i: usize, j: usize) -> f64 {
    let ij = packed_sym_index(i, j);
    let ii = packed_sym_index(i, i);
    let jj = packed_sym_index(j, j);
    let mut sumr = 0.0;

    for r in 1..=n {
        if i == r || j == r {
            sumr += PI;
        } else {
            let rr = packed_sym_index(r, r);
            let ir = packed_sym_index(i, r);
            let rj = packed_sym_index(r, j);

            let num = inprod[ij] - inprod[ir] - inprod[rj] + inprod[rr];
            let aux1 = (inprod[ii] - 2.0 * inprod[ir] + inprod[rr]).sqrt();
            let aux2 = (inprod[jj] - 2.0 * inprod[rj] + inprod[rr]).sqrt();
            let den = aux1 * aux2;

            let mut quo = if den.abs() > 1e-10 { num / den } else { 0.0 };
            quo = quo.clamp(-1.0, 1.0);

            sumr += (PI - quo.acos()).abs();
        }
    }

    sumr
}

pub fn compute_adot(n: usize, inprod: &[f64]) -> Vec<f64> {
    if n == 0 {
        return Vec::new();
    }

    let expected_len = (n * n + n) / 2;
    if inprod.len() != expected_len {
        return Vec::new();
    }

    let out_len = (n * n - n + 2) / 2;
    let mut adot_vec = vec![0.0; out_len];

    adot_vec[0] = PI * (n + 1) as f64;

    // Collect all (i, j) pairs for parallel processing
    let pairs: Vec<(usize, usize)> = (2..=n).flat_map(|i| (1..i).map(move |j| (i, j))).collect();

    // Compute adot values (parallel when feature enabled)
    let results: Vec<(usize, f64)> = iter_maybe_parallel!(pairs)
        .map(|(i, j)| {
            let sumr = adot_pair_sum(inprod, n, i, j);
            let idx = 1 + ((i - 1) * (i - 2) / 2) + j - 1;
            (idx, sumr)
        })
        .collect();

    // Fill in the results
    for (idx, val) in results {
        if idx < adot_vec.len() {
            adot_vec[idx] = val;
        }
    }

    adot_vec
}

/// Compute the PCvM statistic.
pub fn pcvm_statistic(adot_vec: &[f64], residuals: &[f64]) -> f64 {
    let n = residuals.len();

    if n == 0 || adot_vec.is_empty() {
        return 0.0;
    }

    let mut sums = 0.0;
    for i in 2..=n {
        for j in 1..i {
            let idx = 1 + ((i - 1) * (i - 2) / 2) + j - 1;
            if idx < adot_vec.len() {
                sums += residuals[i - 1] * adot_vec[idx] * residuals[j - 1];
            }
        }
    }

    let diag_sum: f64 = residuals.iter().map(|r| r * r).sum();
    adot_vec[0] * diag_sum + 2.0 * sums
}

/// Result of random projection statistics.
pub struct RpStatResult {
    /// CvM statistics for each projection
    pub cvm: Vec<f64>,
    /// KS statistics for each projection
    pub ks: Vec<f64>,
}

/// Compute random projection statistics.
pub fn rp_stat(proj_x_ord: &[i32], residuals: &[f64], n_proj: usize) -> RpStatResult {
    let n = residuals.len();

    if n == 0 || n_proj == 0 || proj_x_ord.len() != n * n_proj {
        return RpStatResult {
            cvm: Vec::new(),
            ks: Vec::new(),
        };
    }

    // Process projections (parallel when feature enabled)
    let stats: Vec<(f64, f64)> = iter_maybe_parallel!(0..n_proj)
        .map(|p| {
            let mut y = vec![0.0; n];
            let mut cumsum = 0.0;

            for i in 0..n {
                let idx = proj_x_ord[p * n + i] as usize;
                if idx > 0 && idx <= n {
                    cumsum += residuals[idx - 1];
                }
                y[i] = cumsum;
            }

            let sum_y_sq: f64 = y.iter().map(|yi| yi * yi).sum();
            let cvm = sum_y_sq / (n * n) as f64;

            let max_abs_y = y.iter().map(|yi| yi.abs()).fold(0.0, f64::max);
            let ks = max_abs_y / (n as f64).sqrt();

            (cvm, ks)
        })
        .collect();

    let cvm_stats: Vec<f64> = stats.iter().map(|(cvm, _)| *cvm).collect();
    let ks_stats: Vec<f64> = stats.iter().map(|(_, ks)| *ks).collect();

    RpStatResult {
        cvm: cvm_stats,
        ks: ks_stats,
    }
}

/// k-NN prediction for functional regression.
///
/// # Arguments
/// * `distance_matrix` - Distance matrix (n_test rows x n_train cols)
/// * `y` - Training response values (length n_train)
/// * `k` - Number of nearest neighbors
pub fn knn_predict(distance_matrix: &FdMatrix, y: &[f64], k: usize) -> Vec<f64> {
    let n_test = distance_matrix.nrows();
    let n_train = distance_matrix.ncols();

    if n_train == 0 || n_test == 0 || k == 0 || y.len() != n_train {
        return vec![0.0; n_test];
    }

    let k = k.min(n_train);

    iter_maybe_parallel!(0..n_test)
        .map(|i| {
            // Get distances from test point i to all training points
            let mut distances: Vec<(usize, f64)> =
                (0..n_train).map(|j| (j, distance_matrix[(i, j)])).collect();

            // Sort by distance
            distances.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));

            // Average of k nearest neighbors
            let sum: f64 = distances.iter().take(k).map(|(j, _)| y[*j]).sum();
            sum / k as f64
        })
        .collect()
}

/// Compute leave-one-out cross-validation error for k-NN.
///
/// # Arguments
/// * `distance_matrix` - Square distance matrix (n x n)
/// * `y` - Response values (length n)
/// * `k` - Number of nearest neighbors
pub fn knn_loocv(distance_matrix: &FdMatrix, y: &[f64], k: usize) -> f64 {
    let n = distance_matrix.nrows();

    if n == 0 || k == 0 || y.len() != n || distance_matrix.ncols() != n {
        return f64::INFINITY;
    }

    let k = k.min(n - 1);

    let errors: Vec<f64> = iter_maybe_parallel!(0..n)
        .map(|i| {
            // Get distances from point i to all other points
            let mut distances: Vec<(usize, f64)> = (0..n)
                .filter(|&j| j != i)
                .map(|j| (j, distance_matrix[(i, j)]))
                .collect();

            // Sort by distance
            distances.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));

            // Prediction
            let pred: f64 = distances.iter().take(k).map(|(j, _)| y[*j]).sum::<f64>() / k as f64;

            // Squared error
            (y[i] - pred).powi(2)
        })
        .collect();

    errors.iter().sum::<f64>() / n as f64
}

#[cfg(test)]
mod tests {
    use super::*;

    fn uniform_grid(n: usize) -> Vec<f64> {
        (0..n).map(|i| i as f64 / (n - 1) as f64).collect()
    }

    #[test]
    fn test_integrate_simpson_constant() {
        let argvals = uniform_grid(11);
        let values = vec![1.0; 11];
        let result = integrate_simpson(&values, &argvals);
        assert!((result - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_inner_product_orthogonal() {
        let argvals = uniform_grid(101);
        let curve1: Vec<f64> = argvals.iter().map(|&t| (2.0 * PI * t).sin()).collect();
        let curve2: Vec<f64> = argvals.iter().map(|&t| (2.0 * PI * t).cos()).collect();
        let result = inner_product(&curve1, &curve2, &argvals);
        assert!(result.abs() < 0.01);
    }

    #[test]
    fn test_inner_product_matrix_symmetry() {
        let n = 5;
        let m = 10;
        let argvals = uniform_grid(m);
        let data: Vec<f64> = (0..n * m).map(|i| (i as f64).sin()).collect();
        let mat = FdMatrix::from_column_major(data, n, m).unwrap();

        let matrix = inner_product_matrix(&mat, &argvals);

        for i in 0..n {
            for j in 0..n {
                let diff = (matrix[(i, j)] - matrix[(j, i)]).abs();
                assert!(diff < 1e-10, "Matrix should be symmetric");
            }
        }
    }

    #[test]
    fn test_knn_predict() {
        let n_train = 10;
        let n_test = 3;
        let k = 3;

        let mut distance_data = vec![0.0; n_test * n_train];
        for i in 0..n_test {
            for j in 0..n_train {
                distance_data[i + j * n_test] = ((i as f64) - (j as f64)).abs();
            }
        }
        let distance_matrix = FdMatrix::from_column_major(distance_data, n_test, n_train).unwrap();

        let y: Vec<f64> = (0..n_train).map(|i| i as f64).collect();
        let predictions = knn_predict(&distance_matrix, &y, k);

        assert_eq!(predictions.len(), n_test);
    }

    // ============== compute_adot tests ==============

    #[test]
    fn test_compute_adot_basic() {
        let n = 4;
        // Upper-triangular packed inner product: (n*(n+1))/2 = 10 elements
        // Layout: (1,1), (2,1), (2,2), (3,1), (3,2), (3,3), (4,1), (4,2), (4,3), (4,4)
        let mut inprod = vec![0.0; (n * (n + 1)) / 2];
        // Set diagonal to 1.0 (identity-like)
        // idx for (i,i): i*(i-1)/2 + i - 1 = i*(i+1)/2 - 1
        for i in 1..=n {
            let idx = i * (i - 1) / 2 + i - 1;
            inprod[idx] = 1.0;
        }

        let adot = compute_adot(n, &inprod);

        let expected_len = (n * n - n + 2) / 2;
        assert_eq!(
            adot.len(),
            expected_len,
            "Adot length should be (n^2-n+2)/2"
        );
        assert!(
            (adot[0] - PI * (n + 1) as f64).abs() < 1e-10,
            "First element should be π*(n+1), got {}",
            adot[0]
        );
        for (i, &val) in adot.iter().enumerate() {
            assert!(val.is_finite(), "Adot[{}] should be finite, got {}", i, val);
        }
    }

    #[test]
    fn test_compute_adot_n1() {
        let n = 1;
        let inprod = vec![1.0]; // (1*(1+1))/2 = 1
        let adot = compute_adot(n, &inprod);

        assert_eq!(adot.len(), 1, "n=1 should give length 1");
        assert!(
            (adot[0] - PI * 2.0).abs() < 1e-10,
            "n=1: first element should be π*2, got {}",
            adot[0]
        );
    }

    #[test]
    fn test_compute_adot_invalid() {
        // n=0
        assert!(compute_adot(0, &[]).is_empty());

        // Wrong inprod length
        assert!(compute_adot(4, &[1.0, 2.0]).is_empty());
    }

    // ============== pcvm_statistic tests ==============

    #[test]
    fn test_pcvm_statistic_basic() {
        let n = 4;
        let mut inprod = vec![0.0; (n * (n + 1)) / 2];
        for i in 1..=n {
            let idx = i * (i - 1) / 2 + i - 1;
            inprod[idx] = 1.0;
        }
        let adot = compute_adot(n, &inprod);
        let residuals = vec![0.5, -0.3, 0.2, -0.1];

        let stat = pcvm_statistic(&adot, &residuals);

        assert!(stat.is_finite(), "PCvM statistic should be finite");
        assert!(stat >= 0.0, "PCvM statistic should be non-negative");
    }

    #[test]
    fn test_pcvm_statistic_zero_residuals() {
        let n = 4;
        let mut inprod = vec![0.0; (n * (n + 1)) / 2];
        for i in 1..=n {
            let idx = i * (i - 1) / 2 + i - 1;
            inprod[idx] = 1.0;
        }
        let adot = compute_adot(n, &inprod);
        let residuals = vec![0.0, 0.0, 0.0, 0.0];

        let stat = pcvm_statistic(&adot, &residuals);
        assert!(
            stat.abs() < 1e-10,
            "PCvM with zero residuals should be ~0, got {}",
            stat
        );
    }

    #[test]
    fn test_pcvm_statistic_empty() {
        assert!(pcvm_statistic(&[], &[]).abs() < 1e-10);
        assert!(pcvm_statistic(&[1.0], &[]).abs() < 1e-10);
    }

    // ============== rp_stat tests ==============

    #[test]
    fn test_rp_stat_basic() {
        let n_proj = 3;
        let residuals = vec![0.5, -0.3, 0.2, -0.1, 0.4];

        // proj_x_ord is n_proj columns of n rows, 1-indexed ranks
        let proj_x_ord: Vec<i32> = vec![
            1, 3, 5, 2, 4, // projection 1
            2, 4, 1, 5, 3, // projection 2
            5, 1, 3, 4, 2, // projection 3
        ];

        let result = rp_stat(&proj_x_ord, &residuals, n_proj);

        assert_eq!(result.cvm.len(), n_proj);
        assert_eq!(result.ks.len(), n_proj);
        for &cvm_val in &result.cvm {
            assert!(cvm_val >= 0.0, "CvM stat should be non-negative");
            assert!(cvm_val.is_finite(), "CvM stat should be finite");
        }
        for &ks_val in &result.ks {
            assert!(ks_val >= 0.0, "KS stat should be non-negative");
            assert!(ks_val.is_finite(), "KS stat should be finite");
        }
    }

    #[test]
    fn test_rp_stat_invalid() {
        let result = rp_stat(&[], &[], 0);
        assert!(result.cvm.is_empty());
        assert!(result.ks.is_empty());

        let result = rp_stat(&[], &[1.0], 0);
        assert!(result.cvm.is_empty());
    }

    // ============== knn_loocv tests ==============

    #[test]
    fn test_knn_loocv_basic() {
        let size = 5;
        let k = 2;
        // Simple distance matrix
        let mut dist_data = vec![0.0; size * size];
        for i in 0..size {
            for j in 0..size {
                dist_data[i + j * size] = ((i as f64) - (j as f64)).abs();
            }
        }
        let dist = FdMatrix::from_column_major(dist_data, size, size).unwrap();
        let y: Vec<f64> = (0..size).map(|i| i as f64 * 2.0).collect();

        let mse = knn_loocv(&dist, &y, k);

        assert!(mse.is_finite(), "k-NN LOOCV MSE should be finite");
        assert!(mse >= 0.0, "k-NN LOOCV MSE should be non-negative");
    }

    #[test]
    fn test_knn_loocv_perfect() {
        // When nearest neighbors have the same y value, MSE should be ~0
        let n = 4;
        let k = 1;
        // Distance matrix where each pair of adjacent points is close
        let mut dist = FdMatrix::from_column_major(vec![100.0; n * n], n, n).unwrap();
        for i in 0..n {
            dist[(i, i)] = 0.0;
        }
        // Make pairs (0,1) and (2,3) very close
        dist[(0, 1)] = 0.1;
        dist[(1, 0)] = 0.1;
        dist[(2, 3)] = 0.1;
        dist[(3, 2)] = 0.1;

        // Same y for paired points
        let y = vec![1.0, 1.0, 5.0, 5.0];
        let mse = knn_loocv(&dist, &y, k);

        assert!(
            mse < 1e-10,
            "k-NN LOOCV MSE should be ~0 for perfectly paired data, got {}",
            mse
        );
    }

    #[test]
    fn test_knn_loocv_invalid() {
        let empty = FdMatrix::zeros(0, 0);
        assert!(knn_loocv(&empty, &[], 1).is_infinite());
        let single = FdMatrix::from_column_major(vec![0.0], 1, 1).unwrap();
        assert!(knn_loocv(&single, &[1.0], 0).is_infinite());
    }
}
