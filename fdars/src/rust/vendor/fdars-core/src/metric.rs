//! Distance metrics and semimetrics for functional data.
//!
//! This module provides various distance measures including:
//! - Lp distances (L1, L2, L∞)
//! - Hausdorff distance
//! - Dynamic Time Warping (DTW)
//! - Fourier-based semimetric
//! - Horizontal shift semimetric
//! - Kullback-Leibler divergence

use crate::helpers::{simpsons_weights, simpsons_weights_2d};
use crate::iter_maybe_parallel;
use crate::matrix::FdMatrix;
#[cfg(feature = "parallel")]
use rayon::iter::ParallelIterator;
use rustfft::num_complex::Complex;
use rustfft::FftPlanner;

/// Compute Lp distance matrix between two sets of functional data.
///
/// # Arguments
/// * `data1` - First dataset matrix (n1 rows x n_points columns)
/// * `data2` - Second dataset matrix (n2 rows x n_points columns)
/// * `argvals` - Evaluation points for integration
/// * `p` - Order of the norm
/// * `user_weights` - Optional user weights (empty slice for none)
///
/// # Returns
/// Distance matrix (n1 rows x n2 columns)
pub fn lp_cross_1d(
    data1: &FdMatrix,
    data2: &FdMatrix,
    argvals: &[f64],
    p: f64,
    user_weights: &[f64],
) -> FdMatrix {
    let n1 = data1.nrows();
    let n2 = data2.nrows();
    let n_points = data1.ncols();

    if n1 == 0 || n2 == 0 || n_points == 0 || argvals.len() != n_points || data2.ncols() != n_points
    {
        return FdMatrix::zeros(0, 0);
    }

    let base_weights = simpsons_weights(argvals);
    let weights: Vec<f64> = if user_weights.len() == n_points {
        base_weights
            .iter()
            .zip(user_weights.iter())
            .map(|(b, u)| b * u)
            .collect()
    } else {
        base_weights
    };

    let pairs: Vec<(usize, usize, f64)> = iter_maybe_parallel!(0..n2)
        .flat_map(|j| {
            (0..n1)
                .map(|i| {
                    let mut integral = 0.0;
                    for k in 0..n_points {
                        let diff = (data1[(i, k)] - data2[(j, k)]).abs();
                        integral += diff.powf(p) * weights[k];
                    }
                    (i, j, integral.powf(1.0 / p))
                })
                .collect::<Vec<_>>()
        })
        .collect();

    let mut dist = FdMatrix::zeros(n1, n2);
    for (i, j, d) in pairs {
        dist[(i, j)] = d;
    }
    dist
}

/// Compute Lp distance matrix for self-distances (symmetric).
///
/// Returns symmetric distance matrix (n rows x n columns).
pub fn lp_self_1d(data: &FdMatrix, argvals: &[f64], p: f64, user_weights: &[f64]) -> FdMatrix {
    let n = data.nrows();
    let n_points = data.ncols();

    if n == 0 || n_points == 0 || argvals.len() != n_points {
        return FdMatrix::zeros(0, 0);
    }

    let base_weights = simpsons_weights(argvals);
    let weights: Vec<f64> = if user_weights.len() == n_points {
        base_weights
            .iter()
            .zip(user_weights.iter())
            .map(|(b, u)| b * u)
            .collect()
    } else {
        base_weights
    };

    let upper_triangle: Vec<(usize, usize, f64)> = iter_maybe_parallel!(0..n)
        .flat_map(|i| {
            ((i + 1)..n)
                .map(|j| {
                    let mut integral = 0.0;
                    for k in 0..n_points {
                        let diff = (data[(i, k)] - data[(j, k)]).abs();
                        integral += diff.powf(p) * weights[k];
                    }
                    (i, j, integral.powf(1.0 / p))
                })
                .collect::<Vec<_>>()
        })
        .collect();

    let mut dist = FdMatrix::zeros(n, n);
    for (i, j, d) in upper_triangle {
        dist[(i, j)] = d;
        dist[(j, i)] = d;
    }

    dist
}

/// Compute Lp distance for 2D functional data (surfaces).
pub fn lp_cross_2d(
    data1: &FdMatrix,
    data2: &FdMatrix,
    argvals_s: &[f64],
    argvals_t: &[f64],
    p: f64,
    user_weights: &[f64],
) -> FdMatrix {
    let n1 = data1.nrows();
    let n2 = data2.nrows();
    let n_points = argvals_s.len() * argvals_t.len();
    if n1 == 0 || n2 == 0 || n_points == 0 || data1.ncols() != n_points || data2.ncols() != n_points
    {
        return FdMatrix::zeros(0, 0);
    }

    let base_weights = simpsons_weights_2d(argvals_s, argvals_t);
    let weights: Vec<f64> = if user_weights.len() == n_points {
        base_weights
            .iter()
            .zip(user_weights.iter())
            .map(|(bw, uw)| bw * uw)
            .collect()
    } else {
        base_weights
    };

    let pairs: Vec<(usize, usize, f64)> = iter_maybe_parallel!(0..n1)
        .flat_map(|i| {
            (0..n2)
                .map(|j| {
                    let mut sum = 0.0;
                    for k in 0..n_points {
                        let diff = (data1[(i, k)] - data2[(j, k)]).abs();
                        sum += weights[k] * diff.powf(p);
                    }
                    (i, j, sum.powf(1.0 / p))
                })
                .collect::<Vec<_>>()
        })
        .collect();

    let mut dist = FdMatrix::zeros(n1, n2);
    for (i, j, d) in pairs {
        dist[(i, j)] = d;
    }
    dist
}

/// Compute Lp self-distance matrix for 2D functional data (symmetric).
pub fn lp_self_2d(
    data: &FdMatrix,
    argvals_s: &[f64],
    argvals_t: &[f64],
    p: f64,
    user_weights: &[f64],
) -> FdMatrix {
    let n = data.nrows();
    let n_points = argvals_s.len() * argvals_t.len();
    if n == 0 || n_points == 0 || data.ncols() != n_points {
        return FdMatrix::zeros(0, 0);
    }

    let base_weights = simpsons_weights_2d(argvals_s, argvals_t);
    let weights: Vec<f64> = if user_weights.len() == n_points {
        base_weights
            .iter()
            .zip(user_weights.iter())
            .map(|(bw, uw)| bw * uw)
            .collect()
    } else {
        base_weights
    };

    let upper_triangle: Vec<(usize, usize, f64)> = iter_maybe_parallel!(0..n)
        .flat_map(|i| {
            ((i + 1)..n)
                .map(|j| {
                    let mut sum = 0.0;
                    for k in 0..n_points {
                        let diff = (data[(i, k)] - data[(j, k)]).abs();
                        sum += weights[k] * diff.powf(p);
                    }
                    (i, j, sum.powf(1.0 / p))
                })
                .collect::<Vec<_>>()
        })
        .collect();

    let mut dist = FdMatrix::zeros(n, n);
    for (i, j, d) in upper_triangle {
        dist[(i, j)] = d;
        dist[(j, i)] = d;
    }

    dist
}

/// Compute Hausdorff distance matrix for self-distances (symmetric).
///
/// The Hausdorff distance treats curves as sets of points (t, f(t)) in 2D space.
pub fn hausdorff_self_1d(data: &FdMatrix, argvals: &[f64]) -> FdMatrix {
    let n = data.nrows();
    let m = data.ncols();

    if n == 0 || m == 0 || argvals.len() != m {
        return FdMatrix::zeros(0, 0);
    }

    // Precompute squared time differences
    let mtt: Vec<f64> = {
        let mut result = Vec::with_capacity(m * m);
        for s in 0..m {
            for t in 0..m {
                let diff = argvals[s] - argvals[t];
                result.push(diff * diff);
            }
        }
        result
    };

    let upper_triangle: Vec<(usize, usize, f64)> = iter_maybe_parallel!(0..n)
        .flat_map(|i| {
            ((i + 1)..n)
                .map(|j| {
                    let max_row_min = (0..m)
                        .map(|s| {
                            let x_s = data[(i, s)];
                            (0..m)
                                .map(|t| {
                                    let y_t = data[(j, t)];
                                    let val_diff = x_s - y_t;
                                    (val_diff * val_diff + mtt[s * m + t]).sqrt()
                                })
                                .fold(f64::INFINITY, |a, b| a.min(b))
                        })
                        .fold(f64::NEG_INFINITY, |a, b| a.max(b));

                    let max_col_min = (0..m)
                        .map(|t| {
                            let y_t = data[(j, t)];
                            (0..m)
                                .map(|s| {
                                    let x_s = data[(i, s)];
                                    let val_diff = x_s - y_t;
                                    (val_diff * val_diff + mtt[s * m + t]).sqrt()
                                })
                                .fold(f64::INFINITY, |a, b| a.min(b))
                        })
                        .fold(f64::NEG_INFINITY, |a, b| a.max(b));

                    (i, j, max_row_min.max(max_col_min))
                })
                .collect::<Vec<_>>()
        })
        .collect();

    let mut dist = FdMatrix::zeros(n, n);
    for (i, j, d) in upper_triangle {
        dist[(i, j)] = d;
        dist[(j, i)] = d;
    }

    dist
}

/// Compute Hausdorff cross-distances for 1D functional data.
pub fn hausdorff_cross_1d(data1: &FdMatrix, data2: &FdMatrix, argvals: &[f64]) -> FdMatrix {
    let n1 = data1.nrows();
    let n2 = data2.nrows();
    let m = data1.ncols();

    if n1 == 0 || n2 == 0 || m == 0 || argvals.len() != m || data2.ncols() != m {
        return FdMatrix::zeros(0, 0);
    }

    let mtt: Vec<f64> = {
        let mut result = Vec::with_capacity(m * m);
        for s in 0..m {
            for t in 0..m {
                let diff = argvals[s] - argvals[t];
                result.push(diff * diff);
            }
        }
        result
    };

    let pairs: Vec<(usize, usize, f64)> = iter_maybe_parallel!(0..n1)
        .flat_map(|i| {
            (0..n2)
                .map(|j| {
                    let max_row_min = (0..m)
                        .map(|s| {
                            let x_s = data1[(i, s)];
                            (0..m)
                                .map(|t| {
                                    let y_t = data2[(j, t)];
                                    let val_diff = x_s - y_t;
                                    (val_diff * val_diff + mtt[s * m + t]).sqrt()
                                })
                                .fold(f64::INFINITY, |a, b| a.min(b))
                        })
                        .fold(f64::NEG_INFINITY, |a, b| a.max(b));

                    let max_col_min = (0..m)
                        .map(|t| {
                            let y_t = data2[(j, t)];
                            (0..m)
                                .map(|s| {
                                    let x_s = data1[(i, s)];
                                    let val_diff = x_s - y_t;
                                    (val_diff * val_diff + mtt[s * m + t]).sqrt()
                                })
                                .fold(f64::INFINITY, |a, b| a.min(b))
                        })
                        .fold(f64::NEG_INFINITY, |a, b| a.max(b));

                    (i, j, max_row_min.max(max_col_min))
                })
                .collect::<Vec<_>>()
        })
        .collect();

    let mut dist = FdMatrix::zeros(n1, n2);
    for (i, j, d) in pairs {
        dist[(i, j)] = d;
    }
    dist
}

/// Allocate and initialize the DTW cost matrix with boundary conditions and window band.
fn initialize_dtw_matrix(n: usize, m: usize, w: usize) -> Vec<Vec<f64>> {
    let mut dtw = vec![vec![0.0_f64; m + 1]; n + 1];
    for j in 0..=m {
        dtw[0][j] = f64::INFINITY;
    }
    for i in 0..=n {
        dtw[i][0] = f64::INFINITY;
    }
    dtw[0][0] = 0.0;
    for i in 1..=n {
        let r_i = i + 1;
        let j_start_r = 2.max(r_i as isize - w as isize) as usize;
        let j_end_r = (m + 1).min(r_i + w);
        for j_r in j_start_r..=j_end_r {
            let j = j_r - 1;
            if j <= m {
                dtw[i][j] = 0.0;
            }
        }
    }
    dtw
}

/// Fill the DTW matrix using dynamic programming within the Sakoe-Chiba band.
fn fill_dtw_matrix(
    dtw: &mut [Vec<f64>],
    x: &[f64],
    y: &[f64],
    n: usize,
    m: usize,
    w: usize,
    p: f64,
) {
    for i in 1..=n {
        let r_i = i + 1;
        let j_start_r = 2.max(r_i as isize - w as isize) as usize;
        let j_end_r = (m + 1).min(r_i + w);
        for j_r in j_start_r..=j_end_r {
            let j = j_r - 1;
            if j <= m && j >= 1 {
                let cost = (x[i - 1] - y[j - 1]).abs().powf(p);
                dtw[i][j] = cost + dtw[i - 1][j].min(dtw[i][j - 1]).min(dtw[i - 1][j - 1]);
            }
        }
    }
}

/// Compute DTW distance between two time series.
pub fn dtw_distance(x: &[f64], y: &[f64], p: f64, w: usize) -> f64 {
    let n = x.len();
    let m = y.len();
    let mut dtw = initialize_dtw_matrix(n, m, w);
    fill_dtw_matrix(&mut dtw, x, y, n, m, w, p);
    dtw[n][m]
}

/// Compute DTW distance matrix for self-distances (symmetric).
pub fn dtw_self_1d(data: &FdMatrix, p: f64, w: usize) -> FdMatrix {
    let n = data.nrows();
    let m = data.ncols();
    if n == 0 || m == 0 {
        return FdMatrix::zeros(0, 0);
    }
    let curves = data.rows();
    let upper_triangle: Vec<(usize, usize, f64)> = iter_maybe_parallel!(0..n)
        .flat_map(|i| {
            ((i + 1)..n)
                .map(|j| {
                    let d = dtw_distance(&curves[i], &curves[j], p, w);
                    (i, j, d)
                })
                .collect::<Vec<_>>()
        })
        .collect();
    let mut dist = FdMatrix::zeros(n, n);
    for (i, j, d) in upper_triangle {
        dist[(i, j)] = d;
        dist[(j, i)] = d;
    }
    dist
}

/// Compute DTW cross-distance matrix.
pub fn dtw_cross_1d(data1: &FdMatrix, data2: &FdMatrix, p: f64, w: usize) -> FdMatrix {
    let n1 = data1.nrows();
    let n2 = data2.nrows();
    let m1 = data1.ncols();
    let m2 = data2.ncols();
    if n1 == 0 || n2 == 0 || m1 == 0 || m2 == 0 {
        return FdMatrix::zeros(0, 0);
    }
    let curves1 = data1.rows();
    let curves2 = data2.rows();
    let pairs: Vec<(usize, usize, f64)> = iter_maybe_parallel!(0..n1)
        .flat_map(|i| {
            (0..n2)
                .map(|j| (i, j, dtw_distance(&curves1[i], &curves2[j], p, w)))
                .collect::<Vec<_>>()
        })
        .collect();
    let mut dist = FdMatrix::zeros(n1, n2);
    for (i, j, d) in pairs {
        dist[(i, j)] = d;
    }
    dist
}

/// Compute Fourier coefficients for a curve using FFT.
fn fft_coefficients(data: &[f64], nfreq: usize) -> Vec<f64> {
    let n = data.len();
    let nfreq = nfreq.min(n / 2);
    let mut planner = FftPlanner::<f64>::new();
    let fft = planner.plan_fft_forward(n);
    let mut buffer: Vec<Complex<f64>> = data.iter().map(|&x| Complex::new(x, 0.0)).collect();
    fft.process(&mut buffer);
    buffer
        .iter()
        .take(nfreq + 1)
        .map(|c| c.norm() / n as f64)
        .collect()
}

/// Compute semimetric based on Fourier coefficients for self-distances.
pub fn fourier_self_1d(data: &FdMatrix, nfreq: usize) -> FdMatrix {
    let n = data.nrows();
    let m = data.ncols();
    if n == 0 || m == 0 {
        return FdMatrix::zeros(0, 0);
    }
    let curves = data.rows();
    let coeffs: Vec<Vec<f64>> = iter_maybe_parallel!(curves)
        .map(|curve| fft_coefficients(&curve, nfreq))
        .collect();
    let upper_triangle: Vec<(usize, usize, f64)> = iter_maybe_parallel!(0..n)
        .flat_map(|i| {
            ((i + 1)..n)
                .map(|j| {
                    let dist_sq: f64 = coeffs[i]
                        .iter()
                        .zip(coeffs[j].iter())
                        .map(|(a, b)| (a - b).powi(2))
                        .sum();
                    (i, j, dist_sq.sqrt())
                })
                .collect::<Vec<_>>()
        })
        .collect();
    let mut dist = FdMatrix::zeros(n, n);
    for (i, j, d) in upper_triangle {
        dist[(i, j)] = d;
        dist[(j, i)] = d;
    }
    dist
}

/// Compute semimetric based on Fourier coefficients for cross-distances.
pub fn fourier_cross_1d(data1: &FdMatrix, data2: &FdMatrix, nfreq: usize) -> FdMatrix {
    let n1 = data1.nrows();
    let n2 = data2.nrows();
    let m = data1.ncols();
    if n1 == 0 || n2 == 0 || m == 0 || data2.ncols() != m {
        return FdMatrix::zeros(0, 0);
    }
    let curves1 = data1.rows();
    let curves2 = data2.rows();
    let coeffs1: Vec<Vec<f64>> = iter_maybe_parallel!(curves1)
        .map(|curve| fft_coefficients(&curve, nfreq))
        .collect();
    let coeffs2: Vec<Vec<f64>> = iter_maybe_parallel!(curves2)
        .map(|curve| fft_coefficients(&curve, nfreq))
        .collect();
    let pairs: Vec<(usize, usize, f64)> = iter_maybe_parallel!(0..n1)
        .flat_map(|i| {
            (0..n2)
                .map(|j| {
                    let dist_sq: f64 = coeffs1[i]
                        .iter()
                        .zip(coeffs2[j].iter())
                        .map(|(a, b)| (a - b).powi(2))
                        .sum();
                    (i, j, dist_sq.sqrt())
                })
                .collect::<Vec<_>>()
        })
        .collect();
    let mut dist = FdMatrix::zeros(n1, n2);
    for (i, j, d) in pairs {
        dist[(i, j)] = d;
    }
    dist
}

/// Compute weighted L2 distance at a given horizontal shift.
fn shifted_l2_distance(x: &[f64], y: &[f64], weights: &[f64], shift: i32) -> Option<f64> {
    let n = x.len();
    let mut sum = 0.0;
    let mut valid_points = 0;
    for i in 0..n {
        let j = i as i32 + shift;
        if j >= 0 && (j as usize) < n {
            let diff = x[i] - y[j as usize];
            sum += weights[i] * diff * diff;
            valid_points += 1;
        }
    }
    if valid_points >= n / 2 {
        Some(sum.sqrt())
    } else {
        None
    }
}

/// Compute minimum L2 distance after horizontal shift between two curves.
fn hshift_distance(x: &[f64], y: &[f64], weights: &[f64], max_shift: usize) -> f64 {
    if x.is_empty() || y.len() != x.len() || weights.len() != x.len() {
        return f64::INFINITY;
    }
    let mut min_dist = f64::INFINITY;
    for shift in -(max_shift as i32)..=(max_shift as i32) {
        if let Some(dist) = shifted_l2_distance(x, y, weights, shift) {
            if dist < min_dist {
                min_dist = dist;
            }
        }
    }
    min_dist
}

/// Compute semimetric based on horizontal shift for self-distances.
pub fn hshift_self_1d(data: &FdMatrix, argvals: &[f64], max_shift: usize) -> FdMatrix {
    let n = data.nrows();
    let m = data.ncols();
    if n == 0 || m == 0 || argvals.len() != m {
        return FdMatrix::zeros(0, 0);
    }
    let weights = simpsons_weights(argvals);
    let curves = data.rows();
    let upper_triangle: Vec<(usize, usize, f64)> = iter_maybe_parallel!(0..n)
        .flat_map(|i| {
            ((i + 1)..n)
                .map(|j| {
                    let d = hshift_distance(&curves[i], &curves[j], &weights, max_shift);
                    (i, j, d)
                })
                .collect::<Vec<_>>()
        })
        .collect();
    let mut dist = FdMatrix::zeros(n, n);
    for (i, j, d) in upper_triangle {
        dist[(i, j)] = d;
        dist[(j, i)] = d;
    }
    dist
}

/// Compute semimetric based on horizontal shift for cross-distances.
pub fn hshift_cross_1d(
    data1: &FdMatrix,
    data2: &FdMatrix,
    argvals: &[f64],
    max_shift: usize,
) -> FdMatrix {
    let n1 = data1.nrows();
    let n2 = data2.nrows();
    let m = data1.ncols();
    if n1 == 0 || n2 == 0 || m == 0 || argvals.len() != m || data2.ncols() != m {
        return FdMatrix::zeros(0, 0);
    }
    let weights = simpsons_weights(argvals);
    let curves1 = data1.rows();
    let curves2 = data2.rows();
    let pairs: Vec<(usize, usize, f64)> = iter_maybe_parallel!(0..n1)
        .flat_map(|i| {
            (0..n2)
                .map(|j| {
                    (
                        i,
                        j,
                        hshift_distance(&curves1[i], &curves2[j], &weights, max_shift),
                    )
                })
                .collect::<Vec<_>>()
        })
        .collect();
    let mut dist = FdMatrix::zeros(n1, n2);
    for (i, j, d) in pairs {
        dist[(i, j)] = d;
    }
    dist
}

/// Compute Hausdorff distance between two 3D point clouds.
pub fn hausdorff_3d(points1: &[(f64, f64, f64)], points2: &[(f64, f64, f64)]) -> f64 {
    let h12 = points1
        .iter()
        .map(|p1| {
            points2
                .iter()
                .map(|p2| {
                    let ds = p1.0 - p2.0;
                    let dt = p1.1 - p2.1;
                    let df = p1.2 - p2.2;
                    (ds * ds + dt * dt + df * df).sqrt()
                })
                .fold(f64::INFINITY, f64::min)
        })
        .fold(0.0_f64, f64::max);
    let h21 = points2
        .iter()
        .map(|p2| {
            points1
                .iter()
                .map(|p1| {
                    let ds = p1.0 - p2.0;
                    let dt = p1.1 - p2.1;
                    let df = p1.2 - p2.2;
                    (ds * ds + dt * dt + df * df).sqrt()
                })
                .fold(f64::INFINITY, f64::min)
        })
        .fold(0.0_f64, f64::max);
    h12.max(h21)
}

/// Extract 2D surface points from a matrix for Hausdorff distance computation.
fn extract_surfaces(
    data: &FdMatrix,
    argvals_s: &[f64],
    argvals_t: &[f64],
) -> Vec<Vec<(f64, f64, f64)>> {
    let n = data.nrows();
    let m1 = argvals_s.len();
    let m2 = argvals_t.len();
    (0..n)
        .map(|curve| {
            let mut points = Vec::with_capacity(m1 * m2);
            for i in 0..m1 {
                for j in 0..m2 {
                    let k = i * m2 + j;
                    points.push((argvals_s[i], argvals_t[j], data[(curve, k)]));
                }
            }
            points
        })
        .collect()
}

/// Compute Hausdorff self-distance for 2D surfaces.
pub fn hausdorff_self_2d(data: &FdMatrix, argvals_s: &[f64], argvals_t: &[f64]) -> FdMatrix {
    let n = data.nrows();
    let n_points = argvals_s.len() * argvals_t.len();
    if n == 0 || n_points == 0 || data.ncols() != n_points {
        return FdMatrix::zeros(0, 0);
    }
    let surfaces = extract_surfaces(data, argvals_s, argvals_t);
    let upper_triangle: Vec<(usize, usize, f64)> = iter_maybe_parallel!(0..n)
        .flat_map(|i| {
            ((i + 1)..n)
                .map(|j| {
                    let d = hausdorff_3d(&surfaces[i], &surfaces[j]);
                    (i, j, d)
                })
                .collect::<Vec<_>>()
        })
        .collect();
    let mut dist = FdMatrix::zeros(n, n);
    for (i, j, d) in upper_triangle {
        dist[(i, j)] = d;
        dist[(j, i)] = d;
    }
    dist
}

/// Compute Hausdorff cross-distance for 2D surfaces.
pub fn hausdorff_cross_2d(
    data1: &FdMatrix,
    data2: &FdMatrix,
    argvals_s: &[f64],
    argvals_t: &[f64],
) -> FdMatrix {
    let n1 = data1.nrows();
    let n2 = data2.nrows();
    let n_points = argvals_s.len() * argvals_t.len();
    if n1 == 0 || n2 == 0 || n_points == 0 || data1.ncols() != n_points || data2.ncols() != n_points
    {
        return FdMatrix::zeros(0, 0);
    }
    let surfaces1 = extract_surfaces(data1, argvals_s, argvals_t);
    let surfaces2 = extract_surfaces(data2, argvals_s, argvals_t);
    let pairs: Vec<(usize, usize, f64)> = iter_maybe_parallel!(0..n1)
        .flat_map(|i| {
            (0..n2)
                .map(|j| (i, j, hausdorff_3d(&surfaces1[i], &surfaces2[j])))
                .collect::<Vec<_>>()
        })
        .collect();
    let mut dist = FdMatrix::zeros(n1, n2);
    for (i, j, d) in pairs {
        dist[(i, j)] = d;
    }
    dist
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::f64::consts::PI;

    fn uniform_grid(n: usize) -> Vec<f64> {
        (0..n).map(|i| i as f64 / (n - 1) as f64).collect()
    }

    #[test]
    fn test_lp_self_distance() {
        let data = FdMatrix::from_column_major(vec![0.0, 1.0, 1.0, 2.0], 2, 2).unwrap();
        let argvals = vec![0.0, 1.0];
        let dist = lp_self_1d(&data, &argvals, 2.0, &[]);
        assert!((dist[(0, 1)] - 1.0).abs() < 0.1);
    }

    #[test]
    fn test_lp_self_symmetric() {
        let n = 5;
        let m = 20;
        let argvals = uniform_grid(m);
        let mut flat = vec![0.0; n * m];
        for i in 0..n {
            for j in 0..m {
                flat[i + j * n] = (2.0 * PI * argvals[j] * (i as f64 + 1.0)).sin();
            }
        }
        let data = FdMatrix::from_column_major(flat, n, m).unwrap();
        let dist = lp_self_1d(&data, &argvals, 2.0, &[]);
        for i in 0..n {
            for j in 0..n {
                assert!(
                    (dist[(i, j)] - dist[(j, i)]).abs() < 1e-10,
                    "Distance matrix should be symmetric"
                );
            }
        }
    }

    #[test]
    fn test_lp_self_diagonal_zero() {
        let n = 4;
        let m = 15;
        let argvals = uniform_grid(m);
        let flat: Vec<f64> = (0..(n * m)).map(|i| i as f64 * 0.1).collect();
        let data = FdMatrix::from_column_major(flat, n, m).unwrap();
        let dist = lp_self_1d(&data, &argvals, 2.0, &[]);
        for i in 0..n {
            assert!(dist[(i, i)].abs() < 1e-10, "Self-distance should be zero");
        }
    }

    #[test]
    fn test_lp_cross_shape() {
        let n1 = 3;
        let n2 = 4;
        let m = 20;
        let argvals = uniform_grid(m);
        let data1 =
            FdMatrix::from_column_major((0..(n1 * m)).map(|i| i as f64 * 0.1).collect(), n1, m)
                .unwrap();
        let data2 =
            FdMatrix::from_column_major((0..(n2 * m)).map(|i| i as f64 * 0.2).collect(), n2, m)
                .unwrap();
        let dist = lp_cross_1d(&data1, &data2, &argvals, 2.0, &[]);
        assert_eq!(dist.nrows(), n1);
        assert_eq!(dist.ncols(), n2);
    }

    #[test]
    fn test_lp_invalid() {
        let empty = FdMatrix::zeros(0, 0);
        assert!(lp_self_1d(&empty, &[], 2.0, &[]).is_empty());
        assert!(lp_cross_1d(&empty, &empty, &[], 2.0, &[]).is_empty());
    }

    #[test]
    fn test_dtw_distance() {
        let x = vec![1.0, 2.0, 3.0];
        let y = vec![1.0, 2.0, 3.0];
        let dist = dtw_distance(&x, &y, 2.0, 10);
        assert!((dist - 0.0).abs() < 1e-10);
    }

    #[test]
    fn test_dtw_distance_different() {
        let x = vec![1.0, 2.0, 3.0];
        let y = vec![2.0, 3.0, 4.0];
        let dist = dtw_distance(&x, &y, 1.0, 10);
        assert!(
            dist > 0.0,
            "Different curves should have positive DTW distance"
        );
    }

    #[test]
    fn test_dtw_self_symmetric() {
        let n = 4;
        let m = 15;
        let data =
            FdMatrix::from_column_major((0..(n * m)).map(|i| i as f64 * 0.1).collect(), n, m)
                .unwrap();
        let dist = dtw_self_1d(&data, 2.0, 5);
        for i in 0..n {
            for j in 0..n {
                assert!(
                    (dist[(i, j)] - dist[(j, i)]).abs() < 1e-10,
                    "DTW matrix should be symmetric"
                );
            }
        }
    }

    #[test]
    fn test_dtw_invalid() {
        let empty = FdMatrix::zeros(0, 0);
        assert!(dtw_self_1d(&empty, 2.0, 5).is_empty());
    }

    #[test]
    fn test_hausdorff_self_symmetric() {
        let n = 4;
        let m = 15;
        let argvals = uniform_grid(m);
        let data = FdMatrix::from_column_major(
            (0..(n * m)).map(|i| (i as f64 * 0.1).sin()).collect(),
            n,
            m,
        )
        .unwrap();
        let dist = hausdorff_self_1d(&data, &argvals);
        for i in 0..n {
            for j in 0..n {
                assert!(
                    (dist[(i, j)] - dist[(j, i)]).abs() < 1e-10,
                    "Hausdorff matrix should be symmetric"
                );
            }
        }
    }

    #[test]
    fn test_hausdorff_self_diagonal_zero() {
        let n = 3;
        let m = 10;
        let argvals = uniform_grid(m);
        let data =
            FdMatrix::from_column_major((0..(n * m)).map(|i| i as f64 * 0.1).collect(), n, m)
                .unwrap();
        let dist = hausdorff_self_1d(&data, &argvals);
        for i in 0..n {
            assert!(dist[(i, i)].abs() < 1e-10, "Self-distance should be zero");
        }
    }

    #[test]
    fn test_hausdorff_invalid() {
        let empty = FdMatrix::zeros(0, 0);
        assert!(hausdorff_self_1d(&empty, &[]).is_empty());
    }

    #[test]
    fn test_fourier_self_symmetric() {
        let n = 4;
        let m = 32;
        let data = FdMatrix::from_column_major(
            (0..(n * m)).map(|i| (i as f64 * 0.1).sin()).collect(),
            n,
            m,
        )
        .unwrap();
        let dist = fourier_self_1d(&data, 5);
        for i in 0..n {
            for j in 0..n {
                assert!(
                    (dist[(i, j)] - dist[(j, i)]).abs() < 1e-10,
                    "Fourier distance should be symmetric"
                );
            }
        }
    }

    #[test]
    fn test_fourier_self_diagonal_zero() {
        let n = 3;
        let m = 32;
        let data = FdMatrix::from_column_major(
            (0..(n * m)).map(|i| (i as f64 * 0.2).cos()).collect(),
            n,
            m,
        )
        .unwrap();
        let dist = fourier_self_1d(&data, 8);
        for i in 0..n {
            assert!(dist[(i, i)].abs() < 1e-10, "Self-distance should be zero");
        }
    }

    #[test]
    fn test_fourier_invalid() {
        let empty = FdMatrix::zeros(0, 0);
        assert!(fourier_self_1d(&empty, 5).is_empty());
    }

    #[test]
    fn test_hshift_self_symmetric() {
        let n = 4;
        let m = 20;
        let argvals = uniform_grid(m);
        let data = FdMatrix::from_column_major(
            (0..(n * m)).map(|i| (i as f64 * 0.1).sin()).collect(),
            n,
            m,
        )
        .unwrap();
        let dist = hshift_self_1d(&data, &argvals, 3);
        for i in 0..n {
            for j in 0..n {
                assert!(
                    (dist[(i, j)] - dist[(j, i)]).abs() < 1e-10,
                    "Hshift distance should be symmetric"
                );
            }
        }
    }

    #[test]
    fn test_hshift_invalid() {
        let empty = FdMatrix::zeros(0, 0);
        assert!(hshift_self_1d(&empty, &[], 3).is_empty());
    }

    #[test]
    fn test_hausdorff_3d_identical() {
        let points1 = vec![(0.0, 0.0, 0.0), (1.0, 1.0, 1.0)];
        let points2 = vec![(0.0, 0.0, 0.0), (1.0, 1.0, 1.0)];
        let dist = hausdorff_3d(&points1, &points2);
        assert!(
            dist.abs() < 1e-10,
            "Identical point sets should have zero distance"
        );
    }

    #[test]
    fn test_hausdorff_3d_different() {
        let points1 = vec![(0.0, 0.0, 0.0)];
        let points2 = vec![(1.0, 1.0, 1.0)];
        let dist = hausdorff_3d(&points1, &points2);
        let expected = (3.0_f64).sqrt();
        assert!(
            (dist - expected).abs() < 1e-10,
            "Expected {}, got {}",
            expected,
            dist
        );
    }

    #[test]
    fn test_lp_2d_symmetric() {
        let n = 3;
        let m1 = 4;
        let m2 = 5;
        let argvals_s = uniform_grid(m1);
        let argvals_t = uniform_grid(m2);
        let n_points = m1 * m2;
        let data = FdMatrix::from_column_major(
            (0..(n * n_points)).map(|i| i as f64 * 0.1).collect(),
            n,
            n_points,
        )
        .unwrap();
        let dist = lp_self_2d(&data, &argvals_s, &argvals_t, 2.0, &[]);
        for i in 0..n {
            for j in 0..n {
                assert!(
                    (dist[(i, j)] - dist[(j, i)]).abs() < 1e-10,
                    "2D Lp distance should be symmetric"
                );
            }
        }
    }

    #[test]
    fn test_lp_2d_invalid() {
        let empty = FdMatrix::zeros(0, 0);
        assert!(lp_self_2d(&empty, &[], &[], 2.0, &[]).is_empty());
    }

    #[test]
    fn test_hausdorff_2d_symmetric() {
        let n = 3;
        let m1 = 4;
        let m2 = 5;
        let argvals_s = uniform_grid(m1);
        let argvals_t = uniform_grid(m2);
        let n_points = m1 * m2;
        let data = FdMatrix::from_column_major(
            (0..(n * n_points))
                .map(|i| (i as f64 * 0.1).sin())
                .collect(),
            n,
            n_points,
        )
        .unwrap();
        let dist = hausdorff_self_2d(&data, &argvals_s, &argvals_t);
        for i in 0..n {
            for j in 0..n {
                assert!(
                    (dist[(i, j)] - dist[(j, i)]).abs() < 1e-10,
                    "2D Hausdorff should be symmetric"
                );
            }
        }
    }

    #[test]
    fn test_hausdorff_2d_invalid() {
        let empty = FdMatrix::zeros(0, 0);
        assert!(hausdorff_self_2d(&empty, &[], &[]).is_empty());
    }

    #[test]
    fn test_hausdorff_cross_1d() {
        let n1 = 3;
        let n2 = 4;
        let m = 15;
        let argvals = uniform_grid(m);
        let data1 = FdMatrix::from_column_major(
            (0..(n1 * m)).map(|i| (i as f64 * 0.1).sin()).collect(),
            n1,
            m,
        )
        .unwrap();
        let data2 = FdMatrix::from_column_major(
            (0..(n2 * m)).map(|i| (i as f64 * 0.2).cos()).collect(),
            n2,
            m,
        )
        .unwrap();
        let dist = hausdorff_cross_1d(&data1, &data2, &argvals);
        assert_eq!(dist.nrows(), n1);
        assert_eq!(dist.ncols(), n2);
        for j in 0..n2 {
            for i in 0..n1 {
                assert!(
                    dist[(i, j)] >= 0.0,
                    "Hausdorff cross distance should be non-negative"
                );
                assert!(
                    dist[(i, j)].is_finite(),
                    "Hausdorff cross distance should be finite"
                );
            }
        }
        let self_dist = hausdorff_self_1d(&data1, &argvals);
        let cross_self = hausdorff_cross_1d(&data1, &data1, &argvals);
        for i in 0..n1 {
            assert!(
                (cross_self[(i, i)] - self_dist[(i, i)]).abs() < 1e-10,
                "Cross-self diagonal should match self diagonal at {}",
                i
            );
        }
    }

    #[test]
    fn test_dtw_cross_1d() {
        let n1 = 3;
        let n2 = 4;
        let m = 15;
        let data1 = FdMatrix::from_column_major(
            (0..(n1 * m)).map(|i| (i as f64 * 0.1).sin()).collect(),
            n1,
            m,
        )
        .unwrap();
        let data2 = FdMatrix::from_column_major(
            (0..(n2 * m)).map(|i| (i as f64 * 0.2).cos()).collect(),
            n2,
            m,
        )
        .unwrap();
        let dist = dtw_cross_1d(&data1, &data2, 2.0, 5);
        assert_eq!(dist.nrows(), n1);
        assert_eq!(dist.ncols(), n2);
        for j in 0..n2 {
            for i in 0..n1 {
                assert!(
                    dist[(i, j)] >= 0.0,
                    "DTW cross distance should be non-negative"
                );
                assert!(
                    dist[(i, j)].is_finite(),
                    "DTW cross distance should be finite"
                );
            }
        }
        let data_same = FdMatrix::from_column_major(
            (0..(n1 * m)).map(|i| (i as f64 * 0.1).sin()).collect(),
            n1,
            m,
        )
        .unwrap();
        let cross_self = dtw_cross_1d(&data_same, &data_same, 2.0, 5);
        for i in 0..n1 {
            assert!(
                cross_self[(i, i)] < 1e-8,
                "DTW self-distance should be ~0, got {}",
                cross_self[(i, i)]
            );
        }
    }

    #[test]
    fn test_fourier_cross_1d() {
        let n1 = 3;
        let n2 = 4;
        let m = 32;
        let data1 = FdMatrix::from_column_major(
            (0..(n1 * m)).map(|i| (i as f64 * 0.1).sin()).collect(),
            n1,
            m,
        )
        .unwrap();
        let data2 = FdMatrix::from_column_major(
            (0..(n2 * m)).map(|i| (i as f64 * 0.2).cos()).collect(),
            n2,
            m,
        )
        .unwrap();
        let dist = fourier_cross_1d(&data1, &data2, 5);
        assert_eq!(dist.nrows(), n1);
        assert_eq!(dist.ncols(), n2);
        for j in 0..n2 {
            for i in 0..n1 {
                assert!(
                    dist[(i, j)] >= 0.0,
                    "Fourier cross distance should be non-negative"
                );
                assert!(
                    dist[(i, j)].is_finite(),
                    "Fourier cross distance should be finite"
                );
            }
        }
    }

    #[test]
    fn test_hshift_cross_1d() {
        let n1 = 3;
        let n2 = 4;
        let m = 20;
        let argvals = uniform_grid(m);
        let data1 = FdMatrix::from_column_major(
            (0..(n1 * m)).map(|i| (i as f64 * 0.1).sin()).collect(),
            n1,
            m,
        )
        .unwrap();
        let data2 = FdMatrix::from_column_major(
            (0..(n2 * m)).map(|i| (i as f64 * 0.2).cos()).collect(),
            n2,
            m,
        )
        .unwrap();
        let dist = hshift_cross_1d(&data1, &data2, &argvals, 3);
        assert_eq!(dist.nrows(), n1);
        assert_eq!(dist.ncols(), n2);
        for j in 0..n2 {
            for i in 0..n1 {
                assert!(
                    dist[(i, j)] >= 0.0,
                    "Hshift cross distance should be non-negative"
                );
                assert!(
                    dist[(i, j)].is_finite(),
                    "Hshift cross distance should be finite"
                );
            }
        }
    }

    #[test]
    fn test_hausdorff_self_2d_properties() {
        let n = 3;
        let m1 = 4;
        let m2 = 5;
        let argvals_s = uniform_grid(m1);
        let argvals_t = uniform_grid(m2);
        let n_points = m1 * m2;
        let data = FdMatrix::from_column_major(
            (0..(n * n_points))
                .map(|i| (i as f64 * 0.1).sin())
                .collect(),
            n,
            n_points,
        )
        .unwrap();
        let dist = hausdorff_self_2d(&data, &argvals_s, &argvals_t);
        for i in 0..n {
            assert!(
                dist[(i, i)].abs() < 1e-10,
                "Hausdorff 2D self-distance should be zero on diagonal"
            );
        }
        for i in 0..n {
            for j in 0..n {
                assert!(
                    (dist[(i, j)] - dist[(j, i)]).abs() < 1e-10,
                    "Hausdorff 2D should be symmetric"
                );
            }
        }
    }

    #[test]
    fn test_hausdorff_cross_2d() {
        let n1 = 2;
        let n2 = 3;
        let m1 = 4;
        let m2 = 5;
        let argvals_s = uniform_grid(m1);
        let argvals_t = uniform_grid(m2);
        let n_points = m1 * m2;
        let data1 = FdMatrix::from_column_major(
            (0..(n1 * n_points))
                .map(|i| (i as f64 * 0.1).sin())
                .collect(),
            n1,
            n_points,
        )
        .unwrap();
        let data2 = FdMatrix::from_column_major(
            (0..(n2 * n_points))
                .map(|i| (i as f64 * 0.2).cos())
                .collect(),
            n2,
            n_points,
        )
        .unwrap();
        let dist = hausdorff_cross_2d(&data1, &data2, &argvals_s, &argvals_t);
        assert_eq!(dist.nrows(), n1);
        assert_eq!(dist.ncols(), n2);
        for j in 0..n2 {
            for i in 0..n1 {
                assert!(
                    dist[(i, j)] >= 0.0,
                    "Hausdorff cross 2D should be non-negative"
                );
                assert!(
                    dist[(i, j)].is_finite(),
                    "Hausdorff cross 2D should be finite"
                );
            }
        }
    }

    #[test]
    fn test_lp_cross_2d() {
        let n1 = 2;
        let n2 = 3;
        let m1 = 4;
        let m2 = 5;
        let argvals_s = uniform_grid(m1);
        let argvals_t = uniform_grid(m2);
        let n_points = m1 * m2;
        let data1 = FdMatrix::from_column_major(
            (0..(n1 * n_points))
                .map(|i| (i as f64 * 0.1).sin())
                .collect(),
            n1,
            n_points,
        )
        .unwrap();
        let data2 = FdMatrix::from_column_major(
            (0..(n2 * n_points))
                .map(|i| (i as f64 * 0.2).cos())
                .collect(),
            n2,
            n_points,
        )
        .unwrap();
        let dist = lp_cross_2d(&data1, &data2, &argvals_s, &argvals_t, 2.0, &[]);
        assert_eq!(dist.nrows(), n1);
        assert_eq!(dist.ncols(), n2);
        for j in 0..n2 {
            for i in 0..n1 {
                assert!(dist[(i, j)] >= 0.0, "Lp cross 2D should be non-negative");
                assert!(dist[(i, j)].is_finite(), "Lp cross 2D should be finite");
            }
        }
        let user_weights: Vec<f64> = vec![1.0; n_points];
        let dist_w = lp_cross_2d(&data1, &data2, &argvals_s, &argvals_t, 2.0, &user_weights);
        assert_eq!(dist_w.nrows(), n1);
        assert_eq!(dist_w.ncols(), n2);
    }

    #[test]
    fn test_lp_self_2d_with_user_weights() {
        let n = 3;
        let m1 = 4;
        let m2 = 5;
        let argvals_s = uniform_grid(m1);
        let argvals_t = uniform_grid(m2);
        let n_points = m1 * m2;
        let data = FdMatrix::from_column_major(
            (0..(n * n_points)).map(|i| i as f64 * 0.1).collect(),
            n,
            n_points,
        )
        .unwrap();
        let user_weights: Vec<f64> = vec![2.0; n_points];
        let dist = lp_self_2d(&data, &argvals_s, &argvals_t, 2.0, &user_weights);
        assert_eq!(dist.nrows(), n);
        assert_eq!(dist.ncols(), n);
        for i in 0..n {
            assert!(
                dist[(i, i)].abs() < 1e-10,
                "Weighted Lp 2D self-distance should be zero on diagonal"
            );
        }
        for i in 0..n {
            for j in 0..n {
                assert!(
                    (dist[(i, j)] - dist[(j, i)]).abs() < 1e-10,
                    "Weighted Lp 2D should be symmetric"
                );
            }
        }
    }
}
