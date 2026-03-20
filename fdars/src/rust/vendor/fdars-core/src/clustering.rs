//! Clustering algorithms for functional data.
//!
//! This module provides k-means and fuzzy c-means clustering algorithms
//! for functional data.

use crate::helpers::{l2_distance, simpsons_weights, NUMERICAL_EPS};
use crate::matrix::FdMatrix;
use crate::{iter_maybe_parallel, slice_maybe_parallel};
use rand::prelude::*;
#[cfg(feature = "parallel")]
use rayon::iter::ParallelIterator;

/// Result of k-means clustering.
pub struct KmeansResult {
    /// Cluster assignments for each observation
    pub cluster: Vec<usize>,
    /// Cluster centers (k x m matrix)
    pub centers: FdMatrix,
    /// Within-cluster sum of squares for each cluster
    pub withinss: Vec<f64>,
    /// Total within-cluster sum of squares
    pub tot_withinss: f64,
    /// Number of iterations
    pub iter: usize,
    /// Whether the algorithm converged
    pub converged: bool,
}

/// K-means++ initialization: select initial centers with probability proportional to D^2.
///
/// # Arguments
/// * `curves` - Vector of curve vectors
/// * `k` - Number of clusters
/// * `weights` - Integration weights for L2 distance
/// * `rng` - Random number generator
///
/// # Returns
/// Vector of k initial cluster centers
fn kmeans_plusplus_init(
    curves: &[Vec<f64>],
    k: usize,
    weights: &[f64],
    rng: &mut StdRng,
) -> Vec<Vec<f64>> {
    let n = curves.len();
    let mut centers: Vec<Vec<f64>> = Vec::with_capacity(k);

    // First center: random
    let first_idx = rng.gen_range(0..n);
    centers.push(curves[first_idx].clone());

    // Remaining centers: probability proportional to D^2
    for _ in 1..k {
        let distances: Vec<f64> = curves
            .iter()
            .map(|curve| {
                centers
                    .iter()
                    .map(|c| l2_distance(curve, c, weights))
                    .fold(f64::INFINITY, f64::min)
            })
            .collect();

        let dist_sq: Vec<f64> = distances.iter().map(|d| d * d).collect();
        let total: f64 = dist_sq.iter().sum();

        if total < NUMERICAL_EPS {
            let idx = rng.gen_range(0..n);
            centers.push(curves[idx].clone());
        } else {
            let r = rng.gen::<f64>() * total;
            let mut cumsum = 0.0;
            let mut chosen = 0;
            for (i, &d) in dist_sq.iter().enumerate() {
                cumsum += d;
                if cumsum >= r {
                    chosen = i;
                    break;
                }
            }
            centers.push(curves[chosen].clone());
        }
    }

    centers
}

/// Compute fuzzy membership values for a single observation.
///
/// # Arguments
/// * `distances` - Distances from the observation to each cluster center
/// * `exponent` - Exponent for fuzzy membership (2 / (fuzziness - 1))
///
/// # Returns
/// Vector of membership values (one per cluster)
fn compute_fuzzy_membership(distances: &[f64], exponent: f64) -> Vec<f64> {
    let k = distances.len();
    let mut membership = vec![0.0; k];

    // Check if observation is very close to any center
    for (c, &dist) in distances.iter().enumerate() {
        if dist < NUMERICAL_EPS {
            // Assign full membership to this cluster
            membership[c] = 1.0;
            return membership;
        }
    }

    // Normal fuzzy membership computation
    for c in 0..k {
        let mut sum = 0.0;
        for c2 in 0..k {
            if distances[c2] > NUMERICAL_EPS {
                sum += (distances[c] / distances[c2]).powf(exponent);
            }
        }
        membership[c] = if sum > NUMERICAL_EPS { 1.0 / sum } else { 1.0 };
    }

    membership
}

/// Build an FdMatrix (k x m) from Vec<Vec<f64>> centers.
fn centers_to_matrix(centers: &[Vec<f64>], k: usize, m: usize) -> FdMatrix {
    let mut flat = vec![0.0; k * m];
    for c in 0..k {
        for j in 0..m {
            flat[c + j * k] = centers[c][j];
        }
    }
    FdMatrix::from_column_major(flat, k, m).unwrap()
}

/// Initialize a random membership matrix (n x k) with rows summing to 1.
fn init_random_membership(n: usize, k: usize, rng: &mut StdRng) -> FdMatrix {
    let mut membership = FdMatrix::zeros(n, k);
    for i in 0..n {
        let mut row_sum = 0.0;
        for c in 0..k {
            let val = rng.gen::<f64>();
            membership[(i, c)] = val;
            row_sum += val;
        }
        for c in 0..k {
            membership[(i, c)] /= row_sum;
        }
    }
    membership
}

/// Group sample indices by their cluster assignment.
fn cluster_member_indices(cluster: &[usize], k: usize) -> Vec<Vec<usize>> {
    let mut indices = vec![Vec::new(); k];
    for (i, &c) in cluster.iter().enumerate() {
        indices[c].push(i);
    }
    indices
}

/// Assign each curve to its nearest center, returning cluster indices.
fn assign_clusters(curves: &[Vec<f64>], centers: &[Vec<f64>], weights: &[f64]) -> Vec<usize> {
    slice_maybe_parallel!(curves)
        .map(|curve| {
            let mut best_cluster = 0;
            let mut best_dist = f64::INFINITY;
            for (c, center) in centers.iter().enumerate() {
                let dist = l2_distance(curve, center, weights);
                if dist < best_dist {
                    best_dist = dist;
                    best_cluster = c;
                }
            }
            best_cluster
        })
        .collect()
}

/// Compute new cluster centers from curve assignments.
fn update_kmeans_centers(
    curves: &[Vec<f64>],
    assignments: &[usize],
    centers: &[Vec<f64>],
    k: usize,
    m: usize,
) -> Vec<Vec<f64>> {
    (0..k)
        .map(|c| {
            let members: Vec<usize> = assignments
                .iter()
                .enumerate()
                .filter(|(_, &cl)| cl == c)
                .map(|(i, _)| i)
                .collect();

            if members.is_empty() {
                centers[c].clone()
            } else {
                let mut center = vec![0.0; m];
                for &i in &members {
                    for j in 0..m {
                        center[j] += curves[i][j];
                    }
                }
                let n_members = members.len() as f64;
                for j in 0..m {
                    center[j] /= n_members;
                }
                center
            }
        })
        .collect()
}

/// Compute within-cluster sum of squares for each cluster.
fn compute_within_ss(
    curves: &[Vec<f64>],
    centers: &[Vec<f64>],
    assignments: &[usize],
    k: usize,
    weights: &[f64],
) -> Vec<f64> {
    let mut withinss = vec![0.0; k];
    for (i, curve) in curves.iter().enumerate() {
        let c = assignments[i];
        let dist = l2_distance(curve, &centers[c], weights);
        withinss[c] += dist * dist;
    }
    withinss
}

/// Update fuzzy c-means cluster centers from membership values.
fn update_fuzzy_centers(
    curves: &[Vec<f64>],
    membership: &FdMatrix,
    k: usize,
    m: usize,
    fuzziness: f64,
) -> Vec<Vec<f64>> {
    let mut centers = vec![vec![0.0; m]; k];
    for c in 0..k {
        let mut numerator = vec![0.0; m];
        let mut denominator = 0.0;

        for (i, curve) in curves.iter().enumerate() {
            let weight = membership[(i, c)].powf(fuzziness);
            for j in 0..m {
                numerator[j] += weight * curve[j];
            }
            denominator += weight;
        }

        if denominator > NUMERICAL_EPS {
            for j in 0..m {
                centers[c][j] = numerator[j] / denominator;
            }
        }
    }
    centers
}

/// Update fuzzy membership values and compute max change.
fn update_fuzzy_membership_step(
    curves: &[Vec<f64>],
    centers: &[Vec<f64>],
    old_membership: &FdMatrix,
    k: usize,
    exponent: f64,
    weights: &[f64],
) -> (FdMatrix, f64) {
    let n = curves.len();
    let mut new_membership = FdMatrix::zeros(n, k);
    let mut max_change = 0.0;

    for (i, curve) in curves.iter().enumerate() {
        let distances: Vec<f64> = centers
            .iter()
            .map(|c| l2_distance(curve, c, weights))
            .collect();

        let memberships = compute_fuzzy_membership(&distances, exponent);

        for c in 0..k {
            new_membership[(i, c)] = memberships[c];
            let change = (memberships[c] - old_membership[(i, c)]).abs();
            if change > max_change {
                max_change = change;
            }
        }
    }

    (new_membership, max_change)
}

/// Compute mean L2 distance from a curve to a set of curve indices.
fn mean_cluster_distance(
    curve: &[f64],
    curves: &[Vec<f64>],
    indices: &[usize],
    weights: &[f64],
) -> f64 {
    if indices.is_empty() {
        return 0.0;
    }
    let sum: f64 = indices
        .iter()
        .map(|&j| l2_distance(curve, &curves[j], weights))
        .sum();
    sum / indices.len() as f64
}

/// Compute cluster centers, global mean, and counts from curves and assignments.
fn compute_centers_and_global_mean(
    curves: &[Vec<f64>],
    assignments: &[usize],
    k: usize,
    m: usize,
) -> (Vec<Vec<f64>>, Vec<f64>, Vec<usize>) {
    let n = curves.len();
    let mut global_mean = vec![0.0; m];
    for curve in curves {
        for j in 0..m {
            global_mean[j] += curve[j];
        }
    }
    for j in 0..m {
        global_mean[j] /= n as f64;
    }

    let mut centers = vec![vec![0.0; m]; k];
    let mut counts = vec![0usize; k];
    for (i, curve) in curves.iter().enumerate() {
        let c = assignments[i];
        counts[c] += 1;
        for j in 0..m {
            centers[c][j] += curve[j];
        }
    }
    for c in 0..k {
        if counts[c] > 0 {
            for j in 0..m {
                centers[c][j] /= counts[c] as f64;
            }
        }
    }

    (centers, global_mean, counts)
}

/// Run one k-means iteration: assign clusters, update centers, compute movement.
fn kmeans_step(
    curves: &[Vec<f64>],
    centers: &[Vec<f64>],
    weights: &[f64],
    k: usize,
    m: usize,
) -> (Vec<usize>, Vec<Vec<f64>>, f64) {
    let new_cluster = assign_clusters(curves, centers, weights);
    let new_centers = update_kmeans_centers(curves, &new_cluster, centers, k, m);
    let max_movement = centers
        .iter()
        .zip(new_centers.iter())
        .map(|(old, new)| l2_distance(old, new, weights))
        .fold(0.0, f64::max);
    (new_cluster, new_centers, max_movement)
}

/// Run the k-means iteration loop until convergence or max iterations.
fn kmeans_iterate(
    curves: &[Vec<f64>],
    mut centers: Vec<Vec<f64>>,
    weights: &[f64],
    k: usize,
    m: usize,
    max_iter: usize,
    tol: f64,
) -> (Vec<usize>, Vec<Vec<f64>>, usize, bool) {
    let n = curves.len();
    let mut cluster = vec![0usize; n];
    let mut converged = false;
    let mut iter = 0;

    for iteration in 0..max_iter {
        iter = iteration + 1;
        let (new_cluster, new_centers, max_movement) = kmeans_step(curves, &centers, weights, k, m);

        if new_cluster == cluster {
            converged = true;
            break;
        }
        cluster = new_cluster;
        centers = new_centers;

        if max_movement < tol {
            converged = true;
            break;
        }
    }

    (cluster, centers, iter, converged)
}

/// K-means clustering for functional data.
///
/// # Arguments
/// * `data` - Functional data matrix (n x m)
/// * `argvals` - Evaluation points
/// * `k` - Number of clusters
/// * `max_iter` - Maximum iterations
/// * `tol` - Convergence tolerance
/// * `seed` - Random seed
pub fn kmeans_fd(
    data: &FdMatrix,
    argvals: &[f64],
    k: usize,
    max_iter: usize,
    tol: f64,
    seed: u64,
) -> KmeansResult {
    let n = data.nrows();
    let m = data.ncols();

    if n == 0 || m == 0 || k == 0 || k > n || argvals.len() != m {
        return KmeansResult {
            cluster: Vec::new(),
            centers: FdMatrix::zeros(0, 0),
            withinss: Vec::new(),
            tot_withinss: 0.0,
            iter: 0,
            converged: false,
        };
    }

    let weights = simpsons_weights(argvals);
    let mut rng = StdRng::seed_from_u64(seed);

    // Extract curves
    let curves = data.rows();

    // K-means++ initialization using helper
    let centers = kmeans_plusplus_init(&curves, k, &weights, &mut rng);

    let (cluster, centers, iter, converged) =
        kmeans_iterate(&curves, centers, &weights, k, m, max_iter, tol);

    let withinss = compute_within_ss(&curves, &centers, &cluster, k, &weights);
    let tot_withinss: f64 = withinss.iter().sum();
    let centers_mat = centers_to_matrix(&centers, k, m);

    KmeansResult {
        cluster,
        centers: centers_mat,
        withinss,
        tot_withinss,
        iter,
        converged,
    }
}

/// Result of fuzzy c-means clustering.
pub struct FuzzyCmeansResult {
    /// Membership matrix (n x k)
    pub membership: FdMatrix,
    /// Cluster centers (k x m)
    pub centers: FdMatrix,
    /// Number of iterations
    pub iter: usize,
    /// Whether the algorithm converged
    pub converged: bool,
}

/// Fuzzy c-means clustering for functional data.
///
/// # Arguments
/// * `data` - Functional data matrix (n x m)
/// * `argvals` - Evaluation points
/// * `k` - Number of clusters
/// * `fuzziness` - Fuzziness parameter (> 1)
/// * `max_iter` - Maximum iterations
/// * `tol` - Convergence tolerance
/// * `seed` - Random seed
pub fn fuzzy_cmeans_fd(
    data: &FdMatrix,
    argvals: &[f64],
    k: usize,
    fuzziness: f64,
    max_iter: usize,
    tol: f64,
    seed: u64,
) -> FuzzyCmeansResult {
    let n = data.nrows();
    let m = data.ncols();

    if n == 0 || m == 0 || k == 0 || k > n || argvals.len() != m || fuzziness <= 1.0 {
        return FuzzyCmeansResult {
            membership: FdMatrix::zeros(0, 0),
            centers: FdMatrix::zeros(0, 0),
            iter: 0,
            converged: false,
        };
    }

    let weights = simpsons_weights(argvals);
    let mut rng = StdRng::seed_from_u64(seed);

    // Extract curves
    let curves = data.rows();

    let mut membership = init_random_membership(n, k, &mut rng);

    let mut centers = vec![vec![0.0; m]; k];
    let mut converged = false;
    let mut iter = 0;
    let exponent = 2.0 / (fuzziness - 1.0);

    for iteration in 0..max_iter {
        iter = iteration + 1;

        centers = update_fuzzy_centers(&curves, &membership, k, m, fuzziness);

        let (new_membership, max_change) =
            update_fuzzy_membership_step(&curves, &centers, &membership, k, exponent, &weights);

        membership = new_membership;

        if max_change < tol {
            converged = true;
            break;
        }
    }

    let centers_mat = centers_to_matrix(&centers, k, m);

    FuzzyCmeansResult {
        membership,
        centers: centers_mat,
        iter,
        converged,
    }
}

/// Compute silhouette score for clustering result.
pub fn silhouette_score(data: &FdMatrix, argvals: &[f64], cluster: &[usize]) -> Vec<f64> {
    let n = data.nrows();
    let m = data.ncols();

    if n == 0 || m == 0 || cluster.len() != n || argvals.len() != m {
        return Vec::new();
    }

    let weights = simpsons_weights(argvals);
    let curves = data.rows();

    let k = cluster.iter().cloned().max().unwrap_or(0) + 1;
    let members = cluster_member_indices(cluster, k);

    iter_maybe_parallel!(0..n)
        .map(|i| {
            let my_cluster = cluster[i];

            let same_indices: Vec<usize> = members[my_cluster]
                .iter()
                .copied()
                .filter(|&j| j != i)
                .collect();
            let a_i = mean_cluster_distance(&curves[i], &curves, &same_indices, &weights);

            let mut b_i = f64::INFINITY;
            for c in 0..k {
                if c != my_cluster && !members[c].is_empty() {
                    b_i = b_i.min(mean_cluster_distance(
                        &curves[i],
                        &curves,
                        &members[c],
                        &weights,
                    ));
                }
            }

            if b_i.is_infinite() {
                0.0
            } else {
                let max_ab = a_i.max(b_i);
                if max_ab > NUMERICAL_EPS {
                    (b_i - a_i) / max_ab
                } else {
                    0.0
                }
            }
        })
        .collect()
}

/// Compute Calinski-Harabasz index for clustering result.
pub fn calinski_harabasz(data: &FdMatrix, argvals: &[f64], cluster: &[usize]) -> f64 {
    let n = data.nrows();
    let m = data.ncols();

    if n == 0 || m == 0 || cluster.len() != n || argvals.len() != m {
        return 0.0;
    }

    let weights = simpsons_weights(argvals);
    let curves = data.rows();

    let k = cluster.iter().cloned().max().unwrap_or(0) + 1;
    if k < 2 {
        return 0.0;
    }

    let (centers, global_mean, counts) = compute_centers_and_global_mean(&curves, cluster, k, m);

    let mut bgss = 0.0;
    for c in 0..k {
        let dist = l2_distance(&centers[c], &global_mean, &weights);
        bgss += counts[c] as f64 * dist * dist;
    }

    let wgss_vec = compute_within_ss(&curves, &centers, cluster, k, &weights);
    let wgss: f64 = wgss_vec.iter().sum();

    if wgss < NUMERICAL_EPS {
        return f64::INFINITY;
    }

    (bgss / (k - 1) as f64) / (wgss / (n - k) as f64)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::f64::consts::PI;

    /// Generate a uniform grid of points
    fn uniform_grid(n: usize) -> Vec<f64> {
        (0..n).map(|i| i as f64 / (n - 1) as f64).collect()
    }

    /// Generate two clearly separated clusters of curves as an FdMatrix
    fn generate_two_clusters(n_per_cluster: usize, m: usize) -> (FdMatrix, Vec<f64>) {
        let t = uniform_grid(m);
        let n = 2 * n_per_cluster;
        let mut col_major = vec![0.0; n * m];

        // Cluster 0: sine waves with low amplitude
        for i in 0..n_per_cluster {
            for (j, &ti) in t.iter().enumerate() {
                col_major[i + j * n] =
                    (2.0 * PI * ti).sin() + 0.1 * (i as f64 / n_per_cluster as f64);
            }
        }

        // Cluster 1: sine waves shifted up by 5
        for i in 0..n_per_cluster {
            for (j, &ti) in t.iter().enumerate() {
                col_major[(i + n_per_cluster) + j * n] =
                    (2.0 * PI * ti).sin() + 5.0 + 0.1 * (i as f64 / n_per_cluster as f64);
            }
        }

        (FdMatrix::from_column_major(col_major, n, m).unwrap(), t)
    }

    // ============== K-means tests ==============

    #[test]
    fn test_kmeans_fd_basic() {
        let m = 50;
        let n_per = 5;
        let (data, t) = generate_two_clusters(n_per, m);
        let n = 2 * n_per;

        let result = kmeans_fd(&data, &t, 2, 100, 1e-6, 42);

        assert_eq!(result.cluster.len(), n);
        assert!(result.converged);
        assert!(result.iter > 0 && result.iter <= 100);
    }

    #[test]
    fn test_kmeans_fd_finds_clusters() {
        let m = 50;
        let n_per = 10;
        let (data, t) = generate_two_clusters(n_per, m);
        let n = 2 * n_per;

        let result = kmeans_fd(&data, &t, 2, 100, 1e-6, 42);

        // First half should be one cluster, second half the other
        let cluster_0 = result.cluster[0];
        let cluster_1 = result.cluster[n_per];

        assert_ne!(cluster_0, cluster_1, "Clusters should be different");

        // Check that first half is in same cluster
        for i in 0..n_per {
            assert_eq!(result.cluster[i], cluster_0);
        }

        // Check that second half is in same cluster
        for i in n_per..n {
            assert_eq!(result.cluster[i], cluster_1);
        }
    }

    #[test]
    fn test_kmeans_fd_deterministic() {
        let m = 30;
        let n_per = 5;
        let (data, t) = generate_two_clusters(n_per, m);

        let result1 = kmeans_fd(&data, &t, 2, 100, 1e-6, 42);
        let result2 = kmeans_fd(&data, &t, 2, 100, 1e-6, 42);

        // Same seed should give same results
        assert_eq!(result1.cluster, result2.cluster);
    }

    #[test]
    fn test_kmeans_fd_withinss() {
        let m = 30;
        let n_per = 5;
        let (data, t) = generate_two_clusters(n_per, m);

        let result = kmeans_fd(&data, &t, 2, 100, 1e-6, 42);

        // Within-cluster sum of squares should be non-negative
        for &wss in &result.withinss {
            assert!(wss >= 0.0);
        }

        // Total should equal sum
        let sum: f64 = result.withinss.iter().sum();
        assert!((sum - result.tot_withinss).abs() < 1e-10);
    }

    #[test]
    fn test_kmeans_fd_centers_shape() {
        let m = 30;
        let n_per = 5;
        let (data, t) = generate_two_clusters(n_per, m);
        let k = 3;

        let result = kmeans_fd(&data, &t, k, 100, 1e-6, 42);

        // Centers should be k x m matrix
        assert_eq!(result.centers.nrows(), k);
        assert_eq!(result.centers.ncols(), m);
    }

    #[test]
    fn test_kmeans_fd_invalid_input() {
        let t = uniform_grid(30);

        // Empty data
        let data = FdMatrix::zeros(0, 0);
        let result = kmeans_fd(&data, &t, 2, 100, 1e-6, 42);
        assert!(result.cluster.is_empty());
        assert!(!result.converged);

        // k > n
        let data = FdMatrix::zeros(5, 30);
        let result = kmeans_fd(&data, &t, 10, 100, 1e-6, 42);
        assert!(result.cluster.is_empty());
    }

    #[test]
    fn test_kmeans_fd_single_cluster() {
        let m = 30;
        let t = uniform_grid(m);
        let n = 10;
        let data = FdMatrix::zeros(n, m);

        let result = kmeans_fd(&data, &t, 1, 100, 1e-6, 42);

        // All should be in cluster 0
        for &c in &result.cluster {
            assert_eq!(c, 0);
        }
    }

    // ============== Fuzzy C-means tests ==============

    #[test]
    fn test_fuzzy_cmeans_fd_basic() {
        let m = 50;
        let n_per = 5;
        let (data, t) = generate_two_clusters(n_per, m);
        let n = 2 * n_per;

        let result = fuzzy_cmeans_fd(&data, &t, 2, 2.0, 100, 1e-6, 42);

        assert_eq!(result.membership.nrows(), n);
        assert_eq!(result.membership.ncols(), 2);
        assert!(result.iter > 0);
    }

    #[test]
    fn test_fuzzy_cmeans_fd_membership_sums_to_one() {
        let m = 30;
        let n_per = 5;
        let (data, t) = generate_two_clusters(n_per, m);
        let n = 2 * n_per;
        let k = 2;

        let result = fuzzy_cmeans_fd(&data, &t, k, 2.0, 100, 1e-6, 42);

        // Each observation's membership should sum to 1
        for i in 0..n {
            let sum: f64 = (0..k).map(|c| result.membership[(i, c)]).sum();
            assert!(
                (sum - 1.0).abs() < 1e-6,
                "Membership should sum to 1, got {}",
                sum
            );
        }
    }

    #[test]
    fn test_fuzzy_cmeans_fd_membership_in_range() {
        let m = 30;
        let n_per = 5;
        let (data, t) = generate_two_clusters(n_per, m);

        let result = fuzzy_cmeans_fd(&data, &t, 2, 2.0, 100, 1e-6, 42);

        // All memberships should be in [0, 1]
        for &mem in result.membership.as_slice() {
            assert!((0.0..=1.0 + 1e-10).contains(&mem));
        }
    }

    #[test]
    fn test_fuzzy_cmeans_fd_fuzziness_effect() {
        let m = 30;
        let n_per = 5;
        let (data, t) = generate_two_clusters(n_per, m);

        let result_low = fuzzy_cmeans_fd(&data, &t, 2, 1.5, 100, 1e-6, 42);
        let result_high = fuzzy_cmeans_fd(&data, &t, 2, 3.0, 100, 1e-6, 42);

        // Higher fuzziness should give more diffuse memberships
        // Measure by entropy-like metric
        let entropy_low: f64 = result_low
            .membership
            .as_slice()
            .iter()
            .map(|&m| if m > 1e-10 { -m * m.ln() } else { 0.0 })
            .sum();

        let entropy_high: f64 = result_high
            .membership
            .as_slice()
            .iter()
            .map(|&m| if m > 1e-10 { -m * m.ln() } else { 0.0 })
            .sum();

        assert!(
            entropy_high >= entropy_low - 0.1,
            "Higher fuzziness should give higher entropy"
        );
    }

    #[test]
    fn test_fuzzy_cmeans_fd_invalid_fuzziness() {
        let t = uniform_grid(30);
        let data = FdMatrix::zeros(10, 30);

        // Fuzziness <= 1 should fail
        let result = fuzzy_cmeans_fd(&data, &t, 2, 1.0, 100, 1e-6, 42);
        assert!(result.membership.is_empty());

        let result = fuzzy_cmeans_fd(&data, &t, 2, 0.5, 100, 1e-6, 42);
        assert!(result.membership.is_empty());
    }

    #[test]
    fn test_fuzzy_cmeans_fd_centers_shape() {
        let m = 30;
        let t = uniform_grid(m);
        let n = 10;
        let k = 3;
        let data = FdMatrix::zeros(n, m);

        let result = fuzzy_cmeans_fd(&data, &t, k, 2.0, 100, 1e-6, 42);

        assert_eq!(result.centers.nrows(), k);
        assert_eq!(result.centers.ncols(), m);
    }

    // ============== Silhouette score tests ==============

    #[test]
    fn test_silhouette_score_well_separated() {
        let m = 30;
        let n_per = 10;
        let (data, t) = generate_two_clusters(n_per, m);
        let n = 2 * n_per;

        // Perfect clustering: first half in 0, second in 1
        let cluster: Vec<usize> = (0..n).map(|i| if i < n_per { 0 } else { 1 }).collect();

        let scores = silhouette_score(&data, &t, &cluster);

        assert_eq!(scores.len(), n);

        // Well-separated clusters should have high silhouette scores
        let mean_score: f64 = scores.iter().sum::<f64>() / n as f64;
        assert!(
            mean_score > 0.5,
            "Well-separated clusters should have high silhouette: {}",
            mean_score
        );
    }

    #[test]
    fn test_silhouette_score_range() {
        let m = 30;
        let n_per = 5;
        let (data, t) = generate_two_clusters(n_per, m);
        let n = 2 * n_per;

        let cluster: Vec<usize> = (0..n).map(|i| if i < n_per { 0 } else { 1 }).collect();

        let scores = silhouette_score(&data, &t, &cluster);

        // Silhouette scores should be in [-1, 1]
        for &s in &scores {
            assert!((-1.0 - 1e-10..=1.0 + 1e-10).contains(&s));
        }
    }

    #[test]
    fn test_silhouette_score_single_cluster() {
        let m = 30;
        let t = uniform_grid(m);
        let n = 10;
        let data = FdMatrix::zeros(n, m);

        // All in one cluster
        let cluster = vec![0usize; n];

        let scores = silhouette_score(&data, &t, &cluster);

        // Single cluster should give zeros
        for &s in &scores {
            assert!(s.abs() < 1e-10);
        }
    }

    #[test]
    fn test_silhouette_score_invalid_input() {
        let t = uniform_grid(30);

        // Empty data
        let data = FdMatrix::zeros(0, 0);
        let scores = silhouette_score(&data, &t, &[]);
        assert!(scores.is_empty());

        // Mismatched cluster length
        let data = FdMatrix::zeros(10, 30);
        let cluster = vec![0; 5]; // Wrong length
        let scores = silhouette_score(&data, &t, &cluster);
        assert!(scores.is_empty());
    }

    // ============== Calinski-Harabasz tests ==============

    #[test]
    fn test_calinski_harabasz_well_separated() {
        let m = 30;
        let n_per = 10;
        let (data, t) = generate_two_clusters(n_per, m);
        let n = 2 * n_per;

        let cluster: Vec<usize> = (0..n).map(|i| if i < n_per { 0 } else { 1 }).collect();

        let ch = calinski_harabasz(&data, &t, &cluster);

        // Well-separated clusters should have high CH index
        assert!(
            ch > 1.0,
            "Well-separated clusters should have high CH: {}",
            ch
        );
    }

    #[test]
    fn test_calinski_harabasz_positive() {
        let m = 30;
        let n_per = 5;
        let (data, t) = generate_two_clusters(n_per, m);
        let n = 2 * n_per;

        let cluster: Vec<usize> = (0..n).map(|i| if i < n_per { 0 } else { 1 }).collect();

        let ch = calinski_harabasz(&data, &t, &cluster);

        assert!(ch >= 0.0, "CH index should be non-negative");
    }

    #[test]
    fn test_calinski_harabasz_single_cluster() {
        let m = 30;
        let t = uniform_grid(m);
        let n = 10;
        let data = FdMatrix::zeros(n, m);

        // All in one cluster
        let cluster = vec![0usize; n];

        let ch = calinski_harabasz(&data, &t, &cluster);

        // Single cluster should give 0
        assert!(ch.abs() < 1e-10);
    }

    #[test]
    fn test_calinski_harabasz_invalid_input() {
        let t = uniform_grid(30);

        // Empty data
        let data = FdMatrix::zeros(0, 0);
        let ch = calinski_harabasz(&data, &t, &[]);
        assert!(ch.abs() < 1e-10);
    }
}
