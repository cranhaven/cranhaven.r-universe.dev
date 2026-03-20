//! Helper functions for numerical integration and common operations.

/// Small epsilon for numerical comparisons (e.g., avoiding division by zero).
pub const NUMERICAL_EPS: f64 = 1e-10;

/// Default convergence tolerance for iterative algorithms.
pub const DEFAULT_CONVERGENCE_TOL: f64 = 1e-6;

/// Extract curves from column-major data matrix.
///
/// Converts a flat column-major matrix into a vector of curve vectors,
/// where each curve contains all evaluation points for one observation.
///
/// # Arguments
/// * `data` - Functional data matrix (n x m)
///
/// # Returns
/// Vector of n curves, each containing m values
pub fn extract_curves(data: &crate::matrix::FdMatrix) -> Vec<Vec<f64>> {
    data.rows()
}

/// Compute L2 distance between two curves using integration weights.
///
/// # Arguments
/// * `curve1` - First curve values
/// * `curve2` - Second curve values
/// * `weights` - Integration weights
///
/// # Returns
/// L2 distance between the curves
pub fn l2_distance(curve1: &[f64], curve2: &[f64], weights: &[f64]) -> f64 {
    let mut dist_sq = 0.0;
    for i in 0..curve1.len() {
        let diff = curve1[i] - curve2[i];
        dist_sq += diff * diff * weights[i];
    }
    dist_sq.sqrt()
}

/// Compute Simpson's rule integration weights for non-uniform grid.
///
/// Returns weights for trapezoidal rule integration.
///
/// # Arguments
/// * `argvals` - Grid points (evaluation points)
///
/// # Returns
/// Vector of integration weights
pub fn simpsons_weights(argvals: &[f64]) -> Vec<f64> {
    let n = argvals.len();
    if n < 2 {
        return vec![1.0; n];
    }

    let mut weights = vec![0.0; n];

    if n == 2 {
        // Trapezoidal rule
        let h = argvals[1] - argvals[0];
        weights[0] = h / 2.0;
        weights[1] = h / 2.0;
        return weights;
    }

    // For non-uniform spacing, use composite trapezoidal rule
    for i in 0..n {
        if i == 0 {
            weights[i] = (argvals[1] - argvals[0]) / 2.0;
        } else if i == n - 1 {
            weights[i] = (argvals[n - 1] - argvals[n - 2]) / 2.0;
        } else {
            weights[i] = (argvals[i + 1] - argvals[i - 1]) / 2.0;
        }
    }

    weights
}

/// Compute 2D integration weights using tensor product of 1D weights.
///
/// Returns a flattened vector of weights for an m1 x m2 grid.
///
/// # Arguments
/// * `argvals_s` - Grid points in s direction
/// * `argvals_t` - Grid points in t direction
///
/// # Returns
/// Flattened vector of integration weights (row-major order)
pub fn simpsons_weights_2d(argvals_s: &[f64], argvals_t: &[f64]) -> Vec<f64> {
    let weights_s = simpsons_weights(argvals_s);
    let weights_t = simpsons_weights(argvals_t);
    let m1 = argvals_s.len();
    let m2 = argvals_t.len();

    let mut weights = vec![0.0; m1 * m2];
    for i in 0..m1 {
        for j in 0..m2 {
            weights[i * m2 + j] = weights_s[i] * weights_t[j];
        }
    }
    weights
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simpsons_weights_uniform() {
        let argvals = vec![0.0, 0.25, 0.5, 0.75, 1.0];
        let weights = simpsons_weights(&argvals);
        let sum: f64 = weights.iter().sum();
        assert!((sum - 1.0).abs() < NUMERICAL_EPS);
    }

    #[test]
    fn test_simpsons_weights_2d() {
        let argvals_s = vec![0.0, 0.5, 1.0];
        let argvals_t = vec![0.0, 0.5, 1.0];
        let weights = simpsons_weights_2d(&argvals_s, &argvals_t);
        let sum: f64 = weights.iter().sum();
        assert!((sum - 1.0).abs() < NUMERICAL_EPS);
    }

    #[test]
    fn test_extract_curves() {
        // Column-major data: 2 observations, 3 points
        // obs 0: [1, 2, 3], obs 1: [4, 5, 6]
        let data = vec![1.0, 4.0, 2.0, 5.0, 3.0, 6.0];
        let mat = crate::matrix::FdMatrix::from_column_major(data, 2, 3).unwrap();
        let curves = extract_curves(&mat);
        assert_eq!(curves.len(), 2);
        assert_eq!(curves[0], vec![1.0, 2.0, 3.0]);
        assert_eq!(curves[1], vec![4.0, 5.0, 6.0]);
    }

    #[test]
    fn test_l2_distance_identical() {
        let curve = vec![1.0, 2.0, 3.0];
        let weights = vec![0.25, 0.5, 0.25];
        let dist = l2_distance(&curve, &curve, &weights);
        assert!(dist.abs() < NUMERICAL_EPS);
    }

    #[test]
    fn test_l2_distance_different() {
        let curve1 = vec![0.0, 0.0, 0.0];
        let curve2 = vec![1.0, 1.0, 1.0];
        let weights = vec![0.25, 0.5, 0.25]; // sum = 1
        let dist = l2_distance(&curve1, &curve2, &weights);
        // dist^2 = 0.25*1 + 0.5*1 + 0.25*1 = 1.0, so dist = 1.0
        assert!((dist - 1.0).abs() < NUMERICAL_EPS);
    }
}
