//! Streaming / online depth computation for functional data.
//!
//! This module decouples reference-set construction from query evaluation,
//! enabling efficient depth computation in streaming scenarios:
//!
//! - [`SortedReferenceState`] pre-sorts reference values per time point for O(log N) rank queries.
//! - [`StreamingMbd`] uses a rank-based combinatorial identity to compute Modified Band Depth
//!   in O(T log N) per query instead of O(N² T).
//! - [`StreamingFraimanMuniz`] computes Fraiman-Muniz depth via binary search on sorted columns.
//! - [`StreamingBd`] computes Band Depth with decoupled reference and early-exit optimisation.
//! - [`RollingReference`] maintains a sliding window of reference curves with incremental
//!   sorted-column updates.

use std::collections::VecDeque;

use crate::iter_maybe_parallel;
use crate::matrix::FdMatrix;
#[cfg(feature = "parallel")]
use rayon::iter::ParallelIterator;

// ---------------------------------------------------------------------------
// Helper: choose-2 combinator
// ---------------------------------------------------------------------------

#[inline]
fn c2(k: usize) -> usize {
    k * k.wrapping_sub(1) / 2
}

// ===========================================================================
// SortedReferenceState
// ===========================================================================

/// Pre-sorted reference values at each time point for O(log N) rank queries.
///
/// Constructed once from a column-major reference matrix and then shared
/// (immutably) by any number of streaming depth estimators.
pub struct SortedReferenceState {
    /// `sorted_columns[t]` contains the reference values at time point `t`, sorted ascending.
    sorted_columns: Vec<Vec<f64>>,
    nori: usize,
    n_points: usize,
}

impl SortedReferenceState {
    /// Build from a column-major reference matrix.
    ///
    /// * `data_ori` – reference matrix of shape `nori × n_points`
    ///
    /// Complexity: O(T × N log N)  (parallelised over time points).
    pub fn from_reference(data_ori: &FdMatrix) -> Self {
        let nori = data_ori.nrows();
        let n_points = data_ori.ncols();
        if nori == 0 || n_points == 0 {
            return Self {
                sorted_columns: Vec::new(),
                nori,
                n_points,
            };
        }
        let sorted_columns: Vec<Vec<f64>> = iter_maybe_parallel!(0..n_points)
            .map(|t| {
                let mut col: Vec<f64> = (0..nori).map(|j| data_ori[(j, t)]).collect();
                col.sort_unstable_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
                col
            })
            .collect();
        Self {
            sorted_columns,
            nori,
            n_points,
        }
    }

    /// Returns `(below, above)` — the count of reference values strictly below
    /// and strictly above `x` at time point `t`.
    ///
    /// Complexity: O(log N) via two binary searches.
    #[inline]
    pub fn rank_at(&self, t: usize, x: f64) -> (usize, usize) {
        let col = &self.sorted_columns[t];
        let below = col.partition_point(|&v| v < x);
        let at_or_below = col.partition_point(|&v| v <= x);
        let above = self.nori - at_or_below;
        (below, above)
    }

    /// Number of reference observations.
    #[inline]
    pub fn nori(&self) -> usize {
        self.nori
    }

    /// Number of evaluation points.
    #[inline]
    pub fn n_points(&self) -> usize {
        self.n_points
    }
}

// ===========================================================================
// StreamingDepth trait
// ===========================================================================

/// Trait for streaming depth estimators backed by a pre-built reference state.
pub trait StreamingDepth {
    /// Depth of a single curve given as a contiguous `&[f64]` of length `n_points`.
    fn depth_one(&self, curve: &[f64]) -> f64;

    /// Batch depth for a matrix of query curves (`nobj × n_points`).
    fn depth_batch(&self, data_obj: &FdMatrix) -> Vec<f64>;

    /// Number of evaluation points.
    fn n_points(&self) -> usize;

    /// Number of reference observations backing this estimator.
    fn n_reference(&self) -> usize;
}

// ===========================================================================
// StreamingMbd — rank-based Modified Band Depth, O(T log N) per query
// ===========================================================================

/// Rank-based Modified Band Depth estimator.
///
/// Uses the combinatorial identity: at time t with `b` values strictly below
/// x(t) and `a` strictly above,
///
/// > pairs containing x(t) = C(N,2) − C(b,2) − C(a,2)
///
/// MBD(x) = (1 / (C(N,2) × T)) × Σ_t [C(N,2) − C(b_t,2) − C(a_t,2)]
///
/// Per-query complexity: **O(T × log N)** instead of O(N² × T).
pub struct StreamingMbd {
    state: SortedReferenceState,
}

impl StreamingMbd {
    pub fn new(state: SortedReferenceState) -> Self {
        Self { state }
    }

    /// Compute MBD for a single row-layout curve using rank formula.
    #[inline]
    fn mbd_one_inner(&self, curve: &[f64]) -> f64 {
        let n = self.state.nori;
        if n < 2 {
            return 0.0;
        }
        let cn2 = c2(n);
        let t_len = self.state.n_points;
        let mut total = 0usize;
        for t in 0..t_len {
            let (below, above) = self.state.rank_at(t, curve[t]);
            total += cn2 - c2(below) - c2(above);
        }
        total as f64 / (cn2 as f64 * t_len as f64)
    }
}

impl StreamingDepth for StreamingMbd {
    fn depth_one(&self, curve: &[f64]) -> f64 {
        self.mbd_one_inner(curve)
    }

    fn depth_batch(&self, data_obj: &FdMatrix) -> Vec<f64> {
        let nobj = data_obj.nrows();
        if nobj == 0 || self.state.n_points == 0 || self.state.nori < 2 {
            return vec![0.0; nobj];
        }
        let n_points = self.state.n_points;
        iter_maybe_parallel!(0..nobj)
            .map(|i| {
                let curve: Vec<f64> = (0..n_points).map(|t| data_obj[(i, t)]).collect();
                self.mbd_one_inner(&curve)
            })
            .collect()
    }

    fn n_points(&self) -> usize {
        self.state.n_points
    }

    fn n_reference(&self) -> usize {
        self.state.nori
    }
}

// ===========================================================================
// StreamingFraimanMuniz — O(T log N) FM Depth
// ===========================================================================

/// Streaming Fraiman-Muniz depth estimator.
///
/// Uses binary search on sorted columns to compute the empirical CDF at each
/// time point: Fn(x) = #{ref ≤ x} / N.
///
/// Per-query complexity: **O(T × log N)** instead of O(T × N).
pub struct StreamingFraimanMuniz {
    state: SortedReferenceState,
    scale: bool,
}

impl StreamingFraimanMuniz {
    pub fn new(state: SortedReferenceState, scale: bool) -> Self {
        Self { state, scale }
    }

    #[inline]
    fn fm_one_inner(&self, curve: &[f64]) -> f64 {
        let n = self.state.nori;
        if n == 0 {
            return 0.0;
        }
        let t_len = self.state.n_points;
        if t_len == 0 {
            return 0.0;
        }
        let scale_factor = if self.scale { 2.0 } else { 1.0 };
        let mut depth_sum = 0.0;
        for t in 0..t_len {
            let col = &self.state.sorted_columns[t];
            let at_or_below = col.partition_point(|&v| v <= curve[t]);
            let fn_x = at_or_below as f64 / n as f64;
            depth_sum += fn_x.min(1.0 - fn_x) * scale_factor;
        }
        depth_sum / t_len as f64
    }
}

impl StreamingDepth for StreamingFraimanMuniz {
    fn depth_one(&self, curve: &[f64]) -> f64 {
        self.fm_one_inner(curve)
    }

    fn depth_batch(&self, data_obj: &FdMatrix) -> Vec<f64> {
        let nobj = data_obj.nrows();
        if nobj == 0 || self.state.n_points == 0 || self.state.nori == 0 {
            return vec![0.0; nobj];
        }
        let n_points = self.state.n_points;
        iter_maybe_parallel!(0..nobj)
            .map(|i| {
                let curve: Vec<f64> = (0..n_points).map(|t| data_obj[(i, t)]).collect();
                self.fm_one_inner(&curve)
            })
            .collect()
    }

    fn n_points(&self) -> usize {
        self.state.n_points
    }

    fn n_reference(&self) -> usize {
        self.state.nori
    }
}

// ===========================================================================
// FullReferenceState + StreamingBd — Band Depth with decoupled reference
// ===========================================================================

/// Full reference state that keeps per-curve values alongside sorted columns.
///
/// Required by Band Depth (BD), which checks all-or-nothing containment across
/// ALL time points and therefore cannot decompose into per-point rank queries.
pub struct FullReferenceState {
    /// Sorted columns for rank queries (shared with MBD/FM estimators if desired).
    pub sorted: SortedReferenceState,
    /// `values_by_curve[j][t]` = reference curve j at time point t (row layout).
    values_by_curve: Vec<Vec<f64>>,
}

impl FullReferenceState {
    /// Build from a column-major reference matrix.
    pub fn from_reference(data_ori: &FdMatrix) -> Self {
        let nori = data_ori.nrows();
        let n_points = data_ori.ncols();
        let sorted = SortedReferenceState::from_reference(data_ori);
        let values_by_curve: Vec<Vec<f64>> = (0..nori)
            .map(|j| (0..n_points).map(|t| data_ori[(j, t)]).collect())
            .collect();
        Self {
            sorted,
            values_by_curve,
        }
    }
}

/// Streaming Band Depth estimator.
///
/// BD requires all-or-nothing containment across ALL time points — it does not
/// decompose per-point like MBD. The streaming advantage here is **reference
/// decoupling** (no re-parsing the matrix) and **early-exit per pair** (break
/// on first time point where x is outside the band), not an asymptotic
/// improvement.
pub struct StreamingBd {
    state: FullReferenceState,
}

impl StreamingBd {
    pub fn new(state: FullReferenceState) -> Self {
        Self { state }
    }

    #[inline]
    fn bd_one_inner(&self, curve: &[f64]) -> f64 {
        let n = self.state.sorted.nori;
        if n < 2 {
            return 0.0;
        }
        let n_pairs = c2(n);
        let n_points = self.state.sorted.n_points;

        let mut count_in_band = 0usize;
        for j in 0..n {
            for k in (j + 1)..n {
                let mut inside = true;
                for t in 0..n_points {
                    let x_t = curve[t];
                    let y_j_t = self.state.values_by_curve[j][t];
                    let y_k_t = self.state.values_by_curve[k][t];
                    let band_min = y_j_t.min(y_k_t);
                    let band_max = y_j_t.max(y_k_t);
                    if x_t < band_min || x_t > band_max {
                        inside = false;
                        break;
                    }
                }
                if inside {
                    count_in_band += 1;
                }
            }
        }
        count_in_band as f64 / n_pairs as f64
    }
}

impl StreamingDepth for StreamingBd {
    fn depth_one(&self, curve: &[f64]) -> f64 {
        self.bd_one_inner(curve)
    }

    fn depth_batch(&self, data_obj: &FdMatrix) -> Vec<f64> {
        let nobj = data_obj.nrows();
        let n = self.state.sorted.nori;
        if nobj == 0 || self.state.sorted.n_points == 0 || n < 2 {
            return vec![0.0; nobj];
        }
        let n_points = self.state.sorted.n_points;
        iter_maybe_parallel!(0..nobj)
            .map(|i| {
                let curve: Vec<f64> = (0..n_points).map(|t| data_obj[(i, t)]).collect();
                self.bd_one_inner(&curve)
            })
            .collect()
    }

    fn n_points(&self) -> usize {
        self.state.sorted.n_points
    }

    fn n_reference(&self) -> usize {
        self.state.sorted.nori
    }
}

// ===========================================================================
// RollingReference — sliding window with incremental sorted-column updates
// ===========================================================================

/// Sliding window of reference curves with incrementally maintained sorted columns.
///
/// When a new curve is pushed and the window is at capacity, the oldest curve
/// is evicted. For each time point the old value is removed (binary-search +
/// `Vec::remove`) and the new value is inserted (binary-search + `Vec::insert`).
///
/// Complexity per push: O(T × N) due to element shifting in the sorted vectors.
pub struct RollingReference {
    curves: VecDeque<Vec<f64>>,
    capacity: usize,
    n_points: usize,
    sorted_columns: Vec<Vec<f64>>,
}

impl RollingReference {
    /// Create an empty rolling window.
    ///
    /// * `capacity` – maximum number of curves in the window (must be ≥ 1).
    /// * `n_points` – number of evaluation points per curve.
    pub fn new(capacity: usize, n_points: usize) -> Self {
        assert!(capacity >= 1, "capacity must be at least 1");
        Self {
            curves: VecDeque::with_capacity(capacity),
            capacity,
            n_points,
            sorted_columns: (0..n_points)
                .map(|_| Vec::with_capacity(capacity))
                .collect(),
        }
    }

    /// Push a new curve into the window.
    ///
    /// If the window is at capacity, the oldest curve is evicted and returned.
    /// For each time point, the sorted column is updated incrementally.
    pub fn push(&mut self, curve: &[f64]) -> Option<Vec<f64>> {
        assert_eq!(
            curve.len(),
            self.n_points,
            "curve length {} does not match n_points {}",
            curve.len(),
            self.n_points
        );

        let evicted = if self.curves.len() == self.capacity {
            let old = self.curves.pop_front().unwrap();
            // Remove old values from sorted columns
            for t in 0..self.n_points {
                let col = &mut self.sorted_columns[t];
                let old_val = old[t];
                let pos = col.partition_point(|&v| v < old_val);
                // Find exact match (handles duplicates by scanning nearby)
                let mut found = false;
                for idx in pos..col.len() {
                    if col[idx] == old_val {
                        col.remove(idx);
                        found = true;
                        break;
                    }
                    if col[idx] > old_val {
                        break;
                    }
                }
                if !found {
                    // Fallback: scan from pos backwards for floating-point edge cases
                    for idx in (0..pos).rev() {
                        if col[idx] == old_val {
                            col.remove(idx);
                            break;
                        }
                        if col[idx] < old_val {
                            break;
                        }
                    }
                }
            }
            Some(old)
        } else {
            None
        };

        // Insert new values into sorted columns
        let new_curve: Vec<f64> = curve.to_vec();
        for t in 0..self.n_points {
            let col = &mut self.sorted_columns[t];
            let val = new_curve[t];
            let pos = col.partition_point(|&v| v < val);
            col.insert(pos, val);
        }
        self.curves.push_back(new_curve);

        evicted
    }

    /// Take a snapshot of the current sorted reference state.
    ///
    /// This clones the sorted columns. For repeated queries, prefer
    /// [`mbd_one`](Self::mbd_one) which queries the window directly.
    pub fn snapshot(&self) -> SortedReferenceState {
        SortedReferenceState {
            sorted_columns: self.sorted_columns.clone(),
            nori: self.curves.len(),
            n_points: self.n_points,
        }
    }

    /// Compute rank-based MBD for a single curve directly against the current window.
    ///
    /// Avoids the overhead of cloning sorted columns into a snapshot.
    pub fn mbd_one(&self, curve: &[f64]) -> f64 {
        let n = self.curves.len();
        if n < 2 || self.n_points == 0 {
            return 0.0;
        }
        assert_eq!(
            curve.len(),
            self.n_points,
            "curve length {} does not match n_points {}",
            curve.len(),
            self.n_points
        );
        let cn2 = c2(n);
        let mut total = 0usize;
        for t in 0..self.n_points {
            let col = &self.sorted_columns[t];
            let below = col.partition_point(|&v| v < curve[t]);
            let at_or_below = col.partition_point(|&v| v <= curve[t]);
            let above = n - at_or_below;
            total += cn2 - c2(below) - c2(above);
        }
        total as f64 / (cn2 as f64 * self.n_points as f64)
    }

    /// Number of curves currently in the window.
    #[inline]
    pub fn len(&self) -> usize {
        self.curves.len()
    }

    /// Whether the window is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.curves.is_empty()
    }

    /// Maximum capacity of the window.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.capacity
    }
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::depth::{band_1d, fraiman_muniz_1d, modified_band_1d};
    use crate::matrix::FdMatrix;
    use std::f64::consts::PI;

    fn uniform_grid(n: usize) -> Vec<f64> {
        (0..n).map(|i| i as f64 / (n - 1) as f64).collect()
    }

    fn generate_centered_data(n: usize, m: usize) -> Vec<f64> {
        let argvals = uniform_grid(m);
        let mut data = vec![0.0; n * m];
        for i in 0..n {
            let offset = (i as f64 - n as f64 / 2.0) / (n as f64);
            for j in 0..m {
                data[i + j * n] = (2.0 * PI * argvals[j]).sin() + offset;
            }
        }
        data
    }

    /// Extract a single curve (row i) from column-major data into row layout.
    fn extract_curve(data: &[f64], i: usize, n: usize, m: usize) -> Vec<f64> {
        (0..m).map(|t| data[i + t * n]).collect()
    }

    // ============== Rank correctness ==============

    #[test]
    fn test_rank_basic() {
        // 5 reference curves, 3 time points
        // Column 0: [1, 2, 3, 4, 5]
        let data = vec![
            1.0, 2.0, 3.0, 4.0, 5.0, // t=0
            10.0, 20.0, 30.0, 40.0, 50.0, // t=1
            100.0, 200.0, 300.0, 400.0, 500.0, // t=2
        ];
        let mat = FdMatrix::from_column_major(data, 5, 3).unwrap();
        let state = SortedReferenceState::from_reference(&mat);

        // At t=0, x=3.0: below=2 (1,2), above=2 (4,5)
        let (below, above) = state.rank_at(0, 3.0);
        assert_eq!(below, 2);
        assert_eq!(above, 2);

        // At t=1, x=25.0: below=2 (10,20), above=3 (30,40,50)
        let (below, above) = state.rank_at(1, 25.0);
        assert_eq!(below, 2);
        assert_eq!(above, 3);
    }

    #[test]
    fn test_rank_boundary_values() {
        // All values identical
        let data = vec![5.0, 5.0, 5.0, 5.0];
        let mat = FdMatrix::from_column_major(data, 4, 1).unwrap();
        let state = SortedReferenceState::from_reference(&mat);

        // x=5.0 exactly: none strictly below, none strictly above
        let (below, above) = state.rank_at(0, 5.0);
        assert_eq!(below, 0);
        assert_eq!(above, 0);

        // x < all: below=0, above=4
        let (below, above) = state.rank_at(0, 3.0);
        assert_eq!(below, 0);
        assert_eq!(above, 4);

        // x > all: below=4, above=0
        let (below, above) = state.rank_at(0, 7.0);
        assert_eq!(below, 4);
        assert_eq!(above, 0);
    }

    #[test]
    fn test_rank_duplicates() {
        // Values with duplicates: [1, 2, 2, 3, 3, 3]
        let data = vec![1.0, 2.0, 2.0, 3.0, 3.0, 3.0];
        let mat = FdMatrix::from_column_major(data, 6, 1).unwrap();
        let state = SortedReferenceState::from_reference(&mat);

        // x=2.0: below=1 (just 1), above=3 (three 3s)
        let (below, above) = state.rank_at(0, 2.0);
        assert_eq!(below, 1);
        assert_eq!(above, 3);

        // x=3.0: below=3 (1,2,2), above=0
        let (below, above) = state.rank_at(0, 3.0);
        assert_eq!(below, 3);
        assert_eq!(above, 0);
    }

    // ============== Batch equivalence ==============

    #[test]
    fn test_streaming_mbd_matches_batch() {
        let n = 15;
        let m = 20;
        let data = generate_centered_data(n, m);

        let mat = FdMatrix::from_slice(&data, n, m).unwrap();
        let batch = modified_band_1d(&mat, &mat);
        let state = SortedReferenceState::from_reference(&mat);
        let streaming = StreamingMbd::new(state);
        let streaming_result = streaming.depth_batch(&mat);

        assert_eq!(batch.len(), streaming_result.len());
        for (b, s) in batch.iter().zip(streaming_result.iter()) {
            assert!(
                (b - s).abs() < 1e-10,
                "MBD mismatch: batch={}, streaming={}",
                b,
                s
            );
        }
    }

    #[test]
    fn test_streaming_fm_matches_batch() {
        let n = 15;
        let m = 20;
        let data = generate_centered_data(n, m);

        let mat = FdMatrix::from_slice(&data, n, m).unwrap();
        for scale in [true, false] {
            let batch = fraiman_muniz_1d(&mat, &mat, scale);
            let state = SortedReferenceState::from_reference(&mat);
            let streaming = StreamingFraimanMuniz::new(state, scale);
            let streaming_result = streaming.depth_batch(&mat);

            assert_eq!(batch.len(), streaming_result.len());
            for (b, s) in batch.iter().zip(streaming_result.iter()) {
                assert!(
                    (b - s).abs() < 1e-10,
                    "FM mismatch (scale={}): batch={}, streaming={}",
                    scale,
                    b,
                    s
                );
            }
        }
    }

    #[test]
    fn test_streaming_bd_matches_batch() {
        let n = 10;
        let m = 20;
        let data = generate_centered_data(n, m);

        let mat = FdMatrix::from_slice(&data, n, m).unwrap();
        let batch = band_1d(&mat, &mat);
        let full_state = FullReferenceState::from_reference(&mat);
        let streaming = StreamingBd::new(full_state);
        let streaming_result = streaming.depth_batch(&mat);

        assert_eq!(batch.len(), streaming_result.len());
        for (b, s) in batch.iter().zip(streaming_result.iter()) {
            assert!(
                (b - s).abs() < 1e-10,
                "BD mismatch: batch={}, streaming={}",
                b,
                s
            );
        }
    }

    // ============== Rolling reference ==============

    #[test]
    fn test_rolling_sorted_columns_maintained() {
        let mut rolling = RollingReference::new(3, 2);

        rolling.push(&[1.0, 10.0]);
        assert_eq!(rolling.sorted_columns[0], vec![1.0]);
        assert_eq!(rolling.sorted_columns[1], vec![10.0]);

        rolling.push(&[3.0, 5.0]);
        assert_eq!(rolling.sorted_columns[0], vec![1.0, 3.0]);
        assert_eq!(rolling.sorted_columns[1], vec![5.0, 10.0]);

        rolling.push(&[2.0, 7.0]);
        assert_eq!(rolling.sorted_columns[0], vec![1.0, 2.0, 3.0]);
        assert_eq!(rolling.sorted_columns[1], vec![5.0, 7.0, 10.0]);

        // Push a 4th — evicts [1.0, 10.0]
        let evicted = rolling.push(&[0.5, 8.0]);
        assert_eq!(evicted, Some(vec![1.0, 10.0]));
        assert_eq!(rolling.sorted_columns[0], vec![0.5, 2.0, 3.0]);
        assert_eq!(rolling.sorted_columns[1], vec![5.0, 7.0, 8.0]);
    }

    #[test]
    fn test_rolling_mbd_matches_batch() {
        let n = 10;
        let m = 15;
        let data = generate_centered_data(n, m);

        // Fill a rolling window with the same curves
        let mut rolling = RollingReference::new(n, m);
        for i in 0..n {
            let curve = extract_curve(&data, i, n, m);
            rolling.push(&curve);
        }

        // mbd_one should match batch for each curve
        let mat = FdMatrix::from_slice(&data, n, m).unwrap();
        let batch = modified_band_1d(&mat, &mat);
        for i in 0..n {
            let curve = extract_curve(&data, i, n, m);
            let rolling_depth = rolling.mbd_one(&curve);
            assert!(
                (batch[i] - rolling_depth).abs() < 1e-10,
                "Rolling MBD mismatch at i={}: batch={}, rolling={}",
                i,
                batch[i],
                rolling_depth
            );
        }
    }

    #[test]
    fn test_rolling_eviction_correctness() {
        let m = 5;
        let mut rolling = RollingReference::new(3, m);

        // Push 5 curves — window should only contain the last 3
        let curves: Vec<Vec<f64>> = (0..5)
            .map(|i| (0..m).map(|t| (i * m + t) as f64).collect())
            .collect();

        for c in &curves {
            rolling.push(c);
        }

        assert_eq!(rolling.len(), 3);

        // Snapshot should match manually-built state from curves 2,3,4
        let snapshot = rolling.snapshot();
        assert_eq!(snapshot.nori(), 3);

        // Build reference data manually from curves 2..5
        let mut ref_data = vec![0.0; 3 * m];
        for (idx, ci) in (2..5).enumerate() {
            for t in 0..m {
                ref_data[idx + t * 3] = curves[ci][t];
            }
        }
        let ref_mat = FdMatrix::from_column_major(ref_data, 3, m).unwrap();
        let expected = SortedReferenceState::from_reference(&ref_mat);

        for t in 0..m {
            assert_eq!(
                snapshot.sorted_columns[t], expected.sorted_columns[t],
                "sorted columns differ at t={}",
                t
            );
        }
    }

    // ============== Properties ==============

    #[test]
    fn test_depth_in_unit_interval() {
        let n = 20;
        let m = 30;
        let data = generate_centered_data(n, m);
        let mat = FdMatrix::from_slice(&data, n, m).unwrap();

        let state_mbd = SortedReferenceState::from_reference(&mat);
        let mbd = StreamingMbd::new(state_mbd);
        for d in mbd.depth_batch(&mat) {
            assert!((0.0..=1.0).contains(&d), "MBD out of range: {}", d);
        }

        let state_fm = SortedReferenceState::from_reference(&mat);
        let fm = StreamingFraimanMuniz::new(state_fm, true);
        for d in fm.depth_batch(&mat) {
            assert!((0.0..=1.0).contains(&d), "FM out of range: {}", d);
        }

        let full = FullReferenceState::from_reference(&mat);
        let bd = StreamingBd::new(full);
        for d in bd.depth_batch(&mat) {
            assert!((0.0..=1.0).contains(&d), "BD out of range: {}", d);
        }
    }

    #[test]
    fn test_central_curves_deeper() {
        let n = 20;
        let m = 30;
        let data = generate_centered_data(n, m);
        let mat = FdMatrix::from_slice(&data, n, m).unwrap();

        let state = SortedReferenceState::from_reference(&mat);
        let mbd = StreamingMbd::new(state);
        let depths = mbd.depth_batch(&mat);

        let central_depth = depths[n / 2];
        let edge_depth = depths[0];
        assert!(
            central_depth > edge_depth,
            "Central curve should be deeper: {} > {}",
            central_depth,
            edge_depth
        );
    }

    #[test]
    fn test_empty_inputs() {
        let empty = FdMatrix::zeros(0, 0);
        let state = SortedReferenceState::from_reference(&empty);
        let mbd = StreamingMbd::new(state);
        assert_eq!(mbd.depth_one(&[]), 0.0);

        let state = SortedReferenceState::from_reference(&empty);
        let fm = StreamingFraimanMuniz::new(state, true);
        assert_eq!(fm.depth_one(&[]), 0.0);
    }

    #[test]
    fn test_depth_one_matches_depth_batch_single() {
        let n = 10;
        let m = 15;
        let data = generate_centered_data(n, m);
        let mat = FdMatrix::from_slice(&data, n, m).unwrap();

        // Build a 1-curve column-major "matrix" from curve 3
        let curve = extract_curve(&data, 3, n, m);
        let single_mat = FdMatrix::from_column_major(curve.clone(), 1, m).unwrap();

        let state = SortedReferenceState::from_reference(&mat);
        let mbd = StreamingMbd::new(state);

        let one = mbd.depth_one(&curve);
        let batch = mbd.depth_batch(&single_mat);
        assert!(
            (one - batch[0]).abs() < 1e-14,
            "depth_one ({}) != depth_batch ({}) for single curve",
            one,
            batch[0]
        );
    }

    // ============== Thread safety ==============

    #[test]
    fn test_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<SortedReferenceState>();
        assert_send_sync::<StreamingMbd>();
        assert_send_sync::<StreamingFraimanMuniz>();
        assert_send_sync::<FullReferenceState>();
        assert_send_sync::<StreamingBd>();
        assert_send_sync::<RollingReference>();
    }

    // ============== Edge cases ==============

    #[test]
    fn test_single_reference_curve() {
        // nori=1: C(1,2) = 0, MBD is undefined → returns 0
        let data = vec![1.0, 2.0, 3.0]; // 1 curve, 3 time points
        let mat = FdMatrix::from_column_major(data, 1, 3).unwrap();
        let state = SortedReferenceState::from_reference(&mat);
        let mbd = StreamingMbd::new(state);
        assert_eq!(mbd.depth_one(&[1.0, 2.0, 3.0]), 0.0);

        // BD also needs at least 2
        let full = FullReferenceState::from_reference(&mat);
        let bd = StreamingBd::new(full);
        assert_eq!(bd.depth_one(&[1.0, 2.0, 3.0]), 0.0);
    }

    #[test]
    fn test_capacity_one_window() {
        let mut rolling = RollingReference::new(1, 3);

        rolling.push(&[1.0, 2.0, 3.0]);
        assert_eq!(rolling.len(), 1);
        // MBD with 1 curve → 0
        assert_eq!(rolling.mbd_one(&[1.0, 2.0, 3.0]), 0.0);

        let evicted = rolling.push(&[4.0, 5.0, 6.0]);
        assert_eq!(evicted, Some(vec![1.0, 2.0, 3.0]));
        assert_eq!(rolling.len(), 1);
    }

    #[test]
    #[should_panic(expected = "curve length")]
    fn test_curve_length_mismatch() {
        let mat = FdMatrix::from_column_major(vec![1.0, 2.0, 3.0, 4.0], 2, 2).unwrap();
        let state = SortedReferenceState::from_reference(&mat);
        let mbd = StreamingMbd::new(state);
        // Curve has 3 elements but n_points is 2 — should ideally be caught.
        // depth_one doesn't assert length (it just indexes), but rolling does.
        let mut rolling = RollingReference::new(5, 2);
        rolling.push(&[1.0, 2.0, 3.0]); // panics: length mismatch
        let _ = mbd; // suppress unused warning
    }

    // ============== Additional: snapshot-based streaming ==============

    #[test]
    fn test_rolling_snapshot_produces_valid_mbd() {
        let n = 8;
        let m = 10;
        let data = generate_centered_data(n, m);

        let mut rolling = RollingReference::new(n, m);
        for i in 0..n {
            let curve = extract_curve(&data, i, n, m);
            rolling.push(&curve);
        }

        let snapshot = rolling.snapshot();
        let mbd = StreamingMbd::new(snapshot);

        let mat = FdMatrix::from_slice(&data, n, m).unwrap();
        let batch_depths = modified_band_1d(&mat, &mat);
        let streaming_depths = mbd.depth_batch(&mat);

        for (b, s) in batch_depths.iter().zip(streaming_depths.iter()) {
            assert!(
                (b - s).abs() < 1e-10,
                "Snapshot MBD mismatch: batch={}, streaming={}",
                b,
                s
            );
        }
    }
}
