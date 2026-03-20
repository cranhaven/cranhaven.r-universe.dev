//! Column-major matrix type for functional data analysis.
//!
//! [`FdMatrix`] provides safe, dimension-tracked access to the flat column-major
//! data layout used throughout this crate. It eliminates manual `data[i + j * n]`
//! index arithmetic and carries dimensions alongside the data.

use nalgebra::DMatrix;

/// Column-major matrix for functional data.
///
/// Stores data in a flat `Vec<f64>` with column-major (Fortran) layout:
/// element `(row, col)` is at index `row + col * nrows`.
///
/// # Conventions
///
/// For functional data, rows typically represent observations and columns
/// represent evaluation points. For 2D surfaces with `m1 x m2` grids,
/// the surface is flattened into `m1 * m2` columns.
///
/// # Examples
///
/// ```
/// use fdars_core::matrix::FdMatrix;
///
/// // 3 observations, 4 evaluation points
/// let data = vec![
///     1.0, 2.0, 3.0,  // column 0 (all obs at point 0)
///     4.0, 5.0, 6.0,  // column 1
///     7.0, 8.0, 9.0,  // column 2
///     10.0, 11.0, 12.0, // column 3
/// ];
/// let mat = FdMatrix::from_column_major(data, 3, 4).unwrap();
///
/// assert_eq!(mat[(0, 0)], 1.0);  // obs 0 at point 0
/// assert_eq!(mat[(1, 2)], 8.0);  // obs 1 at point 2
/// assert_eq!(mat.column(0), &[1.0, 2.0, 3.0]);
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct FdMatrix {
    data: Vec<f64>,
    nrows: usize,
    ncols: usize,
}

impl FdMatrix {
    /// Create from flat column-major data with dimension validation.
    ///
    /// Returns `None` if `data.len() != nrows * ncols`.
    pub fn from_column_major(data: Vec<f64>, nrows: usize, ncols: usize) -> Option<Self> {
        if data.len() != nrows * ncols {
            return None;
        }
        Some(Self { data, nrows, ncols })
    }

    /// Create from a borrowed slice (copies the data).
    ///
    /// Returns `None` if `data.len() != nrows * ncols`.
    pub fn from_slice(data: &[f64], nrows: usize, ncols: usize) -> Option<Self> {
        if data.len() != nrows * ncols {
            return None;
        }
        Some(Self {
            data: data.to_vec(),
            nrows,
            ncols,
        })
    }

    /// Create a zero-filled matrix.
    pub fn zeros(nrows: usize, ncols: usize) -> Self {
        Self {
            data: vec![0.0; nrows * ncols],
            nrows,
            ncols,
        }
    }

    /// Number of rows.
    #[inline]
    pub fn nrows(&self) -> usize {
        self.nrows
    }

    /// Number of columns.
    #[inline]
    pub fn ncols(&self) -> usize {
        self.ncols
    }

    /// Dimensions as `(nrows, ncols)`.
    #[inline]
    pub fn shape(&self) -> (usize, usize) {
        (self.nrows, self.ncols)
    }

    /// Total number of elements.
    #[inline]
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Whether the matrix is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Get a contiguous column slice (zero-copy).
    ///
    /// # Panics
    /// Panics if `col >= ncols`.
    #[inline]
    pub fn column(&self, col: usize) -> &[f64] {
        let start = col * self.nrows;
        &self.data[start..start + self.nrows]
    }

    /// Get a mutable contiguous column slice (zero-copy).
    ///
    /// # Panics
    /// Panics if `col >= ncols`.
    #[inline]
    pub fn column_mut(&mut self, col: usize) -> &mut [f64] {
        let start = col * self.nrows;
        &mut self.data[start..start + self.nrows]
    }

    /// Extract a single row as a new `Vec<f64>`.
    ///
    /// This is an O(ncols) operation because rows are not contiguous
    /// in column-major layout.
    pub fn row(&self, row: usize) -> Vec<f64> {
        (0..self.ncols)
            .map(|j| self.data[row + j * self.nrows])
            .collect()
    }

    /// Extract all rows as `Vec<Vec<f64>>`.
    ///
    /// Equivalent to the former `extract_curves` function.
    pub fn rows(&self) -> Vec<Vec<f64>> {
        (0..self.nrows).map(|i| self.row(i)).collect()
    }

    /// Flat slice of the underlying column-major data (zero-copy).
    #[inline]
    pub fn as_slice(&self) -> &[f64] {
        &self.data
    }

    /// Mutable flat slice of the underlying column-major data.
    #[inline]
    pub fn as_mut_slice(&mut self) -> &mut [f64] {
        &mut self.data
    }

    /// Consume and return the underlying `Vec<f64>`.
    pub fn into_vec(self) -> Vec<f64> {
        self.data
    }

    /// Convert to a nalgebra `DMatrix<f64>`.
    ///
    /// This copies the data into nalgebra's storage. Both use column-major
    /// layout, so the copy is a simple memcpy.
    pub fn to_dmatrix(&self) -> DMatrix<f64> {
        DMatrix::from_column_slice(self.nrows, self.ncols, &self.data)
    }

    /// Create from a nalgebra `DMatrix<f64>`.
    ///
    /// Both use column-major layout so this is a direct copy.
    pub fn from_dmatrix(mat: &DMatrix<f64>) -> Self {
        let (nrows, ncols) = mat.shape();
        Self {
            data: mat.as_slice().to_vec(),
            nrows,
            ncols,
        }
    }

    /// Get element at (row, col) with bounds checking.
    #[inline]
    pub fn get(&self, row: usize, col: usize) -> Option<f64> {
        if row < self.nrows && col < self.ncols {
            Some(self.data[row + col * self.nrows])
        } else {
            None
        }
    }

    /// Set element at (row, col) with bounds checking.
    #[inline]
    pub fn set(&mut self, row: usize, col: usize, value: f64) -> bool {
        if row < self.nrows && col < self.ncols {
            self.data[row + col * self.nrows] = value;
            true
        } else {
            false
        }
    }
}

impl std::ops::Index<(usize, usize)> for FdMatrix {
    type Output = f64;

    #[inline]
    fn index(&self, (row, col): (usize, usize)) -> &f64 {
        debug_assert!(
            row < self.nrows && col < self.ncols,
            "FdMatrix index ({}, {}) out of bounds for {}x{} matrix",
            row,
            col,
            self.nrows,
            self.ncols
        );
        &self.data[row + col * self.nrows]
    }
}

impl std::ops::IndexMut<(usize, usize)> for FdMatrix {
    #[inline]
    fn index_mut(&mut self, (row, col): (usize, usize)) -> &mut f64 {
        debug_assert!(
            row < self.nrows && col < self.ncols,
            "FdMatrix index ({}, {}) out of bounds for {}x{} matrix",
            row,
            col,
            self.nrows,
            self.ncols
        );
        &mut self.data[row + col * self.nrows]
    }
}

impl std::fmt::Display for FdMatrix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FdMatrix({}x{})", self.nrows, self.ncols)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_3x4() -> FdMatrix {
        // 3 rows, 4 columns, column-major
        let data = vec![
            1.0, 2.0, 3.0, // col 0
            4.0, 5.0, 6.0, // col 1
            7.0, 8.0, 9.0, // col 2
            10.0, 11.0, 12.0, // col 3
        ];
        FdMatrix::from_column_major(data, 3, 4).unwrap()
    }

    #[test]
    fn test_from_column_major_valid() {
        let mat = sample_3x4();
        assert_eq!(mat.nrows(), 3);
        assert_eq!(mat.ncols(), 4);
        assert_eq!(mat.shape(), (3, 4));
        assert_eq!(mat.len(), 12);
        assert!(!mat.is_empty());
    }

    #[test]
    fn test_from_column_major_invalid() {
        assert!(FdMatrix::from_column_major(vec![1.0, 2.0], 3, 4).is_none());
    }

    #[test]
    fn test_from_slice() {
        let data = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0];
        let mat = FdMatrix::from_slice(&data, 2, 3).unwrap();
        assert_eq!(mat[(0, 0)], 1.0);
        assert_eq!(mat[(1, 0)], 2.0);
        assert_eq!(mat[(0, 1)], 3.0);
    }

    #[test]
    fn test_from_slice_invalid() {
        assert!(FdMatrix::from_slice(&[1.0, 2.0], 3, 3).is_none());
    }

    #[test]
    fn test_zeros() {
        let mat = FdMatrix::zeros(2, 3);
        assert_eq!(mat.nrows(), 2);
        assert_eq!(mat.ncols(), 3);
        for j in 0..3 {
            for i in 0..2 {
                assert_eq!(mat[(i, j)], 0.0);
            }
        }
    }

    #[test]
    fn test_index() {
        let mat = sample_3x4();
        assert_eq!(mat[(0, 0)], 1.0);
        assert_eq!(mat[(1, 0)], 2.0);
        assert_eq!(mat[(2, 0)], 3.0);
        assert_eq!(mat[(0, 1)], 4.0);
        assert_eq!(mat[(1, 1)], 5.0);
        assert_eq!(mat[(2, 3)], 12.0);
    }

    #[test]
    fn test_index_mut() {
        let mut mat = sample_3x4();
        mat[(1, 2)] = 99.0;
        assert_eq!(mat[(1, 2)], 99.0);
    }

    #[test]
    fn test_column() {
        let mat = sample_3x4();
        assert_eq!(mat.column(0), &[1.0, 2.0, 3.0]);
        assert_eq!(mat.column(1), &[4.0, 5.0, 6.0]);
        assert_eq!(mat.column(3), &[10.0, 11.0, 12.0]);
    }

    #[test]
    fn test_column_mut() {
        let mut mat = sample_3x4();
        mat.column_mut(1)[0] = 99.0;
        assert_eq!(mat[(0, 1)], 99.0);
    }

    #[test]
    fn test_row() {
        let mat = sample_3x4();
        assert_eq!(mat.row(0), vec![1.0, 4.0, 7.0, 10.0]);
        assert_eq!(mat.row(1), vec![2.0, 5.0, 8.0, 11.0]);
        assert_eq!(mat.row(2), vec![3.0, 6.0, 9.0, 12.0]);
    }

    #[test]
    fn test_rows() {
        let mat = sample_3x4();
        let rows = mat.rows();
        assert_eq!(rows.len(), 3);
        assert_eq!(rows[0], vec![1.0, 4.0, 7.0, 10.0]);
        assert_eq!(rows[2], vec![3.0, 6.0, 9.0, 12.0]);
    }

    #[test]
    fn test_as_slice() {
        let mat = sample_3x4();
        let expected = vec![
            1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0,
        ];
        assert_eq!(mat.as_slice(), expected.as_slice());
    }

    #[test]
    fn test_into_vec() {
        let mat = sample_3x4();
        let v = mat.into_vec();
        assert_eq!(v.len(), 12);
        assert_eq!(v[0], 1.0);
    }

    #[test]
    fn test_get_bounds_check() {
        let mat = sample_3x4();
        assert_eq!(mat.get(0, 0), Some(1.0));
        assert_eq!(mat.get(2, 3), Some(12.0));
        assert_eq!(mat.get(3, 0), None); // row out of bounds
        assert_eq!(mat.get(0, 4), None); // col out of bounds
    }

    #[test]
    fn test_set_bounds_check() {
        let mut mat = sample_3x4();
        assert!(mat.set(1, 1, 99.0));
        assert_eq!(mat[(1, 1)], 99.0);
        assert!(!mat.set(5, 0, 99.0)); // out of bounds
    }

    #[test]
    fn test_nalgebra_roundtrip() {
        let mat = sample_3x4();
        let dmat = mat.to_dmatrix();
        assert_eq!(dmat.nrows(), 3);
        assert_eq!(dmat.ncols(), 4);
        assert_eq!(dmat[(0, 0)], 1.0);
        assert_eq!(dmat[(1, 2)], 8.0);

        let back = FdMatrix::from_dmatrix(&dmat);
        assert_eq!(mat, back);
    }

    #[test]
    fn test_empty() {
        let mat = FdMatrix::zeros(0, 0);
        assert!(mat.is_empty());
        assert_eq!(mat.len(), 0);
    }

    #[test]
    fn test_single_element() {
        let mat = FdMatrix::from_column_major(vec![42.0], 1, 1).unwrap();
        assert_eq!(mat[(0, 0)], 42.0);
        assert_eq!(mat.column(0), &[42.0]);
        assert_eq!(mat.row(0), vec![42.0]);
    }

    #[test]
    fn test_display() {
        let mat = sample_3x4();
        assert_eq!(format!("{}", mat), "FdMatrix(3x4)");
    }

    #[test]
    fn test_clone() {
        let mat = sample_3x4();
        let cloned = mat.clone();
        assert_eq!(mat, cloned);
    }

    #[test]
    fn test_as_mut_slice() {
        let mut mat = FdMatrix::zeros(2, 2);
        let s = mat.as_mut_slice();
        s[0] = 1.0;
        s[1] = 2.0;
        s[2] = 3.0;
        s[3] = 4.0;
        assert_eq!(mat[(0, 0)], 1.0);
        assert_eq!(mat[(1, 0)], 2.0);
        assert_eq!(mat[(0, 1)], 3.0);
        assert_eq!(mat[(1, 1)], 4.0);
    }

    #[test]
    fn test_column_major_layout_matches_manual() {
        // Verify that FdMatrix[(i, j)] == data[i + j * n] for all i, j
        let n = 5;
        let m = 7;
        let data: Vec<f64> = (0..n * m).map(|x| x as f64).collect();
        let mat = FdMatrix::from_column_major(data.clone(), n, m).unwrap();

        for j in 0..m {
            for i in 0..n {
                assert_eq!(mat[(i, j)], data[i + j * n]);
            }
        }
    }
}
