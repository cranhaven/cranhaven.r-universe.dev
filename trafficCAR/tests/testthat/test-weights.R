test_that("as_sparse_adjacency rejects NULL and non-matrices", {
  expect_error(as_sparse_adjacency(NULL))
  expect_error(as_sparse_adjacency(1:5))
  expect_error(as_sparse_adjacency(list(a = 1)))
})


test_that("as_sparse_adjacency rejects non-square matrices", {
  A <- matrix(1, nrow = 3, ncol = 2)
  expect_error(as_sparse_adjacency(A))
})


test_that("as_sparse_adjacency rejects NA values", {
  A <- matrix(c(0, 1, NA, 0), 2, 2)
  expect_error(as_sparse_adjacency(A))
})


test_that("as_sparse_adjacency coerces to numeric sparse matrix", {
  A <- matrix(c(TRUE, FALSE, FALSE, TRUE), 2, 2)
  B <- as_sparse_adjacency(A, symmetrize = TRUE)

  expect_s4_class(B, "dgCMatrix")
  expect_type(B@x, "double")
})


test_that("diagonal is forced to zero", {
  A <- matrix(c(1, 1, 1, 1), 2, 2)
  B <- as_sparse_adjacency(A, symmetrize = TRUE)
  expect_equal(Matrix::diag(B), c(0, 0))
})


test_that("as_sparse_adjacency rejects non-finite values", {
  A <- matrix(c(0, Inf, Inf, 0), 2, 2)
  expect_error(as_sparse_adjacency(A), "finite|Inf|infinite", ignore.case = TRUE)

  B <- matrix(c(0, NaN, NaN, 0), 2, 2)
  expect_error(as_sparse_adjacency(B), "NA|NaN", ignore.case = TRUE)
})


test_that("as_sparse_adjacency drops explicitly stored zeros", {
  A <- Matrix::sparseMatrix(
    i = c(1, 2), j = c(2, 1), x = c(0, 0),
    dims = c(2, 2)
  )
  B <- as_sparse_adjacency(A, symmetrize = FALSE, check = FALSE)
  expect_equal(length(B@x), 0L)
})


test_that("degenerate sizes behave consistently", {
  A0 <- matrix(numeric(), 0, 0)
  expect_error(as_sparse_adjacency(A0), "square|0", ignore.case = TRUE)

  A1 <- matrix(5, 1, 1)
  B1 <- as_sparse_adjacency(A1, check = FALSE)
  expect_equal(dim(B1), c(1L, 1L))
  expect_equal(Matrix::diag(B1), 0)
  expect_equal(length(B1@x), 0L)
})


test_that("logical and integer inputs end up numeric sparse", {
  Al <- matrix(c(TRUE, FALSE, FALSE, TRUE), 2, 2)
  Bl <- as_sparse_adjacency(Al, symmetrize = TRUE)
  expect_s4_class(Bl, "dgCMatrix")
  expect_true(is.double(Bl@x))

  Ai <- matrix(c(0L, 2L, 2L, 0L), 2, 2)
  Bi <- as_sparse_adjacency(Ai, symmetrize = TRUE)
  expect_s4_class(Bi, "dgCMatrix")
  expect_equal(Bi[1,2], 2)
})


test_that("handles extreme magnitudes without densifying", {
  big <- .Machine$double.xmax / 4
  small <- .Machine$double.xmin * 4

  A <- matrix(c(0, big, big, 0), 2, 2)
  B <- as_sparse_adjacency(A, symmetrize = TRUE, check = FALSE)
  expect_equal(B[1,2], big)

  A2 <- matrix(c(0, small, small, 0), 2, 2)
  B2 <- as_sparse_adjacency(A2, symmetrize = TRUE, check = FALSE)
  expect_equal(B2[1,2], small)
})


test_that("symmetry is not silently bypassed for large matrices", {
  n <- 2001L
  A <- Matrix::sparseMatrix(i = 1, j = 2, x = 1, dims = c(n, n))  # asymmetric
  expect_error(
    as_sparse_adjacency(A, symmetrize = FALSE, check = TRUE),
    "symmetric",
    ignore.case = TRUE
  )
})


test_that("row_standardize_weights handles mixed degrees and preserves sparsity", {
  A <- Matrix::sparseMatrix(
    i = c(1,1,1, 2,3,4,5,6),
    j = c(2,3,4, 1,1,1,1,1),
    x = 1,
    dims = c(6,6)
  )
  A <- as_sparse_adjacency(A, symmetrize = TRUE, check = FALSE)

  W <- row_standardize_weights(A, zero_policy = "keep")
  rs <- Matrix::rowSums(W)
  expect_true(all(abs(rs[rs > 0] - 1) < 1e-12))
  expect_s4_class(W, "dgCMatrix")
})


test_that("row-standardized weights satisfy key invariants", {
  A <- matrix(c(0, 2, 0,
                2, 0, 1,
                0, 1, 0), 3, 3)

  W <- weights_from_adjacency(A, style = "row-standardized", symmetrize = TRUE)

  expect_equal(Matrix::diag(W), c(0,0,0))
  expect_true(all(is.finite(W@x)))
  expect_true(all(W@x >= 0))
  rs <- Matrix::rowSums(W)
  expect_true(all(abs(rs - 1) < 1e-12))
})


test_that("as_sparse_adjacency returns dgCMatrix as documented", {
  A <- matrix(c(0,1,1,0), 2, 2)
  B <- as_sparse_adjacency(A, symmetrize = TRUE)
  expect_s4_class(B, "dgCMatrix")
})


test_that("isolated nodes are preserved", {
  A <- matrix(0, 3, 3)
  B <- as_sparse_adjacency(A)
  expect_equal(Matrix::rowSums(B), c(0, 0, 0))
})
