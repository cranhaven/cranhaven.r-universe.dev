test_that("ICAR enforces sum-to-zero per connected component", {

  A <- Matrix::Matrix(0, 6, 6, sparse = TRUE)

  # component 1: 1-2-3
  A[1,2] <- A[2,1] <- 1
  A[2,3] <- A[3,2] <- 1

  # component 2: 4-5
  A[4,5] <- A[5,4] <- 1

  # node 6 isolate

  set.seed(1)
  x <- sample_icar(A, tau = 2)

  expect_equal(sum(x[1:3]), 0, tolerance = 1e-6)
  expect_equal(sum(x[4:5]), 0, tolerance = 1e-6)
  expect_true(is.finite(x[6]))
})


test_that("ICAR isolate = drop sets isolated nodes to NA", {

  A <- Matrix::Matrix(0, 4, 4, sparse = TRUE)
  A[1,2] <- A[2,1] <- 1
  A[2,3] <- A[3,2] <- 1
  # node 4 isolate

  x <- sample_icar(A, isolate = "drop")

  expect_true(is.na(x[4]))
  expect_equal(sum(x[1:3]), 0, tolerance = 1e-6)
})


test_that("ICAR sampling is reproducible", {

  A <- Matrix::bandSparse(
    10,
    k = c(-1, 1),
    diag = list(rep(1, 9), rep(1, 9))
  )

  set.seed(123)
  x1 <- sample_icar(A)

  set.seed(123)
  x2 <- sample_icar(A)

  expect_equal(x1, x2)
})


test_that("ICAR handles all-isolates graph", {
  A <- Matrix::Matrix(0, 5, 5, sparse = TRUE)

  set.seed(1)
  x_ind <- sample_icar(A, isolate = "independent", tau = 2)
  expect_true(all(is.finite(x_ind)))
  expect_equal(length(x_ind), 5)

  x_drop <- sample_icar(A, isolate = "drop", tau = 2)
  expect_true(all(is.na(x_drop)))
  expect_equal(length(x_drop), 5)
})


test_that("ICAR works for component of size 2 (single edge)", {
  A <- Matrix::Matrix(0, 3, 3, sparse = TRUE)
  A[1,2] <- A[2,1] <- 1
  # node 3 isolate

  x <- sample_icar(A, isolate = "independent", tau = 1)
  expect_equal(sum(x[1:2]), 0, tolerance = 1e-6)
  expect_true(is.finite(x[3]))
})


test_that("ICAR rejects invalid inputs", {
  A_rect <- Matrix::Matrix(0, 3, 4, sparse = TRUE)
  expect_error(sample_icar(A_rect), "square|nrow|ncol|dimension", ignore.case = TRUE)

  A <- Matrix::Matrix(0, 3, 3, sparse = TRUE)
  A[1,2] <- A[2,1] <- 1

  expect_error(sample_icar(A, tau = 0), "tau", ignore.case = TRUE)
  expect_error(sample_icar(A, tau = -1), "tau", ignore.case = TRUE)

  expect_error(sample_icar(A, kappa = 0), "kappa", ignore.case = TRUE)
  expect_error(sample_icar(A, kappa = -10), "kappa", ignore.case = TRUE)

  expect_error(sample_icar(A, isolate_prec = 0), "isolate_prec", ignore.case = TRUE)
  expect_error(sample_icar(A, isolate_prec = -1), "isolate_prec", ignore.case = TRUE)
})


test_that("ICAR is robust to diagonal entries and mild asymmetry in A", {
  A <- Matrix::Matrix(0, 6, 6, sparse = TRUE)

  # component 1: 1-2-3
  A[1,2] <- 1
  A[2,1] <- 1
  A[2,3] <- 1
  A[3,2] <- 1

  # component 2: 4-5, but introduce mild asymmetry
  A[4,5] <- 1
  A[5,4] <- 2  # asymmetry on purpose

  # diagonal noise
  diag(A) <- 7

  set.seed(10)
  x <- sample_icar(A, tau = 1, isolate = "independent")

  # component-wise centering should still hold (after forceSymmetric + diag=0)
  expect_equal(sum(x[1:3]), 0, tolerance = 1e-6)
  expect_equal(sum(x[4:5]), 0, tolerance = 1e-6)
  expect_true(is.finite(x[6]))  # isolate
})
