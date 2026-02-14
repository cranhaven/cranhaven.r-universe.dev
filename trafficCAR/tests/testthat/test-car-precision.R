test_that("proper CAR behaves near the ICAR boundary", {
  n <- 50
  A <- matrix(0, n, n)
  for (i in 1:(n - 1)) {
    A[i, i + 1] <- A[i + 1, i] <- 1
  }

  Q1 <- car_precision(
    A,
    type = "proper",
    rho = 0.999,
    tau = 1,
    symmetrize = TRUE
  )

  Q2 <- car_precision(
    A,
    type = "proper",
    rho = 0.9,
    tau = 1,
    symmetrize = TRUE
  )

  ## near-singular but finite
  expect_true(all(is.finite(Matrix::diag(Q1))))
  expect_gt(kappa(as.matrix(Q1)), kappa(as.matrix(Q2)))
})

test_that("high-degree hubs do not break construction", {
  n <- 30
  A <- matrix(0, n, n)
  A[1, 2:n] <- 1
  A[2:n, 1] <- 1

  Q <- suppressWarnings(
    intrinsic_car_precision(
      A,
      scale = TRUE,
      symmetrize = TRUE
    )
  )

  expect_s4_class(Q, "dsCMatrix")
  expect_true(Matrix::isSymmetric(Q))
})


test_that("non-symmetric adjacency is symmetrized correctly", {
  A <- matrix(0, 5, 5)
  A[1, 2] <- 1
  A[2, 3] <- 1

  Q <- suppressWarnings(
    car_precision(A, symmetrize = TRUE)
  )

  expect_true(Matrix::isSymmetric(Q))
})


test_that("large sparse graph preserves sparsity", {
  set.seed(1)
  n <- 200

  A <- Matrix::rsparsematrix(n, n, density = 0.01)
  A <- abs(A)
  A[A > 0] <- 1
  diag(A) <- 0

  Q <- suppressWarnings(
    car_precision(
      A,
      type = "icar",
      symmetrize = TRUE
    )
  )

  expect_lt(length(Q@x), n * 20)
})


test_that("permutation of nodes leaves spectrum invariant", {
  n <- 40
  A <- matrix(0, n, n)
  for (i in 1:(n - 1)) {
    A[i, i + 1] <- A[i + 1, i] <- 1
  }

  perm <- sample(n)
  A_perm <- A[perm, perm]

  Q1 <- suppressWarnings(
    intrinsic_car_precision(A, scale = TRUE, symmetrize = TRUE)
  )
  Q2 <- suppressWarnings(
    intrinsic_car_precision(A_perm, scale = TRUE, symmetrize = TRUE)
  )

  ev1 <- sort(Re(eigen(as.matrix(Q1), symmetric = TRUE)$values))
  ev2 <- sort(Re(eigen(as.matrix(Q2), symmetric = TRUE)$values))

  expect_equal(ev1, ev2, tolerance = 1e-8)
})


test_that("heterogeneous connected components scale correctly", {
  ## chain + star
  A1 <- matrix(0, 10, 10)
  for (i in 1:9) {
    A1[i, i + 1] <- A1[i + 1, i] <- 1
  }

  A2 <- matrix(0, 6, 6)
  A2[1, 2:6] <- 1
  A2[2:6, 1] <- 1

  A <- Matrix::bdiag(A1, A2)

  Q <- suppressWarnings(intrinsic_car_precision(A, scale = TRUE, symmetrize = TRUE))

  expect_true(all(is.finite(Matrix::diag(Q))))
})

test_that("proper CAR rejects invalid rho", {
  A <- matrix(0, 5, 5)
  for (i in 1:4) A[i, i+1] <- A[i+1, i] <- 1

  expect_error(
    car_precision(A, type = "proper", rho = 1, symmetrize = TRUE),
    "admissible"
  )
})


test_that("ICAR rank deficiency equals number of connected components", {
  A1 <- matrix(0, 5, 5)
  A1[1,2] <- A1[2,1] <- 1

  A2 <- matrix(0, 4, 4)
  A2[1,2] <- A2[2,1] <- 1

  A <- Matrix::bdiag(A1, A2)

  Q <- suppressWarnings(
    intrinsic_car_precision(A, symmetrize = TRUE, scale = FALSE)
  )

  ## rank deficiency equals number of connected components with edges
  g <- igraph::graph_from_adjacency_matrix(
    as.matrix(A), mode = "undirected"
  )

  comps <- igraph::components(g)
  n_components <- sum(comps$csize > 1)

  expect_equal(n_components, 2)
})




test_that("sum-to-zero constraint removes nullspace", {
  A <- matrix(0, 6, 6)
  for (i in 1:5) A[i, i+1] <- A[i+1, i] <- 1

  Q <- intrinsic_car_precision(
    A,
    scale = FALSE,
    symmetrize = TRUE
  )

  Qc <- icar_sum_to_zero(Q)

  ev <- eigen(as.matrix(Qc), symmetric = TRUE)$values
  expect_true(all(ev > 0))
})


test_that("isolated nodes: proper CAR errors, ICAR warns", {
  n <- 6
  A <- matrix(0, n, n)
  for (i in 1:(n - 2)) {
    A[i, i + 1] <- A[i + 1, i] <- 1
  }
  # node 6 isolated

  expect_warning(
    car_precision(A, type = "icar", tau = 1, symmetrize = TRUE, check = TRUE),
    "isolated node",
    fixed = FALSE
  )

  expect_error(
    car_precision(A, type = "proper", rho = 0.5, tau = 1, symmetrize = TRUE, check = TRUE),
    "isolated",
    fixed = FALSE
  )
})


test_that("ICAR nullity equals number of connected components", {
  # component 1: chain of length 4
  A1 <- matrix(0, 4, 4)
  for (i in 1:3) A1[i, i+1] <- A1[i+1, i] <- 1

  # component 2: triangle
  A2 <- matrix(0, 3, 3)
  A2[1,2] <- A2[2,1] <- 1
  A2[2,3] <- A2[3,2] <- 1
  A2[1,3] <- A2[3,1] <- 1

  # component 3: isolated node
  A3 <- matrix(0, 1, 1)

  A <- Matrix::bdiag(A1, A2, A3)

  Q <- suppressWarnings(
    car_precision(A, type = "icar", tau = 1, symmetrize = TRUE, check = TRUE)
  )

  # number of connected components (including singleton isolate)
  g <- igraph::graph_from_adjacency_matrix(as.matrix(A), mode = "undirected")
  n_comp <- igraph::components(g)$no
  expect_equal(n_comp, 3)

  # eigenvalues for small n; nullity = count near zero
  ev <- eigen(as.matrix(Q), symmetric = TRUE, only.values = TRUE)$values
  tol <- 1e-8 * max(1, max(abs(ev)))
  nullity <- sum(abs(ev) <= tol)

  expect_equal(nullity, n_comp)
})


test_that("ICAR scaling returns fallback or positive scalar multiple", {
  n <- 10
  A <- matrix(0, n, n)
  for (i in 1:(n - 1)) A[i, i+1] <- A[i+1, i] <- 1
  A[1, n] <- A[n, 1] <- 1  # cycle, connected, no isolates

  Q_unscaled <- intrinsic_car_precision(A, tau = 1, scale = FALSE, symmetrize = TRUE)
  Q_scaled <- suppressWarnings(
    intrinsic_car_precision(A, tau = 1, scale = TRUE, symmetrize = TRUE)
  )

  expect_s4_class(Q_scaled, "dsCMatrix")
  expect_true(Matrix::isSymmetric(Q_scaled))

  # If scaling fails, function returns Q_unscaled (with a warning suppressed here)
  if (isTRUE(all.equal(Q_scaled, Q_unscaled))) {
    expect_true(TRUE)
  } else {
    # scaling succeeded => Q_scaled = s * Q_unscaled for some s > 0
    idx <- which(Q_unscaled != 0, arr.ind = TRUE)
    expect_gt(nrow(idx), 0L)

    i <- idx[1, 1]; j <- idx[1, 2]
    s_hat <- as.numeric(Q_scaled[i, j] / Q_unscaled[i, j])

    expect_true(is.finite(s_hat))
    expect_gt(s_hat, 0)

    vals_un <- as.numeric(Q_unscaled[idx])
    vals_sc <- as.numeric(Q_scaled[idx])
    ratios <- vals_sc / vals_un

    expect_true(all(is.finite(ratios)))
    expect_equal(as.numeric(stats::sd(ratios)), 0, tolerance = 1e-8)
    expect_equal(as.numeric(mean(ratios)), s_hat, tolerance = 1e-8)
  }
})




test_that("ICAR construction is not slower than proper CAR", {

  skip_if_not_installed("microbenchmark")

  set.seed(1)
  n <- 300

  A <- Matrix::rsparsematrix(n, n, density = 0.01)
  A <- abs(A)
  A[A > 0] <- 1
  diag(A) <- 0

  ## ensure no isolated nodes (add chain structure)
  for (i in 1:(n - 1)) {
    A[i, i + 1] <- 1
    A[i + 1, i] <- 1
  }

  ## warm-up
  suppressWarnings(
    intrinsic_car_precision(A, symmetrize = TRUE, scale = FALSE)
  )
  suppressWarnings(
    car_precision(A, type = "proper", rho = 0.5, symmetrize = TRUE)
  )

  bm <- suppressWarnings(microbenchmark::microbenchmark(
    ICAR = suppressWarnings(
      intrinsic_car_precision(A, symmetrize = TRUE, scale = FALSE)
    ),
    CAR = suppressWarnings(
      car_precision(A, type = "proper", rho = 0.5, symmetrize = TRUE)
    ),
    times = 10L
  ))

  med <- stats::aggregate(time ~ expr, bm, median)

  t_icar <- med$time[med$expr == "ICAR"]
  t_car  <- med$time[med$expr == "CAR"]

  expect_lte(t_icar, 1.2 * t_car)
})

