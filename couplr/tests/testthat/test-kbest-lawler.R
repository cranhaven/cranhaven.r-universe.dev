# Helper: compare costs against Murty if available
has_murty <- exists("lap_kbest_murty") || exists("kbest_murty")

get_murty <- function(M, k, method_base, maximize) {
  result <- NULL
  if (exists("lap_kbest_murty")) {
    result <- lap_kbest_murty(M, k, maximize = maximize)
  } else if (exists("kbest_murty")) {
    result <- kbest_murty(M, k = k, method_base = method_base, maximize = maximize)
  }
  
  # Check if result is valid
  if (is.null(result) || length(result) == 0) {
    return(NULL)
  }
  
  return(result)
}

# Basic properties on small squares
test_that("Lawler returns nondecreasing costs and valid matchings", {
  set.seed(1)
  for (n in 3:7) {
    M <- matrix(runif(n*n), n)
    k <- min(12, factorial(n))
    L <- lap_kbest_lawler(M, k, method_base = "jv", maximize = FALSE)

    expect_true(length(L) >= 1)
    cs <- vapply(L, `[[`, numeric(1), "total_cost")
    expect_true(all(diff(cs) >= -1e-12))

    for (s in L) {
      p <- as.integer(s$match)
      expect_equal(length(p), n)
      expect_true(all(p >= 1 & p <= n))
      expect_equal(length(unique(p)), length(p))  # permutation
    }

    # No duplicates
    keys <- vapply(L, function(x) paste(x$match, collapse=","), character(1))
    expect_equal(length(unique(keys)), length(keys))
  }
})

# Rectangles and NA handling
test_that("Lawler handles rectangles and NA", {
  set.seed(3)
  n <- 5; m <- 8
  M <- matrix(runif(n*m), n, m)
  # add some NA but keep feasibility
  idx <- sample.int(length(M), 6)
  M[idx] <- NA_real_
  for (i in 1:n) if (all(!is.finite(M[i, ]))) M[i, sample.int(m, 1)] <- runif(1)

  k <- 6
  L <- lap_kbest_lawler(M, k, method_base = "sap", maximize = FALSE)
  expect_true(length(L) >= 1)
  cs <- vapply(L, `[[`, numeric(1), "total_cost")
  expect_true(all(diff(cs) >= -1e-12))
})

# Maximize symmetry check
test_that("Lawler maximize corresponds to negated minimize", {
  set.seed(4)
  n <- 6
  M <- matrix(runif(n*n), n)
  k <- 12
  La <- lap_kbest_lawler(M,  k, method_base = "jv", maximize = TRUE)
  Lb <- lap_kbest_lawler(-M, k, method_base = "jv", maximize = FALSE)

  ca <- vapply(La, `[[`, numeric(1), "total_cost")
  cb <- vapply(Lb, `[[`, numeric(1), "total_cost")
  expect_equal(sort(ca), sort(-cb), tolerance = 1e-10)
})

# Auto-transpose (m < n) support
test_that("Lawler works when m < n (auto-transpose)", {
  set.seed(5)
  n <- 9; m <- 5
  M <- matrix(runif(n*m), n, m)
  # ensure each row has at least one finite
  for (i in 1:n) if (all(!is.finite(M[i, ]))) M[i, sample.int(m, 1)] <- runif(1)

  k <- 7
  L <- lap_kbest_lawler(M, k, method_base = "csflow", maximize = FALSE)
  expect_true(length(L) >= 1)
  cs <- vapply(L, `[[`, numeric(1), "total_cost")
  expect_true(all(diff(cs) >= -1e-12))
})

# k larger than number of unique assignments
test_that("Lawler returns <= k solutions and at least one", {
  set.seed(6)
  n <- 4
  M <- matrix(runif(n*n), n)
  k <- 50  # far above n!
  L <- lap_kbest_lawler(M, k, method_base = "hungarian", maximize = FALSE)
  expect_true(length(L) >= 1)
  expect_true(length(L) <= k)
})

# Large sparse rectangular quick check (skip on CRAN)
test_that("Lawler enumerates on sparse rectangular with csflow", {
  skip_on_cran()
  set.seed(7)
  n <- 40; m <- 120
  M <- matrix(Inf, n, m)
  nz <- ceiling(0.04 * length(M))
  id <- sample.int(length(M), nz)
  M[id] <- runif(nz)
  for (i in 1:n) if (all(!is.finite(M[i, ]))) M[i, sample.int(m, 1)] <- runif(1)

  k <- 10
  L <- lap_kbest_lawler(M, k, method_base = "csflow", maximize = FALSE)
  expect_true(length(L) >= 1)
  cs <- vapply(L, `[[`, numeric(1), "total_cost")
  expect_true(all(diff(cs) >= -1e-12))
})

# Consistency across base methods for first solution
test_that("First Lawler solution equals base method optimum", {
  set.seed(8)
  for (n in 3:7) {
    M <- matrix(runif(n*n), n)
    L <- lap_kbest_lawler(M, 1, method_base = "jv", maximize = FALSE)
    jb <- lap_solve_jv(M, FALSE)
    expect_equal(L[[1]]$total_cost, as.numeric(jb$total_cost), tolerance = 1e-10)
  }
})
