context("Reduce Standardize")
library(clusterability)

test_that("dist_corr", {
  # Setup
  constdata <- matrix(c(1, 8, 2, 1, 5, 3, 1, 7, 9), ncol = 3)

  # Tests
  expect_error(dist_corr(constdata), info = "Constant data should generate an error.")
})

test_that("dist_sqcorr", {
  # Setup
  constdata <- matrix(c(1, 8, 2, 1, 5, 3, 1, 7, 9), ncol = 3)

  # Tests
  expect_error(dist_sqcorr(constdata), info = "Constant data should generate an error")
})

test_that("standardizedata", {
  # Setup
  constcol <- matrix(c(1, 1, 1, 4, 6, 10, 9, 5, 3), nrow = 3)

  set.seed(1234)
  data2 <- cbind(rnorm(100, 7, 2), rnorm(100, 4, 3))

  noneresult <- standardizedata(data2, "NONE")
  stdresult <- standardizedata(data2, "STD")
  meanresult <- standardizedata(data2, "MEAN")
  medianresult <- standardizedata(data2, "MEDIAN")

  # Tests - Error handling
  expect_warning(standardizedata(constcol, "STD"), info = "Warn when constant variable causes NaN's")

  # Tests - is the output actually correct?
  expect_equal(colMeans(noneresult), colMeans(data2), tolerance = 1e-14)
  expect_equal(colMeans(stdresult), c(0, 0), tolerance = 1e-14)
  expect_equal(colMeans(meanresult), c(0, 0), tolerance = 1e-14)
  expect_equal(apply(medianresult, 2, median), c(0, 0), tolerance = 1e-14)

  expect_equal(apply(noneresult, 2, sd), apply(data2, 2, sd), tolerance = 1e-14)
  expect_equal(apply(stdresult, 2, sd), c(1, 1), tolerance = 1e-14)
  expect_equal(apply(meanresult, 2, sd), apply(data2, 2, sd), tolerance = 1e-14)
  expect_equal(apply(medianresult, 2, sd), apply(data2, 2, sd), tolerance = 1e-14)
})

test_that("completecases", {
  # Setup
  test1 <- matrix(c(1, NA, 8, 2, 4, 0, 9, 7, 7), nrow = 3)
  test2 <- matrix(c(1, 4, 8, 2, 4, 0, 9, 7, 7), nrow = 3)

  # Test - does it work?
  expect_equal(nrow(getcompletecases(test1)), 2, info = "getcompletecases removes NA rows")
  expect_equal(nrow(getcompletecases(test2)), 3, info = "getcompletecases doesn't remove complete rows")
})

test_that("computedistances", {
  # Setup
  test2 <- matrix(c(1, 4, 8, 2, 4, 0, 9, 7, 7), nrow = 3)

  true_mink <- as.vector(dist(test2, method = "minkowski", p = 2))
  true_euclid <- as.vector(dist(test2, method = "euclidean"))
  true_corr <- dist_corr(test2)

  # Test - does it work? Testing each of the three paths
  expect_equal(computedistances(test2, "minkowski(2)"), true_mink, tolerance = 1e-14)
  expect_equal(computedistances(test2, "euclidean"), true_euclid, tolerance = 1e-14)
  expect_equal(computedistances(test2, "corr"), true_corr, tolerance = 1e-14)
})

test_that("performpca", {
  # Setup
  test2 <- matrix(c(1, 4, 8, 2, 4, 0, 9, 7, 7), nrow = 3)

  pca_cs <- performpca(test2, TRUE, TRUE)
  pca_c <- performpca(test2, TRUE, FALSE)
  pca_s <- performpca(test2, FALSE, TRUE)
  pca_n <- performpca(test2, FALSE, FALSE)

  adjustsign <-  function(x) if(x$rotation[1, 1] < 0) {
    return(-1 * x$x[, 1])
  } else {
    return(x$x[, 1])
  }

  tpca_cs <- adjustsign(prcomp(test2, center = TRUE, scale. = TRUE, retx = TRUE))
  tpca_c <- adjustsign(prcomp(test2, center = TRUE, scale. = FALSE, retx = TRUE))
  tpca_s <- adjustsign(prcomp(test2, center = FALSE, scale. = TRUE, retx = TRUE))
  tpca_n <- adjustsign(prcomp(test2, center = FALSE, scale. = FALSE, retx = TRUE))

  # Test - comparing results to "known truth"
  expect_equal(pca_cs, tpca_cs, tolerance = 1e-14)
  expect_equal(pca_c, tpca_c, tolerance = 1e-14)
  expect_equal(pca_s, tpca_s, tolerance = 1e-14)
  expect_equal(pca_n, tpca_n, tolerance = 1e-14)
})

test_that("get_lower_triangle", {
  # Includes unit and integration tests

  # Integration
  # Setup
  set.seed(123)
  x <- rnorm(100, 50, 2)
  xdist <- as.matrix(dist(x, "euclidean"))

  # Run the test
  res1 <- clusterabilitytest(xdist, "dip", "NONE", is_dist_matrix = TRUE)
  res2 <- clusterabilitytest(x, "dip", "distance", distance_standardize = "none")

  # Compare results
  expect_equal(res1$pvalue, res2$pvalue, tol = 1e-14)
  expect_equal(res1$dipstatistic, res2$dipstatistic, tol = 1e-14)


  # Unit. Verify get_lower_triangle returns valid result.
  expect_setequal(get_lower_triangle(xdist), xdist[lower.tri(xdist)])
  expect_setequal(get_lower_triangle(xdist), computedistances(x, "euclidean"))
})
