test_that("find_representatives returns R_ij and R_ji with centroid linkage", {
  set.seed(123)
  X <- matrix(rnorm(12), ncol = 2)
  rownames(X) <- paste0("pt", 1:6)
  Y <- factor(c("A", "A", "A", "B", "B", "B"))

  result <- find_representatives("A", "B", X, Y, linkage = "CT")
  expect_named(result, c("R_ij", "R_ji"))
  expect_length(result$R_ij, 2)
  expect_length(result$R_ji, 2)
})

test_that("find_representatives works with single linkage", {
  set.seed(123)
  X <- matrix(rnorm(12), ncol = 2)
  rownames(X) <- paste0("pt", 1:6)
  Y <- factor(c("A", "A", "A", "B", "B", "B"))

  result <- find_representatives("A", "B", X, Y, linkage = "SG")
  expect_named(result, c("R_ij", "R_ji"))
})

test_that("find_representatives warns on same-category input", {
  X <- matrix(rnorm(8), ncol = 2)
  rownames(X) <- paste0("pt", 1:4)
  Y <- factor(c("A", "A", "B", "B"))

  expect_warning(
    find_representatives("A", "A", X, Y, linkage = "CT"),
    "same"
  )
})

test_that("calculate_supervised_distance_matrix returns symmetric matrix", {
  X <- matrix(c(2, 0, 5, 1, 0, 4, 1, 5), byrow = TRUE, ncol = 2)
  rownames(X) <- paste0("pt", 1:4)
  Y <- factor(c("A", "A", "B", "B"))

  result <- calculate_supervised_distance_matrix(X, Y, linkage = "CT")
  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 4)
  expect_equal(result, t(result))
  expect_true(all(diag(result) == 0))
})

test_that("calculate_supervised_distance_matrix same-class uses euclidean", {
  X <- matrix(c(0, 0, 3, 4), byrow = TRUE, ncol = 2)
  rownames(X) <- c("p1", "p2")
  Y <- factor(c("A", "A"))

  result <- calculate_supervised_distance_matrix(X, Y, linkage = "CT")
  expect_equal(result[1, 2], 5)
  expect_equal(result[2, 1], 5)
})

test_that("calculate_RGAR returns 0 for small matrices", {
  D <- matrix(0, 2, 2)
  expect_equal(calculate_RGAR(D, w = 2), 0)
})

test_that("calculate_RGAR returns value in [0, 1]", {
  set.seed(42)
  D <- as.matrix(dist(matrix(rnorm(50), ncol = 2)))
  result <- calculate_RGAR(D, w = 3)
  expect_true(result >= 0 && result <= 1)
})
