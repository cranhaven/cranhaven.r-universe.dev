library(testthat)

context("Function: reconstruct")

test_that("reconstruct functionallity tests (kmedoid)", {
  dat <- matrix(
    data = c(
      1, 2, 3, 4,
      2.1, 3.9, 6.2, 7.5,
      0, 1, 3, 2
    ),
    nrow = 3, ncol = 4,
    byrow = TRUE,
    dimnames = list(c(1:3), c(1:4))
  )

  sim <- MultIS::get_similarity_matrix(
    readouts = dat,
    self = 1,
    upper = TRUE,
    method = "rsquared",
    parallel = FALSE
  )

  rec <- MultIS::reconstruct(
    readouts = dat,
    target_communities = 2,
    method = "kmedoids",
    sim = sim
  )

  expect_equal(nrow(rec), nrow(sim))
  expect_equal(ncol(rec), 2)
  expect_equal(mclust::adjustedRandIndex(rec[, "Clone"], c("1", "1", "2")), 1)
})

test_that("reconstruct functionallity tests (kmeans)", {
  dat <- matrix(
    data = c(
      1, 2, 3, 4,
      2.1, 3.9, 6.2, 7.5,
      0, 1, 3, 2
    ),
    nrow = 3, ncol = 4,
    byrow = TRUE,
    dimnames = list(c(1:3), c(1:4))
  )

  sim <- MultIS::get_similarity_matrix(
    readouts = dat,
    self = 1,
    upper = TRUE,
    method = "rsquared",
    parallel = FALSE
  )

  rec <- MultIS::reconstruct(
    readouts = dat,
    target_communities = 2,
    method = "kmeans",
    sim = sim
  )

  expect_equal(nrow(rec), nrow(sim))
  expect_equal(ncol(rec), 2)
  expect_equal(mclust::adjustedRandIndex(rec[, "Clone"], c("1", "2", "1")), 1)
})

test_that("reconstruct functionallity tests (ward.D2)", {
  dat <- matrix(
    data = c(
      1, 2, 3, 4,
      2.1, 3.9, 6.2, 7.5,
      0, 1, 3, 2
    ),
    nrow = 3, ncol = 4,
    byrow = TRUE,
    dimnames = list(c(1:3), c(1:4))
  )

  sim <- MultIS::get_similarity_matrix(
    readouts = dat,
    self = 1,
    upper = TRUE,
    method = "rsquared",
    parallel = FALSE
  )

  rec <- MultIS::reconstruct(
    readouts = dat,
    target_communities = 2,
    method = "ward.D2",
    sim = sim
  )

  expect_equal(nrow(rec), nrow(sim))
  expect_equal(ncol(rec), 2)
  expect_equal(mclust::adjustedRandIndex(rec[, "Clone"], c("1", "1", "2")), 1)
})

test_that("reconstruct functionallity tests (clusterObj)", {
  dat <- matrix(
    data = c(
      1, 2, 3, 4,
      2.1, 3.9, 6.2, 7.5,
      0, 1, 3, 2
    ),
    nrow = 3, ncol = 4,
    byrow = TRUE,
    dimnames = list(c(1:3), c(1:4))
  )

  sim <- MultIS::get_similarity_matrix(
    readouts = dat,
    self = 1,
    upper = TRUE,
    method = "rsquared",
    parallel = FALSE
  )

  rec <- MultIS::reconstruct(
    readouts = dat,
    target_communities = 2,
    method = "kmedoids",
    sim = sim,
    cluster_obj = TRUE
  )

  expect_true(inherits(rec, "clusterObj"))

  rec <- rec$mapping

  expect_equal(nrow(rec), nrow(sim))
  expect_equal(ncol(rec), 2)
  expect_equal(mclust::adjustedRandIndex(rec[, "Clone"], c("1", "1", "2")), 1)
})
