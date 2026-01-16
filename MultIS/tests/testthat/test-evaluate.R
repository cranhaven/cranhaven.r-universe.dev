library(testthat)

context("Function: find_best_nr_cluster")

test_that("find_best_nr_cluster functionallity tests (normal)", {
  dat <- matrix(data = c(1, 2, 3, 4,
                         2.1, 3.9, 6.2, 7.5,
                         0, 1, 3, 2,
                         2, 2, 11, 32,
                         2, 3, 41, 3,
                         2, 0, 1, 0
  ),
  nrow = 6, ncol = 4,
  byrow = TRUE,
  dimnames = list(c(1:6), c(1:4)))

  sim <- MultIS::get_similarity_matrix(readouts = dat,
                                     self = 1,
                                     upper = TRUE,
                                     method = "rsquared",
                                     parallel = FALSE)

  bnc <- MultIS::find_best_nr_cluster(data = dat, sim = sim)
  expect_equal(bnc, 2)
})

test_that("find_best_nr_cluster functionallity tests (report)", {
  dat <- matrix(data = c(1, 2, 3, 4,
                         2.1, 3.9, 6.2, 7.5,
                         0, 1, 3, 2,
                         2, 2, 11, 32,
                         2, 3, 41, 3,
                         2, 0, 1, 0
  ),
  nrow = 6, ncol = 4,
  byrow = TRUE,
  dimnames = list(c(1:6), c(1:4)))

  sim <- MultIS::get_similarity_matrix(readouts = dat,
                                     self = 1,
                                     upper = TRUE,
                                     method = "rsquared",
                                     parallel = FALSE)

  expect_output(MultIS::find_best_nr_cluster(data = dat,
                                             sim = sim,
                                             report = TRUE))
})

test_that("find_best_nr_cluster functionallity tests (return_all)", {
  dat <- matrix(data = c(1, 2, 3, 4,
                         2.1, 3.9, 6.2, 7.5,
                         0, 1, 3, 2,
                         2, 2, 11, 32,
                         2, 3, 41, 3,
                         2, 0, 1, 0
  ),
  nrow = 6, ncol = 4,
  byrow = TRUE,
  dimnames = list(c(1:6), c(1:4)))

  sim <- MultIS::get_similarity_matrix(readouts = dat,
                                     self = 1,
                                     upper = TRUE,
                                     method = "rsquared",
                                     parallel = FALSE)

  ev <- MultIS::find_best_nr_cluster(data = dat, sim = sim, return_all = TRUE)

  expect_equal(length(ev), 4)
})

context("Function: evaluateClustering")

test_that("evaluateClustering functionallity tests (silhouette)", {
  dat <- matrix(data = c(1, 2, 3, 4,
                         2.1, 3.9, 6.2, 7.5,
                         0, 1, 3, 2,
                         2, 2, 11, 32,
                         2, 3, 41, 3,
                         2, 0, 1, 0
  ),
  nrow = 6, ncol = 4,
  byrow = TRUE,
  dimnames = list(c(1:6), c(1:4)))

  sim <- MultIS::get_similarity_matrix(readouts = dat,
                                     self = 1,
                                     upper = TRUE,
                                     method = "rsquared",
                                     parallel = FALSE)

  rec <- reconstruct(readouts = dat, target_communities = 3, sim = sim)

  ev <- evaluate_clustering(readouts = dat,
                            clustering = rec,
                            sim = sim,
                            method = "silhouette")
  expect_equal(ev, 0.41281389796458822783)
})

test_that("evaluateClustering functionallity tests (sdindex)", {
  dat <- matrix(data = c(1, 2, 3, 4,
                         2.1, 3.9, 6.2, 7.5,
                         0, 1, 3, 2,
                         2, 2, 11, 32,
                         2, 3, 41, 3,
                         2, 0, 1, 0
  ),
  nrow = 6, ncol = 4,
  byrow = TRUE,
  dimnames = list(c(1:6), c(1:4)))

  sim <- MultIS::get_similarity_matrix(readouts = dat,
                                     self = 1,
                                     upper = TRUE,
                                     method = "rsquared",
                                     parallel = FALSE)

  rec <- reconstruct(readouts = dat, target_communities = 3, sim = sim)

  ev <- evaluate_clustering(readouts = dat,
                            clustering = rec,
                            sim = sim,
                            method = "sdindex")
  expect_equal(ev, 0.38461041245274385503)
})

test_that("evaluateClustering functionallity tests (ptbiserial)", {
  dat <- matrix(data = c(1, 2, 3, 4,
                         2.1, 3.9, 6.2, 7.5,
                         0, 1, 3, 2,
                         2, 2, 11, 32,
                         2, 3, 41, 3,
                         2, 0, 1, 0
  ),
  nrow = 6, ncol = 4,
  byrow = TRUE,
  dimnames = list(c(1:6), c(1:4)))

  sim <- MultIS::get_similarity_matrix(readouts = dat,
                                     self = 1,
                                     upper = TRUE,
                                     method = "rsquared",
                                     parallel = FALSE)

  rec <- reconstruct(readouts = dat, target_communities = 3, sim = sim)

  ev <- evaluate_clustering(readouts = dat,
                            clustering = rec,
                            sim = sim,
                            method = "ptbiserial")
  expect_equal(ev, 0.83990304016878913895)
})

test_that("evaluateClustering functionallity tests (failure cases)", {
  expect_error(MultIS::evaluate_clustering(
    readouts = NULL,
    clustering = NULL,
    sim = NULL,
    method = "foobar"))
})
