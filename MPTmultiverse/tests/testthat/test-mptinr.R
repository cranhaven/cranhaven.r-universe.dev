context("MPTinR basic tests")

test_that("No-pooling approaches work", {

  ##testthat::skip_on_cran()

  EQN_FILE <- system.file("extdata", "prospective_memory.eqn",
                          package = "MPTmultiverse")
  DATA_FILE <- system.file("extdata", "smith_et_al_2011.csv",
                           package = "MPTmultiverse")

  data <- read.csv(DATA_FILE, fileEncoding = "UTF-8-BOM")
  data <- data[c(1:5, 113:118),]
  COL_CONDITION <- "WM_EX"
  data[[COL_CONDITION]] <- factor(
    data[[COL_CONDITION]]
    , levels = 1:2
    , labels = c("low_WM", "high_WM")
  )
  op <- mpt_options()
  capture_output(mpt_options("test"))
  mpt_options("n.CPU" = 1)  ## use 1 CPU, so it can run on CRAN
  set.seed(10)  ## for reproducibility

  only_asymptotic <- fit_mpt(
    method = "asymptotic_no"
    , dataset = DATA_FILE
    , data = data
    , model = EQN_FILE
    , condition = COL_CONDITION
  )
  expect_equal(nrow(only_asymptotic), 1)
  expect_equal(only_asymptotic$pooling, "no")
  expect_equal(only_asymptotic$method, "asymptotic")

  ## dput(round(only_asymptotic$est_group[[1]]$est, 3))

  ## extract correct parameters:
  new <- only_asymptotic$est_group[[1]]
  ref <- tibble(condition = c("low_WM", "low_WM", "low_WM", "low_WM",
                              "high_WM", "high_WM", "high_WM", "high_WM"),
                parameter = c("C1", "C2", "M", "P", "C1", "C2", "M", "P"),
                core = c(FALSE, FALSE,FALSE, FALSE, FALSE, FALSE, FALSE,
                         FALSE),
                est = c(0.933, 0.903, 0.701, 0.887, 0.922, 0.972, 0.912,
                        0.725)
  )
  expect_equal(dplyr::arrange(new, condition, parameter)$est,
               dplyr::arrange(ref, condition, parameter)$est,
               tolerance = 0.001)

  # dput(round(only_asymptotic$gof_group[[1]]$stat_obs, 2))
  expect_equal(only_asymptotic$gof_group[[1]]$stat_obs,
               c(27.2, 31.75),
               tolerance = 0.01)

  new <- only_asymptotic$est_indiv[[1]]
  ref <- tibble(
    id = c("1", "1", "1", "1", "2", "2", "2", "2", "3", "3", "3",
                       "3", "4", "4", "4", "4", "5", "5", "5", "5", "6", "6",
                       "6", "6", "7", "7", "7", "7", "8", "8", "8", "8", "9",
                       "9", "9", "9", "10", "10", "10", "10", "11", "11", "11",
                       "11"),
    condition = c("low_WM", "low_WM", "low_WM", "low_WM", "low_WM", "low_WM",
                  "low_WM", "low_WM", "low_WM", "low_WM", "low_WM", "low_WM",
                  "low_WM", "low_WM", "low_WM", "low_WM", "low_WM", "low_WM",
                  "low_WM", "low_WM", "high_WM", "high_WM", "high_WM",
                  "high_WM", "high_WM", "high_WM", "high_WM", "high_WM",
                  "high_WM", "high_WM", "high_WM", "high_WM", "high_WM",
                  "high_WM", "high_WM", "high_WM", "high_WM", "high_WM",
                  "high_WM", "high_WM", "high_WM", "high_WM", "high_WM",
                  "high_WM"),
    parameter = c("C1", "C2", "M", "P", "C1", "C2", "M", "P", "C1", "C2", "M",
                  "P", "C1", "C2", "M", "P", "C1", "C2", "M", "P", "C1", "C2",
                  "M", "P", "C1", "C2", "M", "P", "C1", "C2", "M", "P", "C1",
                  "C2", "M", "P", "C1", "C2", "M", "P", "C1", "C2", "M", "P"),
    se = c(NA, NA, NA, NA, NA, NA, NA, NA, 0.02, 0.02, 0.05, 0.19, NA,
                 NA, NA, NA, 0.02, 0.02, 0.06, 0.17, NA, NA, NA, NA, NA, NA, NA,
                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                 NA))
  expect_equal(dplyr::arrange(new, id, condition, parameter)$se,
               dplyr::arrange(ref, id, condition, parameter)$se,
               tolerance = 0.01)

  ## required to skip remaining if not using latest RNG
  testthat::skip_if(getRversion() < "3.6.0")

  only_pb <- fit_mpt(
    method = "pb_no"
    , dataset = DATA_FILE
    , data = data
    , model = EQN_FILE
    , condition = COL_CONDITION
  )
  expect_equal(nrow(only_pb), 1)
  expect_equal(only_pb$pooling, "no")
  expect_equal(only_pb$method, "PB/MLE")

  ### NOTE: ordering of conditions differs between asymptotic and PB!

  new <- only_pb$est_group[[1]]
  ref <- tibble(
    condition = c("low_WM", "low_WM", "low_WM", "low_WM",
                  "high_WM", "high_WM", "high_WM", "high_WM"),
    parameter = c("C1", "C2", "M", "P", "C1", "C2", "M", "P"),
    est = c(0.933, 0.903, 0.701, 0.887, 0.922, 0.972, 0.912, 0.75)
  )

  expect_equal(dplyr::arrange(new, condition, parameter)$est,
               dplyr::arrange(ref, condition, parameter)$est,
               tolerance = 0.001)

  new_gof <- only_pb$gof_group[[1]]
  ref_gof <- tibble(
    condition = c("low_WM", "high_WM"),
    stat_obs = c(27.2, 31.75),
    p = c(0.09, 0.09)
  )

  expect_equal(dplyr::arrange(new_gof, condition)$stat_obs,
               dplyr::arrange(ref_gof, condition)$stat_obs,
               tolerance = 0.01)

  expect_equal(dplyr::arrange(new_gof, condition)$p,
               dplyr::arrange(ref_gof, condition)$p,
               tolerance = 0.01)

  new_indiv <- only_pb$est_indiv[[1]]
  ref_indiv <- tibble(
    id = c("1", "1", "1", "1", "2", "2", "2", "2",
           "3", "3", "3", "3", "4", "4", "4", "4", "5", "5", "5", "5", "6",
           "6", "6", "6", "7", "7", "7", "7", "8", "8", "8", "8", "9", "9",
           "9", "9", "10", "10", "10", "10", "11", "11", "11", "11"),
    condition =
      c("low_WM", "low_WM", "low_WM", "low_WM", "low_WM", "low_WM",
        "low_WM", "low_WM", "low_WM", "low_WM", "low_WM", "low_WM", "low_WM",
        "low_WM", "low_WM", "low_WM", "low_WM", "low_WM", "low_WM", "low_WM",
        "high_WM", "high_WM", "high_WM", "high_WM", "high_WM", "high_WM",
        "high_WM", "high_WM", "high_WM", "high_WM", "high_WM", "high_WM",
        "high_WM", "high_WM", "high_WM", "high_WM", "high_WM", "high_WM",
        "high_WM", "high_WM", "high_WM", "high_WM", "high_WM", "high_WM"),
    parameter =
      c("C1", "C2", "M", "P", "C1", "C2", "M", "P", "C1", "C2", "M", "P", "C1",
        "C2", "M", "P", "C1", "C2", "M", "P", "C1", "C2", "M", "P", "C1",
        "C2", "M", "P", "C1", "C2", "M", "P", "C1", "C2", "M", "P", "C1",
        "C2", "M", "P", "C1", "C2", "M", "P"),
    est = c(0.97, 1, 1, 0.81, 0.98, 0.88, 1, 0.69, 0.89, 0.88, 0.22, 1,
            0.91, 0.93, 1, 0.94, 0.92, 0.82, 0.28, 1, 0.79, 0.95, 1, 0.31,
            0.9, 0.97, 1, 0.81, 0.95, 1, 0.47, 1, 0.91, 0.95, 1, 0.87, 0.98,
            0.97, 1, 0.88, 1, 1, 1, 0.62),
    se =
      c(0.02, 0, 0, 0.09, 0.02, 0.04, 0, 0.1, 0.06, 0.03, 0.34, 0.28,
        0.04, 0.04, 0, 0.08, 0.03, 0.04, 0.32, 0.33, 0.06, 0.02, 0, 0.14,
        0.04, 0.02, 0, 0.09, 0.02, 0, 0.10, 0.07, 0.03, 0.02, 0, 0.05,
        0.01, 0.02, 0, 0.09, 0, 0, 0, 0.12)
    )

  expect_equal(dplyr::arrange(new_indiv, id, condition, parameter)$est,
               dplyr::arrange(ref_indiv, id, condition, parameter)$est,
               tolerance = 0.01)

  if (capabilities()[["long.double"]]) {
    expect_equal(dplyr::arrange(new_indiv, id, condition, parameter)$se,
                 dplyr::arrange(ref_indiv, id, condition, parameter)$se,
                 tolerance = 0.05)
  }

  only_npb <- fit_mpt(
    method = "npb_no"
    , dataset = DATA_FILE
    , data = data
    , model = EQN_FILE
    , condition = COL_CONDITION
  )
  expect_equal(nrow(only_npb), 1)
  expect_equal(only_npb$pooling, "no")
  expect_equal(only_npb$method, "NPB/MLE")


  new <- only_npb$est_group[[1]]
  ref <- tibble(
    condition = c("low_WM", "low_WM", "low_WM", "low_WM",
                  "high_WM", "high_WM", "high_WM", "high_WM"),
    parameter = c("C1", "C2", "M", "P", "C1", "C2", "M", "P"),
    est = c(0.933, 0.903, 0.701, 0.887, 0.922, 0.972, 0.912, 0.75)
  )

  expect_equal(dplyr::arrange(new, condition, parameter)$est,
               dplyr::arrange(ref, condition, parameter)$est,
               tolerance = 0.001)

  new_gof <- only_npb$gof_group[[1]]
  ref_gof <- tibble(
    condition = c("low_WM", "high_WM"),
    stat_obs = c(27.2, 31.75),
    p = c(0.64, 1) ## is now: c(0.64, 0.91)
  )

  expect_equal(dplyr::arrange(new_gof, condition)$stat_obs,
               dplyr::arrange(ref_gof, condition)$stat_obs,
               tolerance = 0.01)

  expect_equal(dplyr::arrange(new_gof, condition)$p,
               dplyr::arrange(ref_gof, condition)$p,
               tolerance = 0.1)

  new_indiv <- only_npb$est_indiv[[1]]
  ref_indiv <- tibble(
    id = c("1", "1", "1", "1", "2", "2", "2", "2",
           "3", "3", "3", "3", "4", "4", "4", "4", "5", "5", "5", "5", "6",
           "6", "6", "6", "7", "7", "7", "7", "8", "8", "8", "8", "9", "9",
           "9", "9", "10", "10", "10", "10", "11", "11", "11", "11"),
    condition =
      c("low_WM", "low_WM", "low_WM", "low_WM", "low_WM", "low_WM",
        "low_WM", "low_WM", "low_WM", "low_WM", "low_WM", "low_WM", "low_WM",
        "low_WM", "low_WM", "low_WM", "low_WM", "low_WM", "low_WM", "low_WM",
        "high_WM", "high_WM", "high_WM", "high_WM", "high_WM", "high_WM",
        "high_WM", "high_WM", "high_WM", "high_WM", "high_WM", "high_WM",
        "high_WM", "high_WM", "high_WM", "high_WM", "high_WM", "high_WM",
        "high_WM", "high_WM", "high_WM", "high_WM", "high_WM", "high_WM"),
    parameter =
      c("C1", "C2", "M", "P", "C1", "C2", "M", "P", "C1", "C2", "M", "P", "C1",
        "C2", "M", "P", "C1", "C2", "M", "P", "C1", "C2", "M", "P", "C1",
        "C2", "M", "P", "C1", "C2", "M", "P", "C1", "C2", "M", "P", "C1",
        "C2", "M", "P", "C1", "C2", "M", "P"),
    est = c(0.97, 1, 1, 0.81, 0.98, 0.88, 1, 0.69, 0.89, 0.88, 0.22, 1,
                 0.91, 0.93, 1, 0.94, 0.92, 0.82, 0.28, 1, 0.79, 0.95, 1, 0.31,
                 0.9, 0.97, 1, 0.81, 0.95, 1, 0.47, 1, 0.91, 0.95, 1, 0.87, 0.98,
                 0.97, 1, 0.88, 1, 1, 1, 0.62),
    se =
      c(0.02, 0, 0, 0.09, 0.01, 0.03, 0, 0.14, 0.04, 0.05, 0.06, 0,
                 0.02, 0.03, 0, 0.07, 0.03, 0.04, 0.12, 0.09, 0.05, 0.02, 0, 0.13,
                 0.04, 0.03, 0, 0.11, 0.04, 0, 0.18, 0.16, 0.05, 0.03, 0, 0.08,
                 0.02, 0.02, 0, 0.06, 0, 0, 0, 0.16)
    )

  expect_equal(dplyr::arrange(new_indiv, id, condition, parameter)$est,
               dplyr::arrange(ref_indiv, id, condition, parameter)$est,
               tolerance = 0.01)

  if (capabilities()[["long.double"]]) {
    expect_equal(dplyr::arrange(new_indiv, id, condition, parameter)$se,
                 dplyr::arrange(ref_indiv, id, condition, parameter)$se,
                 tolerance = 0.05)
  }
  mpt_options(op)
})


test_that("Complete-pooling approaches work", {

  ##testthat::skip_on_cran()

  EQN_FILE <- system.file("extdata", "prospective_memory.eqn",
                          package = "MPTmultiverse")
  DATA_FILE <- system.file("extdata", "smith_et_al_2011.csv",
                           package = "MPTmultiverse")

  data <- read.csv(DATA_FILE, fileEncoding = "UTF-8-BOM")
  data <- data[c(1:5, 113:118),]
  COL_CONDITION <- "WM_EX"
  data[[COL_CONDITION]] <- factor(
    data[[COL_CONDITION]]
    , levels = 1:2
    , labels = c("low_WM", "high_WM")
  )
  op <- mpt_options()
  capture_output(mpt_options("test"))
  mpt_options("n.CPU" = 1)  ## use 1 CPU, so it can run on CRAN
  set.seed(10)  ## for reproducibility

  only_asymptotic <- fit_mpt(
    method = "asymptotic_complete"
    , dataset = DATA_FILE
    , data = data
    , model = EQN_FILE
    , condition = COL_CONDITION
  )
  expect_equal(nrow(only_asymptotic), 1)
  expect_equal(only_asymptotic$pooling, "complete")
  expect_equal(only_asymptotic$method, "asymptotic")

  ## dput(round(only_asymptotic$est_group[[1]]$est, 3))
  expect_equal(only_asymptotic$est_group[[1]]$est,
               c(0.933, 0.902, 0.587, 1, 0.922, 0.972, 0.822, 0.809),
               tolerance = 0.001)

  # dput(round(only_asymptotic$gof_group[[1]]$stat_obs, 2))
  expect_equal(only_asymptotic$gof_group[[1]]$stat_obs,
               c(6.86, 4.93),
               tolerance = 0.01)

  expect_equal(only_asymptotic$est_indiv[[1]], tibble::tibble())

  expect_equal(only_asymptotic$gof[[1]]$p,
               0.117,
               tolerance = 0.001)
  mpt_options(op)

  # test_within
  est_group <- only_asymptotic$est_group[[1]]
  test_within <- only_asymptotic$test_within[[1]]

  pairs <- combn(x = unique(est_group$parameter), m = 2)

  for (j in seq_len(ncol(pairs))) {
    est_diff <- est_group$est[est_group$parameter == pairs[1, j]] - est_group$est[est_group$parameter == pairs[2, j]]
    expect_equal(object = est_diff, expected = test_within$est[test_within$parameter1 == pairs[1, j] & test_within$parameter2 == pairs[2, j]])
  }



})
