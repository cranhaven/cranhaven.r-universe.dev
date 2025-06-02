context("TreeBUGS basic tests")

test_that("All Bayesian methods work", {

  testthat::skip_on_cran()
  testthat::skip_on_travis()

  EQN_FILE <- system.file("extdata", "2HTSM_Submodel4.eqn", package = "MPTmultiverse")
  DATA_FILE <- system.file("extdata", "Kuhlmann_dl7.csv", package = "MPTmultiverse")

  # if .csv format uses semicolons ";" (German format):
  data <- read.csv2(DATA_FILE, fileEncoding = "UTF-8-BOM")  ## use correct encoding if necessary

  COL_ID <- "Subject"         # name of the variable encoding subject ID
  COL_CONDITION <- "ExpCond"  # name of the variable encoding group membership

  data[[COL_CONDITION]] <- factor(
    data[[COL_CONDITION]]
    , levels = c(1:2)
    , labels = c("no_load", "load")
  )
  capture_output(mpt_options("test"))
  mpt_options(extend_max = 0L, silent_jags = TRUE)
  set.seed(10L)

  res_bayes <- fit_mpt(
    method = c("trait", "trait_uncorrelated", "beta", "betacpp", "simple", "simple_pooling")
    , dataset = DATA_FILE
    , data = data
    , model = EQN_FILE
    , condition = COL_CONDITION
  )

  testthat::expect_equal(
    object = nrow(res_bayes)
    , expected = 6L
  )
})



test_that("Partial Pooling approaches work", {
  # testthat::skip("TreeBUGS test appear to fail")
  testthat::skip_on_cran()
  testthat::skip_on_travis()

  EQN_FILE <- system.file("extdata", "prospective_memory.eqn", package = "MPTmultiverse")
  DATA_FILE <- system.file("extdata", "smith_et_al_2011.csv", package = "MPTmultiverse")

  data <- read.csv(DATA_FILE, fileEncoding = "UTF-8-BOM")
  data <- data[c(1:5, 113:118),]
  COL_CONDITION <- "WM_EX"
  data[[COL_CONDITION]] <- factor(
    data[[COL_CONDITION]]
    , levels = 1:2
    , labels = c("low_WM", "high_WM")
  )
  op <- mpt_options()
  testthat::capture_output(mpt_options("default"))
  mpt_options(n.chains = 2L)  ## use 2 chains, hopefully it still runs on CRAN
  mpt_options(n.CPU = 1L)
  mpt_options(Neff_min = 100)
  mpt_options(n.iter = 50000)
  mpt_options(save_models = FALSE)

  set.seed(10L)  ## for reproducibility

  capture_output(
    res_bayes <- fit_mpt(
      method = "trait"
      , dataset = DATA_FILE
      , data = data
      , model = EQN_FILE
      , condition = COL_CONDITION
    )
  )
  expect_equal(nrow(res_bayes), 1)
  expect_equal(res_bayes$pooling, "partial")
  expect_equal(res_bayes$method, "trait")

  expect_equal(nrow(res_bayes$est_indiv[[1]]), 4 * 11)
  expect_equal(nrow(res_bayes$est_rho[[1]]), 3*2*2)
  expect_equal(nrow(res_bayes$fungibility[[1]]), 3*2*2)
  expect_equal(nrow(res_bayes$test_between[[1]]), 4)

  ## dput(round(res_bayes$est_group[[1]]$est, 2))
  expect_equal(res_bayes$est_group[[1]]$est,
               c(c(0.91, 0.84, 0.63, 0.83, 0.88, 0.94, 0.78, 0.78)),
               tolerance = 0.02)

  # dput(round(res_bayes$gof_group[[1]]$stat_obs, 2))
  expect_equal(res_bayes$gof_group[[1]]$stat_obs,
               c(2, 1.36, 195.16, 158.33),
               tolerance = 0.1)
})
