test_that("loglogis_rp", {
  # (optional) load the small test dataset
  # data_file <- system.file("extdata", "small.txt", package = "marp", mustWork = TRUE)
  # data <- read.table(data_file)$V1

  # set some parameters
  m <- 10 # number of iterations for MLE optimization
  t <- seq(100,200,by=10) # time intervals
  y <- 304 # cut-off year for estimating probablity

  # fix the random seed
  set.seed(42)

  # sample data for testing
  data <- rgamma(30, 3, 0.01)

  # fit renewal model
  suppressWarnings(  # suppressing warnings from stats::nlm: NA/Inf replaced by maximum positive value
    res <- marp::loglogis_rp(data, t, m, y)
  )

  # check result
  expect_equal(res$par1, 2.6037079185931518)
  expect_equal(res$par2, 247.59811806509711)
  expect_equal(res$logL, -195.12976531752238)
  expect_equal(res$AIC, 394.25953063504477)
  expect_equal(res$BIC, 397.06192539836906)
  expect_equal(res$mu_hat, 319.72010329420084)
  expect_equal(res$pr_hat, 0.53433492133739346)

  haz_hat_expected <-
    c(
      -6.0990224350990703,
      -5.9701692150581938,
      -5.8576922967014680,
      -5.7593904228651995,
      -5.6734988535545332,
      -5.5985634116876613,
      -5.5333575575736793,
      -5.4768266965366923,
      -5.4280501449345273,
      -5.3862147352308334,
      -5.3505961677509442
    )
  expect_true(all.equal(res$haz_hat, haz_hat_expected))
})
