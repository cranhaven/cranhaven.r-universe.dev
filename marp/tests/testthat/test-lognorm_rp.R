test_that("lognorm_rp", {
  # (optional) load the small test dataset
  # data_file <- system.file("extdata", "small.txt", package = "marp", mustWork = TRUE)
  # data <- read.table(data_file)$V1

  # set some parameters
  # m <- 10 # number of iterations for MLE optimization
  t <- seq(100,200,by=10) # time intervals
  y <- 304 # cut-off year for estimating probablity

  # fix the random seed
  set.seed(42)

  # sample data for testing
  data <- rgamma(30, 3, 0.01)

  # fit renewal model
  res <- marp::lognorm_rp(data, t, y)

  # check result
  expect_equal(res$par1, 5.4882183788378658)
  expect_equal(res$par2, 0.65084541680686814)
  expect_equal(res$logL, -194.33021376270909)
  expect_equal(res$AIC, 392.66042752541819)
  expect_equal(res$BIC, 395.46282228874247)
  expect_equal(res$mu_hat, 298.87285747700128)
  expect_equal(res$pr_hat, 0.56416103510057725)

  haz_hat_expected <-
    c(
      -5.9235521978533958,
      -5.8023896004395645,
      -5.7047473880293342,
      -5.6252373537796752,
      -5.5599409055534252,
      -5.5059486025117375,
      -5.4610610586440487,
      -5.4235891601883868,
      -5.3922173604047572,
      -5.3659081375131672,
      -5.3438339586221275
    )
  expect_true(all.equal(res$haz_hat, haz_hat_expected))
})
