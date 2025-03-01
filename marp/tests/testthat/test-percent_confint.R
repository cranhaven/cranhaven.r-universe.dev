test_that("percent_confint", {
  # skipping long tests
  skip_on_cran()
  skip_on_ci()

  # (optional) load the small test dataset
  # data_file <- system.file("extdata", "small.txt", package = "marp", mustWork = TRUE)
  # data <- read.table(data_file)$V1

  # set some parameters
  B <- 100 # number of non-parametric bootstraps
  m <- 10 # number of iterations for MLE optimization
  t <- seq(100,200,by=10) # time intervals
  y <- 304 # cut-off year for estimating probablity
  which.model <- 2 # specify the generating model

  # fix the random seed
  set.seed(42)

  # sample data for testing
  data <- rgamma(30, 3, 0.01)

  # construct percentile bootstrap confidence invtervals
  suppressWarnings(  # suppressing warnings from stats::nlm: NA/Inf replaced by maximum positive value
    res <- marp::percent_confint(data, B, t, m, y, which.model)
  )

  # check result
  expect_equal(res$weights_bstp, c(0.00000000000000000, 0.20999999999999999, 0.02000000000000000, 0.55000000000000004, 0.00000000000000000, 0.22000000000000000), tolerance = 1e-6)
  expect_equal(res$mu_gen, 292.08206818821196, tolerance = 1e-6)
  expect_equal(res$mu_gen_lower, 235.56640051713609, tolerance = 1e-6)
  expect_equal(res$mu_gen_upper, 351.96986933767391, tolerance = 1e-6)
  expect_equal(res$mu_best, 292.35847316038348, tolerance = 1e-6)
  expect_equal(res$mu_best_lower, 235.88161691518911, tolerance = 1e-6)
  expect_equal(res$mu_best_upper, 350.55380743519584, tolerance = 1e-6)
  expect_equal(res$pr_gen, 0.42696783822556628, tolerance = 1e-6)
  expect_equal(res$pr_gen_lower, -0.099829288380939865, tolerance = 1e-6)
  expect_equal(res$pr_gen_upper, 1.0176049810630703, tolerance = 1e-6)
  expect_equal(res$pr_best, 0.37491759487174814, tolerance = 1e-6)
  expect_equal(res$pr_best_lower, -0.26621297252914633, tolerance = 1e-6)
  expect_equal(res$pr_best_upper, 1.0652011393716518, tolerance = 1e-6)
  expect_true(all.equal(res$haz_gen, c(-6.1574170954748446, -6.0407761711947678, -5.9513434419645019, -5.8726252515108950, -5.7986258130703252, -5.7247031109364173, -5.6593175122781005, -5.6058477726400291, -5.5664407327496370, -5.5269464721905024, -5.4947702265066649), tolerance = 1e-6))
  expect_true(all.equal(res$haz_gen_lower, c(-6.8652855742696639, -6.6841943887157651, -6.5233913223844731, -6.3834006443162910, -6.2532430920540900, -6.1500358967529181, -6.0577002538173348, -5.9866519978125403, -5.9252774220067499, -5.8692401967114405, -5.8178826157855337), tolerance = 1e-6))
  expect_true(all.equal(res$haz_gen_upper, c(-5.6448720091211388, -5.5577520756873628, -5.4822090862122135, -5.4235704661344162, -5.3726609438443393, -5.3302729091434609, -5.2922260794579961, -5.2578820332505032, -5.2244418469567844, -5.1797804892141119, -5.1391644376197494), tolerance = 1e-6))
  })
