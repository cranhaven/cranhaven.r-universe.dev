test_that("gamma_rp", {
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
    res <- marp::gamma_rp(data, t, m, y)

    # check result
    expect_equal(res$par1, 2.7626793657057762)
    expect_equal(res$par2, 0.0094307059277139432)
    expect_equal(res$logL, -193.76650950646049)
    expect_equal(res$AIC, 391.53301901292099)
    expect_equal(res$BIC, 394.33541377624528)
    expect_equal(res$mu_hat, 292.94512912200048)
    expect_equal(res$pr_hat, 0.42154974433034809, tolerance = 1e-6)

    haz_hat_expected <-
      c(
        -6.0942031084732298,
        -5.9967873794574516,
        -5.9117418563554684,
        -5.8368230853439300,
        -5.7703089176306639,
        -5.7108525626839901,
        -5.6573839062669986,
        -5.6090408956082456,
        -5.5651206740587922,
        -5.5250440506799734,
        -5.4883291920475745
      )
    expect_true(all.equal(res$haz_hat, haz_hat_expected))
})
