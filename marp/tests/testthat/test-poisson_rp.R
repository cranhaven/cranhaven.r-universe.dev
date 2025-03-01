test_that("poisson_rp", {
    # set some parameters
    m <- 10 # number of iterations for MLE optimization
    t <- seq(100,200,by=10) # time intervals
    y <- 304 # cut-off year for estimating probablity

    # fix the random seed
    set.seed(42)

    # sample data for testing
    data <- rgamma(30, 3, 0.01)

    # fit renewal model
    res <- marp::poisson_rp(data, t, y)

    # check result
    expect_equal(res$par1, 0.0034136086430979953)
    expect_equal(res$par2, NA)
    expect_equal(res$logL, -200.39955882401648)
    expect_equal(res$AIC, 402.79911764803296)
    expect_equal(res$BIC, 404.2003150296951)
    expect_equal(res$mu_hat, 292.94512187913182)
    expect_equal(res$pr_hat, 0.60038574701819891)
    expect_true(all.equal(res$haz_hat, rep(c(-5.6799852941338829), times=11)))
})
