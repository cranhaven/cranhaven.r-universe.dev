
test_that("Check that the outputs are correct for run_mcmc", {
  suppressMessages({
    ss <- set_n(
      ssC = 80,
      ssE = 90,
      ssExt = 150
    )

    covset1 <- set_cov(
      n_cat = 2,
      n_cont = 1,
      mu_int = c(0, 0.5, 0.5),
      mu_ext = c(0.7, 0.5, 0.9),
      var = c(1, 1.2, 1),
      cov = c(0.5, 0.7, 0.9),
      prob_int = c(0.45, 0.55),
      prob_ext = c(0.65, 0.55)
    )

    sample_cov <- simu_cov(
      ssObj = ss,
      covObj = covset1,
      HR = c(0.67),
      driftHR = c(1),
      nsim = 4,
      seed = 47
    )

    evt <- set_event(
      event = "weibull",
      shape = 0.9,
      lambdaC = 0.0135,
      beta = 0.5
    )

    c_int <- set_clin(
      gamma = c(2, 3, 16),
      e_itv = c(5, 10),
      CCOD = "fixed-first",
      CCOD_t = 45,
      etaC = c(0.02, 0.03),
      etaE = c(0.2, 0.3),
      d_itv = 2
    )

    c_ext <- set_clin(
      gamma = 10,
      CCOD = "event",
      CCOD_t = 150,
      etaC = 0.05
    )

    sample_time <- simu_time(
      dt = sample_cov,
      eventObj = evt,
      clinInt = c_int,
      clinExt = c_ext,
      seed = 47
    )

    res <- run_mcmc(
      dt = sample_time,
      set_prior(pred = "all", prior = "gamma", r0 = 1,  alpha = c(0, 0)),
      n.chains = 2,
      n.adapt = 100,
      n.burn = 100,
      n.iter = 200,
      seed = 47
    )

    res2 <- run_mcmc_p(
      dt = sample_time,
      set_prior(pred = "all", prior = "gamma", r0 = 1,  alpha = c(0, 0)),
      n.chains = 2,
      n.adapt = 100,
      n.burn = 100,
      n.iter = 200,
      seed = 47,
      n.cores = 2
    )

  })

    # Data frame with appropriate dimensions
    expect_equal(class(res),'data.frame')
    expect_equal(class(res2),'data.frame')
    expect_equal(dim(res)[1],4)
    expect_equal(dim(res)[2],9)
    expect_equal(dim(res2)[1],4)
    expect_equal(dim(res2)[2],9)

    # Data types are correct
    char_cols <- c('prior','pred')
    doub_cols <- c('HR','driftHR','reject','mean_HR','sd_HR','mean_driftHR','sd_driftHR')

    for(col in char_cols) {
      expect_equal(typeof(res[,col]),'character')
      expect_equal(typeof(res2[,col]),'character')
    }

    for(col in doub_cols) {
      expect_equal(typeof(res[,col]),'double')
      expect_equal(typeof(res2[,col]),'double')
    }
})
