test_that("inputs are correctly carried through to outputs", {

  # Set inputs that will be evaluated
  driftHRin <- c(0.12, 4.1)
  HRin <- c(.08, 2.999)
  ssCin <- 120
  ssEin <- 120
  ssExtin <- 80
  nsimin <- 1

  # Run simulation analysis
  suppressMessages({
    ss <- set_n(
      ssC = ssCin,
      ssE = ssEin,
      ssExt = ssExtin
    )

    covset1 <- set_cov(
      n_cat = 2,
      n_cont = 1,
      mu_int = c(0, 0.4, 0.4),
      mu_ext = c(0.7, 0.4, 0.2),
      var = c(1, 1.1, 1),
      cov = c(0.5, 0.1, 0.9),
      prob_int = c(0.95, 0.54),
      prob_ext = c(0.85, 0.54)
    )

    sample_cov <- simu_cov(
      ssObj = ss,
      covObj = covset1,
      HR = HRin,
      driftHR = driftHRin,
      nsim = nsimin,
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
      CCOD_t = 100,
      etaC = c(0.02, 0.03),
      etaE = c(0.2, 0.3),
      d_itv = 2
    )

    c_ext <- set_clin(
      gamma = 10,
      CCOD = "event",
      CCOD_t = 100,
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

    summ <- get_summary(res)
    summ2 <- get_summary(res2)

  })

  # Check N input combinations captured correctly
  expect_equal(
    dim(sample_cov),
    c(nsimin*NROW(HRin), NROW(driftHRin))
  )

  # Check N patients, HR, driftHR captured correctly in simulated data
  for(i in 1:dim(sample_cov)[1]) {
    for(j in 1:dim(sample_cov)[2]) {

      # N patients
      expect_equal(
        ssCin,
        sum(sample_cov[[i,j]][,'ext']==0 & sample_cov[[i,j]][,'trt']==0)
      )
      expect_equal(
        ssEin,
        sum(sample_cov[[i,j]][,'ext']==0 & sample_cov[[i,j]][,'trt']==1)
      )
      expect_equal(
        ssExtin,
        sum(sample_cov[[i,j]][,'ext']==1 & sample_cov[[i,j]][,'trt']==0)
      )

      # HR
      expect_equal(
        rep(HRin,each=nsimin)[i],
        unique(sample_cov[[i,j]][,'HR'])
      )

      # driftHR
      expect_equal(
        driftHRin[j],
        unique(sample_cov[[i,j]][,'driftHR'])
      )

    }
  }

  # Check driftHR and HR captured correctly in sample of MCMC outputs
  ## run_mcmc()
  ### res
  expect_equal(
    driftHRin,
    unique(res$driftHR)[order(unique(res$driftHR))]
  )
  expect_equal(
    HRin,
    unique(res$HR)[order(unique(res$HR))]
  )

  ### summ
  expect_equal(
    driftHRin,
    unique(summ$driftHR)[order(unique(summ$driftHR))]
  )
  expect_equal(
    HRin,
    unique(summ$HR)[order(unique(summ$HR))]
  )

  ## run_mcmc_p()
  ### res
  expect_equal(
    driftHRin,
    unique(res2$driftHR)[order(unique(res2$driftHR))]
  )
  expect_equal(
    HRin,
    unique(res2$HR)[order(unique(res2$HR))]
  )

  ### summ
  expect_equal(
    driftHRin,
    unique(summ2$driftHR)[order(unique(summ2$driftHR))]
  )
  expect_equal(
    HRin,
    unique(summ2$HR)[order(unique(summ2$HR))]
  )

})


