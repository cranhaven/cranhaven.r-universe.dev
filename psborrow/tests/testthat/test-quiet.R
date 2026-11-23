library(dplyr)

get_sim_data <- function() {
    set.seed(3015)

    ss <- set_n(
        ssC = 70,
        ssE = 90,
        ssExt = 130
    )

    covset1 <- set_cov(
        n_cat = 1,
        n_cont = 1,
        mu_int = c(0, 0.5),
        mu_ext = c(0.7, 0.5),
        var = c(1, 1.2, 1),
        cov = c(0.5, 0.7),
        prob_int = c(0.45),
        prob_ext = c(0.65)
    )

    sample_cov <- simu_cov(
        ssObj = ss,
        covObj = covset1,
        HR = c(0.67),
        driftHR = c(1),
        nsim = 1,
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

    summ <- get_summary(res)
    summ2 <- get_summary(res2)

    return(list(
      summ,
      summ2
    ))
}



test_that("Using quiet supresses messages as expected", {


    options("psborrow.quiet" = TRUE)
    quiet_output <- capture_output({
        quiet_msg <- capture_messages({
            quiet_result <- get_sim_data()
        })
    })

    options("psborrow.quiet" = FALSE)
    loud_output <- capture_output({
        loud_msg <- capture_messages({
            loud_result <- get_sim_data()
        })
    })

    skip_if(!Sys.getenv("RUN_ALL_TESTS") == TRUE)
    skip_on_cran
    expect_true(quiet_output == "")
    expect_true(length(quiet_msg) == 0)
    expect_true(loud_output != "")
    expect_true(length(loud_msg) > 0)
    expect_equal(quiet_result, loud_result)
})
