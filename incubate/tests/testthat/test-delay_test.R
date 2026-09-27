# test cases for significance tests for delay

futureAvailable <- requireNamespace("future", quietly = TRUE) &&
  requireNamespace("future.callr", quietly = TRUE) &&
  requireNamespace("future.apply", quietly = TRUE)

test_that('Structure of test objects.', code = {
  set.seed(123)

  x <- rexp_delayed(n = 131L, delay1 = 5, rate1 = .1)
  y <- rexp_delayed(n = 111L, delay1 = 5, rate1 = .3)

  tediff_d_bs <- test_diff(
    x = x,
    y = y,
    distribution = 'expon',
    param = 'delay1',
    R = 19,
    type = 'bootstrap',
    chiSqApprox = TRUE
  )

  expect_s3_class(tediff_d_bs, class = 'incubate_test')
  expect_named(
    tediff_d_bs,
    expected = c(
      'distribution',
      't_obs',
      'testDist',
      'R',
      'chisq_df_hat',
      'param',
      'P'
    )
  )
  expect_named(
    tediff_d_bs$P,
    expected = c('bootstrap', 'logrank', 'logrank_pp')
  )
  expect_type(tediff_d_bs$P$bootstrap, type = 'double')
  expect_lte(tediff_d_bs$P$bootstrap, expected = 1L)
  expect_gte(tediff_d_bs$P$bootstrap, expected = 0L)

  tediff_d_logrank <- test_diff(
    x = x,
    y = y,
    distribution = 'expon',
    param = 'delay1',
    type = 'logrank',
    chiSqApprox = TRUE
  )

  expect_s3_class(tediff_d_logrank, class = 'incubate_test')
  expect_named(
    tediff_d_logrank,
    expected = c(
      'distribution',
      'P'
    )
  )
  expect_named(tediff_d_logrank$P, expected = c('logrank', 'logrank_pp'))
  expect_type(tediff_d_logrank$P$logrank, type = 'double')
  expect_lte(tediff_d_logrank$P$logrank, expected = 1L)
  expect_gte(tediff_d_logrank$P$logrank, expected = 0L)

  expect_type(tediff_d_logrank$P$logrank_pp, type = 'double')
  expect_lte(tediff_d_logrank$P$logrank_pp, expected = 1L)
  expect_gte(tediff_d_logrank$P$logrank_pp, expected = 0L)

  # provide multiple parameters
  tediff_ds_LRT1 <- test_diff(
    x = x,
    y = y,
    distribution = 'weibull',
    param = c('delay1', "scale1"),
    type = 'LRT'
  )

  tediff_ds_LRT2 <- test_diff(
    x = x,
    y = y,
    distribution = 'weibull',
    param = 'delay1 + scale1',
    type = 'LRT'
  )

  expect_named(
    tediff_ds_LRT1,
    expected = c(
      'distribution',
      't_obs',
      'param',
      'P'
    )
  )
  expect_identical(tediff_ds_LRT1, tediff_ds_LRT2)
})


test_that('GOF-test on single-group exponentials', code = {
  testthat::skip_on_cran()
  testthat::skip(message = 'Too long to run every time!')

  if (futureAvailable) {
    future::plan(future.callr::callr, workers = 6L)

    # GOF-tests on true exponential data with varying sample size, delay and rate
    # results in a matrix of dimension #scenarios x #replications
    scenarios <- expand.grid(
      n = c(10, 25, 50),
      delay1 = c(0, 5, 15),
      rate1 = c(.01, .2, .4, 1, 1.5, 4)
    )
    fitting_expos <- future_replicate(n = 467L, simplify = FALSE, expr = {
      # fit exponential models with varying n, delay and rate
      purrr::pmap(
        .l = scenarios,
        .f = ~ delay_model(
          x = rexp_delayed(n = ..1, delay1 = ..2, rate1 = ..3),
          distribution = 'exponential'
        )
      )
    })
    # get a list of scenarios, each containing its models of replicated data
    fitting_expos <- purrr::transpose(fitting_expos)

    # list: for each scenario, the vector of Moran's GOF-test p-value
    GOF_pvals <- list(
      moran = purrr::map(
        .x = fitting_expos,
        .f = ~ purrr::map_dbl(., function(fit) {
          test_GOF(fit, method = 'moran')$p.value
        })
      ),
      pearson = purrr::map(
        .x = fitting_expos,
        .f = ~ purrr::map_dbl(., function(fit) {
          test_GOF(fit, method = 'pearson')$p.value
        })
      )
      #ad = purrr::map(.x = fitting_expos, .f = ~ purrr::map_dbl(., function(fit) test_GOF(fit, method = 'ad')$p.value))
    )

    # # to visualize the GOF P-values:
    # as_tibble(GOF_pvals[['moran']], .name_repair = 'unique') %>%
    #   tidyr::pivot_longer(cols = everything()) %>%
    #   ggplot(aes(x=value, col = name)) +
    #   geom_freqpoly(binwidth = .15) + xlim(0, 1)

    # expect uniform P-values for GOF under valid H0
    # go over the three types of GOF-tests, within test type, check each scenario
    # use purrr::flatten(GOF_pvals) as data argument to walk to test *all* in one go
    purrr::walk(
      GOF_pvals[['pearson']],
      .f = ~ expect_gt(object = mean(.), expected = 0.25)
    )
    purrr::walk(
      GOF_pvals[['pearson']],
      .f = ~ expect_lt(object = mean(.), expected = 0.75)
    )
    # purrr::walk(GOF_pvals[['ad']], .f = ~expect_gt(object = mean(.), expected = 0.25))
    # purrr::walk(GOF_pvals[['ad']], .f = ~expect_lt(object = mean(.), expected = 0.75))

    purrr::walk(
      GOF_pvals[['moran']],
      .f = ~ expect_gt(object = mean(.), expected = 0.35)
    )
    purrr::walk(
      GOF_pvals[['moran']],
      .f = ~ expect_lt(object = mean(.), expected = 0.65)
    )
    # Moran's test shows indeed P-values on average close to 0.5
    purrr::walk(
      GOF_pvals[['moran']],
      .f = ~ expect_equal(
        object = mean(.),
        expected = 0.5,
        tolerance = .22,
        info = 'moran'
      )
    )
    # Moran's test does not give P-values very close to 0, less than expected under uniformity
    GOF_moran_pvals_KSpval <- purrr::map_dbl(
      GOF_pvals[['moran']],
      .f = ~ suppressWarnings(
        ks.test(.[which(. > .085)], y = 'punif', min = 0.085)$p.value
      )
    )
    # not too many small P-values
    expect_lte(
      length(which(GOF_moran_pvals_KSpval < .05)) /
        length(GOF_moran_pvals_KSpval),
      expected = .3
    )
    # some high P-values
    expect_gte(
      length(which(GOF_moran_pvals_KSpval > .6)) /
        length(GOF_moran_pvals_KSpval),
      expected = .15
    )

    future::plan(future::sequential)
  } #fi
})


test_that("Bootstrap test on difference in delay for two exponential fits", code = {
  testthat::skip_on_cran()
  testthat::skip(message = 'Too long to run every time!')

  if (futureAvailable) {
    future::plan(future.callr::callr, workers = 6L)

    set.seed(2025 - 04 - 10)
    x <- rexp_delayed(13L, delay1 = 11, rate1 = .05)
    y <- rexp_delayed(17L, delay1 = 11, rate1 = .08)

    # increasing effect
    te_diff_delays <- purrr::map(
      purrr::set_names(c(0, 9, 19)),
      .f = ~ test_diff(
        x = x + .x,
        y = y,
        param = "delay1",
        type = 'bootstrap',
        R = 399
      )
    )

    te_diff_delays_P_bs <- purrr::map_dbl(
      te_diff_delays,
      .f = ~ purrr::chuck(., "P", "bootstrap")
    )

    # null model (no effect) has a high P-value
    expect_gt(te_diff_delays_P_bs[["0"]], expected = .1)

    # the bigger the effect (=difference in delay) the smaller the P-value
    expect_true(all(diff(te_diff_delays_P_bs) < 0L))
    # negative correlation betw effect size and P-values
    expect_lt(
      cor(x = as.integer(names(te_diff_delays)), y = te_diff_delays_P_bs),
      expected = -.67
    )

    #test effect of sample size: increase n and power goes up.
    set.seed(123456)
    # data with difference in delay by 2 time units
    #+but different sample sizes
    xs <- purrr::map(
      purrr::set_names(c(13, 20, 32, 37)),
      .f = ~ rexp_delayed(.x, delay1 = 7, rate1 = .07)
    )

    ys <- purrr::map(
      purrr::set_names(c(13, 20, 32, 37)),
      .f = ~ rexp_delayed(.x, delay1 = 9, rate1 = .07)
    )

    te_diff_delays_n <- purrr::map2(
      .x = xs,
      .y = ys,
      .f = ~ suppressWarnings(test_diff(
        x = .x,
        y = .y,
        param = "delay1",
        type = "bootstrap",
        R = 403
      ))
    )

    # sample size goes up and P-values for difference in delay1 go down (true difference is 2)
    expect_lt(
      cor(
        x = as.integer(names(te_diff_delays_n)),
        y = purrr::map_dbl(
          te_diff_delays_n,
          ~ purrr::chuck(., "P", "bootstrap")
        )
      ),
      expected = -.67
    )

    future::plan(future::sequential)
  } #fi future
})


test_that("Bootstrap test: chosen method matters", code = {
  set.seed(2023 - 10 - 04)
  # similar delay in both groups.
  #Hence, we expect high P-values for each method
  x <- rexp_delayed(n = 37, delay1 = 5, rate1 = .63)
  y <- rexp_delayed(n = 39, delay1 = 5.1, rate1 = .51)

  est_meths <- c("MPSE", "MLEn", "MLEc", "MLEw") |>
    purrr::set_names()
  teDiffs <- purrr::map(
    .x = est_meths,
    .f = ~ test_diff(
      x = x,
      y = y,
      param = "delay1",
      type = "bootstrap",
      method = .x,
      R = 571,
      profiled = .x != "MPSE"
    )
  )

  # bootstrap P-values vary depending on chosen method
  expect_gt(
    stats::sd(purrr::map_dbl(teDiffs, c("P", "bootstrap"))),
    expected = 1e-3
  )
})


test_that("Bootstrap test for difference in delay under H0 (no difference in delay)", code = {
  testthat::skip_on_cran()
  testthat::skip(message = "Too long to run every time!")

  if (futureAvailable) {
    future::plan(future.callr::callr, workers = 5L)

    set.seed(2021 - 05 - 06)

    testres_P_H0 <- future.apply::future_vapply(
      X = seq_len(25L),
      FUN = function(dummy) {
        x <- rexp_delayed(13, delay1 = 4, rate1 = .07)
        y <- rexp_delayed(11, delay1 = 4, rate1 = .1)

        # return P-value of bootstrap test
        Pval <- NA_real_
        try(
          Pval <- purrr::chuck(
            test_diff(
              x = x,
              y = y,
              param = "delay1",
              distribution = "expon",
              R = 301,
              type = "bootstrap"
            ),
            "P",
            "bootstrap"
          ),
          silent = TRUE
        )

        Pval
      },
      FUN.VALUE = double(1L),
      future.seed = TRUE
    )

    testres_P_H0 <- testres_P_H0[is.finite(testres_P_H0)]

    # KS-test does not reject H0 at sig.level = 0.2: uniform distribution
    expect_gt(
      suppressWarnings(stats::ks.test(x = testres_P_H0, y = "punif")$p.value),
      expected = .2
    )

    future::plan(future::sequential)
  }
})


test_that("Moran/Pearson GOF-test", code = {
  testthat::skip_on_cran()

  expect_true(requireNamespace("survival", quietly = TRUE))
  nTests <- 181

  if (futureAvailable) {
    future::plan(future.callr::callr, workers = 5L)
    # testres1: all observed events
    testres1 <- future.apply::future_vapply(
      X = seq_len(nTests),
      function(dummy) {
        x <- rexp_delayed(n = 23, delay1 = 3, rate1 = .7)
        fm <- delay_model(x = x, method = "MPSE")

        unlist(test_GOF(delayFit = fm, method = "moran")[c(
          "statistic",
          "p.value"
        )])
      },
      FUN.VALUE = double(2L),
      future.seed = TRUE
    )

    # all test statistics are positive
    expect_gt(min(testres1[1L, ]), expected = 0)
    # p-values are not too far off from uniform
    expect_gt(mean(testres1[2L, ]), expected = .3)
    expect_lt(mean(testres1[2L, ]), expected = .8)

    # testres2: right-censored events
    testres2 <- future.apply::future_vapply(
      X = seq_len(nTests),
      FUN = function(dummy) {
        x <- rexp_delayed(n = 29, delay1 = 3, rate1 = .7)
        evStatus <- sample(x = c(0, 1, 1), size = length(x), replace = TRUE)
        fm <- delay_model(
          x = survival::Surv(x, event = evStatus),
          method = "MPSE"
        )

        c(
          unlist(test_GOF(delayFit = fm, method = "moran")[c(
            "statistic",
            "p.value"
          )]),
          unlist(test_GOF(delayFit = fm, method = "pearson")[c(
            "statistic",
            "p.value"
          )])
        )
      },
      FUN.VALUE = double(4L),
      future.seed = TRUE
    )

    # all test statistics are positive
    expect_gt(min(testres2[1L, ]), expected = 0) #stat moran
    expect_gt(min(testres2[3L, ]), expected = 0) #stat pearson
    # P-values GOF moran
    # p-values are not too far off from uniform
    expect_gt(mean(testres2[2L, ]), expected = .33)
    # P-values are highly skewed upwards, median close to .9
    expect_lte(mean(testres2[2L, ]), expected = .95)

    # P-values GOF pearson
    expect_gt(mean(testres2[4L, ]), expected = .33)
    # P-values are highly skewed upwards, median close to .9
    expect_lte(mean(testres2[4L, ]), expected = .95)

    # testres3: GOF-tests, nonSurv, two group scenario
    testres3 <- future.apply::future_vapply(
      X = seq_len(nTests),
      FUN = function(dummy) {
        x <- rexp_delayed(n = 29, delay1 = 3, rate1 = 1.1)
        y <- rexp_delayed(n = 23, delay1 = 5, rate1 = .2)
        #evStatus <- sample(x = c(0, 1, 1), size = length(x), replace = TRUE)

        fm <- delay_model(x = x, y = y, method = "MPSE")

        c(
          unlist(test_GOF(delayFit = fm, method = "moran")[c(
            "statistic",
            "p.value"
          )]),
          unlist(test_GOF(delayFit = fm, method = "pearson")[c(
            "statistic",
            "p.value"
          )])
        )
      },
      FUN.VALUE = double(4L),
      future.seed = TRUE
    )

    # all test statistics are positive
    expect_gt(min(testres3[1L, ]), expected = 0) #stat moran
    expect_gt(min(testres3[3L, ]), expected = 0) #stat pearson

    # p-values are not too far off from uniform
    expect_gte(mean(testres3[2L, ]), expected = .4)
    expect_equal(mean(testres3[2L, ]), expected = .5, tolerance = .09)

    # P-values are rather lower than expected
    expect_gte(mean(testres3[4L, ]), expected = .33)
    expect_lte(mean(testres3[4L, ]), expected = .8)
    #boxplot(list(moran=testres3[2L,], pearson = testres3[4L,]), main = "H0, non-Surv, two-group")

    # data from Poisson model (=not H0), many ties
    #fm0 <- delay_model(x = c(8, 9, 9, 9, 10, 11, 12, 12, 14, 16), method = "MPSE")
    #tieInfo <- rlang::env_get(rlang::fn_env(fm0$objFun), "tieInfo")
    expect_gte(
      test_GOF(
        delayFit = delay_model(x = c(8, 9, 9, 10, 11, 12, 14), method = "MPSE"),
        method = "moran"
      )$statistic,
      expected = 0
    )

    # GOF-moran tests on tied data from poisson (HA), noSurv, two group, many ties
    testres3a <- future.apply::future_vapply(
      X = seq_len(nTests),
      FUN = function(dymmy) {
        yObs <- 8 + rpois(23, lambda = 3)
        #yEv <- sample(x = c(0, 1, 1, 1), size = length(yObs), replace = T)
        fm <- delay_model(
          x = 5 + rpois(17, lambda = 5),
          y = yObs,
          distribution = "expon",
          method = "MPSE"
        )

        c(
          moran = test_GOF(delayFit = fm, method = "moran")[["statistic"]],
          pearson = test_GOF(delayFit = fm, method = "pearson")[["statistic"]]
        )
      },
      FUN.VALUE = double(2L),
      future.seed = TRUE
    )

    expect_gte(min(testres3a[1L, ]), expected = 0) #stat moran
    expect_gte(min(testres3a[2L, ]), expected = 0) #stat pearson
    #boxplot(list(mo = testres3a[1L,], pe = testres3a[2L,]), main = "H0, nonSurv, two group", sub = "many ties", ylab = "Moran test stat")
    #hist(testres3a[1L,]) #Moran GOF-test statistic looks normal
    #hist(testres3a[2L,]) #Pearson GOF-test statistic looks normal, too (but more shifted to postive)

    # testres4: GOF-tests H0, Surv, two group
    testres4 <- future.apply::future_vapply(
      X = seq_len(nTests),
      FUN = function(dummy) {
        x <- rexp_delayed(n = 29, delay1 = 3, rate1 = 1.1)
        y <- rexp_delayed(n = 23, delay1 = 5, rate1 = .2)
        evStatus_x <- sample(x = c(0, 1, 1), size = length(x), replace = TRUE)

        fm <- delay_model(
          x = survival::Surv(x, evStatus_x),
          y = y,
          distribution = "expon",
          method = "MPSE"
        )

        c(
          unlist(test_GOF(delayFit = fm, method = "moran")[c(
            "statistic",
            "p.value"
          )]),
          unlist(test_GOF(delayFit = fm, method = "pearson")[c(
            "statistic",
            "p.value"
          )])
        )
      },
      FUN.VALUE = double(4L),
      future.seed = TRUE
    )

    # all test statistics are non-negative
    expect_gt(min(testres4[1L, ]), expected = 0) #stat moran
    expect_gt(min(testres4[3L, ]), expected = 0) #stat pearson
    # XXX test statistic of Moran GOF looks rather "normal", not chi-squared

    # Are p-values not too far off from uniform?
    #P-values moran
    expect_gte(
      mean(testres4[2L, ]) - 2 * sd(testres4[2L, ]) / sqrt(nTests),
      expected = .3
    )
    # P-values from Moran-GOF are strongly skewed upwards, median close to .9
    # we expected more small P-values (by chance)
    expect_lte(
      mean(testres4[2L, ]) + 2 * sd(testres4[2L, ]) / sqrt(nTests),
      expected = .85
    )

    #P-values pearson
    expect_gte(
      mean(testres4[4L, ]) - 2 * sd(testres4[4L, ]) / sqrt(nTests),
      expected = .3
    )
    # pearson GOF-test gives P-values strongly skewed upwards
    # pearson not ok, yet.
    #
    # expect_lte(
    #   mean(testres4[4L, ]) + 2 * sd(testres4[4L, ]) / sqrt(nTests),
    #   expected = .85
    # )
    #expect_lte(mean(testres4[4L,]), expected = .85)
    #boxplot(list(moran=testres4[2L,], pearson = testres4[4L,]), main = "H0, Surv, two group")

    # GOF-moran tests on tied data from Poisson (HA situation). Surv, two group, many ties
    testres4a <- future.apply::future_vapply(
      X = seq_len(nTests),
      FUN = function(dymmy) {
        yObs <- 8 + stats::rpois(23, lambda = 3)
        yEv <- sample(x = c(0, 1, 1, 1), size = length(yObs), replace = TRUE)
        fm <- delay_model(
          x = 5 + stats::rpois(17, lambda = 5),
          y = survival::Surv(yObs, event = yEv),
          method = "MPSE"
        )

        c(
          moran = test_GOF(delayFit = fm, method = "moran")[["statistic"]],
          pearson = test_GOF(delayFit = fm, method = "pearson")[["statistic"]]
        )
      },
      FUN.VALUE = double(2L),
      future.seed = TRUE
    )

    # XXX test statistics Moran looks normal
    expect_gte(min(testres4a[1L, ]), expected = 0) #stat moran
    expect_gte(min(testres4a[2L, ]), expected = 0) #stat pearson
    #boxplot(list(mo = testres4a[1L,], pe = testres4a[2L,]), main = "H0, Surv, two group", sub = "many ties", ylab = "Moran test stat")

    # GOF-tests for 2-groups with bind= parameter
    # testres5: GOF-tests for H0 (delays are equal), bind delay1, Surv, two group scenario
    testres5 <- future.apply::future_vapply(
      X = seq_len(nTests),
      FUN = function(dummy) {
        x <- rexp_delayed(n = 29, delay1 = 5, rate1 = 1.1)
        y <- rexp_delayed(n = 23, delay1 = 5, rate1 = .2)
        evStatus_x <- sample(x = c(0, 1, 1), size = length(x), replace = TRUE)

        fm <- delay_model(
          x = survival::Surv(x, evStatus_x),
          y = y,
          bind = "delay1",
          method = "MPSE"
        )

        c(
          unlist(test_GOF(delayFit = fm, method = "moran")[c(
            "statistic",
            "p.value"
          )]),
          unlist(test_GOF(delayFit = fm, method = "pearson")[c(
            "statistic",
            "p.value"
          )])
        )
      },
      FUN.VALUE = double(4L),
      future.seed = TRUE
    )

    # all test statistics are positive
    expect_gt(min(testres5[1L, ]), expected = 0) #stat moran
    expect_gt(min(testres5[3L, ]), expected = 0) #stat pearson

    # XXX does theory hold here that would guarantee uniform distribution? (with bind under H0)
    #expect_equal(mean(testres5[2L,]), expected = .5, tolerance = .43) #moran
    #expect_equal(mean(testres5[4L,]), expected = .5, tolerance = .43) #pearson
    #boxplot(list(moran=testres5[2L,], pearson = testres5[4L,]), main = "H0, bind=delay1")

    # testres6: GOF-tests for two group scenario with censored observations, bind delay1 (HA, as delay diff is 3)
    testres6 <- future.apply::future_vapply(
      X = seq_len(nTests),
      FUN = function(dummy) {
        x <- rexp_delayed(n = 29, delay1 = 5, rate1 = 1.1)
        y <- rexp_delayed(n = 23, delay1 = 8, rate1 = .4)
        evStatus_x <- sample(x = c(0, 1, 1), size = length(x), replace = TRUE)

        fm <- delay_model(
          x = survival::Surv(x, evStatus_x),
          y = y,
          bind = "delay1",
          method = "MPSE"
        )

        c(
          unlist(test_GOF(delayFit = fm, method = "moran")[c(
            "statistic",
            "p.value"
          )]),
          unlist(test_GOF(delayFit = fm, method = "pearson")[c(
            "statistic",
            "p.value"
          )])
        )
      },
      FUN.VALUE = double(4L),
      future.seed = TRUE
    )

    # all test statistics are positive
    expect_gt(min(testres6[1L, ]), expected = 0) #stat moran
    expect_gt(min(testres6[3L, ]), expected = 0) #stat pearson

    # XXX does theory hold here to guarantee uniform P-value distribution?
    #expect_lte(mean(testres6[2L,]), expected = .4) #moran
    #expect_lte(mean(testres6[4L,]), expected = .4) #pearson
    #boxplot(list(moran=testres6[2L,], pearson = testres6[4L,]))

    # teardown
    future::plan(future::sequential)
  } #fi future
})
