context("test-tvgeom")

test_that(
  "The values returned by functions for the plain-vanilla geometric
  distribution and the time-varying geometric distribution are
  identical IF probability of success is assumed to be the same for
  each trial", {

    # ---- Example calculation 1 ----

    # What's the probability of 7 failures *before* the first success if you
    # were trying to roll a 3 on a fair die (for which prob = 1/6)?
    dgeom_ex1 <- dgeom(7, 1 / 6) # stats::dgeom

    # The tvgeom package utilizes a 'shifted' parameterization of the geometric
    # distribution; thus, to make an apples-to-apples comparison between the
    # two, we must ask: what is the probability that 8 trials are needed to get
    # one success? Note that dtvgeom requires a vector of probabilities, one
    # for each time-step.
    dtvgeom_ex1 <- dtvgeom(8, c(rep(1 / 6, 8))) # tvgeom::dtvgeom
    
    expect_equal(dgeom_ex1, dtvgeom_ex1)
    
    # ---- Example calculation 2 ----
    
    # The probability of making your first score in the ﬁrst 5 shots at the
    # goal if 31% of your shots score?
    dgeom_ex2 <- pgeom(4, .31)
    dtvgeom_ex2 <- ptvgeom(5, c(rep(.31, 5)))
    expect_equal(dgeom_ex2, dtvgeom_ex2)
    }
)

test_that(
  "The values returned by ptvgeom are consistent for different lower.tail and
  log.p arguments", {
    
    # Test that log.p and lower.tail arguments to ptvgeom are working and yield
    # expected results.
    probs <- c(rep(.001, 10), rbeta(40, 3, 40))
    ptvgeom_1 <-
      round(ptvgeom(q = 15, probs, lower.tail = TRUE, log.p = FALSE), 10)
    ptvgeom_2 <-
      round(1 - exp(ptvgeom(q = 15, probs, lower.tail = FALSE, log.p = TRUE)),
            10
      )
    expect_equal(ptvgeom_1, ptvgeom_2)
    }
)

test_that(
  "coverage for dtvgeom", {
    expect_silent(dtvgeom(x = 1:3, prob = c(rep(0.1, 14)), log = TRUE))
    expect_silent(dtvgeom(x = 1:3, prob = c(rep(0.1, 14)), log = FALSE))
    
    ## Errors
    expect_error(dtvgeom(x = 8, prob = c(rep(6, 8))))
  }
)

test_that(
  "coverage for ptvgeom", {
    expect_silent(
      ptvgeom(q = 3, prob = c(rep(0.1, 14)), lower.tail = FALSE, log.p = TRUE)
    )
    expect_silent(
      ptvgeom(q = 3, prob = c(rep(0.1, 14)), lower.tail = TRUE, log.p = FALSE)
    )
    expect_silent(
      ptvgeom(q = 5, prob = c(rep(0.1, 14)), lower.tail = FALSE)
    )
    expect_silent(
      ptvgeom(q = c(-1:18), prob = c(rep(0.1, 14)), lower.tail = TRUE)
    )
    
    ## Errors
    expect_error(ptvgeom(q = 2, prob = c(rep(-1, 5))))
  }
)

test_that(
  "coverage for qtvgeom", {
    expect_silent(
      qtvgeom(p = log(0.5), 
              prob = c(rep(0.1, 14)), 
              lower.tail = TRUE, 
              log.p = TRUE)
    )
    expect_silent(
      qtvgeom(p = 0.5, 
              prob = c(rep(0.1, 14)), 
              lower.tail = FALSE, 
              log.p = FALSE)
    )
    
    ## Errors
    expect_error(qtvgeom(p = -1, prob = c(rep(0.1, 5))))
    expect_error(qtvgeom(p = 0.5, prob = c(rep(-0.1, 5)), log.p = TRUE))
  }
)

test_that(
  "coverage for rtvgeom", {
    expect_silent(rtvgeom(n = 5, prob = c(rep(0.1, 14))))
    
    ## Errors
    expect_error(rtvgeom(n = 1, prob = c(rep(1.5, 5))))
    expect_error(rtvgeom(n = 1.5, prob = c(rep(0.1, 5))))
  }
)

test_that(
  "coverage for rttvgeom", {
    expect_silent(rttvgeom(n = 5, prob = c(rep(0.1, 14)), lower = 1, upper = 5))
    
    ## Errors
    expect_error(rttvgeom(n = 5, prob = c(rep(0.1, 14)), lower = -1, upper = 5))
    expect_error(rttvgeom(n = 5, prob = c(rep(0.1, 14)), lower = 1, upper = 50))
    expect_error(rttvgeom(n = 1, prob = c(rep(-0.1, 5))))
    expect_error(rttvgeom(n = 1.5, prob = c(rep(0.1, 5))))
  }
)

test_that(
  "Invalid arguments result in errors", {
    expect_error(qtvgeom(p = 2, prob = c(rep(1 / 6, 8)), log.p = TRUE))
    expect_error(rttvgeom(
      n = 10, prob = c(rep(1 / 6, 8)),
      lower = 2, upper = 18
    ))
    expect_error(rttvgeom(
      n = 10, prob = c(rep(1 / 6, 8)),
      lower = -1, upper = 8
    ))
  }
)

test_that(
  "The values returned by qtvgeom are consistent for different lower.tail and
  log.p arguments", {
    
    # Test that log.p and lower.tail arguments to qtvgeom are working and yield
    # expected results.
    probs <- c(rep(.001, 10), rbeta(40, 3, 40))
    qtvgeom_1 <-
      as.numeric(qtvgeom(p = .55, probs, lower.tail = TRUE, log.p = FALSE))
    qtvgeom_2 <-
      as.numeric(qtvgeom(p = log(.45), probs, lower.tail = FALSE, log.p = TRUE))
    expect_equal(qtvgeom_1, qtvgeom_2)
  }
)