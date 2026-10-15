test_that("mcmc_derive.nlist", {
  nlist <- nlist::nlist(x = 1:2, y = matrix(1:4, 2))

  expr <- "
    gamma <- x + 2
    z <- y[1,2]
  "

  expect_equal(
    mcmc_derive(nlist, expr, silent = TRUE),
    nlist::nlist(gamma = c(3, 4), z = 3L)
  )
})

test_that("mcmc_derive.nlists", {
  nlist <- nlist::nlists(
    nlist::nlist(x = 1:2, y = matrix(1:4, 2)),
    nlist::nlist(x = 3:4, y = matrix(4:1, 2))
  )

  expr <- "
    gamma <- x + 2
    z <- y[1,2]
  "

  expect_equal(
    mcmc_derive(nlist, expr, silent = TRUE),
    
    nlist <- nlist::nlists(
      nlist::nlist(gamma = c(3, 4), z = 3L),
      nlist::nlist(gamma = c(5, 6), z = 2L)
    )
  )
})

test_that("mcmc_derive.mcmc", {
  mcmc <- coda::as.mcmc(subset(mcmcr::mcmcr_example, 1L, 1:2))

  expr <- "
    gamma <- alpha + beta
  alpha2 <- alpha * 2
  znot <- alpha * 2
  for(i in seq_along(x)) {
    alpha3[i] <- alpha[1] * x[i]
  }
  "

  values <- list(x = 2:10)

  expect_equal(
    mcmc_derive(mcmc, expr, values = values, monitor = "^g|^a", silent = TRUE),
    structure(c(
      14.34626, 5.90506, 16.34626, 7.90506, 14.34626, 5.90506,
      21.51939, 8.85759, 28.69252, 11.81012, 35.86565, 14.76265, 43.03878,
      17.71518, 50.21191, 20.66771, 57.38504, 23.62024, 64.55817, 26.57277,
      71.7313, 29.5253, 5.60693, 4.4559, 7.60693, 6.4559, 6.60693,
      5.4559, 8.60693, 7.4559
    ), .Dim = c(2L, 15L), .Dimnames = list(
      NULL, c(
        "alpha2[1]", "alpha2[2]", "alpha3[1]", "alpha3[2]",
        "alpha3[3]", "alpha3[4]", "alpha3[5]", "alpha3[6]", "alpha3[7]",
        "alpha3[8]", "alpha3[9]", "gamma[1,1]", "gamma[2,1]", "gamma[1,2]",
        "gamma[2,2]"
      )
    ), mcpar = c(1, 2, 1), class = "mcmc")
  )
})

test_that("mcmc_derive.mcmc.list", {
  mcmc.list <- coda::as.mcmc.list(subset(mcmcr::mcmcr_example, 1L, 1:2))

  expr <- "
    gamma <- alpha + beta
  alpha2 <- alpha * 2
  znot <- alpha * 2
  for(i in seq_along(x)) {
    alpha3[i] <- alpha[1] * x[i]
  }
  "

  values <- list(x = 2:10)

  expect_equal(
    mcmc_derive(mcmc.list, expr, values = values, monitor = "^g|^a", silent = TRUE),
    structure(list(structure(c(
      14.34626, 5.90506, 16.34626, 7.90506,
      14.34626, 5.90506, 21.51939, 8.85759, 28.69252, 11.81012, 35.86565,
      14.76265, 43.03878, 17.71518, 50.21191, 20.66771, 57.38504, 23.62024,
      64.55817, 26.57277, 71.7313, 29.5253, 5.60693, 4.4559, 7.60693,
      6.4559, 6.60693, 5.4559, 8.60693, 7.4559
    ), .Dim = c(2L, 15L), .Dimnames = list(
      NULL, c(
        "alpha2[1]", "alpha2[2]", "alpha3[1]", "alpha3[2]",
        "alpha3[3]", "alpha3[4]", "alpha3[5]", "alpha3[6]", "alpha3[7]",
        "alpha3[8]", "alpha3[9]", "gamma[1,1]", "gamma[2,1]", "gamma[1,2]",
        "gamma[2,2]"
      )
    ), mcpar = c(1, 2, 1), class = "mcmc")), class = "mcmc.list")
  )
})

test_that("mcmc_derive.mcmcr", {
  mcmcr <- subset(mcmcr::mcmcr_example, 1:2, 1:2)

  expr <- "
    gamma <- alpha + beta
  alpha2 <- alpha * 2
  znot <- alpha * 2
  for(i in seq_along(x)) {
    alpha3[i] <- alpha[1] * x[i]
  }
  "

  values <- list(x = 2:10)

  expect_equal(
    mcmc_derive(mcmcr, expr, values = values, monitor = "^g|^a", silent = TRUE),
    structure(list(alpha2 = structure(c(
      14.34626, 4.133, 5.90506,
      5.10146, 16.34626, 6.133, 7.90506, 7.10146
    ), .Dim = c(
      2L, 2L,
      2L
    ), class = "mcmcarray"), alpha3 = structure(c(
      14.34626, 4.133,
      5.90506, 5.10146, 21.51939, 6.1995, 8.85759, 7.65219, 28.69252,
      8.266, 11.81012, 10.20292, 35.86565, 10.3325, 14.76265, 12.75365,
      43.03878, 12.399, 17.71518, 15.30438, 50.21191, 14.4655, 20.66771,
      17.85511, 57.38504, 16.532, 23.62024, 20.40584, 64.55817, 18.5985,
      26.57277, 22.95657, 71.7313, 20.665, 29.5253, 25.5073
    ), .Dim = c(
      2L,
      2L, 9L
    ), class = "mcmcarray"), gamma = structure(c(
      5.60693, 3.01633,
      4.4559, 3.084598, 7.60693, 5.01633, 6.4559, 5.084598, 6.60693,
      4.01633, 5.4559, 4.084598, 8.60693, 6.01633, 7.4559, 6.084598
    ), .Dim = c(2L, 2L, 2L, 2L), class = "mcmcarray")), class = "mcmcr")
  )
})

test_that("mcmc_derive.mcmcrs", {
  mcmcr <- subset(mcmcr::mcmcr_example, 1L, 1L)
  mcmcrs <- mcmcr::as.mcmcrs(list(mcmcr, mcmcr))

  expect_equal(
    mcmc_derive(mcmcrs, "gamma <- alpha + beta", silent = TRUE),
    structure(list(
      mcmcr1 = structure(list(gamma = structure(c(
        5.60693,
        7.60693, 6.60693, 8.60693
      ), .Dim = c(1L, 1L, 2L, 2L), class = "mcmcarray")), class = "mcmcr"),
      mcmcr2 = structure(list(gamma = structure(c(
        5.60693, 7.60693,
        6.60693, 8.60693
      ), .Dim = c(1L, 1L, 2L, 2L), class = "mcmcarray")), class = "mcmcr")
    ), class = "mcmcrs")
  )
})

test_that("mcmc_derive.mcmcr with all missing data.frame", {
  mcmcr <- subset(mcmcr::mcmcr_example, 1L, 1L)

  values <- data.frame(theta = c(2, 3))

  expect_equal(
    mcmc_derive(mcmcr, "prediction <- alpha + beta + theta",
      silent = TRUE,
      monitor = "^prediction$",
      values = values
    ),
    structure(list(prediction = structure(c(
      7.60693, 10.60693, 8.60693,
      11.60693
    ), .Dim = c(1L, 1L, 2L, 2L), class = "mcmcarray")), class = "mcmcr")
  )
})

test_that("mcmc_derive in parallel", {
  skip_on_os("windows") # not working on GitHub actions but is on check_win_devel()
  # need to switch to furrr
  mcmcr <- subset(mcmcr::mcmcr_example, 1:2, 1:2)

  expr <- "
    gamma <- alpha + beta
  alpha2 <- alpha * 2
  znot <- alpha * 2
  for(i in seq_along(x)) {
    alpha3[i] <- alpha[1] * x[i]
  }
  "

  values <- list(x = 2:10)

  doParallel::registerDoParallel(2)

  expect_equal(
    mcmc_derive(mcmcr, expr,
      values = values, monitor = "^g|^a",
      parallel = TRUE, silent = TRUE
    ),
    structure(list(alpha2 = structure(c(
      14.34626, 4.133, 5.90506,
      5.10146, 16.34626, 6.133, 7.90506, 7.10146
    ), .Dim = c(
      2L, 2L,
      2L
    ), class = "mcmcarray"), alpha3 = structure(c(
      14.34626, 4.133,
      5.90506, 5.10146, 21.51939, 6.1995, 8.85759, 7.65219, 28.69252,
      8.266, 11.81012, 10.20292, 35.86565, 10.3325, 14.76265, 12.75365,
      43.03878, 12.399, 17.71518, 15.30438, 50.21191, 14.4655, 20.66771,
      17.85511, 57.38504, 16.532, 23.62024, 20.40584, 64.55817, 18.5985,
      26.57277, 22.95657, 71.7313, 20.665, 29.5253, 25.5073
    ), .Dim = c(
      2L,
      2L, 9L
    ), class = "mcmcarray"), gamma = structure(c(
      5.60693, 3.01633,
      4.4559, 3.084598, 7.60693, 5.01633, 6.4559, 5.084598, 6.60693,
      4.01633, 5.4559, 4.084598, 8.60693, 6.01633, 7.4559, 6.084598
    ), .Dim = c(2L, 2L, 2L, 2L), class = "mcmcarray")), class = "mcmcr")
  )
})

test_that("mcmc_derive matrix in values", {
  mcmcr <- subset(mcmcr::mcmcr_example, 1:2, 1:2, pars = "beta")

  expr <- "
    x <- Z
    for(i in 1:nrow(beta)) {
      for(j in 1:ncol(beta)) {
        x[i,j] <- beta[i,j]
      }
    }
  "

  Z <- matrix(0, 2, 2)

  values <- list(Z = Z)

  expect_equal(
    mcmc_derive(mcmcr, expr, values = values, monitor = "x"),
    structure(list(x = structure(c(
      -1.5662, 0.94983, 1.50337, 0.533868,
      -0.5662, 1.94983, 2.50337, 1.533868, -0.5662, 1.94983, 2.50337,
      1.533868, 0.4338, 2.94983, 3.50337, 2.533868
    ), .Dim = c(
      2L, 2L,
      2L, 2L
    ), class = "mcmcarray")), class = "mcmcr")
  )
})

test_that("mcmc_derive warnings and errors", {
  mcmcr <- subset(mcmcr::mcmcr_example, 1:2, 1:10)

  expect_error(
    mcmc_derive(mcmcr, expr = "garbage r code"),
    "garbage r"
  )

  expect_error(
    mcmc_derive(mcmcr,
      expr = "alpha <- beta",
      values = list(alpha = 1, beta = 2, sigma = 3)
    ),
    "^All the parameters in object are also in values[.]$"
  )

  expect_warning(
    mcmc_derive(mcmcr,
      expr = "alpha2 <- beta * sigma + alpha",
      values = list(alpha = 1)
    ),
    "^The following parameters were also in values and so were dropped from object: 'alpha'[.]$"
  )

  expect_warning(
    mcmc_derive(mcmcr,
      expr = "alpha2 <- beta * sigma + alpha",
      values = list(alpha3 = 1)
    ),
    "^None of the variables in values are in expr[.]$"
  )

  expect_warning(
    mcmc_derive(mcmcr,
      expr = "alpha2 <- beta + beta3 * sigma + alpha",
      values = list(alpha3 = 1, beta3 = 2)
    ),
    "^The following variables were not in expr and so were dropped from values: 'alpha3'[.]$"
  )

  expect_warning(
    mcmc_derive(mcmcr, expr = "alpha2 <- beta * alpha"),
    "^The following parameters were not in expr and so were dropped from object: 'sigma'[.]$"
  )

  expect_error(
    mcmc_derive(mcmcr, expr = "unknown <- unknowable"),
    "^None of the parameters in object are in expr[.]$"
  )


  expect_error(
    mcmc_derive(mcmcr, expr = "alpha <- beta * sigma"),
    "^`expr` must include at least one variable that is not in object or values[.]$"
  )

  expect_error(
    mcmc_derive(mcmcr, expr = "alpha2 <- beta * sigma * alpha", monitor = "alpha3"),
    "^`monitor` 'alpha3' must match at least one of the following variables in expr: 'alpha2'[.]$"
  )

  expect_identical(
    mcmc_derive(mcmcr, expr = "alpha2 <- beta * sigma * alpha * alpha3", monitor = "2$"),
    mcmcr::fill_all(set_pars(subset(mcmcr, pars = "beta"), "alpha2"), NA_real_))

  expect_error(
    mcmc_derive(mcmcr,
      expr = "gamma <- alpha", monitor = "something",
      silent = TRUE
    ),
    "^`monitor` 'something' must match at least one of the following variables in expr: 'gamma'[.]$"
  )
})


test_that("mcmc_derive with primary = TRUE", {
  mcmcr <- subset(mcmcr::mcmcr_example, 1L, 1L)

  expect_equal(
    mcmc_derive(mcmcr, "gamma <- alpha + beta", primary = FALSE, silent = TRUE),
    structure(list(gamma = structure(c(
      5.60693, 7.60693, 6.60693,
      8.60693
    ), .Dim = c(1L, 1L, 2L, 2L), class = "mcmcarray")), class = "mcmcr")
  )

  # doesn't include alpha as primary = FALSE
  expect_equal(
    mcmc_derive(mcmcr,
      "gamma <- alpha + beta
                           alpha <- alpha * 2",
      primary = FALSE, silent = TRUE
    ),
    structure(list(gamma = structure(c(
      5.60693, 7.60693, 6.60693,
      8.60693
    ), .Dim = c(1L, 1L, 2L, 2L), class = "mcmcarray")), class = "mcmcr")
  )

  expect_equal(
    mcmc_derive(mcmcr, "gamma <- alpha + beta", primary = TRUE, silent = TRUE),
    structure(list(
      alpha = structure(c(7.17313, 8.17313), .Dim = c(
        1L,
        1L, 2L
      ), class = "mcmcarray"), beta = structure(c(
        -1.5662, -0.5662,
        -0.5662, 0.4338
      ), .Dim = c(1L, 1L, 2L, 2L), class = "mcmcarray"),
      gamma = structure(c(5.60693, 7.60693, 6.60693, 8.60693), .Dim = c(
        1L,
        1L, 2L, 2L
      ), class = "mcmcarray"), sigma = structure(11.2331, .Dim = c(
        1L,
        1L, 1L
      ), class = "mcmcarray")
    ), class = "mcmcr")
  )

  # goes with original alpha
  expect_equal(
    mcmc_derive(mcmcr,
      "gamma <- alpha + beta
                           alpha <- alpha * 2",
      primary = TRUE, silent = TRUE
    ),
    structure(list(
      alpha = structure(c(7.17313, 8.17313), .Dim = c(
        1L,
        1L, 2L
      ), class = "mcmcarray"), beta = structure(c(
        -1.5662, -0.5662,
        -0.5662, 0.4338
      ), .Dim = c(1L, 1L, 2L, 2L), class = "mcmcarray"),
      gamma = structure(c(5.60693, 7.60693, 6.60693, 8.60693), .Dim = c(
        1L,
        1L, 2L, 2L
      ), class = "mcmcarray"), sigma = structure(11.2331, .Dim = c(
        1L,
        1L, 1L
      ), class = "mcmcarray")
    ), class = "mcmcr")
  )
})

test_that("mcmc_derive with alpha", {
  mcmcr <- subset(mcmcr::mcmcr_example, 1L, 1L)

  expect_equal(
    mcmc_derive(mcmcr,
      "gamma <- alpha + beta
   alpha <- alpha * 2",
      primary = FALSE, silent = TRUE
    ),
    structure(list(gamma = structure(c(
      5.60693, 7.60693, 6.60693,
      8.60693
    ), .Dim = c(1L, 1L, 2L, 2L), class = "mcmcarray")), class = "mcmcr")
  )

  expect_equal(
    mcmc_derive(mcmcr,
      "gamma <- alpha + beta
   alpha <- alpha * 2",
      values = list(alpha = 3), primary = FALSE, silent = TRUE
    ),
    structure(list(gamma = structure(c(1.4338, 2.4338, 2.4338, 3.4338), .Dim = c(1L, 1L, 2L, 2L), class = "mcmcarray")), class = "mcmcr")
  )

  expect_equal(
    mcmc_derive(mcmcr,
      "gamma <- alpha + beta
   alpha <- alpha * 2",
      values = list(alpha = 3), primary = TRUE, silent = TRUE
    ),
    structure(list(
      alpha = structure(c(7.17313, 8.17313), .Dim = c(
        1L,
        1L, 2L
      ), class = "mcmcarray"), beta = structure(c(
        -1.5662, -0.5662,
        -0.5662, 0.4338
      ), .Dim = c(1L, 1L, 2L, 2L), class = "mcmcarray"),
      gamma = structure(c(1.4338, 2.4338, 2.4338, 3.4338), .Dim = c(
        1L,
        1L, 2L, 2L
      ), class = "mcmcarray"), sigma = structure(11.2331, .Dim = c(
        1L,
        1L, 1L
      ), class = "mcmcarray")
    ), class = "mcmcr")
  )
})

test_that("mcmc_derive.nlist", {
  mcmcr <- subset(mcmcr::mcmcr_example, 1L, 1L)

  values <- list()

  expect_equal(
    mcmc_derive(mcmcr, "gamma <- alpha + beta",
      values = values,
      silent = TRUE
    ),
    structure(list(gamma = structure(c(
      5.60693, 7.60693, 6.60693,
      8.60693
    ), .Dim = c(1L, 1L, 2L, 2L), class = "mcmcarray")), class = "mcmcr")
  )

  values <- list(3)

  expect_error(mcmc_derive(mcmcr, "gamma <- alpha + beta",
    values = values,
    silent = TRUE
  ), "`values` must be named[.]", class = "chk_error")

  values <- list(alpha = 3, alpha = 1)

  expect_error(mcmc_derive(mcmcr, "gamma <- alpha + beta",
    values = values,
    silent = TRUE
  ), "`names[(]values[)]` must be unique.", class = "chk_error")

  values <- list(alpha = 3)

  expect_equal(
    mcmc_derive(mcmcr, "gamma <- alpha + beta", values = values, silent = TRUE),
    structure(list(gamma = structure(c(1.4338, 2.4338, 2.4338, 3.4338), .Dim = c(1L, 1L, 2L, 2L), class = "mcmcarray")), class = "mcmcr")
  )


  values <- nlist::nlist(alpha = 3)

  expect_equal(
    mcmc_derive(mcmcr, "gamma <- alpha + beta", values = values, silent = TRUE),
    structure(list(gamma = structure(c(1.4338, 2.4338, 2.4338, 3.4338), .Dim = c(1L, 1L, 2L, 2L), class = "mcmcarray")), class = "mcmcr")
  )
})
