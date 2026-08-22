#' @srrstats {G5.0}
#' @srrstats {G5.1}
#' @srrstats {G5.2}
#' @srrstats {G5.2a}
#' @srrstats {G5.2b}

test_that("simulation works", {
  df_gaussian <-
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = 1,
      acro = 1,
      period = 24,
      family = "gaussian"
    )
  expect_s3_class(df_gaussian, "data.frame")
})


test_that("assess error messaging", {
  # bad 'n'
  expect_error(
    simulate_cosinor(
      n = 100.1,
      mesor = 1,
      amp = 1,
      acro = 1,
      period = 24,
      family = "gaussian"
    ),
    regex = "n must be an integer greater than 0", fixed = TRUE
  )

  # bad 'n_components'
  expect_error(
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = 1,
      acro = 1,
      period = 24,
      n_components = 0,
      family = "gaussian"
    ),
    regex = "n_components must be an integer greater than 0", fixed = TRUE
  )

  # bad 'mesor'
  expect_error(
    simulate_cosinor(
      n = 100,
      mesor = c(1, 2),
      amp = 1,
      acro = 1,
      period = 24,
      n_components = 1,
      family = "gaussian"
    ),
    regex = "mesor must a single number", fixed = TRUE
  )

  # bad 'amp'
  expect_error(
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = c(1, 2),
      acro = 1,
      period = 24,
      n_components = 1,
      family = "gaussian"
    ),
    regex = paste(
      "amp must be a vector containing numbers, with",
      "length equal to n_components"
    ), fixed = TRUE
  )

  # bad 'acro'
  expect_error(
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = 1,
      acro = c(1, 2),
      period = 24,
      n_components = 1,
      family = "gaussian"
    ),
    regex = paste(
      "acro must be a vector containing numbers, with",
      "length equal to n_components"
    ), fixed = TRUE
  )

  # bad 'period'
  expect_error(
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = 1,
      acro = 1,
      period = c(24, 12),
      n_components = 1,
      family = "gaussian"
    ),
    regex = paste(
      "period must be a vector containing numbers, with",
      "length equal to n_components"
    ), fixed = TRUE
  )

  # bad 'family'
  expect_error(
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = 1,
      acro = 1,
      period = 24,
      n_components = 1,
      family = "bad family"
    ),
    regex = paste(
      '\'arg\' should be one of "gaussian", "poisson",',
      '"binomial", "gamma"'
    ), fixed = TRUE
  )

  # bad 'beta.mesor'
  expect_error(
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = 1,
      acro = 1,
      period = 24,
      n_components = 1,
      family = "gaussian",
      beta.group = TRUE,
      beta.mesor = "2",
      beta.amp = 2,
      beta.acro = 2
    ),
    regex = "beta.mesor must be a single number", fixed = TRUE
  )

  # bad 'beta.amp'
  expect_error(
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = 1,
      acro = 1,
      period = 24,
      n_components = 1,
      family = "gaussian",
      beta.group = TRUE,
      beta.mesor = 2,
      beta.amp = "2",
      beta.acro = 2
    ),
    regex = paste(
      "beta.amp must be a vector containing numbers,",
      "with length equal to n_components"
    ), fixed = TRUE
  )

  # bad 'beta.acro'
  expect_error(
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = 1,
      acro = 1,
      period = 24,
      n_components = 1,
      family = "gaussian",
      beta.group = TRUE,
      beta.mesor = 2,
      beta.amp = 2,
      beta.acro = "2"
    ),
    regex = paste(
      "beta.acro must be a vector containing numbers,",
      "with length equal to n_components"
    ), fixed = TRUE
  )

  # bad 'beta.group'
  expect_error(
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = 1,
      acro = 1,
      period = 24,
      n_components = 1,
      family = "gaussian",
      beta.group = "TRUE",
      beta.mesor = 2,
      beta.amp = 2,
      beta.acro = 2
    ),
    regex = "beta.group argument must be logical", fixed = TRUE
  )

  # missing 'beta.acro'
  expect_error(
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = 1,
      acro = 1,
      period = 24,
      n_components = 1,
      family = "gaussian",
      beta.group = TRUE,
      beta.mesor = 2,
      beta.amp = 2
    ),
    regex = '"beta.acro" is missing, with no default', fixed = TRUE
  )
})
