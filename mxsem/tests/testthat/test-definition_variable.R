test_that("definition variable works", {
  set.seed(1234)
  dataset <- simulate_latent_growth_curve(N = 100)
  model <- "
  I =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
  S =~ data.t_1 * y1 + data.t_2 * y2 + data.t_3 * y3 + data.t_4 * y4 + data.t_5 * y5

  I ~ int*1
  S ~ slp*1

  # set intercepts of manifest variables to zero
  y1 ~ 0*1; y2 ~ 0*1; y3 ~ 0*1; y4 ~ 0*1; y5 ~ 0*1;
  "

  # we get a warning here because all loadings of S have been assigned labels
  testthat::expect_warning(
    mod <- mxsem(model = model,
                 data = dataset) |>
      mxTryHard()
  )
  testthat::expect_true(abs(omxGetParameters(mod)["int"] - 1) < .2)
  testthat::expect_true(abs(omxGetParameters(mod)["slp"] - .4) < .1)
  testthat::expect_true(abs(omxGetParameters(mod)["I\u2194I"] - 1) < .2)
  testthat::expect_true(abs(omxGetParameters(mod)["S\u2194S"] - 1) < .2)

})
