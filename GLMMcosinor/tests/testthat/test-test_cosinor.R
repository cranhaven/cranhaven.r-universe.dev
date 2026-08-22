#' @srrstats {G5.0}
#' @srrstats {G5.0}
#' @srrstats {G5.1}
#' @srrstats {G5.2}
#' @srrstats {G5.2a}
#' @srrstats {G5.2b}

test_that("script works and warnings are displayed appropriately", {
  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )
  test_object <- test_cosinor_levels(object, x_str = "X")
  expect_s3_class(test_object, "cglmmTest")

  # Test the comparison_type variable, and test the print output
  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", n_components = 2, period = c(12, 11)),
    data = vitamind
  )

  expect_no_error(
    test_cosinor_components(
      object,
      x_str = "X",
      level_index = 1,
      comparison_A = 1,
      comparison_B = 2
    )
  )

  expect_no_error(
    print(
      test_cosinor_components(
        object,
        x_str = "X",
        level_index = 1,
        comparison_A = 1,
        comparison_B = 2
      )
    )
  )


  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_no_error(test_cosinor_levels(object, x_str = "X"))

  # Testing error messages
  # Error message test 1
  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    test_cosinor_levels(object, x_str = 10),
    regex = "is.character(x_str) is not TRUE",
    fixed = TRUE
  )

  # Error message test 2
  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    test_cosinor_levels(object, x_str = "Y"),
    regex = "x_str must be the name of a group in object",
    fixed = TRUE
  )

  # Error message test 3
  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    test_cosinor_levels(
      object,
      x_str = "X",
      param = "phase"
    ),
    regex = "Invalid parameter. Expected 'amp' or 'acr'",
    fixed = TRUE
  )

  # Error message test 4
  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    test_cosinor_levels(object, x_str = "X", param = "phase"),
    regex = "Expected 'amp' or 'acr'.",
    fixed = TRUE
  )

  # Error message test 5
  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    test_cosinor_levels(
      object,
      x_str = "X",
      comparison_A = 4,
    ),
    regex = "'comparison_A' must correspond to a level within the group
    specified by 'x_str'",
    fixed = TRUE
  )

  # Error message test 6
  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    test_cosinor_components(object, x_str = "X", comparison_A = 4),
    regex = paste(
      "'comparison_A' and 'comparison_B' must be numbers",
      "corresponding to a component in the model"
    ),
    fixed = TRUE
  )

  # Error message test 7
  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    test_cosinor_levels(object, x_str = "X", component_index = 10),
    regex = paste(
      "'component_index' must be supplied and it must be a",
      "number corresponding to a component in the model"
    ),
    fixed = TRUE
  )

  # Error message test 9
  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", n_components = 2, period = c(12, 11)),
    data = vitamind
  )

  expect_error(
    test_cosinor_components(
      object,
      x_str = "X",
      level_index = 10,
      comparison_A = 1,
      comparison_B = 2
    ),
    regex = paste(
      "'level_index' must be supplied and it must be a number",
      "corresponding to a level in the model"
    ),
    fixed = TRUE
  )


  # Error message test 10
  obj <- test_cosinor_levels(object, x_str = "X")
  expect_s3_class(obj, "cglmmTest")
})

test_that("multi-component comparison works, print functions work", {
  TrueMesor_a <- 1
  TrueMesor_b <- 0.5
  TrueAmp_a <- 2
  TrueAmp_b <- 1
  TrueAcr_a <- 3
  TrueAcr_b <- 0.3

  # test parameter estimation of guassian simulated data
  withr::local_seed(50)
  comod <- simulate_cosinor(
    10000,
    mesor = c(TrueMesor_a),
    amp = c(TrueAmp_a, TrueAmp_a),
    acro = c(TrueAcr_a, TrueAcr_a),
    beta.mesor = c(TrueMesor_b),
    beta.amp = c(TrueAmp_b, TrueAmp_b),
    beta.acro = c(TrueAcr_b, TrueAcr_b),
    family = "gaussian",
    n_components = 2,
    period = c(10, 12),
    beta.group = TRUE
  )
  object <- cglmm(
    Y ~ group +
      amp_acro(times, n_components = 2, group = "group", period = c(10, 12)),
    data = comod
  )

  expect_no_error(
    test_cosinor_components(
      object,
      x_str = "group",
      comparison_A = 1,
      comparison_B = 2,
      level_index = 1
    )
  )
  expect_snapshot(print(object, digits = 2))
})


test_that("acrophase differences are within (-pi, pi)", {
  # test parameter estimation of guassian simulated data
  withr::local_seed(50)

  acr_estimates <- lapply(1:50, \(...) {
    comod <- simulate_cosinor(
      100,
      mesor = 1,
      amp = 5,
      acro = 0,
      beta.mesor = 2,
      beta.amp = 2,
      beta.acro = pi,
      family = "gaussian",
      n_components = 1,
      period = 24,
      beta.group = TRUE
    )
    object <- cglmm(
      Y ~ group + amp_acro(times, n_components = 1, group = "group", period = 24),
      data = comod
    )

    test_res <- test_cosinor_levels(object, x_str = "group", param = "acr")

    test_res$ind.test$conf.int[1, 1] # return acr difference estimates
  }) |>
    unlist()

  expect_true(all(acr_estimates < pi & acr_estimates > -pi))
})
