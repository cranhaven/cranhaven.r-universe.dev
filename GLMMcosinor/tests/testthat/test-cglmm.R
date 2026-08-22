#' @srrstats {G5.0}
#' @srrstats {G5.1}
#' @srrstats {G5.2}
#' @srrstats {G5.2a}
#' @srrstats {G5.2b}
#' @srrstats {G5.5}
#' @srrstats {G5.4}
#' @srrstats {G5.4a}
#' @srrstats {G5.4b}
#' @srrstats {G5.6}
#' @srrstats {G5.6a}
#' @srrstats {G5.6b}
#' @srrstats {G5.9b}
#' @srrstats {G2.11}

test_that("model returns accurate parameters", {
  withr::local_seed(50)

  f_round <- function(x) {
    unname(round(x, digits = 4))
  }

  TrueMesor_a <- 1
  TrueMesor_b <- 0.5
  TrueAmp_a <- 2
  TrueAmp_b <- 1
  TrueAcr_a <- 3
  TrueAcr_b <- 0.3
  TruePeriod <- 12

  # test parameter estimation of Gaussian simulated data
  comod <- simulate_cosinor(
    n = 10000,
    mesor = TrueMesor_a,
    amp = TrueAmp_a,
    acro = TrueAcr_a,
    beta.mesor = TrueMesor_b,
    beta.amp = TrueAmp_b,
    beta.acro = TrueAcr_b,
    family = "gaussian",
    period = TruePeriod,
    beta.group = TRUE
  )
  object <- cglmm(
    Y ~ group + amp_acro(times, n_components = 1, group = "group", period = 12),
    data = comod
  )
  sum_glm <- summary(object)
  SE_sum_glm <- round(sum_glm$transformed.table$standard.error, digits = 4)

  cosinor_lm_mod <- cosinor::cosinor.lm(
    Y ~ time(times) + group + amp.acro(group),
    data = comod
  )
  sum_lm <- summary(cosinor_lm_mod)
  SE_sum_lm <- round(sum_lm$transformed.table$standard.error, digits = 4)

  expect_true(all.equal(
    SE_sum_glm,
    SE_sum_lm
  ))
  expect_snapshot(f_round(object$coefficients))

  # test similarity to cosinor::cosinor.lm()
  comparison_df <- cbind(
    cosinor_lm_mod$coefficients,
    object$coefficients
  )

  comparison_df <- as.data.frame(
    comparison_df[rownames(comparison_df) != "acr", ]
  )
  expect_equal(comparison_df$V1, comparison_df$V2, tolerance = 0.1)



  # test another parameter estimation of Gaussian simulated data
  comod <- simulate_cosinor(
    n = 10000,
    mesor = TrueMesor_a,
    amp = TrueAmp_a,
    acro = TrueAcr_a,
    beta.mesor = TrueMesor_b,
    beta.amp = TrueAmp_b,
    beta.acro = TrueAcr_b,
    family = "gaussian",
    period = TruePeriod,
    beta.group = TRUE
  )
  object <- cglmm(
    Y ~ group + amp_acro(times, n_components = 1, group = "group", period = 12),
    data = comod
  )

  expect_snapshot(f_round(object$coefficients))

  # test parameter estimation of poisson simulated data
  comod <- simulate_cosinor(
    n = 10000,
    mesor = TrueMesor_a,
    amp = TrueAmp_a,
    acro = TrueAcr_a,
    beta.mesor = TrueMesor_b,
    beta.amp = TrueAmp_b,
    beta.acro = TrueAcr_b,
    family = "poisson",
    period = TruePeriod,
    beta.group = TRUE
  )
  object <- cglmm(
    Y ~ group + amp_acro(times, n_components = 1, group = "group", period = 12),
    data = comod,
    family = poisson
  )

  expect_snapshot(f_round(object$coefficients))

  # test parameter estimation of Gamma(link="log") simulated data
  comod <- simulate_cosinor(
    n = 10000,
    mesor = TrueMesor_a,
    amp = TrueAmp_a,
    acro = TrueAcr_a,
    beta.mesor = TrueMesor_b,
    beta.amp = TrueAmp_b,
    beta.acro = TrueAcr_b,
    family = "gamma",
    period = TruePeriod,
    beta.group = TRUE
  )
  object <- cglmm(
    Y ~ group + amp_acro(times, n_components = 1, group = "group", period = 12),
    data = comod,
    family = Gamma(link = "log")
  )
  expect_snapshot(f_round(object$coefficients))

  # test parameter estimation of binomial simulated data
  comod <- simulate_cosinor(
    n = 10000,
    mesor = TrueMesor_a,
    amp = TrueAmp_a,
    acro = TrueAcr_a,
    beta.mesor = TrueMesor_b,
    beta.amp = TrueAmp_b,
    beta.acro = TrueAcr_b,
    family = "binomial",
    period = TruePeriod,
    beta.group = TRUE
  )
  object <- cglmm(
    Y ~ group + amp_acro(times, n_components = 1, group = "group", period = 12),
    data = comod,
    family = binomial
  )

  expect_snapshot(f_round(object$coefficients))
})
test_that("model output is class cglmm", {
  withr::local_seed(50)

  object <- cglmm(
    vit_d ~ X + amp_acro(time, group = "X", period = 12),
    data = vitamind
  )
  expect_s3_class(object, "cglmm")

  object <- cglmm(
    vit_d ~ X + amp_acro(time, group = "X", period = 12),
    data = vitamind,
    dispformula = ~ 0 + amp_acro(time, group = "X", period = 12),
    ziformula = ~ 0 + amp_acro(time, group = "X", period = 12)
  )
  expect_no_error(object)
  expect_snapshot(print(object, digits = 2))
  expect_s3_class(object, "cglmm")
  expect_true(inherits(object, "cglmm"))

  #' @srrstats {RE7.2}
  #' @srrstats {RE7.3}

  # check if the column names from vitamind are present in object_cols
  vitamind_cols <- colnames(vitamind)
  object_cols <- colnames(object$newdata)
  expect_true(all(vitamind_cols %in% object_cols))

  # test that coefficients and formula can be accessed from object
  expect_no_error(coefficients(object))
  expect_no_error(formula(object))

  # testing mixed model specification
  expect_warning(
    cglmm(
      vit_d ~ X +
        amp_acro(time, n_components = 1, group = "X", period = 12) +
        (1 | X) + (0 + amp_acro1 | X),
      data = vitamind
    )
  )

  sim_data <- simulate_cosinor(
    n = 500,
    mesor = 5,
    amp = c(2, 1),
    acro = c(1, 1.5),
    beta.mesor = 2,
    beta.amp = c(2, 1),
    beta.acro = c(1, 1.5),
    family = "gaussian",
    period = c(12, 6),
    n_components = 2,
    beta.group = TRUE,
  )

  suppressWarnings({
    object <- cglmm(
      Y ~ group +
        amp_acro(times, n_components = 2, group = "group", period = c(6, 12)) +
        (0 + amp_acro2 | group),
      data = sim_data,
      family = gaussian
    )
  })

  expect_equal(
    ignore_attr = TRUE,
    object$formula,
    Y ~ group + group:main_rrr1 + group:main_sss1 + group:main_rrr2 +
      group:main_sss2 + (0 + main_rrr2 + main_sss2 | group)
  )
  expect_snapshot(print(object, digits = 2))
})


test_that("mixed model estimates parameters well", {
  withr::local_seed(42)
  f_sample_id <- function(id_num,
                          n = 30,
                          mesor = rnorm(1),
                          amp = c(1, rnorm(n = 1, mean = 5, sd = 1)),
                          acro = c(1, rnorm(n = 1, mean = pi, sd = 1)),
                          family = "gaussian",
                          sd = 0.2,
                          period = c(12, 6),
                          n_components = 2,
                          beta.group = TRUE) {
    data <- simulate_cosinor(
      n = n,
      mesor = mesor,
      amp = amp,
      acro = acro,
      family = family,
      sdv = sd,
      period = period,
      n_components = n_components
    )

    data$id <- id_num
    data
  }

  df_mixed <- dplyr::bind_rows(lapply(1:75, f_sample_id))

  object <- cglmm(
    Y ~ amp_acro(times, n_components = 2, period = c(12, 6)) +
      (0 + amp_acro2 | id),
    data = dplyr::mutate(df_mixed, id = as.factor(id)),
    family = gaussian
  )

  expect_s3_class(object, "cglmm")
})


test_that("alternative inputs work", {
  expect_no_error(cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  ))
  expect_no_error(cglmm(
    vit_d ~ amp_acro(time, group = X, period = 12),
    data = vitamind
  ))

  expect_no_error(cglmm(
    vit_d ~ amp_acro("time", group = X, period = 12),
    data = vitamind
  ))
})

test_that("specifying no amp_acro term works", {
  withr::local_seed(50)


  expect_no_error_and_snapshot <- function(f) {
    expect_no_error(f())
    expect_snapshot(f())
  }

  vitamind2 <- lapply(1:5, \(x) vitamind) |>
    dplyr::bind_rows() |>
    dplyr::rowwise() |>
    dplyr::mutate(vit_d = vit_d + stats::rnorm(n = 1))

  fit_disp_model <- function() {
    cglmm(
      vit_d ~ X + amp_acro(time, group = "X", period = 12),
      data = vitamind2,
      dispformula = ~X
    )
  }

  expect_no_error_and_snapshot(fit_disp_model)

  fit_zi_model <- function() {
    cglmm(
      vit_d ~ X + amp_acro(time, group = "X", period = 12),
      data = vitamind2,
      ziformula = ~X
    )
  }

  expect_no_error_and_snapshot(fit_zi_model)

  fit_disp_and_zi_model <- function() {
    cglmm(
      vit_d ~ X + amp_acro(time, group = "X", period = 12),
      data = vitamind2,
      dispformula = ~X,
      ziformula = ~X
    )
  }

  expect_no_error_and_snapshot(fit_disp_and_zi_model)
})
