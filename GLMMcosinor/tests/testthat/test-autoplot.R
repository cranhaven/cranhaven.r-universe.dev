#' @srrstats {G5.0}
#' @srrstats {G5.1}
#' @srrstats {G5.2}
#' @srrstats {G5.2a}
#' @srrstats {G5.2b}
#'
test_that("autoplot works with simple inputs and ziformula and dispformula", {
  object_zi <- cglmm(
    vit_d ~ X + amp_acro(time, group = "X", period = 12),
    data = vitamind,
    ziformula = ~ 0 + amp_acro(time, group = "X", period = 12)
  )

  object_disp <- cglmm(
    vit_d ~ X + amp_acro(time, group = "X", period = 12),
    data = vitamind,
    dispformula = ~ 0 + amp_acro(time, group = "X", period = 12),
  )

  vdiffr::expect_doppelganger(
    "plot with dispformula",
    autoplot(object_disp)
  )
  vdiffr::expect_doppelganger(
    "plot with ziformula",
    autoplot(object_zi)
  )
})

test_that("autoplot works with non-grouped model", {
  object <- cglmm(
    vit_d ~ amp_acro(time, period = 12),
    data = vitamind[vitamind$X == 1, ]
  )

  # testing autoplot with a simple model
  vdiffr::expect_doppelganger(
    "non-grouped plot with data",
    autoplot(object, superimpose.data = TRUE)
  )
  vdiffr::expect_doppelganger(
    "non-grouped plot with predict ribbon",
    autoplot(object, predict.ribbon = TRUE)
  )
})

test_that("autoplot works model including ziformula", {
  # TODO: come up with some better examples with data that are actually zero-inflated!
  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind,
    ziformula = ~X
  )

  # testing autoplot with a simple model
  vdiffr::expect_doppelganger(
    "simple-ziformula-model",
    autoplot(object, superimpose.data = TRUE, predict.ribbon = FALSE)
  )
})

test_that("autoplot works model including dispformula", {
  # TODO: come up with some better examples with data that are actually overdispersed!
  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind,
    dispformula = ~X
  )

  # testing autoplot with a simple model
  vdiffr::expect_doppelganger(
    "simple-dispformula-model",
    autoplot(object, superimpose.data = TRUE, predict.ribbon = FALSE)
  )
})


test_that("autoplot works model that has other covariates", {
  withr::local_seed(50)
  test_data <- vitamind[vitamind$X == 1, ]

  test_data$new_cat_var <- sample(
    c("a", "b", "c"),
    size = nrow(test_data),
    replace = TRUE
  )

  test_data$new_num_var <- rnorm(n = nrow(test_data))

  object <- cglmm(
    vit_d ~ amp_acro(time, period = 12) + new_cat_var + new_num_var,
    data = test_data
  )

  vdiffr::expect_doppelganger(
    "model-with-covariates",
    autoplot(
      object,
      cov_list = list(
        new_cat_var = "a",
        new_num_var = 1
      ),
      superimpose.data = TRUE
    )
  )

  vdiffr::expect_doppelganger(
    "model-with-covariates_not_specified",
    autoplot(object)
  )
  expect_message(autoplot(object, quietly = FALSE),
    regex = paste0(
      "'cov_list' was not specified, but there are",
      " covariates in the original model; the first element of each",
      " covariate column from the original dataframe will be used",
      " as reference levels:
cov_list = list(new_cat_var = 'c', new_num_var = -0.540933398827827)"
    ),
    fixed = TRUE
  )

  vdiffr::expect_doppelganger(
    "model-with-covariates_some_specified",
    autoplot(object,
      cov_list = list(new_cat_var = "c")
    )
  )

  expect_message(autoplot(object, cov_list = list(new_cat_var = "c"), quietly = FALSE))

  # test for when time is a covariate
  object_with_time_covariate <- cglmm(
    vit_d ~ time + amp_acro(time, period = 12) + new_cat_var + new_num_var,
    data = test_data
  )
  vdiffr::expect_doppelganger(
    "plot_with_time_as_covariate",
    autoplot(object_with_time_covariate)
  )

  object_with_time_covariate_2 <- cglmm(
    vit_d ~ time + X + amp_acro(time, period = 12),
    data = vitamind
  )
  vdiffr::expect_doppelganger(
    "plot_with_time_as_covariate_2",
    autoplot(object_with_time_covariate_2)
  )
})

test_that("autoplot works with simple inputs", {
  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  # testing autoplot with a simple model
  vdiffr::expect_doppelganger(
    "plot with superimposed data",
    autoplot(object, superimpose.data = TRUE)
  )
  vdiffr::expect_doppelganger(
    "plot with predict ribbon enabled",
    autoplot(object, predict.ribbon = TRUE)
  )
})


test_that("autoplot produces error messages", {
  withr::local_seed(50)
  # Testing error messages
  # test 'x_str' argument
  object <- cglmm(
    vit_d ~ 1 + amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    autoplot(object, x_str = 10),
    regex = paste(
      "'x_str' must be string corresponding to a group",
      "name in cglmm object"
    ),
    fixed = TRUE
  )

  # test plots with different groups on different components
  test_vitamind <- vitamind
  test_vitamind$Z <- rbinom(length(test_vitamind$X), 3, prob = 0.5)
  object <- cglmm(
    vit_d ~ X + amp_acro(
      time,
      n_components = 3,
      group = c("Z", NA, "X"),
      period = c(12, 10, 8)
    ),
    data = test_vitamind
  )

  vdiffr::expect_doppelganger(
    "check plots with multiple groups",
    autoplot(object, x_str = c("X", "Z"), pred.length.out = 200)
  )

  vdiffr::expect_doppelganger(
    "check a simple plot command",
    autoplot(object)
  )

  # test 'type' argument
  object <- cglmm(
    vit_d ~ 1 + amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    autoplot(object, type = 20),
    regex = paste(
      "'type' must be a string. See type in ?predict",
      "for more information about valid inputs"
    ),
    fixed = TRUE
  )

  # test 'xlims' argument
  object <- cglmm(
    vit_d ~ 1 + amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    autoplot(object, xlims = c(2, 1)),
    regex = paste(
      "'xlims' must be a vector with the first element",
      "being the lower x coordinate, and the second being",
      "the upper x coordinate"
    ),
    fixed = TRUE
  )

  # test 'pred.length.out' argument
  object <- cglmm(
    vit_d ~ 1 + amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    autoplot(object, pred.length.out = 100.5),
    regex = "'pred.length.out' must be an integer greater than 0", fixed = TRUE
  )

  # test 'superimpose.data' argument
  object <- cglmm(
    vit_d ~ 1 + amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    autoplot(object, superimpose.data = 10),
    regex = paste(
      "'superimpose.data' must be a logical argument,",
      "either TRUE or FALSE"
    ),
    fixed = TRUE
  )

  # test 'data_opacity' argument
  object <- cglmm(
    vit_d ~ 1 + amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    autoplot(object, data_opacity = 1.5),
    regex = "data_opacity' must be a number between 0 and 1 inclusive",
    fixed = TRUE
  )

  # test 'predict.ribbon' argument
  object <- cglmm(
    vit_d ~ 1 + amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    autoplot(object, predict.ribbon = 10),
    regex = "'predict.ribbon' must be a logical argument, either TRUE or FALSE",
    fixed = TRUE
  )

  # check that plots are generated correctly
  vdiffr::expect_doppelganger(
    "check predict.ribbon arg",
    autoplot(object, predict.ribbon = TRUE)
  )

  vdiffr::expect_doppelganger("check x_str arg", autoplot(object, x_str = NULL))

  vdiffr::expect_doppelganger(
    "check superimpose.data arg",
    autoplot(object, x_str = "X", superimpose.data = TRUE)
  )

  vdiffr::expect_doppelganger(
    "chceck that multiple args work",
    autoplot(object, x_str = NULL, predict.ribbon = TRUE, superimpose.data = TRUE)
  )
})

test_that("autoplot works mixed models", {
  withr::local_seed(50)

  # test mixed model plots
  f_sample_id <- function(id_num,
                          n = 30,
                          mesor,
                          amp,
                          acro,
                          family = "gaussian",
                          sd = 0.2,
                          period,
                          n_components,
                          beta.group = TRUE) {
    data <- simulate_cosinor(
      n = n,
      mesor = mesor,
      amp = amp,
      acro = acro,
      family = family,
      sd = sd,
      period = period,
      n_components = n_components
    )
    data$subject <- id_num
    data
  }

  # generate a dataset with a random effect variable comprising 30 subjects
  dat_mixed <- do.call(
    "rbind",
    lapply(1:5, function(x) {
      f_sample_id(
        id_num = x,
        mesor = rnorm(1, mean = 0, sd = 1),
        amp = rnorm(1, mean = 3, sd = 0.5),
        acro = rnorm(1, mean = 1.5, sd = 0.2),
        period = 24,
        n_components = 1
      )
    })
  )
  dat_mixed$subject <- as.factor(dat_mixed$subject)

  mixed_mod <- cglmm(
    Y ~ amp_acro(times,
      n_components = 1,
      period = 24
    ) + (1 + amp_acro1 | subject),
    data = dat_mixed
  )

  # test that the plots return expected figures for different configurations
  vdiffr::expect_doppelganger("check simple plot", autoplot(mixed_mod))
  vdiffr::expect_doppelganger(
    "test superimpose",
    autoplot(mixed_mod,
      x_str = NULL,
      superimpose.data = TRUE
    )
  )
  expect_error(autoplot(mixed_mod, ranef_plot = "group"))
  vdiffr::expect_doppelganger(
    "test ranef_plot arg",
    autoplot(mixed_mod,
      ranef_plot = "subject",
      superimpose.data = TRUE
    )
  )

  # testing a more complicated mixed model with multiple groups
  f_sample_id_2 <- function(id_num,
                            n = 30,
                            mesor,
                            amp,
                            acro,
                            family = "gaussian",
                            sd,
                            beta.sd,
                            period,
                            n_components,
                            beta.mesor,
                            beta.amp,
                            beta.acro,
                            beta.group = TRUE) {
    data <- simulate_cosinor(
      n = n,
      mesor = mesor,
      amp = amp,
      acro = acro,
      family = family,
      sd = sd,
      beta.sd = beta.sd,
      period = period,
      n_components = n_components,
      beta.mesor = beta.mesor,
      beta.amp = beta.amp,
      beta.acro = beta.acro,
      beta.group = beta.group
    )
    if (id_num %% 2 == 0) {
      # data$group = 1
      rows_to_keep <- seq_len(nrow(data) %/% 2)
      data <- data[-rows_to_keep, ]
    } else {
      # data$group =0
      rows_to_delete <- seq_len(nrow(data) %/% 2)
      data <- data[rows_to_delete, ]
    }
    data$subject <- id_num
    data
  }

  dat_mixed_2 <- do.call(
    "rbind",
    lapply(1:15, function(x) {
      f_sample_id_2(
        id_num = x,
        mesor = rnorm(1, mean = 0, sd = 1),
        amp = rnorm(1, mean = 3, sd = 0.5),
        acro = rnorm(1, mean = 1.5, sd = 0.2),
        beta.mesor = rnorm(1, mean = 10, sd = 1),
        beta.amp = rnorm(1, mean = 5, sd = 0.5),
        beta.acro = rnorm(1, mean = 1.5, sd = 0.2),
        period = 24,
        n_components = 1,
        sd = 0.2,
        beta.sd = 0.2
      )
    })
  )

  dat_mixed_2$subject <- as.factor(dat_mixed_2$subject)

  mixed_mod_2 <- cglmm(
    Y ~ group + amp_acro(times, n_components = 1, period = 24, group = "group") +
      (1 + amp_acro1 | subject),
    data = dat_mixed_2
  )

  # test that the plots return expected figures for different configurations
  vdiffr::expect_doppelganger(
    "test_complex_mixed_mod",
    autoplot(mixed_mod_2, superimpose.data = TRUE, ranef_plot = "subject")
  )

  vdiffr::expect_doppelganger(
    "test group arg",
    autoplot(
      mixed_mod_2,
      x_str = "group",
      superimpose.data = TRUE,
      ranef_plot = "subject"
    )
  )
  vdiffr::expect_doppelganger(
    "test superimpose arg",
    autoplot(mixed_mod_2, x_str = "group", superimpose.data = TRUE)
  )
})
