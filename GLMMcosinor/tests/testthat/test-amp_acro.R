#' @srrstats {G5.0}
#' @srrstats {G5.1}
#' @srrstats {G5.2}
#' @srrstats {G5.2a}
#' @srrstats {G5.2b}
#' @srrstats {G2.12}

test_that("example amp_acro object", {
  multi_component_amp_acro <- amp_acro(
    time_col = time,
    n_components = 2,
    group = "X",
    period = c(24, 12),
    .data = vitamind,
    .formula = vit_d ~ X + amp_acro("time",
      n_components = 2,
      group = "X",
      period = c(24, 12)
    )
  )

  expect_amp_acro(multi_component_amp_acro)
})

test_that("bad inputs return useful errors", {
  # non-count n_components
  expect_error(
    amp_acro(
      time_col = time, n_components = 1.1, group = "X", period = 12,
      .data = vitamind,
      .formula = vit_d ~ X +
        amp_acro(time, n_components = 1.1, group = "X", period = 12)
    ),
    regexp = "assertthat::is.count(n_components) is not TRUE", fixed = TRUE
  )

  # negative period
  expect_error(
    amp_acro(
      time_col = time, n_components = 1, group = "X", period = -12,
      .data = vitamind,
      .formula = vit_d ~ X +
        amp_acro(time, n_components = 1, group = "X", period = -12)
    ),
    regexp = "period > 0"
  )


  # time as factor
  vitamind_mod <- vitamind
  vitamind_mod$time <- as.factor(rbinom(length(vitamind$time), 1, 0.5))

  expect_error(
    amp_acro(
      time_col = time, n_components = 1, group = "X", period = 12,
      .data = vitamind_mod,
      .formula = vit_d ~ X +
        amp_acro(time, n_components = 1, group = "X", period = 12)
    ),
    regexp = "ttt is not a numeric or integer vector"
  )

  # two time columns
  vitamind_2 <- vitamind
  vitamind_2$time <- cbind(vitamind_2$time, vitamind_2$time)
  expect_error(
    amp_acro(
      time_col = time, n_components = 1, group = "X", period = 12,
      .data = vitamind_2,
      .formula = vit_d ~ X +
        amp_acro(time, n_components = 1, group = "X", period = 12)
    ),
    regexp = "time_col must be univariate"
  )

  # bad time_col arg
  expect_error(
    amp_acro(
      time_col = time_values, n_components = 1, group = "X", period = 12,
      .data = vitamind,
      .formula = vit_d ~ X +
        amp_acro(time, n_components = 1, group = "X", period = 12)
    ),
    regexp = "time_col must be the name of a column in dataframe"
  )

  # bad form of input data
  vitamind_mod <- as.list(vitamind)
  expect_error(
    amp_acro(
      time_col = time, n_components = 1, group = "X", period = 12,
      .data = vitamind_mod,
      .formula = vit_d ~ X +
        amp_acro(time, n_components = 1, group = "X", period = 12)
    ),
    regexp = "'data' must be of class 'data.frame', 'matrix', or 'tibble'"
  )

  # conflicting variable name with internals
  vitamind_mod <- vitamind
  colnames(vitamind_mod)[1] <- "rrr2"
  expect_error(
    amp_acro(
      time_col = time, n_components = 1, group = "rrr2", period = 12,
      .data = vitamind_mod,
      .formula = vit_d ~ X +
        amp_acro(time, n_components = 1, group = "rrr2", period = 12)
    ),
    regexp = "Group variable names cannot contain 'rrr' or 'sss'"
  )

  # can't have two grouping vars and a single component
  vitamind_two_groups <- vitamind
  vitamind_two_groups["X2"] <- vitamind_two_groups["X"]
  expect_error(
    amp_acro(
      time_col = time, n_components = 1, group = c("X", "X2"), period = 12,
      .data = vitamind_two_groups,
      .formula = vit_d ~ X +
        amp_acro(time, n_components = 1, group = c("X", "X2"), period = 12)
    ),
    regexp = paste(
      "Grouping variable in amp_acro() must be of",
      "length 1 or the same as n_components"
    ),
    fixed = TRUE
  )

  # mismatch between period arg and n_components
  expect_error(
    amp_acro(
      time_col = time, n_components = 1, group = "X", period = c(8, 12),
      .data = vitamind,
      .formula = vit_d ~ X +
        amp_acro(time, n_components = 1, group = "X", period = c(8, 12))
    ),
    regexp = paste(
      "period value(s) in amp_acro() must be of",
      "length 1 or the same as n_components"
    ),
    fixed = TRUE
  )

  # missing variable in data
  expect_error(
    amp_acro(
      time_col = time, n_components = 1, group = "Z", period = c(8, 12),
      .data = vitamind,
      .formula = vit_d ~ X +
        amp_acro(time, n_components = 1, group = "Z", period = c(8, 12))
    ),
    regexp = "Grouping variable(s) not found in input data:",
    fixed = TRUE
  )
})

test_that("matrix, or tibble inputs are converted to dataframe ", {
  withr::local_seed(50)
  vitamind_mod <- dplyr::as_tibble(vitamind)

  f <- function() {
    amp_acro(
      time_col = time, n_components = 1, group = "X", period = 12,
      .data = vitamind_mod,
      .formula = vit_d ~ X +
        amp_acro(time, n_components = 1, group = "X", period = 12),
      .quietly = FALSE
    )
  }
  suppressMessages(expect_message(f(),
    regexp = "main_rrr1 and main_sss1 have been added to dataframe"
  ))

  suppressMessages(expect_message(f(),
    regexp = "Data has been reformatted as dataframe"
  ))

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

  dat_mixed <- do.call(
    "rbind",
    lapply(1:30, function(x) {
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

  f_round <- function(x) {
    unname(round(x, digits = 4))
  }


  # Testing mixed model specification
  expect_no_error(
    update_formula_and_data(
      formula = Y ~ amp_acro(times, n_components = 1, period = 24) +
        (1 + amp_acro1 * treatment * hospital:patient | subject),
      data = dat_mixed
    )
  )

  # Testing mixed model specification
  expect_no_error(
    update_formula_and_data(
      formula = Y ~ amp_acro(times, n_components = 1, period = 24) +
        (amp_acro1 * treatment * hospital:patient + 1 | subject),
      data = dat_mixed
    )
  )

  expect_no_error(
    update_formula_and_data(
      formula = Y ~ amp_acro(times, n_components = 2, period = c(12, 6)) +
        (amp_acro1 * treatment * hospital:patient + 1 +
          amp_acro2 * treatment | subject),
      data = dat_mixed
    )
  )

  expect_snapshot(f_round(mixed_mod$coefficients))
})
