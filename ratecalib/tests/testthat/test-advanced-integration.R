# check_calibration_data() and calibration_feasibility() were written for the
# legacy single-dimension proportion-on-outcome targets. These tests pin down
# that they now tolerate the advanced target types (interaction, mean/total,
# non-outcome proportion) by analysing only the simple targets and skipping the
# rest instead of misjudging them.

mk <- function(n = 800) {
  set.seed(7)
  data.frame(
    sex = sample(c("M", "F"), n, TRUE),
    residence = sample(c("Urban", "Rural"), n, TRUE),
    income = stats::rlnorm(n, log(5e4), 0.3),
    y = stats::rbinom(n, 1, 0.6),
    w = stats::runif(n, 0.5, 2),
    stringsAsFactors = FALSE
  )
}

test_that("check_calibration_data does not flag an interaction target as invalid", {
  d <- mk()
  targets <- make_rate_targets(
    groups = list(sex = c(M = 0.7, F = 0.65)),
    interactions = list("sex:residence" = c("M:Urban" = 0.72)))
  report <- check_calibration_data(d, "y", "w", c("sex", "residence"), targets)
  expect_true(report$ok)
  # the interaction row must not be reported as "not in group_vars"
  ts <- report$target_support
  inter <- ts[ts$variable == "sex:residence", ]
  expect_false(any(grepl("not in group_vars", inter$reason)))
})

test_that("check_calibration_data tolerates mean/total targets without erroring", {
  d <- mk()
  targets <- make_rate_targets(
    overall = 0.7,
    means = data.frame(variable = ".overall", level = ".all",
                       value_var = "income", target = 50000,
                       stringsAsFactors = FALSE))
  report <- check_calibration_data(d, "y", "w", "sex", targets)
  expect_true(report$ok)
})

test_that("calibration_feasibility analyses simple targets and skips advanced ones", {
  d <- mk()
  targets <- make_rate_targets(
    overall = 0.62,
    groups = list(sex = c(M = 0.66, F = 0.60)),
    interactions = list("sex:residence" = c("M:Urban" = 0.72)),
    means = data.frame(variable = ".overall", level = ".all",
                       value_var = "income", target = 50000,
                       stringsAsFactors = FALSE))
  fz <- calibration_feasibility(d, "y", "w", c("sex", "residence"), targets)
  # the simple overall + sex targets are analysed (consistency pins exist)
  expect_true(".overall" %in% fz$consistency$pins$source)
  expect_true("sex" %in% fz$consistency$pins$source)
  # the interaction and mean rows are NOT analysed as marginal intervals
  expect_false("sex:residence" %in% fz$marginal$variable)
  # a note records that some targets were skipped
  expect_true(any(grepl("not analy", fz$note, ignore.case = TRUE)))
})

test_that("make_rate_targets builds proportion-of-value rows via proportions=", {
  tg <- make_rate_targets(
    proportions = data.frame(variable = ".overall", level = ".all",
                             value_var = "grade", value = "A", target = 0.4,
                             stringsAsFactors = FALSE))
  expect_true(all(c("statistic", "value_var", "value") %in% names(tg)))
  row <- tg[tg$value_var == "grade", ]
  expect_equal(row$statistic, "proportion")
  expect_equal(row$value, "A")
  expect_equal(row$target_rate, 0.4)
})
