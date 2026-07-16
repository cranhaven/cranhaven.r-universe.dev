make_ok_data <- function(n = 800) {
  set.seed(99)
  data.frame(
    y = stats::rbinom(n, 1, 0.6),
    w = stats::runif(n, 0.5, 2),
    g = sample(c("a", "b"), n, TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("check passes on valid data", {
  d <- make_ok_data()
  report <- check_calibration_data(d, "y", "w", "g")
  expect_true(report$ok)
  expect_length(report$errors, 0)
  expect_s3_class(report, "ratecalib_check")
  expect_true(all(c("metric", "value") %in% names(report$overview)))
})

test_that("check flags missing columns", {
  d <- make_ok_data()
  report <- check_calibration_data(d, "y", "w", "nonexistent")
  expect_false(report$ok)
  expect_true(any(grepl("Missing columns", report$errors)))
})

test_that("check flags non-positive weights and non-binary outcomes", {
  d <- make_ok_data()
  d$w[1] <- 0
  r1 <- check_calibration_data(d, "y", "w", "g")
  expect_false(r1$ok)
  expect_true(any(grepl("greater than 0", r1$errors)))

  d2 <- make_ok_data()
  d2$y[1] <- 2
  r2 <- check_calibration_data(d2, "y", "w", "g")
  expect_false(r2$ok)
  expect_true(any(grepl("only 0 and 1", r2$errors)))
})

test_that("check flags missing values in grouping variables", {
  d <- make_ok_data()
  d$g[1] <- NA
  report <- check_calibration_data(d, "y", "w", "g")
  expect_false(report$ok)
  expect_true(any(grepl("missing values", report$errors)))
})

test_that("check warns about all-pass / all-fail groups and unsupported targets", {
  set.seed(5)
  n <- 400
  d <- data.frame(
    y = c(rep(1, 200), stats::rbinom(200, 1, 0.5)),
    w = rep(1, n),
    g = rep(c("allpass", "mixed"), each = 200),
    stringsAsFactors = FALSE
  )
  targets <- make_rate_targets(groups = list(g = c(allpass = 0.5, mixed = 0.5)))
  report <- check_calibration_data(d, "y", "w", "g", targets = targets)

  expect_true(any(grepl("only passing", report$warnings)))
  expect_false(is.null(report$target_support))
  # The all-pass group cannot reach a target below 1.
  bad <- report$target_support[report$target_support$level == "allpass", ]
  expect_false(bad$supported)
})

test_that("check warns when group targets imply an overall rate that conflicts with the overall target", {
  # g = a,a,b,b ; weight 1 ; y = 1,0,1,0 -> W_a = W_b = 2, grand total 4
  d <- data.frame(
    y = c(1L, 0L, 1L, 0L),
    w = c(1, 1, 1, 1),
    g = c("a", "a", "b", "b"),
    stringsAsFactors = FALSE
  )
  # g implies overall (2*0.8 + 2*0.6)/4 = 0.7, but overall target is 0.5
  targets <- make_rate_targets(overall = 0.5, groups = list(g = c(a = 0.8, b = 0.6)))
  report <- check_calibration_data(d, "y", "w", "g", targets = targets)
  expect_true(report$ok)  # informational: not fatal (soft mode still solves)
  expect_true(any(grepl("overall", report$warnings, ignore.case = TRUE)))
})

test_that("check does not warn about consistency when group targets agree with the overall target", {
  d <- data.frame(
    y = c(1L, 0L, 1L, 0L),
    w = c(1, 1, 1, 1),
    g = c("a", "a", "b", "b"),
    stringsAsFactors = FALSE
  )
  targets <- make_rate_targets(overall = 0.7, groups = list(g = c(a = 0.8, b = 0.6)))
  report <- check_calibration_data(d, "y", "w", "g", targets = targets)
  expect_false(any(grepl("conflicting overall", report$warnings, ignore.case = TRUE)))
})

test_that("check tolerates sub-tolerance overall inconsistencies (round-number targets)", {
  # g implies overall 0.7; overall target 0.705 differs by only 0.005 (< 0.01).
  # Continuous weights almost never make round targets exactly consistent, so a
  # tight check would warn on nearly every realistic use; the default tolerance
  # must absorb this and warn only on material (gross) inconsistencies.
  d <- data.frame(
    y = c(1L, 0L, 1L, 0L),
    w = c(1, 1, 1, 1),
    g = c("a", "a", "b", "b"),
    stringsAsFactors = FALSE
  )
  targets <- make_rate_targets(overall = 0.705, groups = list(g = c(a = 0.8, b = 0.6)))
  report <- check_calibration_data(d, "y", "w", "g", targets = targets)
  expect_false(any(grepl("conflicting overall", report$warnings, ignore.case = TRUE)))
})

test_that("check marks targets whose variable is not a grouping variable", {
  d <- make_ok_data()
  targets <- data.frame(
    variable = "not_a_group", level = "x", target_rate = 0.5,
    stringsAsFactors = FALSE
  )
  report <- check_calibration_data(d, "y", "w", "g", targets = targets)
  expect_false(report$ok)
  expect_false(report$target_support$supported)
  expect_match(report$target_support$reason, "not in group_vars")
})
