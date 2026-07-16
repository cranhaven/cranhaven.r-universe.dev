# Tests for calibration_feasibility(): the narrowed-scope feasibility precheck.
# Two deterministic, closed-form checks:
#   1. overall-vs-group consistency identity
#   2. single-target marginal achievable interval (necessary condition)

# A tiny hand-computable fixture:
#   sex = M,M,F,F  weight = 1 each  outcome = 1,0,1,0
#   -> W_M = 2 (pass 1, fail 1), W_F = 2 (pass 1, fail 1), grand total = 4
fixture <- function() {
  data.frame(
    sex = c("M", "M", "F", "F"),
    w = c(1, 1, 1, 1),
    y = c(1L, 0L, 1L, 0L),
    stringsAsFactors = FALSE
  )
}

test_that("a complete group variable implies the overall rate via the marginal identity", {
  d <- fixture()
  targets <- make_rate_targets(groups = list(sex = c(M = 0.8, F = 0.6)))
  fz <- calibration_feasibility(d, outcome = "y", weight = "w",
                                group_vars = "sex", targets = targets)
  # implied overall = (W_M * r_M + W_F * r_F) / W_total = (2*0.8 + 2*0.6)/4 = 0.7
  pin <- fz$consistency$pins
  sex_pin <- pin$implied_overall[pin$source == "sex"]
  expect_equal(sex_pin, 0.7)
})

test_that("a conflicting explicit overall target is flagged inconsistent", {
  d <- fixture()
  # sex implies overall 0.7, but the user also asks for overall 0.5
  targets <- make_rate_targets(overall = 0.5,
                               groups = list(sex = c(M = 0.8, F = 0.6)))
  fz <- calibration_feasibility(d, outcome = "y", weight = "w",
                                group_vars = "sex", targets = targets)
  expect_false(fz$consistency$consistent)
})

test_that("a matching explicit overall target is consistent", {
  d <- fixture()
  targets <- make_rate_targets(overall = 0.7,
                               groups = list(sex = c(M = 0.8, F = 0.6)))
  fz <- calibration_feasibility(d, outcome = "y", weight = "w",
                                group_vars = "sex", targets = targets)
  expect_true(fz$consistency$consistent)
})

test_that("a partially-covered group variable does not pin the overall rate", {
  d <- fixture()
  # only M is targeted; sex is incomplete and must not create a pin/conflict
  targets <- make_rate_targets(overall = 0.5,
                               groups = list(sex = c(M = 0.8)))
  fz <- calibration_feasibility(d, outcome = "y", weight = "w",
                                group_vars = "sex", targets = targets)
  expect_false("sex" %in% fz$consistency$pins$source)
  expect_true(fz$consistency$consistent)
})

test_that("the single-target achievable interval is the two-block water-filling range", {
  d <- fixture()
  # group F: P=1, Fl=1, W=2, bounds [0.25, 4]
  #   max_pass = min(4*1, 2 - 0.25*1) = 1.75 -> max_rate = 0.875
  #   min_pass = max(0.25*1, 2 - 4*1) = 0.25 -> min_rate = 0.125
  targets <- make_rate_targets(groups = list(sex = c(M = 0.6, F = 0.6)))
  fz <- calibration_feasibility(d, outcome = "y", weight = "w",
                                group_vars = "sex", targets = targets,
                                lower = 0.25, upper = 4)
  m <- fz$marginal
  fr <- m[m$variable == "sex" & m$level == "F", ]
  expect_equal(fr$min_achievable, 0.125)
  expect_equal(fr$max_achievable, 0.875)
  expect_true(fr$within_range)
})

test_that("a target outside the marginal interval is flagged and fails necessary feasibility", {
  d <- fixture()
  # F achievable in [0.125, 0.875]; ask for 0.95 -> out of range
  targets <- make_rate_targets(groups = list(sex = c(M = 0.6, F = 0.95)))
  fz <- calibration_feasibility(d, outcome = "y", weight = "w",
                                group_vars = "sex", targets = targets)
  fr <- fz$marginal[fz$marginal$level == "F", ]
  expect_false(fr$within_range)
  expect_false(fz$necessary_ok)
})

test_that("the result carries an explicit joint-feasibility caveat", {
  d <- fixture()
  targets <- make_rate_targets(groups = list(sex = c(M = 0.6, F = 0.6)))
  fz <- calibration_feasibility(d, outcome = "y", weight = "w",
                                group_vars = "sex", targets = targets)
  expect_s3_class(fz, "ratecalib_feasibility")
  expect_true(any(grepl("joint", fz$note, ignore.case = TRUE)))
})
