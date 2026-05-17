#' @srrstats {G5.2} *Error and warning behaviour is explicitly demonstrated through tests.*
#' @srrstats {G5.2a} *Every error message is unique and tested.*
#' @srrstats {G5.2b} *Tests trigger every error message and compare with expected values.*
#' @srrstats {G5.3} *Return objects tested for absence of NA, NaN, Inf.*
#' @srrstats {G5.6} *Parameter recovery tests verify implementations produce expected results given data with known properties.*
#' @srrstats {G5.6a} *Parameter recovery tests succeed within defined tolerance rather than exact values.*
#' @srrstats {G5.6b} *Parameter recovery tests run with multiple random seeds when randomness is involved.*
#' @srrstats {G5.7} *Algorithm performance tests verify implementations perform correctly as data properties change.*
#' @srrstats {G5.8} *Edge condition tests verify appropriate behavior with extreme data properties.*
#' @srrstats {G5.8a} *Zero-length data tests trigger clear errors.*
#' @srrstats {G5.8b} *Unsupported data type tests trigger clear errors.*
#' @srrstats {G5.8c} *All-NA and all-identical data tests trigger clear errors or warnings.*
#' @srrstats {G5.8d} *Out-of-scope data tests verify appropriate behavior.*
#' @srrstats {G5.9} *Noise susceptibility tests verify stochastic behavior stability.*
#' @srrstats {G5.9a} *Trivial noise tests show results are stable at machine epsilon scale.*
#' @srrstats {G5.9b} *Random seed stability tests show consistent behavior across different seeds.*

test_that("Planned Value (PV) calculation is correct", {
  bac <- 100000
  schedule <- c(0.1, 0.2, 0.4, 0.7, 1.0)
  time_period <- 3

  pv_value <- pv(bac, schedule, time_period)
  expect_equal(pv_value, 40000)
})

test_that("Earned Value (EV) calculation is correct", {
  bac <- 100000
  actual_per_complete <- 0.35

  ev_value <- ev(bac, actual_per_complete)
  expect_equal(ev_value, 35000)
})

test_that("Actual Cost (AC) calculation is correct", {
  actual_costs <- c(9000, 18000, 36000, 70000, 100000)
  time_period <- 3

  ac_value <- ac(actual_costs, time_period)
  expect_equal(ac_value, 36000)
})

test_that("Schedule Variance (SV) calculation is correct", {
  ev_value <- 35000
  pv_value <- 40000

  sv_value <- sv(ev_value, pv_value)
  expect_equal(sv_value, -5000)
})

test_that("Cost Variance (CV) calculation is correct", {
  ev_value <- 35000
  ac_value <- 36000

  cv_value <- cv(ev_value, ac_value)
  expect_equal(cv_value, -1000)
})

test_that("Schedule Performance Index (SPI) calculation is correct", {
  ev_value <- 35000
  pv_value <- 40000

  spi_value <- spi(ev_value, pv_value)
  expect_equal(spi_value, 0.875)
})

test_that("Cost Performance Index (CPI) calculation is correct", {
  ev_value <- 35000
  ac_value <- 36000

  cpi_value <- cpi(ev_value, ac_value)
  expect_equal(cpi_value, 0.9722222, tolerance = 1e-6)
})

# ============================================================================
# PV Error Handling Tests
# ============================================================================
test_that("pv validates NULL inputs", {
  expect_error(pv(NULL, c(0.1, 0.2), 1), "bac, schedule, and time_period must not be NULL")
  expect_error(pv(100000, NULL, 1), "bac, schedule, and time_period must not be NULL")
  expect_error(pv(100000, c(0.1, 0.2), NULL), "bac, schedule, and time_period must not be NULL")
})

test_that("pv validates numeric inputs", {
  expect_error(pv("100000", c(0.1, 0.2), 1), "bac, schedule, and time_period must be numeric")
  expect_error(pv(100000, "schedule", 1), "bac, schedule, and time_period must be numeric")
  expect_error(pv(100000, c(0.1, 0.2), "1"), "bac, schedule, and time_period must be numeric")
})

test_that("pv validates single value inputs", {
  expect_error(pv(c(100000, 200000), c(0.1, 0.2), 1), "bac must be a single numeric value")
  expect_error(pv(100000, c(0.1, 0.2), c(1, 2)), "time_period must be a single numeric value")
})

test_that("pv validates NA inputs", {
  # NA alone is logical, so it fails numeric check first; use NA_real_ for numeric NA
  expect_error(pv(NA_real_, c(0.1, 0.2), 1), "bac, schedule, and time_period must not contain NA values")
  expect_error(pv(100000, c(0.1, NA), 1), "bac, schedule, and time_period must not contain NA values")
  expect_error(pv(100000, c(0.1, 0.2), NA_real_), "bac, schedule, and time_period must not contain NA values")
})

test_that("pv rejects NaN inputs", {
  expect_error(pv(NaN, c(0.1, 0.2), 1), "bac, schedule, and time_period must not contain NaN values")
  expect_error(pv(100000, c(0.1, NaN), 1), "bac, schedule, and time_period must not contain NaN values")
  expect_error(pv(100000, c(0.1, 0.2), NaN), "bac, schedule, and time_period must not contain NaN values")
})

test_that("pv rejects Inf inputs", {
  expect_error(pv(Inf, c(0.1, 0.2), 1), "bac, schedule, and time_period must not contain infinite values")
  expect_error(pv(100000, c(0.1, Inf), 1), "bac, schedule, and time_period must not contain infinite values")
  expect_error(pv(100000, c(0.1, 0.2), Inf), "bac, schedule, and time_period must not contain infinite values")
})

test_that("pv validates schedule range", {
  expect_error(pv(100000, c(0.1, 1.5), 1), "schedule values must be between 0 and 1")
  expect_error(pv(100000, c(-0.1, 0.5), 1), "schedule values must be between 0 and 1")
})

test_that("pv validates time_period range", {
  expect_error(pv(100000, c(0.1, 0.2, 0.5), 0), "time_period must be within the range of the schedule vector")
  expect_error(pv(100000, c(0.1, 0.2, 0.5), 5), "time_period must be within the range of the schedule vector")
})

test_that("pv validates non-negative bac", {
  expect_error(pv(-100000, c(0.1, 0.2), 1), "bac must be non-negative")
})

test_that("pv validates non-empty schedule", {
  expect_error(pv(100000, numeric(0), 1), "schedule must not be empty")
})

# ============================================================================
# EV Error Handling Tests
# ============================================================================
test_that("ev validates NULL inputs", {
  expect_error(ev(NULL, 0.5), "bac and actual_per_complete must not be NULL")
  expect_error(ev(100000, NULL), "bac and actual_per_complete must not be NULL")
})

test_that("ev validates numeric inputs", {
  expect_error(ev("100000", 0.5), "bac and actual_per_complete must be numeric")
  expect_error(ev(100000, "0.5"), "bac and actual_per_complete must be numeric")
})

test_that("ev validates single value inputs", {
  expect_error(ev(c(100000, 200000), 0.5), "bac must be a single numeric value")
  expect_error(ev(100000, c(0.3, 0.5)), "actual_per_complete must be a single numeric value")
})

test_that("ev rejects NaN inputs", {
  expect_error(ev(NaN, 0.5), "bac and actual_per_complete must not be NaN")
  expect_error(ev(100000, NaN), "bac and actual_per_complete must not be NaN")
})

test_that("ev rejects NA inputs", {
  expect_error(ev(NA_real_, 0.5), "bac and actual_per_complete must not be NA")
  expect_error(ev(100000, NA_real_), "bac and actual_per_complete must not be NA")
})

test_that("ev rejects Inf inputs", {
  expect_error(ev(Inf, 0.5), "bac and actual_per_complete must not be infinite")
  expect_error(ev(100000, Inf), "bac and actual_per_complete must not be infinite")
})

test_that("ev validates percentage range", {
  expect_error(ev(100000, 1.5), "actual_per_complete must be between 0 and 1")
  expect_error(ev(100000, -0.1), "actual_per_complete must be between 0 and 1")
})

test_that("ev validates non-negative bac", {
  expect_error(ev(-100000, 0.5), "bac must be non-negative")
})

# ============================================================================
# AC Tests
# ============================================================================
test_that("ac works with cumulative costs (default)", {
  cumulative_costs <- c(9000, 27000, 63000)
  expect_equal(ac(cumulative_costs, 2), 27000)
  expect_equal(ac(cumulative_costs, 3), 63000)
})

test_that("ac works with period costs", {
  period_costs <- c(9000, 18000, 36000)
  expect_equal(ac(period_costs, 2, cumulative = FALSE), 27000)
  expect_equal(ac(period_costs, 3, cumulative = FALSE), 63000)
})

test_that("ac validates NULL inputs", {
  expect_error(ac(NULL, 1), "actual_costs and time_period must not be NULL")
  expect_error(ac(c(1000, 2000), NULL), "actual_costs and time_period must not be NULL")
})

test_that("ac validates cumulative parameter", {
  expect_error(ac(c(1000, 2000), 1, cumulative = "yes"), "cumulative must be a single logical value")
  expect_error(ac(c(1000, 2000), 1, cumulative = c(TRUE, FALSE)), "cumulative must be a single logical value")
})

test_that("ac validates numeric inputs", {
  expect_error(ac("not numeric", 1), "actual_costs and time_period must be numeric")
  expect_error(ac(c(1000, 2000), "1"), "actual_costs and time_period must be numeric")
})

test_that("ac validates single value time_period", {
  expect_error(ac(c(1000, 2000), c(1, 2)), "time_period must be a single numeric value")
})

test_that("ac rejects NaN inputs", {
  expect_error(ac(c(NaN, 2000), 1), "actual_costs and time_period must not contain NaN values")
  expect_error(ac(c(1000, 2000), NaN), "actual_costs and time_period must not contain NaN values")
})

test_that("ac rejects NA inputs", {
  expect_error(ac(c(NA_real_, 2000), 1), "actual_costs and time_period must not contain NA values")
  expect_error(ac(c(1000, 2000), NA_real_), "actual_costs and time_period must not contain NA values")
})

test_that("ac rejects Inf inputs", {
  expect_error(ac(c(Inf, 2000), 1), "actual_costs and time_period must not contain infinite values")
  expect_error(ac(c(1000, 2000), Inf), "actual_costs and time_period must not contain infinite values")
})

test_that("ac validates non-empty costs", {
  expect_error(ac(numeric(0), 1), "actual_costs must not be empty")
})

test_that("ac validates time_period range", {
  expect_error(ac(c(1000, 2000), 0), "time_period must be within the range of the actual_costs vector")
  expect_error(ac(c(1000, 2000), 5), "time_period must be within the range of the actual_costs vector")
})

test_that("ac validates non-negative costs", {
  expect_error(ac(c(1000, -2000), 1), "actual_costs must be non-negative")
})

# ============================================================================
# SV Error Handling Tests
# ============================================================================
test_that("sv validates NULL inputs", {
  expect_error(sv(NULL, 40000), "ev and pv must not be NULL")
  expect_error(sv(35000, NULL), "ev and pv must not be NULL")
})

test_that("sv validates numeric inputs", {
  expect_error(sv("35000", 40000), "ev and pv must be numeric")
  expect_error(sv(35000, "40000"), "ev and pv must be numeric")
})

test_that("sv validates single value inputs", {
  expect_error(sv(c(35000, 36000), 40000), "ev and pv must be single numeric values")
  expect_error(sv(35000, c(40000, 41000)), "ev and pv must be single numeric values")
})

test_that("sv rejects NaN inputs", {
  expect_error(sv(NaN, 40000), "ev and pv must not be NaN")
  expect_error(sv(35000, NaN), "ev and pv must not be NaN")
})

test_that("sv rejects NA inputs", {
  expect_error(sv(NA_real_, 40000), "ev and pv must not be NA")
  expect_error(sv(35000, NA_real_), "ev and pv must not be NA")
})

test_that("sv rejects Inf inputs", {
  expect_error(sv(Inf, 40000), "ev and pv must not be infinite")
  expect_error(sv(35000, Inf), "ev and pv must not be infinite")
})

test_that("sv validates non-negative inputs", {
  expect_error(sv(-35000, 40000), "ev must be non-negative")
  expect_error(sv(35000, -40000), "pv must be non-negative")
})

# ============================================================================
# CV Error Handling Tests
# ============================================================================
test_that("cv validates NULL inputs", {
  expect_error(cv(NULL, 36000), "ev and ac must not be NULL")
  expect_error(cv(35000, NULL), "ev and ac must not be NULL")
})

test_that("cv validates numeric inputs", {
  expect_error(cv("35000", 36000), "ev and ac must be numeric")
  expect_error(cv(35000, "36000"), "ev and ac must be numeric")
})

test_that("cv validates single value inputs", {
  expect_error(cv(c(35000, 36000), 36000), "ev and ac must be single numeric values")
  expect_error(cv(35000, c(36000, 37000)), "ev and ac must be single numeric values")
})

test_that("cv rejects NaN inputs", {
  expect_error(cv(NaN, 36000), "ev and ac must not be NaN")
  expect_error(cv(35000, NaN), "ev and ac must not be NaN")
})

test_that("cv rejects NA inputs", {
  expect_error(cv(NA_real_, 36000), "ev and ac must not be NA")
  expect_error(cv(35000, NA_real_), "ev and ac must not be NA")
})

test_that("cv rejects Inf inputs", {
  expect_error(cv(Inf, 36000), "ev and ac must not be infinite")
  expect_error(cv(35000, Inf), "ev and ac must not be infinite")
})

test_that("cv validates non-negative inputs", {
  expect_error(cv(-35000, 36000), "ev must be non-negative")
  expect_error(cv(35000, -36000), "ac must be non-negative")
})

# ============================================================================
# SPI Error Handling Tests
# ============================================================================
test_that("spi validates NULL inputs", {
  expect_error(spi(NULL, 40000), "ev and pv must not be NULL")
  expect_error(spi(35000, NULL), "ev and pv must not be NULL")
})

test_that("spi validates numeric inputs", {
  expect_error(spi("35000", 40000), "ev and pv must be numeric")
  expect_error(spi(35000, "40000"), "ev and pv must be numeric")
})

test_that("spi validates single value inputs", {
  expect_error(spi(c(35000, 36000), 40000), "ev and pv must be single numeric values")
  expect_error(spi(35000, c(40000, 41000)), "ev and pv must be single numeric values")
})

test_that("spi rejects NaN inputs", {
  expect_error(spi(NaN, 40000), "ev and pv must not be NaN")
  expect_error(spi(35000, NaN), "ev and pv must not be NaN")
})

test_that("spi rejects NA inputs", {
  expect_error(spi(NA_real_, 40000), "ev and pv must not be NA")
  expect_error(spi(35000, NA_real_), "ev and pv must not be NA")
})

test_that("spi rejects Inf inputs", {
  expect_error(spi(Inf, 40000), "ev and pv must not be infinite")
  expect_error(spi(35000, Inf), "ev and pv must not be infinite")
})

test_that("spi validates ev non-negative", {
  expect_error(spi(-35000, 40000), "ev must be non-negative")
})

test_that("spi validates pv greater than zero", {
  expect_error(spi(35000, 0), "pv must be greater than zero")
  expect_error(spi(35000, -40000), "pv must be greater than zero")
})

# ============================================================================
# CPI Error Handling Tests
# ============================================================================
test_that("cpi validates NULL inputs", {
  expect_error(cpi(NULL, 36000), "ev and ac must not be NULL")
  expect_error(cpi(35000, NULL), "ev and ac must not be NULL")
})

test_that("cpi validates numeric inputs", {
  expect_error(cpi("35000", 36000), "ev and ac must be numeric")
  expect_error(cpi(35000, "36000"), "ev and ac must be numeric")
})

test_that("cpi validates single value inputs", {
  expect_error(cpi(c(35000, 36000), 36000), "ev and ac must be single numeric values")
  expect_error(cpi(35000, c(36000, 37000)), "ev and ac must be single numeric values")
})

test_that("cpi rejects NaN inputs", {
  expect_error(cpi(NaN, 36000), "ev and ac must not be NaN")
  expect_error(cpi(35000, NaN), "ev and ac must not be NaN")
})

test_that("cpi rejects NA inputs", {
  expect_error(cpi(NA_real_, 36000), "ev and ac must not be NA")
  expect_error(cpi(35000, NA_real_), "ev and ac must not be NA")
})

test_that("cpi rejects Inf inputs", {
  expect_error(cpi(Inf, 36000), "ev and ac must not be infinite")
  expect_error(cpi(35000, Inf), "ev and ac must not be infinite")
})

test_that("cpi validates ev non-negative", {
  expect_error(cpi(-35000, 36000), "ev must be non-negative")
})

test_that("cpi validates ac greater than zero", {
  expect_error(cpi(35000, 0), "ac must be greater than zero")
  expect_error(cpi(35000, -36000), "ac must be greater than zero")
})

# ============================================================================
# EAC Tests
# ============================================================================
test_that("eac typical method calculation is correct", {
  bac <- 100000
  cpi_val <- 0.8
  expected <- bac / cpi_val
  expect_equal(eac(bac, cpi = cpi_val), expected)
})

test_that("eac atypical method calculation is correct", {
  bac <- 100000
  ac_val <- 63000
  ev_val <- 35000
  expected <- ac_val + (bac - ev_val)
  expect_equal(eac(bac, method = "atypical", ac = ac_val, ev = ev_val), expected)
})

test_that("eac combined method calculation is correct", {
  bac <- 100000
  cpi_val <- 0.8
  spi_val <- 0.9
  ac_val <- 63000
  ev_val <- 35000
  expected <- ac_val + (bac - ev_val) / (cpi_val * spi_val)
  expect_equal(eac(bac, method = "combined", cpi = cpi_val, ac = ac_val, ev = ev_val, spi = spi_val), expected)
})

test_that("eac validates method parameter", {
  expect_error(eac(100000, method = "invalid"), "method must be one of: typical, atypical, combined")
})

test_that("eac typical method validates required parameters", {
  expect_error(eac(100000, method = "typical"), "cpi is required for the 'typical' method")
  expect_error(eac(100000, method = "typical", cpi = 0), "cpi must be greater than zero")
})

test_that("eac atypical method validates required parameters", {
  expect_error(eac(100000, method = "atypical", ac = 63000), "ac and ev are required for the 'atypical' method")
  expect_error(eac(100000, method = "atypical", ev = 35000), "ac and ev are required for the 'atypical' method")
})

test_that("eac combined method validates required parameters", {
  expect_error(
    eac(100000, method = "combined", cpi = 0.8, ac = 63000, ev = 35000),
    "cpi, ac, ev, and spi are required for the 'combined' method"
  )
  expect_error(
    eac(100000, method = "combined", cpi = 0, ac = 63000, ev = 35000, spi = 0.9),
    "cpi and spi must be greater than zero"
  )
})

test_that("eac validates bac", {
  expect_error(eac(NULL, cpi = 0.8), "bac must not be NULL")
  expect_error(eac(-100000, cpi = 0.8), "bac must be non-negative")
})

test_that("eac rejects NaN bac", {
  expect_error(eac(NaN, cpi = 0.8), "bac must be a single non-NaN numeric value")
})

test_that("eac rejects NA bac", {
  expect_error(eac(NA_real_, cpi = 0.8), "bac must be a single numeric value")
})

test_that("eac rejects Inf bac", {
  expect_error(eac(Inf, cpi = 0.8), "bac must not be infinite")
})

# Typical method NaN/NA/Inf for cpi
test_that("eac typical rejects NaN cpi", {
  expect_error(eac(100000, method = "typical", cpi = NaN), "cpi must be a single non-NaN numeric value")
})

test_that("eac typical rejects NA cpi", {
  expect_error(eac(100000, method = "typical", cpi = NA_real_), "cpi must be a single numeric value")
})

test_that("eac typical rejects Inf cpi", {
  expect_error(eac(100000, method = "typical", cpi = Inf), "cpi must not be infinite")
})

# Atypical method NaN/NA/Inf for ac, ev
test_that("eac atypical rejects NaN ac", {
  expect_error(eac(100000, method = "atypical", ac = NaN, ev = 35000), "ac must be a single non-NaN numeric value")
})

test_that("eac atypical rejects NA ac", {
  expect_error(eac(100000, method = "atypical", ac = NA_real_, ev = 35000), "ac must be a single numeric value")
})

test_that("eac atypical rejects Inf ac", {
  expect_error(eac(100000, method = "atypical", ac = Inf, ev = 35000), "ac must not be infinite")
})

test_that("eac atypical rejects NaN ev", {
  expect_error(eac(100000, method = "atypical", ac = 63000, ev = NaN), "ev must be a single non-NaN numeric value")
})

test_that("eac atypical rejects NA ev", {
  expect_error(eac(100000, method = "atypical", ac = 63000, ev = NA_real_), "ev must be a single numeric value")
})

test_that("eac atypical rejects Inf ev", {
  expect_error(eac(100000, method = "atypical", ac = 63000, ev = Inf), "ev must not be infinite")
})

test_that("eac atypical rejects negative ac/ev", {
  expect_error(eac(100000, method = "atypical", ac = -1, ev = 35000), "ac and ev must be non-negative")
  expect_error(eac(100000, method = "atypical", ac = 63000, ev = -1), "ac and ev must be non-negative")
})

# Combined method NaN/NA/Inf for cpi, spi, ac, ev
test_that("eac combined rejects NaN cpi", {
  expect_error(
    eac(100000, method = "combined", cpi = NaN, ac = 63000, ev = 35000, spi = 0.9),
    "cpi must be a single non-NaN numeric value for 'combined' method"
  )
})

test_that("eac combined rejects NA cpi", {
  expect_error(
    eac(100000, method = "combined", cpi = NA_real_, ac = 63000, ev = 35000, spi = 0.9),
    "cpi must be a single numeric value"
  )
})

test_that("eac combined rejects Inf cpi", {
  expect_error(
    eac(100000, method = "combined", cpi = Inf, ac = 63000, ev = 35000, spi = 0.9),
    "cpi must not be infinite for 'combined' method"
  )
})

test_that("eac combined rejects NaN spi", {
  expect_error(
    eac(100000, method = "combined", cpi = 0.8, ac = 63000, ev = 35000, spi = NaN),
    "spi must be a single non-NaN numeric value"
  )
})

test_that("eac combined rejects NA spi", {
  expect_error(
    eac(100000, method = "combined", cpi = 0.8, ac = 63000, ev = 35000, spi = NA_real_),
    "spi must be a single numeric value"
  )
})

test_that("eac combined rejects Inf spi", {
  expect_error(
    eac(100000, method = "combined", cpi = 0.8, ac = 63000, ev = 35000, spi = Inf),
    "spi must not be infinite"
  )
})

test_that("eac combined rejects NaN ac", {
  expect_error(
    eac(100000, method = "combined", cpi = 0.8, ac = NaN, ev = 35000, spi = 0.9),
    "ac must be a single non-NaN numeric value for 'combined' method"
  )
})

test_that("eac combined rejects NA ac", {
  expect_error(
    eac(100000, method = "combined", cpi = 0.8, ac = NA_real_, ev = 35000, spi = 0.9),
    "ac must be a single numeric value"
  )
})

test_that("eac combined rejects Inf ac", {
  expect_error(
    eac(100000, method = "combined", cpi = 0.8, ac = Inf, ev = 35000, spi = 0.9),
    "ac must not be infinite for 'combined' method"
  )
})

test_that("eac combined rejects NaN ev", {
  expect_error(
    eac(100000, method = "combined", cpi = 0.8, ac = 63000, ev = NaN, spi = 0.9),
    "ev must be a single non-NaN numeric value for 'combined' method"
  )
})

test_that("eac combined rejects NA ev", {
  expect_error(
    eac(100000, method = "combined", cpi = 0.8, ac = 63000, ev = NA_real_, spi = 0.9),
    "ev must be a single numeric value"
  )
})

test_that("eac combined rejects Inf ev", {
  expect_error(
    eac(100000, method = "combined", cpi = 0.8, ac = 63000, ev = Inf, spi = 0.9),
    "ev must not be infinite for 'combined' method"
  )
})

test_that("eac combined rejects negative ac/ev", {
  expect_error(
    eac(100000, method = "combined", cpi = 0.8, ac = -1, ev = 35000, spi = 0.9),
    "ac and ev must be non-negative"
  )
  expect_error(
    eac(100000, method = "combined", cpi = 0.8, ac = 63000, ev = -1, spi = 0.9),
    "ac and ev must be non-negative"
  )
})

# ============================================================================
# ETC Tests
# ============================================================================
test_that("etc calculation without cpi is correct", {
  bac <- 100000
  ev_val <- 35000
  expected <- bac - ev_val
  expect_equal(etc(bac, ev_val), expected)
})

test_that("etc calculation with cpi is correct", {
  bac <- 100000
  ev_val <- 35000
  cpi_val <- 0.8
  expected <- (bac - ev_val) / cpi_val
  expect_equal(etc(bac, ev_val, cpi_val), expected)
})

test_that("etc validates NULL inputs", {
  expect_error(etc(NULL, 35000), "bac and ev must not be NULL")
  expect_error(etc(100000, NULL), "bac and ev must not be NULL")
})

test_that("etc validates cpi greater than zero", {
  expect_error(etc(100000, 35000, 0), "cpi must be greater than zero")
  expect_error(etc(100000, 35000, -0.5), "cpi must be greater than zero")
})

test_that("etc validates numeric inputs", {
  expect_error(etc("100000", 35000), "bac and ev must be numeric")
  expect_error(etc(100000, "35000"), "bac and ev must be numeric")
})

test_that("etc validates single value inputs", {
  expect_error(etc(c(100000, 200000), 35000), "bac and ev must be single numeric values")
  expect_error(etc(100000, c(35000, 36000)), "bac and ev must be single numeric values")
})

test_that("etc rejects NaN inputs", {
  expect_error(etc(NaN, 35000), "bac and ev must not be NaN")
  expect_error(etc(100000, NaN), "bac and ev must not be NaN")
})

test_that("etc rejects NA inputs", {
  expect_error(etc(NA_real_, 35000), "bac and ev must not be NA")
  expect_error(etc(100000, NA_real_), "bac and ev must not be NA")
})

test_that("etc rejects Inf inputs", {
  expect_error(etc(Inf, 35000), "bac and ev must not be infinite")
  expect_error(etc(100000, Inf), "bac and ev must not be infinite")
})

test_that("etc rejects NaN cpi", {
  expect_error(etc(100000, 35000, NaN), "cpi must be a single non-NaN numeric value")
})

test_that("etc rejects NA cpi", {
  expect_error(etc(100000, 35000, NA_real_), "cpi must be a single numeric value")
})

test_that("etc rejects Inf cpi", {
  expect_error(etc(100000, 35000, Inf), "cpi must not be infinite")
})

test_that("etc validates non-negative inputs", {
  expect_error(etc(-100000, 35000), "bac and ev must be non-negative")
  expect_error(etc(100000, -35000), "bac and ev must be non-negative")
})

# ============================================================================
# VAC Tests
# ============================================================================
test_that("vac calculation is correct", {
  bac <- 100000
  eac_val <- 120000
  expected <- bac - eac_val
  expect_equal(vac(bac, eac_val), expected)
})

test_that("vac validates NULL inputs", {
  expect_error(vac(NULL, 120000), "bac and eac must not be NULL")
  expect_error(vac(100000, NULL), "bac and eac must not be NULL")
})

test_that("vac validates numeric inputs", {
  expect_error(vac("100000", 120000), "bac and eac must be numeric")
  expect_error(vac(100000, "120000"), "bac and eac must be numeric")
})

test_that("vac validates single value inputs", {
  expect_error(vac(c(100000, 200000), 120000), "bac and eac must be single numeric values")
  expect_error(vac(100000, c(120000, 130000)), "bac and eac must be single numeric values")
})

test_that("vac rejects NaN inputs", {
  expect_error(vac(NaN, 120000), "bac and eac must not be NaN")
  expect_error(vac(100000, NaN), "bac and eac must not be NaN")
})

test_that("vac rejects NA inputs", {
  expect_error(vac(NA_real_, 120000), "bac and eac must not be NA")
  expect_error(vac(100000, NA_real_), "bac and eac must not be NA")
})

test_that("vac rejects Inf inputs", {
  expect_error(vac(Inf, 120000), "bac and eac must not be infinite")
  expect_error(vac(100000, Inf), "bac and eac must not be infinite")
})

test_that("vac validates non-negative inputs", {
  expect_error(vac(-100000, 120000), "bac and eac must be non-negative")
  expect_error(vac(100000, -120000), "bac and eac must be non-negative")
})

# ============================================================================
# TCPI Tests
# ============================================================================
test_that("tcpi to BAC calculation is correct", {
  bac <- 100000
  ev_val <- 35000
  ac_val <- 40000
  expected <- (bac - ev_val) / (bac - ac_val)
  expect_equal(tcpi(bac, ev_val, ac_val), expected)
})

test_that("tcpi to EAC calculation is correct", {
  bac <- 100000
  ev_val <- 35000
  ac_val <- 40000
  eac_val <- 120000
  expected <- (bac - ev_val) / (eac_val - ac_val)
  expect_equal(tcpi(bac, ev_val, ac_val, target = "eac", eac = eac_val), expected)
})

test_that("tcpi validates NULL inputs", {
  expect_error(tcpi(NULL, 35000, 40000), "bac, ev, and ac must not be NULL")
  expect_error(tcpi(100000, NULL, 40000), "bac, ev, and ac must not be NULL")
  expect_error(tcpi(100000, 35000, NULL), "bac, ev, and ac must not be NULL")
})

test_that("tcpi validates target parameter", {
  expect_error(tcpi(100000, 35000, 40000, target = "invalid"), "target must be either 'bac' or 'eac'")
})

test_that("tcpi validates eac required for eac target", {
  expect_error(tcpi(100000, 35000, 40000, target = "eac"), "eac is required when target = 'eac'")
})

test_that("tcpi validates denominator not zero for BAC target", {
  expect_error(tcpi(100000, 35000, 100000), "Cannot calculate TCPI: actual cost already meets or exceeds budget")
})

test_that("tcpi validates numeric inputs", {
  expect_error(tcpi("100000", 35000, 40000), "bac, ev, and ac must be numeric")
  expect_error(tcpi(100000, "35000", 40000), "bac, ev, and ac must be numeric")
  expect_error(tcpi(100000, 35000, "40000"), "bac, ev, and ac must be numeric")
})

test_that("tcpi validates single value inputs", {
  expect_error(tcpi(c(100000, 200000), 35000, 40000), "bac, ev, and ac must be single numeric values")
  expect_error(tcpi(100000, c(35000, 36000), 40000), "bac, ev, and ac must be single numeric values")
  expect_error(tcpi(100000, 35000, c(40000, 41000)), "bac, ev, and ac must be single numeric values")
})

test_that("tcpi rejects NaN inputs", {
  expect_error(tcpi(NaN, 35000, 40000), "bac, ev, and ac must not be NaN")
  expect_error(tcpi(100000, NaN, 40000), "bac, ev, and ac must not be NaN")
  expect_error(tcpi(100000, 35000, NaN), "bac, ev, and ac must not be NaN")
})

test_that("tcpi rejects NA inputs", {
  expect_error(tcpi(NA_real_, 35000, 40000), "bac, ev, and ac must not be NA")
  expect_error(tcpi(100000, NA_real_, 40000), "bac, ev, and ac must not be NA")
  expect_error(tcpi(100000, 35000, NA_real_), "bac, ev, and ac must not be NA")
})

test_that("tcpi rejects Inf inputs", {
  expect_error(tcpi(Inf, 35000, 40000), "bac, ev, and ac must not be infinite")
  expect_error(tcpi(100000, Inf, 40000), "bac, ev, and ac must not be infinite")
  expect_error(tcpi(100000, 35000, Inf), "bac, ev, and ac must not be infinite")
})

test_that("tcpi validates non-negative inputs", {
  expect_error(tcpi(-100000, 35000, 40000), "bac, ev, and ac must be non-negative")
  expect_error(tcpi(100000, -35000, 40000), "bac, ev, and ac must be non-negative")
  expect_error(tcpi(100000, 35000, -40000), "bac, ev, and ac must be non-negative")
})

test_that("tcpi eac target rejects NaN eac", {
  expect_error(
    tcpi(100000, 35000, 40000, target = "eac", eac = NaN),
    "eac must be a single non-NaN numeric value"
  )
})

test_that("tcpi eac target rejects NA eac", {
  expect_error(
    tcpi(100000, 35000, 40000, target = "eac", eac = NA_real_),
    "eac must be a single numeric value"
  )
})

test_that("tcpi eac target rejects Inf eac", {
  expect_error(
    tcpi(100000, 35000, 40000, target = "eac", eac = Inf),
    "eac must not be infinite"
  )
})

test_that("tcpi eac target rejects negative eac", {
  expect_error(
    tcpi(100000, 35000, 40000, target = "eac", eac = -1),
    "eac must be non-negative"
  )
})

test_that("tcpi validates denominator not zero for EAC target", {
  expect_error(
    tcpi(100000, 35000, 120000, target = "eac", eac = 120000),
    "Cannot calculate TCPI: actual cost already meets or exceeds EAC"
  )
})

# ============================================================================
# G5.3: EVM Return Value Tests
# ============================================================================
test_that("pv result contains no NA, NaN, or Inf", {
  result <- pv(100000, c(0.1, 0.2, 0.4, 0.7, 1.0), 3)
  expect_false(is.na(result))
  expect_false(is.nan(result))
  expect_false(is.infinite(result))
})

test_that("ev result contains no NA, NaN, or Inf", {
  result <- ev(100000, 0.35)
  expect_false(is.na(result))
  expect_false(is.nan(result))
  expect_false(is.infinite(result))
})

test_that("ac result contains no NA, NaN, or Inf", {
  result <- ac(c(9000, 18000, 36000), 2)
  expect_false(is.na(result))
  expect_false(is.nan(result))
  expect_false(is.infinite(result))
})

test_that("sv result contains no NA, NaN, or Inf", {
  result <- sv(35000, 40000)
  expect_false(is.na(result))
  expect_false(is.nan(result))
  expect_false(is.infinite(result))
})

test_that("cv result contains no NA, NaN, or Inf", {
  result <- cv(35000, 36000)
  expect_false(is.na(result))
  expect_false(is.nan(result))
  expect_false(is.infinite(result))
})

test_that("spi result contains no NA, NaN, or Inf", {
  result <- spi(35000, 40000)
  expect_false(is.na(result))
  expect_false(is.nan(result))
  expect_false(is.infinite(result))
})

test_that("cpi result contains no NA, NaN, or Inf", {
  result <- cpi(35000, 36000)
  expect_false(is.na(result))
  expect_false(is.nan(result))
  expect_false(is.infinite(result))
})

test_that("eac result contains no NA, NaN, or Inf", {
  result <- eac(100000, cpi = 0.8)
  expect_false(is.na(result))
  expect_false(is.nan(result))
  expect_false(is.infinite(result))
})

test_that("etc result contains no NA, NaN, or Inf", {
  result <- etc(100000, 35000)
  expect_false(is.na(result))
  expect_false(is.nan(result))
  expect_false(is.infinite(result))
})

test_that("vac result contains no NA, NaN, or Inf", {
  result <- vac(100000, 120000)
  expect_false(is.na(result))
  expect_false(is.nan(result))
  expect_false(is.infinite(result))
})

test_that("tcpi result contains no NA, NaN, or Inf", {
  result <- tcpi(100000, 35000, 40000)
  expect_false(is.na(result))
  expect_false(is.nan(result))
  expect_false(is.infinite(result))
})

# ============================================================================
# Parameter Recovery Tests (G5.6, G5.6a)
# ============================================================================

test_that("pv recovers known planned value from schedule", {
  bac <- 100000
  schedule <- c(0.1, 0.2, 0.4, 0.7, 1.0)
  time_period <- 3

  result <- pv(bac, schedule, time_period)

  # Expected: BAC * schedule[time_period] = 100000 * 0.4 = 40000
  expected <- 100000 * 0.4

  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("ev recovers known earned value from completion", {
  bac <- 100000
  actual_per_complete <- 0.35

  result <- ev(bac, actual_per_complete)

  # Expected: BAC * actual_per_complete = 100000 * 0.35 = 35000
  expected <- 100000 * 0.35

  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("sv recovers known schedule variance", {
  ev_val <- 35000
  pv_val <- 40000

  result <- sv(ev_val, pv_val)

  # Expected: EV - PV = 35000 - 40000 = -5000
  expected <- 35000 - 40000

  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("cv recovers known cost variance", {
  ev_val <- 35000
  ac_val <- 36000

  result <- cv(ev_val, ac_val)

  # Expected: EV - AC = 35000 - 36000 = -1000
  expected <- 35000 - 36000

  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("spi recovers known schedule performance index", {
  ev_val <- 35000
  pv_val <- 40000

  result <- spi(ev_val, pv_val)

  # Expected: EV / PV = 35000 / 40000 = 0.875
  expected <- 35000 / 40000

  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("cpi recovers known cost performance index", {
  ev_val <- 35000
  ac_val <- 36000

  result <- cpi(ev_val, ac_val)

  # Expected: EV / AC = 35000 / 36000 = 0.9722...
  expected <- 35000 / 36000

  expect_equal(result, expected, tolerance = 1e-6)
})

# ============================================================================
# Edge Condition Tests (G5.8a) - Zero-Length Data
# ============================================================================

test_that("pv rejects zero-length schedule", {
  expect_error(pv(100000, numeric(0), 1), "schedule must not be empty")
})

test_that("ac rejects zero-length actual_costs", {
  expect_error(ac(numeric(0), 1), "actual_costs must not be empty")
})

# ============================================================================
# Edge Condition Tests (G5.8d) - Out-of-Scope Data
# ============================================================================

test_that("pv rejects time_period beyond schedule length", {
  bac <- 100000
  schedule <- c(0.1, 0.2, 0.4, 0.7, 1.0)

  expect_error(pv(bac, schedule, 10), "time_period must be within the range of the schedule vector")
})

test_that("ac rejects time_period beyond actual_costs length", {
  actual_costs <- c(10000, 20000, 30000)

  expect_error(ac(actual_costs, 10), "time_period must be within the range of the actual_costs vector")
})

# ============================================================================
# Noise Susceptibility Tests (G5.9a) - Trivial Noise
# ============================================================================

test_that("pv is stable to trivial noise in inputs", {
  bac <- 100000
  schedule <- c(0.1, 0.2, 0.4, 0.7, 1.0)
  time_period <- 3

  result_clean <- pv(bac, schedule, time_period)

  # Add trivial noise
  bac_noisy <- bac + runif(1, -.Machine$double.eps, .Machine$double.eps)
  result_noisy <- pv(bac_noisy, schedule, time_period)

  expect_equal(result_clean, result_noisy, tolerance = 10 * .Machine$double.eps)
})
