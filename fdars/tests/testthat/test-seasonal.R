# Tests for seasonal analysis functions

test_that("detect.peaks finds known peaks in sine wave", {
  # Pure sine wave with known peak locations
  # sin(2*pi*t/period) has peaks at t = period/4 + k*period
  t <- seq(0, 10, length.out = 200)
  period <- 2
  X <- matrix(sin(2 * pi * t / period), nrow = 1)
  fd <- fdata(X, argvals = t)

  # Expected peaks at t = 0.5, 2.5, 4.5, 6.5, 8.5
  expected_peaks <- c(0.5, 2.5, 4.5, 6.5, 8.5)

  result <- detect.peaks(fd)
  peaks <- result$peaks[[1]]

  expect_equal(nrow(peaks), 5)
  expect_equal(peaks$time, expected_peaks, tolerance = 0.15)
  expect_equal(result$mean_period, period, tolerance = 0.1)
})

test_that("detect.peaks works with different periods", {
  # Higher frequency: period = 1
  t <- seq(0, 10, length.out = 400)
  X <- matrix(sin(2 * pi * t / 1), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detect.peaks(fd)
  peaks <- result$peaks[[1]]

  # Should find 10 peaks for period=1 over [0,10]
  expect_equal(nrow(peaks), 10)
  expect_equal(result$mean_period, 1.0, tolerance = 0.1)
})

test_that("detect.peaks respects min_distance", {
  t <- seq(0, 10, length.out = 200)
  X <- matrix(sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  # With min_distance = 1.5, should find all 5 peaks (spacing = 2)
  result1 <- detect.peaks(fd, min_distance = 1.5)
  expect_equal(nrow(result1$peaks[[1]]), 5)

  # With min_distance = 2.5, should find fewer peaks
  result2 <- detect.peaks(fd, min_distance = 2.5)
  expect_lt(nrow(result2$peaks[[1]]), 5)
})

test_that("detect.peaks handles shifted sine wave", {
  # Shifted sine: sin(2*pi*t/2) + 1
  # Same peak locations, just shifted up
  t <- seq(0, 10, length.out = 200)
  X <- matrix(sin(2 * pi * t / 2) + 1, nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detect.peaks(fd)
  peaks <- result$peaks[[1]]

  # Should still find 5 peaks
  expect_equal(nrow(peaks), 5)

  # Peak values should be around 2.0 (max of sin + 1)
  expect_equal(peaks$value, rep(2.0, 5), tolerance = 0.05)
})

test_that("detect.peaks handles smoothing for noisy data", {
  set.seed(123)
  t <- seq(0, 10, length.out = 200)
  X_noisy <- matrix(sin(2 * pi * t / 2) + rnorm(200, sd = 0.3), nrow = 1)
  fd <- fdata(X_noisy, argvals = t)

  # With smoothing, should find approximately 5 peaks
  result_smooth <- detect.peaks(fd, min_distance = 1.5, smooth_first = TRUE)

  # Allow some tolerance due to noise

  expect_gte(nrow(result_smooth$peaks[[1]]), 4)
  expect_lte(nrow(result_smooth$peaks[[1]]), 6)
})

test_that("detect.peaks prominence filtering works", {
  # Create signal with main peaks and small ripples
  t <- seq(0, 10, length.out = 200)
  base <- sin(2 * pi * t / 2)
  ripple <- 0.1 * sin(2 * pi * t * 4)
  X <- matrix(base + ripple, nrow = 1)
  fd <- fdata(X, argvals = t)

  # Without prominence filter, may find extra peaks from ripples
  result_no_filter <- detect.peaks(fd)

  # With prominence filter, should only find major peaks
  result_filtered <- detect.peaks(fd, min_prominence = 0.5)

  # Filtered should have fewer or equal peaks
  expect_lte(nrow(result_filtered$peaks[[1]]), nrow(result_no_filter$peaks[[1]]))
})

test_that("detect.peaks handles multiple curves", {
  t <- seq(0, 10, length.out = 200)
  # Two curves with same period but different phases
  X <- rbind(
    sin(2 * pi * t / 2),
    sin(2 * pi * t / 2 + pi / 4)
  )
  fd <- fdata(X, argvals = t)

  result <- detect.peaks(fd)

  # Should have results for both curves
  expect_equal(length(result$peaks), 2)
  expect_equal(nrow(result$peaks[[1]]), 5)
  expect_equal(nrow(result$peaks[[2]]), 5)
})

test_that("analyze.peak.timing works for periodic data", {
  t <- seq(0, 10, length.out = 500)
  X <- matrix(sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- analyze.peak.timing(fd, period = 2)

  # Should find peaks
  expect_gt(length(result$peak_times), 0)

  # Pure sine should have low timing variability
  expect_true(is.finite(result$mean_timing))
  expect_true(result$std_timing < 0.1 || is.nan(result$std_timing))
})

test_that("estimate.period correctly estimates sine wave period", {
  t <- seq(0, 20, length.out = 400)
  period_true <- 2.5
  X <- matrix(sin(2 * pi * t / period_true), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- estimate.period(fd)

  expect_equal(result$period, period_true, tolerance = 0.2)
  expect_gt(result$confidence, 1.0)
})

test_that("seasonal.strength is high for pure sine", {
  t <- seq(0, 20, length.out = 400)
  X <- matrix(sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  strength <- seasonal.strength(fd, period = 2)

  # Pure sine should have very high seasonal strength
  expect_gt(strength, 0.8)
})

test_that("detect.peaks works with different amplitudes", {
  t <- seq(0, 10, length.out = 200)

  for (amplitude in c(0.5, 1.0, 2.0, 5.0)) {
    X <- matrix(amplitude * sin(2 * pi * t / 2), nrow = 1)
    fd <- fdata(X, argvals = t)

    result <- detect.peaks(fd)
    peaks <- result$peaks[[1]]

    expect_equal(nrow(peaks), 5,
                 info = paste("Amplitude", amplitude, "should find 5 peaks"))

    # Peak values should be close to amplitude
    expect_equal(peaks$value, rep(amplitude, 5), tolerance = 0.1,
                 info = paste("Peak values should equal amplitude", amplitude))
  }
})

test_that("detect.peaks handles varying frequency (chirp)", {
  t <- seq(0, 10, length.out = 400)

  # Chirp signal: frequency increases with time
  # Phase = 2*pi * (0.5*t + 0.05*t^2)
  phase <- 2 * pi * (0.5 * t + 0.05 * t^2)
  X <- matrix(sin(phase), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detect.peaks(fd)
  peaks <- result$peaks[[1]]

  # Should find multiple peaks
  expect_gte(nrow(peaks), 5)

  # Inter-peak distances should decrease over time
  distances <- result$inter_peak_distances[[1]]
  if (length(distances) >= 4) {
    early_avg <- mean(distances[1:2])
    late_avg <- mean(distances[(length(distances)-1):length(distances)])
    expect_lt(late_avg, early_avg)
  }
})

test_that("detect.peaks handles sum of sines with different periods", {
  t <- seq(0, 12, length.out = 300)

  # Sum of two sines: period 2 and period 3
  X <- matrix(sin(2 * pi * t / 2) + 0.5 * sin(2 * pi * t / 3), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detect.peaks(fd, min_distance = 1.0)
  peaks <- result$peaks[[1]]

  # Should find peaks
  expect_gte(nrow(peaks), 4)

  # Inter-peak distances should vary (not all equal)
  distances <- result$inter_peak_distances[[1]]
  if (length(distances) >= 2) {
    expect_gt(max(distances), min(distances) * 1.1)
  }
})

# Tests for wavelet-based seasonal strength
test_that("seasonal.strength with wavelet method detects pure sine", {
  t <- seq(0, 20, length.out = 400)
  X <- matrix(sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  strength <- seasonal.strength(fd, period = 2, method = "wavelet")

  # Pure sine should have high wavelet seasonal strength
  expect_gt(strength, 0.5)
})

test_that("seasonal.strength wavelet method is low for noise", {
  set.seed(42)
  t <- seq(0, 20, length.out = 400)
  X <- matrix(rnorm(400), nrow = 1)
  fd <- fdata(X, argvals = t)

  strength <- seasonal.strength(fd, period = 2, method = "wavelet")

  # Noise should have low seasonal strength
  expect_lt(strength, 0.4)
})

test_that("seasonal.strength wavelet is lower at wrong period", {
  t <- seq(0, 20, length.out = 400)
  X <- matrix(sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  strength_correct <- seasonal.strength(fd, period = 2, method = "wavelet")
  strength_wrong <- seasonal.strength(fd, period = 4, method = "wavelet")

  # Strength at correct period should be higher than at wrong period
  expect_gt(strength_correct, strength_wrong)
})

test_that("seasonal.strength all methods work consistently", {
  t <- seq(0, 20, length.out = 400)
  X <- matrix(sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  strength_var <- seasonal.strength(fd, period = 2, method = "variance")
  strength_spec <- seasonal.strength(fd, period = 2, method = "spectral")
  strength_wav <- seasonal.strength(fd, period = 2, method = "wavelet")

  # All methods should detect seasonality for pure sine
  expect_gt(strength_var, 0.7)
  expect_gt(strength_spec, 0.7)
  expect_gt(strength_wav, 0.5)

  # All should be finite
  expect_true(is.finite(strength_var))
  expect_true(is.finite(strength_spec))
  expect_true(is.finite(strength_wav))
})

# ==============================================================================
# Additional tests for uncovered functions
# ==============================================================================

test_that("detect.periods finds multiple periods", {
  t <- seq(0, 20, length.out = 400)
  # Signal with two periods: 2 and 7
  X <- matrix(sin(2 * pi * t / 2) + 0.6 * sin(2 * pi * t / 7), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detect.periods(fd, max_periods = 3, min_confidence = 0.3, min_strength = 0.1)

  expect_s3_class(result, "multiple_periods")
  expect_gte(result$n_periods, 1)
  expect_equal(length(result$periods), result$n_periods)
  expect_equal(length(result$confidence), result$n_periods)
  expect_equal(length(result$strength), result$n_periods)
})

test_that("print.multiple_periods works", {
  t <- seq(0, 20, length.out = 400)
  X <- matrix(sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detect.periods(fd, max_periods = 2)

  expect_output(print(result), "Multiple Period Detection")
  expect_output(print(result), "Periods detected:")
})

test_that("print.peak_detection works", {
  t <- seq(0, 10, length.out = 200)
  X <- matrix(sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detect.peaks(fd)

  expect_output(print(result), "Peak Detection Result")
  expect_output(print(result), "Number of curves:")
})

test_that("print.period_estimate works", {
  t <- seq(0, 20, length.out = 400)
  X <- matrix(sin(2 * pi * t / 2.5), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- estimate.period(fd)

  expect_output(print(result), "Period Estimate")
  expect_output(print(result), "Period:")
})

test_that("seasonal.strength.curve returns time-varying strength", {
  t <- seq(0, 20, length.out = 400)
  X <- matrix(sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- seasonal.strength.curve(fd, period = 2, window_size = 4)

  expect_s3_class(result, "fdata")
  expect_equal(ncol(result$data), ncol(fd$data))
})

test_that("seasonal.strength.curve with spectral method works", {
  t <- seq(0, 20, length.out = 400)
  X <- matrix(sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- seasonal.strength.curve(fd, period = 2, method = "spectral")

  expect_s3_class(result, "fdata")
})

test_that("detect.seasonality.changes finds transitions", {
  t <- seq(0, 30, length.out = 600)
  # Signal that starts non-seasonal, becomes seasonal
  X <- ifelse(t < 15,
              rnorm(sum(t < 15), sd = 0.3),
              sin(2 * pi * t[t >= 15] / 2))
  X <- matrix(X, nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detect.seasonality.changes(fd, period = 2, threshold = 0.3)

  expect_s3_class(result, "seasonality_changes")
  expect_true("change_points" %in% names(result))
  expect_true("strength_curve" %in% names(result))
})

test_that("print.seasonality_changes works", {
  t <- seq(0, 20, length.out = 400)
  X <- matrix(sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detect.seasonality.changes(fd, period = 2)

  expect_output(print(result), "Seasonality Change Detection")
})

test_that("instantaneous.period returns fdata objects", {
  t <- seq(0, 10, length.out = 200)
  X <- matrix(sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- instantaneous.period(fd)

  expect_true("period" %in% names(result))
  expect_true("frequency" %in% names(result))
  expect_true("amplitude" %in% names(result))
  expect_s3_class(result$period, "fdata")
  expect_s3_class(result$frequency, "fdata")
  expect_s3_class(result$amplitude, "fdata")
})

test_that("print.peak_timing works", {
  t <- seq(0, 10, length.out = 500)
  X <- matrix(sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- analyze.peak.timing(fd, period = 2)

  expect_output(print(result), "Peak Timing Variability Analysis")
  expect_output(print(result), "Number of peaks:")
})

test_that("classify.seasonality identifies stable seasonal patterns", {
  t <- seq(0, 10, length.out = 500)
  X <- matrix(sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- classify.seasonality(fd, period = 2)

  expect_s3_class(result, "seasonality_classification")
  expect_true(result$is_seasonal)
  expect_true("classification" %in% names(result))
  expect_true("seasonal_strength" %in% names(result))
})

test_that("print.seasonality_classification works", {
  t <- seq(0, 10, length.out = 500)
  X <- matrix(sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- classify.seasonality(fd, period = 2)

  expect_output(print(result), "Seasonality Classification")
  expect_output(print(result), "Classification:")
})

test_that("classify.seasonality identifies non-seasonal data", {
  set.seed(42)
  t <- seq(0, 10, length.out = 200)
  X <- matrix(rnorm(200), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- classify.seasonality(fd, period = 2)

  expect_false(result$is_seasonal)
  expect_equal(result$classification, "NonSeasonal")
})

test_that("detect.seasonality.changes.auto uses Otsu threshold", {
  t <- seq(0, 20, length.out = 400)
  X <- matrix(sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detect.seasonality.changes.auto(fd, period = 2, threshold_method = "otsu")

  expect_s3_class(result, "seasonality_changes_auto")
  expect_true("computed_threshold" %in% names(result))
  expect_true(is.finite(result$computed_threshold))
})

test_that("detect.seasonality.changes.auto with fixed threshold works", {
  t <- seq(0, 20, length.out = 400)
  X <- matrix(sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detect.seasonality.changes.auto(fd, period = 2,
                                             threshold_method = "fixed",
                                             threshold_value = 0.5)

  expect_s3_class(result, "seasonality_changes_auto")
})

test_that("print.seasonality_changes_auto works", {
  t <- seq(0, 20, length.out = 400)
  X <- matrix(sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detect.seasonality.changes.auto(fd, period = 2)

  expect_output(print(result), "Auto Threshold")
  expect_output(print(result), "Computed threshold:")
})

test_that("detrend removes linear trend", {
  t <- seq(0, 10, length.out = 200)
  X <- matrix(2 + 0.5 * t + sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detrend(fd, method = "linear")

  expect_s3_class(result, "fdata")
  # Detrended data should have approximately zero mean trend
  mean_val <- mean(result$data)
  expect_lt(abs(mean_val), 1)
})

test_that("detrend with polynomial method works", {
  t <- seq(0, 10, length.out = 200)
  X <- matrix(1 + 0.3 * t + 0.05 * t^2 + sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detrend(fd, method = "polynomial", degree = 2)

  expect_s3_class(result, "fdata")
})

test_that("detrend with loess method works", {
  t <- seq(0, 10, length.out = 200)
  X <- matrix(2 + 0.5 * t + sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detrend(fd, method = "loess", bandwidth = 0.3)

  expect_s3_class(result, "fdata")
})

test_that("detrend with diff1 reduces series length", {
  t <- seq(0, 10, length.out = 200)
  X <- matrix(2 + 0.5 * t + sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detrend(fd, method = "diff1")

  expect_s3_class(result, "fdata")
  expect_equal(ncol(result$data), ncol(fd$data) - 1)
})

test_that("detrend with return_trend returns both components", {
  t <- seq(0, 10, length.out = 200)
  X <- matrix(2 + 0.5 * t + sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detrend(fd, method = "linear", return_trend = TRUE)

  expect_true(is.list(result))
  expect_true("detrended" %in% names(result))
  expect_true("trend" %in% names(result))
  expect_s3_class(result$detrended, "fdata")
  expect_s3_class(result$trend, "fdata")
})

test_that("detrend with auto method works", {
  t <- seq(0, 10, length.out = 200)
  X <- matrix(2 + 0.5 * t + sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detrend(fd, method = "auto")

  expect_s3_class(result, "fdata")
})

test_that("decompose separates trend and seasonal", {
  t <- seq(0, 20, length.out = 400)
  X <- matrix(2 + 0.3 * t + sin(2 * pi * t / 2.5), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- decompose(fd, period = 2.5, method = "additive")

  expect_true("trend" %in% names(result))
  expect_true("seasonal" %in% names(result))
  expect_true("remainder" %in% names(result))
  expect_s3_class(result$trend, "fdata")
  expect_s3_class(result$seasonal, "fdata")
  expect_s3_class(result$remainder, "fdata")
})

test_that("decompose multiplicative method works", {
  t <- seq(0, 20, length.out = 400)
  # Multiplicative: amplitude grows with level
  X <- matrix((2 + 0.3 * t) * (1 + 0.3 * sin(2 * pi * t / 2.5)), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- decompose(fd, period = 2.5, method = "multiplicative")

  expect_true("trend" %in% names(result))
  expect_equal(result$method, "multiplicative")
})

test_that("decompose with spline trend method works", {
  t <- seq(0, 20, length.out = 400)
  X <- matrix(2 + 0.3 * t + sin(2 * pi * t / 2.5), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- decompose(fd, period = 2.5, trend_method = "spline")

  expect_s3_class(result$trend, "fdata")
})

test_that("decompose estimates period automatically", {
  t <- seq(0, 20, length.out = 400)
  X <- matrix(sin(2 * pi * t / 2.5), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- decompose(fd, period = NULL)

  expect_true(is.finite(result$period))
  expect_gt(result$period, 0)
})

test_that("detect_amplitude_modulation detects emerging pattern", {
  t <- seq(0, 1, length.out = 200)
  amplitude <- 0.2 + 0.8 * t  # Growing amplitude
  X <- matrix(amplitude * sin(2 * pi * t / 0.2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detect_amplitude_modulation(fd, period = 0.2)

  expect_s3_class(result, "amplitude_modulation")
  expect_true("modulation_type" %in% names(result))
  expect_true("amplitude_trend" %in% names(result))
})

test_that("detect_amplitude_modulation wavelet method works", {
  t <- seq(0, 1, length.out = 200)
  X <- matrix(sin(2 * pi * t / 0.2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detect_amplitude_modulation(fd, period = 0.2, method = "wavelet")

  expect_s3_class(result, "amplitude_modulation")
})

test_that("print.amplitude_modulation works", {
  t <- seq(0, 1, length.out = 200)
  X <- matrix(sin(2 * pi * t / 0.2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detect_amplitude_modulation(fd, period = 0.2)

  expect_output(print(result), "Amplitude Modulation Detection")
  expect_output(print(result), "Modulation type:")
})

test_that("estimate.period with detrending works", {
  t <- seq(0, 20, length.out = 400)
  X <- matrix(2 + 0.5 * t + sin(2 * pi * t / 2.5), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- estimate.period(fd, detrend_method = "linear")

  expect_equal(result$period, 2.5, tolerance = 0.3)
})

test_that("estimate.period with acf method works", {
  t <- seq(0, 20, length.out = 400)
  X <- matrix(sin(2 * pi * t / 2.5), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- estimate.period(fd, method = "acf")

  expect_true(is.finite(result$period))
  expect_gt(result$period, 0)
})

test_that("detect.peaks with detrending works", {
  t <- seq(0, 10, length.out = 200)
  X <- matrix(2 + 0.5 * t + sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  result <- detect.peaks(fd, detrend_method = "linear")

  expect_s3_class(result, "peak_detection")
  expect_gte(length(result$peaks[[1]]$time), 3)
})

test_that("seasonal.strength with detrending works", {
  t <- seq(0, 20, length.out = 400)
  X <- matrix(2 + 0.5 * t + sin(2 * pi * t / 2), nrow = 1)
  fd <- fdata(X, argvals = t)

  strength <- seasonal.strength(fd, period = 2, detrend_method = "linear")

  expect_gt(strength, 0.5)
})
