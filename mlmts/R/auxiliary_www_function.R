

# Auxiliary functions for the computation of transformed series and features

auxiliary_function_transformed_series <- function(X) {

  series_length <- length(X)

  # Correction in case X takes the value of 0

  max_value <- max(X)
  if (sum(X == 0) > 0 & sum(X < 0) == 0) {X <- X + (1/1000) * max_value}

  # Applying Box-Cox transformation

  if (sum(X > 0) == series_length) {

    transformed_series <- AID::boxcoxnc(X, lambda = seq(-1, 1, 0.01))$tf.data

  } else {

    transformed_series <- X

  }


  # Decomposing the transformed series in trend, seasonality and noise

  freq_transformed_series <- forecast::findfrequency(transformed_series)
  if (freq_transformed_series > 1) {

    transformed_series <- stats::ts(transformed_series, frequency = freq_transformed_series)
    stl_decomposition <- stats::stl(transformed_series, s.window = 'periodic')$time.series
    T_t <- stl_decomposition[,2]
    S_t <- stl_decomposition[,1]
    E_t <- stl_decomposition[,3]


  } else {

    spline_model <- pspline::sm.spline(x = 1 : series_length, y = transformed_series, cv = T)
    S_t <- rep(0, series_length)
    T_t <- stats::predict(spline_model)$ysmth
    E_t <- transformed_series - T_t

  }


  de_trended_series <- transformed_series - T_t
  de_seasonalized_series <- transformed_series - S_t
  de_trended_seasonalized_series <- transformed_series - T_t - S_t

  return_list <- list(de_trended_series = de_trended_series,
                      de_seasonalized_series = de_seasonalized_series,
                      de_trended_seasonalized_series = de_trended_seasonalized_series)

}


# Trend and seasonality

auxiliary_function_trend_seasonality <- function(X) {

  series_length <- length(X)


  # Computing the measures concerning trend and seasonality

  transformed_series_list <- auxiliary_function_transformed_series(X)
  de_trended_series <- transformed_series_list$de_trended_series
  de_seasonalized_series <- transformed_series_list$de_seasonalized_series
  de_trended_seasonalized_series <- transformed_series_list$de_trended_seasonalized_series


  feature_trend <- 1 - stats::var(de_trended_seasonalized_series)/stats::var(de_seasonalized_series)
  feature_seasonality <- 1 - stats::var(de_trended_seasonalized_series)/stats::var(de_trended_series)

  c(feature_trend, feature_seasonality)

}


# Periodicity

auxiliary_function_periodicity <- function(X) {

  series_length <- length(X)


  # De-trending the series

  spline_model <- pspline::sm.spline(x = 1 : series_length, y = X, cv = T)
  T_t <- stats::predict(spline_model)$ysmth
  detrended_series <- X - T_t


  # Finding autocorrelation function for all lags up to 1/3 of series length

  autocorrelations <- as.numeric(TSA::acf(detrended_series, lag.max = ceiling(series_length/3), plot = F)$acf)


}


# Box-Pierce statistic

auxiliary_function_box_pierce_statistic <- function(X, h = 20) {

  series_length <- length(X)

  # Removing trend and seasonality from the series X

  # Decomposing the transformed series in trend, seasonality and noise

  freq_x <- forecast::findfrequency(X)

  if (freq_x > 1) {

    X <- stats::ts(X, frequency = freq_x)
    stl_decomposition <- stats::stl(X, s.window = 'periodic')$time.series
    T_t <- stl_decomposition[,2]
    S_t <- stl_decomposition[,1]
    transformed_series <- stl_decomposition[,3]


  } else {

    spline_model <- pspline::sm.spline(x = 1 : series_length, y = X, cv = T)
    S_t <- rep(0, series_length)
    T_t <- stats::predict(spline_model)$ysmth
    transformed_series <- X - T_t

  }

  box_pierce_statistics_original_series <- stats::Box.test(stats::ts(X), lag = h, type = 'Box-Pierce')$statistic
  box_pierce_statistics_transformed_series <- stats::Box.test(stats::ts(transformed_series), lag = h, type = 'Box-Pierce')$statistic

  return(c(box_pierce_statistics_original_series, box_pierce_statistics_transformed_series))

}


# Functions for data scaling

auxiliary_function_transformation_1 <- function(Q, a, b) {

  numerator <- exp(a * Q) - 1
  denominator <- b + exp(a * Q)
  numerator/denominator

}

auxiliary_function_transformation_2 <- function(Q, a, b) {

  numerator <- (exp(a * Q) - 1) * (b + exp(a))
  denominator <- (b + exp(a * Q)) * (exp(a) - 1)
  numerator/denominator

}

auxiliary_function_transformation_3 <- function(Q, a, b) {

  numerator <- exp((Q - a)/b) - 1
  denominator <- (b + exp((Q - a)/b))
  numerator/denominator

}


# Auxiliary function for feature extraction

auxiliary_www_function <- function(X, h = 20) {


  # Feature extraction stage

  series_length <- length(X[,1])


  # Features of trend and seasonality

  features_trend_seasonality <- c(apply(X, 2, auxiliary_function_trend_seasonality))

  # Features of periodicity

  # features_periodicity <- apply(X, 2, auxiliary_function_periodicity)

  # Computing the UTS transformed series in a form of MTS

  transformed_series <- apply(X, 2, function(x){auxiliary_function_transformed_series(x)$de_trended_seasonalized_series})

  # Features of Box-Pierce statistic

  features_box_pierce_statistic_original <- apply(X, 2, function(x){stats::Box.test(stats::ts(x), lag = h, type = 'Box-Pierce')$statistic})
  features_box_pierce_statistic_transformed <- apply(transformed_series,
                                                     2, function(x){stats::Box.test(stats::ts(x), lag = h, type = 'Box-Pierce')$statistic})

  # Features non-linearity

  features_non_linearity_original <- apply(X, 2, function(x) {tseries::terasvirta.test(stats::ts(x))$statistic})
  features_non_linearity_transformed <- apply(transformed_series, 2, function(x) {tseries::terasvirta.test(stats::ts(x))$statistic})

  # Features skewness

  features_skewness_original <- apply(X, 2, TSA::skewness)
  features_skewness_transformed <- apply(transformed_series, 2, TSA::skewness)

  # Features kurtosis

  features_kurtosis_original <- apply(X, 2, TSA::kurtosis)
  features_kurtosis_transformed <- apply(transformed_series, 2, TSA::kurtosis)

  # Features self-similarity

  features_self_similarity_original <- apply(X, 2, tsfeatures::hurst)

  # Features chaos

  pre_features_chaos_original <- apply(X, 2, tseriesChaos::lyap_k, m = 2, d = 1, t = 1, k = 5, ref = series_length - 1, s = 1, eps = 1)
  features_chaos_original <- exp(pre_features_chaos_original)/(1 + exp(pre_features_chaos_original))

  return(c(features_trend_seasonality, features_box_pierce_statistic_original, features_box_pierce_statistic_transformed,
           features_non_linearity_original, features_non_linearity_transformed, features_skewness_original,
           features_skewness_transformed, features_kurtosis_original, features_kurtosis_transformed,
           features_self_similarity_original, features_chaos_original))

}

