

auxiliary_hwl_function <- function(X) {


  ts_features <- tsfeatures::tsfeatures(X)

  mean_x <- mean(X) # Mean
  if (is.null(mean_x)) {mean_x <- 0}

  var_x <- stats::var(X) # Variance
  if (is.null(var_x)) {var_x <- 0}

  acf_1 <- ts_features$x_acf1 # First order of autocorrelation
  if (is.null(acf_1)) {acf_1 <- 0}

  trend_strength <- ts_features$trend # Strength of the trend
  if (is.null(trend_strength)) {trend_strength <- 0}

  linearity_strength <- ts_features$linearity # Linearity
  if (is.null(linearity_strength)) {linearity_strength <- 0}

  curvature_strength <- ts_features$curvature # Curvature
  if (is.null(curvature_strength)) {curvature_strength <- 0}

  seasonality_strength <- ts_features$seasonal_strength # Strength of the seasonality
  if (is.null(seasonality_strength)) {seasonality_strength <- 0}

  peak_strength <- ts_features$peak # Strength of the peak
  if (is.null(peak_strength)) {peak_strength <- 0}

  trough_strength <- ts_features$trough # Strength of the trough
  if (is.null(trough_strength)) {trough_strength <- 0}

  entropy <- ts_features$entropy # Spectral entropy
  if (is.null(entropy)) {entropy <- 0}

  lumpiness <- as.numeric(tsfeatures::lumpiness(X)) # Changing variance in remainder
  if (is.null(lumpiness)) {lumpiness <- 0}

  spikiness <- ts_features$spike # Strength of spikiness
  if (is.null(spikiness)) {spikiness <- 0}

  level_shift <- as.numeric(tsfeatures::max_level_shift(X)[1]) # Level shift using rolling window
  if (is.null(level_shift)) {level_shift <- 0}

  max_var_shift <- as.numeric(tsfeatures::max_var_shift(X)[1]) # Variance change
  if (is.null(max_var_shift)) {max_var_shift <- 0}

  flat_spots <- as.numeric(tsfeatures::flat_spots(X)) # Flat spots using discretisation
  if (is.null(flat_spots)) {flat_spots <- 0}

  crossing_points <- as.numeric(tsfeatures::crossing_points(X)) # The number of crossing points
  if (is.null(crossing_points)) {crossing_points <- 0}

  max_kl_shift <- as.numeric(tsfeatures::max_kl_shift(X)[1]) # Kullback-Leibler score
  if (is.null(max_kl_shift)) {max_kl_shift <- 0}

  index_max_kl_shift <- as.numeric(tsfeatures::max_kl_shift(X)[2]) # Index of the maximum KL score
  if (is.null(index_max_kl_shift)) {index_max_kl_shift <- 0}


  # Creating the vector of features

  feature_vector <- c(mean_x, var_x, acf_1, trend_strength, linearity_strength, curvature_strength,
                           seasonality_strength, peak_strength, trough_strength, entropy, lumpiness, spikiness,
                           level_shift, max_var_shift, flat_spots, crossing_points, max_kl_shift, index_max_kl_shift)


  # Returning the vector of features

  feature_vector

}
