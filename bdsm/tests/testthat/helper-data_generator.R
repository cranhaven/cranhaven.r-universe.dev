generate_test_data <- function() {
  data.frame(
    entities = rep(1:4, 5),
    times = rep(seq(1960, 2000, 10), each = 4),
    dep_var = stats::rnorm(20), a = stats::rnorm(20), b = stats::rnorm(20)
  )
}

generate_test_feature_standard_data <- function(cross_sectional = FALSE, scale = TRUE) {
  feature_standardization(
    generate_test_data(),
    timestamp_col = times,
    entity_col = entities,
    cross_sectional,
    scale
  )
}
