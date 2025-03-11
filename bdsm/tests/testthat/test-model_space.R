test_that("optimal_model_space correctly computes economic_growth_ms", {
  skip_on_os(c("windows", "linux"))
  set.seed(23)

  data_prepared <- bdsm::economic_growth[,1:7] %>%
    feature_standardization(timestamp_col = year, entity_col = country) %>%
    feature_standardization(timestamp_col = year, entity_col = country,
                            cross_sectional = TRUE, scale = FALSE)

  model_space <- optimal_model_space(df = data_prepared, dep_var_col = gdp,
                                     timestamp_col = year, entity_col = country,
                                     init_value = 0.5)

  expect_equal(model_space, economic_growth_ms)
})
