test_that(paste("likelihoods_summary computes correct approximations of",
                "standard deviations based on economic_growth_ms"), {
  set.seed(23)

  data_prepared <- bdsm::economic_growth[,1:7] %>%
    feature_standardization(timestamp_col = year, entity_col = country) %>%
    feature_standardization(timestamp_col = year, entity_col = country,
                            cross_sectional = TRUE, scale = FALSE)

  lik_info <- likelihoods_summary(df = data_prepared, dep_var_col = gdp,
                                  timestamp_col = year, entity_col = country,
                                  model_space = economic_growth_ms)
  expect_equal(lik_info, economic_growth_liks)
})

test_that(paste("parameters_summary computes correct approximations of",
                "BMA parameters of interest based on economic_growth_ms"), {
  skip_on_os(c("windows", "linux"))
  set.seed(20)

  data_prepared <- bdsm::economic_growth[,1:7] %>%
    feature_standardization(timestamp_col = year, entity_col = country) %>%
    feature_standardization(timestamp_col = year, entity_col = country,
                            cross_sectional = TRUE, scale = FALSE)

  regressors <- regressor_names(data_prepared, year, country, gdp)

  bma_result <- bma_summary(df = data_prepared, dep_var_col = gdp,
                            timestamp_col = year, entity_col = country,
                            model_space = economic_growth_ms)

  bma_params <- parameters_summary(
    regressors = regressors, bet = bma_result$bet, pvarh = bma_result$pvarh,
    pvarr = bma_result$pvarr, fy = bma_result$fy,
    fyt = bma_result$fyt, ppmsize = bma_result$ppmsize,
    cout = bma_result$cout, nts = bma_result$nts, pts = bma_result$pts,
    variables_n = bma_result$variables_n
  )

  expect_equal(bma_params, economic_growth_bma_params)
})
