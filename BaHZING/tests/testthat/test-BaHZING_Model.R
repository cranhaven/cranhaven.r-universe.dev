test_that("test BaHZING_Model", {
  ### Load example data
  data("iHMP_Reduced")

  ### Format microbiome data
  formatted_data <- Format_BaHZING(iHMP_Reduced)

  ### Perform Bayesian hierarchical zero-inflated negative binomial regression with g-computation
  ### Specify a mixture of exposures
  x <- c("soft_drinks_dietnum","diet_soft_drinks_dietnum"
         # "fruit_juice_dietnum","water_dietnum",
         # "alcohol_dietnum","yogurt_dietnum","dairy_dietnum","probiotic_dietnum","fruits_no_juice_dietnum",
         # "vegetables_dietnum","beans_soy_dietnum","whole_grains_dietnum","starch_dietnum"
         # ,"eggs_dietnum",
         # "processed_meat_dietnum","red_meat_dietnum","white_meat_dietnum","shellfish_dietnum","fish_dietnum",
         # "sweets_dietnum"
         )

  ### Specify a set of covariates
  # covar <- c("consent_age","sex",paste0("race",0:3),paste0("educ",0:7))
  covar <- c("consent_age")

  ###Set exposure standardization to standard_normal or quantiles
  exposure_standardization = "standard_normal"

  # Test when it works -----
  ## Test BaHZING with covariates ----
  results <- BaHZING_Model(formatted_data = formatted_data,
                           x = x,
                           covar = covar,
                           exposure_standardization = "none",
                           n.chains = 1,
                           n.adapt = 60,
                           n.iter.burnin = 2,
                           n.iter.sample= 2,
                           counterfactual_profiles = matrix(c(0,0,1,1),
                                                            ncol = 2,
                                                            byrow = TRUE))
  testthat::expect_equal(object = ncol(results), expected = 11)


  ## Test BaH-ZING without covariates ----
  results <- BaHZING_Model(formatted_data = formatted_data,
                           covar = NULL,
                           x = x,
                           exposure_standardization = "standard_normal",
                           n.chains = 1,
                           n.adapt = 60,
                           n.iter.burnin = 2,
                           n.iter.sample= 2,
                           counterfactual_profiles = c(-0.5, 0.5),
                           q = 2)

  testthat::expect_equal(object = ncol(results), expected = 11)

  ## Test BaH-ZING with quantiles ----
  results <- BaHZING_Model(formatted_data = formatted_data,
                           covar = NULL,
                           x = c("reads_human", "reads_qc_fail"),
                           exposure_standardization = "quantile",
                           n.chains = 1,
                           n.adapt = 60,
                           n.iter.burnin = 2,
                           n.iter.sample= 2,
                           q = 2)

  testthat::expect_equal(object = ncol(results), expected = 11)

  # Test Errors ----

  ## if counterfactual_profiles is a vector ----
  # Fail if wrong dimension vector
  testthat::expect_error(
    BaHZING_Model(formatted_data = formatted_data,
                                       x = x,
                  counterfactual_profiles = c(1,2,3)),
    "counterfactual_profiles must have 2 elements when provided as a vector")

  # Fail if not numeric vector
  testthat::expect_error(
    BaHZING_Model(formatted_data = formatted_data,
                  x = x,
                  counterfactual_profiles = c("A", "B")),
    "counterfactual_profiles must be numeric")

  # Check if matrix ----
  # Fail if wrong dimension matrix
  testthat::expect_error(
    BaHZING_Model(formatted_data = formatted_data,
                  x = x,
                  counterfactual_profiles = matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3)),
    "When provided as a matrix, the number of columns in counterfactual_profiles must be equal to the number of exposures in the model.")

  testthat::expect_error(
    BaHZING_Model(formatted_data = formatted_data,
                  x = x,
                  counterfactual_profiles = matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2)),
    "counterfactual_profiles must have 2 rows when provided as a matrix.")


  # Fail if dataframe ----
  testthat::expect_error(
    BaHZING_Model(formatted_data = formatted_data,
                  x = x,
                  counterfactual_profiles = data.frame(matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3))),
    "counterfactual_profiles must be either a numeric 2xP matrix or a numeric vector with length P.")


  # Fail if non-numeric matrix ---
  testthat::expect_error(
    BaHZING_Model(formatted_data = formatted_data,
                  x = x,
                  counterfactual_profiles = matrix(c(rep("A", 4)), nrow = 2, ncol = 2)),
    "counterfactual_profiles must be numeric.")

})
