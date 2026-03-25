# tests for twfeiv_decomp() function

# without controls
test_that("twfeiv_decomp() decomposes the TWFEIV estimate precisely without controls", {
  data <- data.frame(
  id        = rep(1:5, each = 6),
  time      = rep(2000:2005, times = 5),
  instrument = c(0,0,1,1,1,1,  0,1,1,1,1,1,  0,0,0,0,0,1,  0,0,0,1,1,1, 0,1,1,1,1,1),
  treatment = c(
    10, 10, 20, 20, 20, 20,   
    10, 20, 20, 20, 20, 20,   
    10, 10, 10, 10, 10, 20,   
    10, 10, 10, 20, 20, 20,
    10, 30, 30, 30, 30, 30     
  ),
  outcome = c(
    100, 100, 200, 200, 200, 200,   
    100, 200, 200, 200, 200, 200,
    100, 100, 100, 100, 100, 200,  
    100, 100, 100, 200, 200, 200,
    100, 200, 200, 200, 200, 200  
  )
)
  # TWFEIV estimate without controls
  twfeiv_estimate <- AER::ivreg(outcome ~ treatment + factor(time) + factor(id)|factor(time) + factor(id) + instrument, data = data)$coef["treatment"]
  
  # decomposition result
  result <- twfeiv_decomp(outcome ~ treatment|instrument, data = data, id_var = "id", time_var = "time", summary_output = FALSE)

  # weighted mean of the Wald-DID estimates
  weighted_mean_Wald_DID_estimates <- weighted.mean(result$Wald_DID_estimate, result$weight)

  # test
  expect_equal(as.numeric(weighted_mean_Wald_DID_estimates), as.numeric(twfeiv_estimate), tolerance = 1e-12)
})

# with controls
test_that("twfeiv_decomp() decomposes the TWFEIV estimate precisely with controls", {
  data_controls <- data.frame(
  id        = rep(1:5, each = 6),
  time      = rep(2000:2005, times = 5),
  instrument = c(0,0,1,1,1,1,  0,1,1,1,1,1,  0,0,0,0,0,1,  0,0,0,1,1,1, 0,1,1,1,1,1),
  treatment = c(
    10, 10, 20, 20, 20, 20,   
    10, 20, 20, 20, 20, 20,   
    10, 10, 10, 10, 10, 20,   
    10, 10, 10, 20, 20, 20,
    10, 30, 30, 30, 30, 30    
  ),
  outcome = c(
    100, 100, 200, 200, 200, 200,   
    100, 200, 200, 200, 200, 200,
    100, 100, 100, 100, 100, 200,  
    100, 100, 100, 200, 200, 200,
    100, 200, 200, 200, 200, 200 
  ),
  covariate = c(
    3.2, 4.5, 7.8, 2.3, 9.4, 11.6,   
    5.7, 8.1, 6.2, 10.4, 12.7, 3.9,  
    4.1, 9.8, 2.7, 7.5, 5.3, 11.2,   
    6.4, 2.9, 8.7, 3.1, 9.0, 7.2,
    6.9, 3.9, 5.7, 1.4, 5.2, 3.2     
  )
)

  # TWFEIV estimate with controls
  twfeiv_estimate_controls <- AER::ivreg(outcome ~ treatment + covariate + factor(time) + factor(id)|covariate + factor(time) + factor(id) + instrument, data = data_controls)$coef["treatment"]
  
  # decomposition result with controls
  result_controls <- twfeiv_decomp(outcome ~ treatment + covariate|covariate + instrument, data = data_controls, id_var = "id", time_var = "time", summary_output = FALSE)

  # within IV coefficient
  within_IV_coefficient <- result_controls$within_IV_coefficient
  Omega <- result_controls$Omega

  # between IV coefficient
  between_IV_coefficient_non_decompose <- result_controls$between_IV_coefficient

  # weighted mean of within and between IV coefficients
  weighted_mean_within_between_IV_coefficients_non_decompose <- Omega * within_IV_coefficient + (1 - Omega) * between_IV_coefficient_non_decompose
  
  # between IV coefficient
  between_IV_coefficient <- weighted.mean(result_controls$exposed_unexposed_combinations$between_IV_coefficient_kl,
                                          result_controls$exposed_unexposed_combinations$weight_kl)
  
  # weighted mean of within and between IV coefficients
  weighted_mean_within_between_IV_coefficients <- Omega * within_IV_coefficient + (1 - Omega) * between_IV_coefficient

  # test
  expect_equal(as.numeric(weighted_mean_within_between_IV_coefficients_non_decompose), as.numeric(twfeiv_estimate_controls), tolerance = 1e-12)
  expect_equal(as.numeric(weighted_mean_within_between_IV_coefficients), as.numeric(twfeiv_estimate_controls), tolerance = 1e-12)
})
