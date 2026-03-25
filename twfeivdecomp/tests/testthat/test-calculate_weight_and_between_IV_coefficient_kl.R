# Tests for calculate_weight_and_between_IV_coefficient_kl() function

test_that("calculate_weight_and_between_IV_coefficient_kl computes the weight and between IV coefficient correctly", {
  data <- data.frame(
  id = rep(1:3, each = 5),
  time = rep(2000:2004, times = 3),
  outcome = c(100,200,200,200,200,   
              100,100,300,300,300,    
              100,100,100,400,400),  
  treatment = c(10,20,20,20,20,      
                10,10,30,30,30,       
                10,10,10,40,40),       
  instrument = c(0,1,1,1,1,  
                 0,0,1,1,1,   
                 0,0,0,1,1),  
  control1 = c(5, 8, 6, 9, 7,      
               4, 7, 5, 6, 8,      
               6, 5, 9, 7, 8),
  cohort = c(2001,2001,2001,2001,2001,   
             2002,2002,2002,2002,2002,   
             2003,2003,2003,2003,2003),  
  
  control1_cohort_time_average = c(
    5, 8, 6, 9, 7,     
    4, 7, 5, 6, 8,     
    6, 5, 9, 7, 8
  )     
)

  control_vars <- c("control1")
  data$residual_Z <- residuals(lm(instrument ~ factor(id) + factor(time), data = data))
  data$residual_control1 <- residuals(lm(control1 ~ factor(id) + factor(time), data = data))
  data <- calculate_mean_linear_projection(data, control_vars)

  result <- calculate_weight_and_between_IV_coefficient_kl(data, control_vars)

  expect_type(result, "list")
  expect_true(all(c("weight_kl", "between_IV_coefficient_kl") %in% names(result)))

  result_partial_out_instrument <- partial_out_instrument(data)
  data1 <- result_partial_out_instrument$data
  V_Z <- result_partial_out_instrument$V_Z

  data2 <- partial_out_cohort_time_covariate(data1, control_vars)

  result_calculate_p_tilde_kl <- calculate_p_tilde_kl(data2, control_vars)
  data3 <- result_calculate_p_tilde_kl$data
  r_squared <- result_calculate_p_tilde_kl$r_squared

  result_calculate_V_b_kl_zp <- calculate_V_b_kl_zp(data3)
  data4 <- result_calculate_V_b_kl_zp$data
  V_b_kl_zp <- result_calculate_V_b_kl_zp$V_b_kl_zp

  between_kl_zp_outcome <- calculate_between_kl_zp(data4, "outcome")
  between_kl_zp_treatment <- calculate_between_kl_zp(data4, "treatment")
  between_kl_outcome <- calculate_between_kl(data4, control_vars, "outcome")
  between_kl_treatment <- calculate_between_kl(data4, control_vars, "treatment")
  N <- nrow(data4)

  expected_between_IV_coefficient_kl <- calculate_between_IV_coefficient_kl(
    r_squared, V_Z, between_kl_outcome, V_b_kl_zp,
    between_kl_zp_outcome, between_kl_treatment, between_kl_zp_treatment
  )
  expected_weight_kl <- calculate_weight_kl(
    N, r_squared, V_Z, V_b_kl_zp,
    between_kl_treatment, between_kl_zp_treatment
  )

  expect_equal(result$between_IV_coefficient_kl, expected_between_IV_coefficient_kl, tolerance = 1e-12)
  expect_equal(result$weight_kl, expected_weight_kl, tolerance = 1e-12)
})