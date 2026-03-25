# Tests for calculate_mean_linear_projection() function

test_that("calculate_mean_linear_projection computes the various means of the individual linear projection presicely", {
  data <- data.frame(
  id        = rep(1:4, each = 6),
  time      = rep(2000:2005, times = 4),
  cohort    = c(rep(2002,6), rep(2001,6), rep(2005,6), rep(2000,6)),
  instrument= c(0,0,1,1,1,1,  0,1,1,1,1,1,  0,0,0,0,0,1,  0,0,0,1,1,1),
  control1 = c(100,120,140,160,180,200,   
               150,160,170,180,190,200,   
               80,90,100,200,250,300,     
               200,190,180,170,160,150),  
  control2 = c(10,15,20,25,30,35,         
               20,22,24,26,28,30,         
               5,10,15,40,45,50,          
               30,28,26,24,22,20)          
)
  data$residual_control1 <- resid(lm(control1 ~ factor(id) + factor(time), data = data))
  data$residual_control2 <- resid(lm(control2 ~ factor(id) + factor(time), data = data))
  data$residual_Z <- resid(lm(instrument ~ factor(id) + factor(time), data = data))

  result <- calculate_mean_linear_projection(data, c("control1", "control2"))
  
  expect_true(all(c(
  "projection_id_time",
  "projection_id",
  "projection_time",
  "projection_cohort_time",
  "projection_cohort",
  "projection_grand_mean"
) %in% names(result)))

  gamma <- coef(lm(residual_Z ~ residual_control1 + residual_control2 - 1, data = data))
  g_control1 <- as.numeric(gamma["residual_control1"])
  g_control2 <- as.numeric(gamma["residual_control2"])

  expected_projection_id_time <- g_control1 * data$control1 + g_control2 * data$control2
  expect_equal(result$projection_id_time, expected_projection_id_time, tolerance = 1e-10)

  control1_id_avg <- ave(data$control1, data$id, FUN = mean)
  control2_id_avg <- ave(data$control2, data$id, FUN = mean)
  expected_projection_id <- g_control1 * control1_id_avg + g_control2 * control2_id_avg
  expect_equal(result$projection_id, expected_projection_id, tolerance = 1e-10)

  control1_time_avg <- ave(data$control1, data$time, FUN = mean)
  control2_time_avg <- ave(data$control2, data$time, FUN = mean)
  expected_projection_time <- g_control1 * control1_time_avg + g_control2 * control2_time_avg
  expect_equal(result$projection_time, expected_projection_time, tolerance = 1e-10)

  control1_cohort_time_avg <- ave(data$control1, interaction(data$cohort, data$time, drop = TRUE), FUN = mean)
  control2_cohort_time_avg <- ave(data$control2, interaction(data$cohort, data$time, drop = TRUE), FUN = mean)
  expected_projection_cohort_time <- g_control1 * control1_cohort_time_avg + g_control2 * control2_cohort_time_avg
  expect_equal(result$projection_cohort_time, expected_projection_cohort_time, tolerance = 1e-10)

  control1_cohort_avg <- ave(data$control1, data$cohort, FUN = mean)
  control2_cohort_avg <- ave(data$control2, data$cohort, FUN = mean)
  expected_projection_cohort <- g_control1 * control1_cohort_avg + g_control2 * control2_cohort_avg
  expect_equal(result$projection_cohort, expected_projection_cohort, tolerance = 1e-10)

  control1_grand_mean <- mean(data$control1, na.rm = TRUE)
  control2_grand_mean <- mean(data$control2, na.rm = TRUE)
  expected_projection_grand_mean <- rep(g_control1 * control1_grand_mean + g_control2 * control2_grand_mean, nrow(data))
  expect_equal(result$projection_grand_mean, expected_projection_grand_mean, tolerance = 1e-10)
})
