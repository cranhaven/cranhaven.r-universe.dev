# Tests for calculate_within_between_terms() function

test_that("calculate_within_between_terms computes within and between terms correctly", {
  data <- data.frame(
    id = rep(1:2, each = 5),
    time = rep(2000:2004, times = 2),
    cohort = c(rep(2000, 5), rep(2002, 5)),
    z_it = c(1, 3, 2, 5, 4,   
             2, 4, 3, 6, 5)   
  )

  result <- calculate_within_between_terms(data)

  z_i_bar <- ave(data$z_it, data$id, FUN = mean)
  z_t_bar <- ave(data$z_it, data$time, FUN = mean)
  z_bar_bar <- mean(data$z_it)
  z_kt_bar <- ave(data$z_it, data$cohort, data$time, FUN = mean)
  z_k_bar <- ave(data$z_it, data$cohort, FUN = mean)

  expected_within_z <- (data$z_it - z_i_bar) - (z_kt_bar - z_k_bar)
  expected_between_z <- (z_kt_bar - z_k_bar) - (z_t_bar - z_bar_bar)
  
  expect_true(all(c("within_z", "between_z") %in% names(result)))
  expect_equal(result$within_z, expected_within_z, tolerance = 1e-12)
  expect_equal(result$between_z, expected_between_z, tolerance = 1e-12)
})


