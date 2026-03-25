# Tests for create_time_share() function

test_that("create_time_share computes average exposure correctly", {
  data <- data.frame(
    id = rep(1:3, each = 4),
    time = rep(2000:2003, times = 3),
    instrument = c(
      0,1,1,1,   
      0,0,1,1,   
      0,0,0,0    
    ),
    cohort = c(
      2001,2001,2001,2001,  
      2002,2002,2002,2002,  
      999999,999999,999999,999999  
    )
  )

  result <- create_time_share(data)

  expect_true("time_share" %in% names(result))

  expected_time_share <- c(
    mean(c(0,1,1,1)), 
    mean(c(0,0,1,1)), 
    0                 
  )

  cohort_means <- unique(result[, c("cohort", "time_share")])

  expect_equal(cohort_means$time_share[cohort_means$cohort == 2001], expected_time_share[1], tolerance = 1e-12)
  expect_equal(cohort_means$time_share[cohort_means$cohort == 2002], expected_time_share[2], tolerance = 1e-12)
  expect_equal(cohort_means$time_share[cohort_means$cohort == 999999], expected_time_share[3], tolerance = 1e-12)
})
