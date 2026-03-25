# Tests for the create_cohort_share() function

test_that("create_cohort_share computes correct cohort shares", {
  data <- data.frame(
    id = rep(1:3, each = 2),
    time = rep(c(2000, 2001), times = 3),
    cohort = c(
      2000, 2000,  
      2001, 2001,  
      2000, 2000   
    )
  )

  # 4 obs in cohort 2000, 2 obs in cohort 2001 → shares: 4/6 and 2/6
  result <- create_cohort_share(data)

  expect_true("cohort_share" %in% colnames(result))
  expect_equal(
    unique(result$cohort_share[result$cohort == 2000]),
    4 / 6
  )
  expect_equal(
    unique(result$cohort_share[result$cohort == 2001]),
    2 / 6
  )
})
