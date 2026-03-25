# Tests for create_cohort_share() function 

test_that("create_cohort_share computes cohort shares correctly for unequal cohorts", {
  data <- data.frame(
    id = 1:12,
    time = rep(2000, 12),  
    cohort = c(rep(2000, 6),  
               rep(2001, 3),  
               rep(2002, 2),  
               999999)        
  )

  result <- create_cohort_share(data)

  expect_true("cohort_share" %in% names(result))

  counts <- table(data$cohort)
  total <- nrow(data)
  expected_cohort_share <- as.numeric(counts) / total

  cohort_means <- unique(result[, c("cohort", "cohort_share")])
  expect_equal(
    cohort_means$cohort_share[order(cohort_means$cohort)],
    expected_cohort_share[order(as.numeric(names(counts)))],
    tolerance = 1e-12
  )

  expect_equal(sum(unique(result$cohort_share)), 1, tolerance = 1e-12)
})
