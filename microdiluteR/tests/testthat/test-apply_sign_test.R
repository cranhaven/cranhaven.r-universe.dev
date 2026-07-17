# Define test data
stats_data <- data.frame(
  Group = c(rep("A", 5), rep("B", 7)),
  Value = c(-2:2, c(80,100,110,100,120,90,100))
)
summarized_data <- data.frame(
  Group = c("A", "B"),
  Mean = c(0, 100)
)

test_that("apply_sign_test() correctly applies sign test", {

  # Call function
  result <- apply_sign_test(stats_data,
                            summarized_data,
                            grouping = "Group")
  
  # Test case 1: Check if result is a data frame
  expect_s3_class(result, "data.frame")

  # Check if result contains expected columns
  expect_named(result,
               c("Group", "Mean", ".y.", "group1", "group2", "n", "statistic", "df", "p", "p.signif"),
               ignore.order = T)
  
  # Check if p.signif values are properly calculated
  expect_equal(unique(result$p), c(1, 0.0156))
  
  # Check if first test results to 'NA' as character in column 'p.signif' due to sample size < 6
  expect_equal(result$p.signif[1], "NA")
})
