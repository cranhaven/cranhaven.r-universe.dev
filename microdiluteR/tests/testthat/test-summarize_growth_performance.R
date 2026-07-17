# Define test data
test_data <- data.frame(
  Group = rep(LETTERS[1:2], each = 3),
  Treatment = rep(c("Control", "Treatment", "Treatment"), 2),
  Concentration = c(1, 1, 1, 3, 3, 3),
  Timepoint = c(1, 1, 1, 2, 2, 2),
  Value = c(10, 12, 15, 18, 20, 15)
)

test_that("summarize_growth_performance() correctly computes summarized data", {
  
  # Test case 1: Function call with parameter compute_sd = FALSE
  result_stderr <- summarize_growth_performance(test_data, compute_sd = FALSE)
  expect_named(result_stderr,
               c("Group","Treatment","Concentration","Timepoint","mean","stderr"))
  expect_equal(nrow(result_stderr), 2)  # Expecting two groups
  
  # Test case 2: Function call with parameter compute_sd = TRUE
  result_sd <- summarize_growth_performance(test_data, compute_sd = TRUE)
  expect_named(result_sd,
               c("Group", "Treatment", "Concentration", "Timepoint", "mean", "sd", "n", "stderr"))
  expect_equal(nrow(result_sd), 2)  # Expecting two groups
})
