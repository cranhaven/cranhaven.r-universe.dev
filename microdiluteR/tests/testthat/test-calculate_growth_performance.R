# Define test data
test_data <- data.frame(
  Group = rep(LETTERS[1:2], each = 100, times = 2),
  Experiment = rep(c("Exp1", "Exp2"), each = 50, times = 2),
  Treatment = rep(c("Control", "Trt1", "Trt2", "Trt3"), times = 100), 
  Concentration = rep(1:2, each = 20, times = 10),
  Timepoint = rep(paste0("T",0:1), each = 50, times = 4),
  Value = runif(400, 1, 10)
)

test_that("calculate_growth_performance() correctly computes growth performance", {
  
  # Test case 1: Call function without grouping
  result_no_grouping <- calculate_growth_performance(test_data)
  expect_named(result_no_grouping,
               c("Group", "Experiment", "Treatment", "Concentration", 
                 "Timepoint", "Value", "control_mean"))
  
  # Test case 2: Call function with treatment grouping
  result_with_treatment_grouping <- calculate_growth_performance(test_data,
                                                                 treatment_grouping = TRUE)
  expect_named(result_with_treatment_grouping,
               c("Group", "Experiment", "Treatment", "Concentration", 
                 "Timepoint", "Value", "control_mean"))
  
  # Test case 3: Call function with concentration grouping
  result_with_concentration_grouping <- calculate_growth_performance(test_data,
                                                                     concentration_grouping = TRUE)
  expect_named(result_with_concentration_grouping,
               c("Group", "Experiment", "Treatment", "Concentration", 
                 "Timepoint", "Value", "control_mean"))
  
  # Test case 4: Call function with both treatment and concentration grouping
  result_with_both_grouping <- calculate_growth_performance(test_data,
                                                            treatment_grouping = TRUE,
                                                            concentration_grouping = TRUE)
  expect_identical(colnames(result_with_both_grouping),
                   c("Group", "Experiment", "Treatment", "Concentration",
                     "Timepoint", "Value", "control_mean"))
})
