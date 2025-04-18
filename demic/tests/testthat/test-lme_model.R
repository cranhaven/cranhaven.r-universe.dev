# Mock data for testing
test_data <- data.frame(
  log_cov = rnorm(50),
  GC_content = rnorm(50),
  sample = rep(letters[1:5], each = 10),
  contig = rep(1:10, each = 5)
)

# Test 1: Check if the output is a data frame
test_that("output is a data frame", {
  result <- lme4_model(test_data)
  expect_is(result, "data.frame")
})

# Test 2: Check if the output data frame has the correct structure
test_that("output data frame structure", {
  result <- lme4_model(test_data)
  dput(result)
  expect_named(result, c("(Intercept)", "GC_content", "s_c"))
  expect_equal(nrow(result), 10)
  expect_type(result$GC_content, "double")
  expect_type(result$`(Intercept)`, "double")
})
