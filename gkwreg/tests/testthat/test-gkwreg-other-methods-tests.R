library(testthat)
library(gkwreg)

# Test data setup
data("GasolineYield")

test_that("Basic model update works for simple formulas", {
  # Fit initial model
  m0 <- gkwreg(yield ~ 1, data = GasolineYield, family = "kw")

  # Add predictor
  m1 <- update(m0, . ~ . + temp)
  expect_s3_class(m1, "gkwreg")
  expect_gt(AIC(m0), AIC(m1)) # Better fit expected

  # Remove predictor (back to intercept-only)
  m2 <- update(m1, . ~ . - temp)
  expect_equal(AIC(m0), AIC(m2), tolerance = 1e-6)
})

test_that("Multi-part formula updates work correctly", {
  # Start with two-part model
  m0 <- gkwreg(yield ~ 1 | 1, data = GasolineYield, family = "kw")

  # Add temp to alpha only
  m1 <- update(m0, . ~ . + temp | .)
  expect_s3_class(m1, "gkwreg")

  # Add batch to beta only
  m2 <- update(m1, . ~ . | . + batch)
  expect_s3_class(m2, "gkwreg")

  # Verify both predictors are included appropriately
  expect_true(any(grepl("temp", names(coef(m1)))))
  expect_true(any(grepl("batch", names(coef(m2)))))
})
