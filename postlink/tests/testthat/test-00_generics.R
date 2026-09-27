# tests/testthat/test-s3_architecture.R

library(testthat)
library(postlink)

# Test Graceful Failures for Unsupported Generics

test_that("Unsupported generics gracefully fail for plmodel and plglm objects", {

 # Create a mock fitted object with the new class hierarchy
 mock_fit <- structure(list(), class = c("plglm", "plmodel"))

 expected_error <- "This metric is not directly applicable for post-linkage adjusted models"

 # Global Likelihood Intercepts
 expect_error(logLik(mock_fit), expected_error)
 expect_error(profile(mock_fit), expected_error)
 expect_error(anova(mock_fit), expected_error)
 expect_error(extractAIC(mock_fit), expected_error)

 # GLM-Specific Residual Intercepts
 expect_error(cooks.distance(mock_fit), expected_error)
 expect_error(rstudent(mock_fit), expected_error)
})

# Test coeftest() Compatibility for glmELE

test_that("coef() and vcov() for glmELE satisfy lmtest::coeftest() requirements", {

 # Mock glmELE object (matrix of coefficients, list of variance matrices)
 mock_ele <- structure(
  list(
   coefficients = matrix(c(1.1, 2.2, 3.3, 4.4), nrow = 2, byrow = TRUE,
                         dimnames = list(c("ratio", "BLUE"), c("Intercept", "X1"))),
   var = list(
    ratio = matrix(c(0.1, 0.05, 0.05, 0.2), nrow = 2,
                   dimnames = list(c("Intercept", "X1"), c("Intercept", "X1"))),
    BLUE = matrix(c(0.08, 0.04, 0.04, 0.15), nrow = 2,
                  dimnames = list(c("Intercept", "X1"), c("Intercept", "X1")))
   ),
   df.residual = 98
  ),
  class = c("glmELE", "plglm", "plmodel")
 )

 # Test default extraction (should default to first row: 'ratio')
 est_default <- coef(mock_ele)
 expect_type(est_default, "double")
 expect_null(dim(est_default))
 expect_named(est_default, c("Intercept", "X1"))

 # Test extraction ('BLUE')
 est_blue <- coef(mock_ele, weight.matrix = "BLUE")
 vcov_blue <- vcov(mock_ele, weight.matrix = "BLUE")

 expect_equal(est_blue[["Intercept"]], 3.3)
 expect_true(is.matrix(vcov_blue))
 expect_equal(rownames(vcov_blue), names(est_blue))
 expect_equal(colnames(vcov_blue), names(est_blue))

 # Test residual degrees of freedom extraction
 expect_equal(df.residual(mock_ele), 98)
})

# Test coeftest() Compatibility for glmMixture

test_that("coef() and vcov() for glmMixture satisfy lmtest::coeftest() requirements", {

 mock_mix <- structure(
  list(
   coefficients = c(Intercept = -1.2, X1 = 0.8),
   var = matrix(c(0.5, -0.1, -0.1, 0.3), nrow = 2,
                dimnames = list(c("Intercept", "X1"), c("Intercept", "X1"))),
   df.residual = 150
  ),
  class = c("glmMixture", "plglm", "plmodel")
 )

 est <- coef(mock_mix)
 v <- vcov(mock_mix)

 expect_type(est, "double")
 expect_null(dim(est))
 expect_named(est, c("Intercept", "X1"))

 expect_true(is.matrix(v))
 expect_equal(rownames(v), names(est))
 expect_equal(colnames(v), names(est))
})

# Test coeftest() Compatibility for glmMixBayes

test_that("coef() for glmMixBayes correctly collapses MCMC posterior matrices", {

 # Mock MCMC draws (100 draws, 2 predictors)
 set.seed(42)
 draws <- matrix(rnorm(200), nrow = 100, ncol = 2,
                 dimnames = list(NULL, c("Intercept", "X1")))

 mock_bayes <- structure(
  list(
   estimates = list(coefficients = draws)
  ),
  class = c("glmMixBayes", "plglm", "plmodel")
 )

 est <- coef(mock_bayes)

 expect_type(est, "double")
 expect_null(dim(est))
 expect_named(est, c("Intercept", "X1"))
 expect_equal(est[["Intercept"]], mean(draws[, "Intercept"]))
})

# Test NextMethod("print") for Adjustment Objects

test_that("print.adjustment successfully passes metadata to subclass prints", {

 # Create a dummy dataset
 dummy_data <- data.frame(id = 1:50, val = rnorm(50))

 # Construct the adjustment objects using the actual package constructors
 adj_ele <- adjELE(linked.data = dummy_data, m.rate = 0.1)
 adj_mix <- adjMixture(linked.data = dummy_data, m.rate = 0.1)
 adj_bayes <- adjMixBayes(linked.data = dummy_data)

 # Capture the printed output
 out_ele <- capture.output(print(adj_ele))
 out_mix <- capture.output(print(adj_mix))
 out_bayes <- capture.output(print(adj_bayes))

 # Assert that the specific header prints
 expect_true(any(grepl("Adjustment Object: Exchangeable Linkage Errors", out_ele)))
 expect_true(any(grepl("Adjustment Object: Mixture Model", out_mix)))
 expect_true(any(grepl("Adjustment Object: Bayesian Mixture", out_bayes)))

 # Assert that NextMethod() successfully printed the shared base class data
 expect_true(any(grepl("\\* Linked Data:", out_ele)))
 expect_true(any(grepl("Observations:\\s+50", out_ele)))

 expect_true(any(grepl("\\* Linked Data:", out_mix)))
 expect_true(any(grepl("Observations:\\s+50", out_mix)))

 expect_true(any(grepl("\\* Linked Data:", out_bayes)))
 expect_true(any(grepl("Observations:\\s+50", out_bayes)))
})
