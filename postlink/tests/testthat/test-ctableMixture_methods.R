local_edition(3)

# -------------------------------------------------------------------------
# Helper: Mock Object Generator
# -------------------------------------------------------------------------
# Creates a valid 'ctableMixture' object structure without running the engine.
# This ensures we are testing the *methods*, not the statistical estimation.
create_mock_fit <- function() {

 # Mock Estimated Probabilities (2x2 table)
 # Row-major order: (R1, C1), (R1, C2), (R2, C1), (R2, C2)
 p_hat <- matrix(c(0.2, 0.3,
                   0.1, 0.4), nrow = 2, byrow = TRUE)
 dimnames(p_hat) <- list(Row = c("A", "B"), Col = c("X", "Y"))

 # Mock Effective Counts (N=100)
 f_table <- as.table(p_hat * 100)

 # Mock Covariance Matrix (4x4)
 # Diagonal = 0.01, Off-diagonal = 0.001
 # Labels must match the flattening order: R1C1, R1C2, R2C1, R2C2
 param_names <- c("(A, X)", "(A, Y)", "(B, X)", "(B, Y)")
 v_mat <- matrix(0.001, 4, 4)
 diag(v_mat) <- 0.01
 colnames(v_mat) <- rownames(v_mat) <- param_names

 # Construct Object
 obj <- list(
  phat = p_hat,
  ftable = f_table,
  var = v_mat,
  adjustment = list(m.rate = 0.15), # 15% mismatch rate
  converged = TRUE,
  objective = c(100, 90, 85, 84.9999),
  call = call("plctable", formula = ~ Row + Col)
 )

 class(obj) <- "ctableMixture"
 return(obj)
}

# -------------------------------------------------------------------------
# Test: vcov
# -------------------------------------------------------------------------

test_that("vcov.ctableMixture extracts the covariance matrix correctly", {
 fit <- create_mock_fit()
 vc <- vcov(fit)

 expect_true(is.matrix(vc))
 expect_equal(dim(vc), c(4, 4))
 expect_equal(colnames(vc), c("(A, X)", "(A, Y)", "(B, X)", "(B, Y)"))
 expect_equal(vc["(A, X)", "(A, X)"], 0.01) # Check diagonal retrieval
})

# -------------------------------------------------------------------------
# Test: confint
# -------------------------------------------------------------------------

test_that("confint.ctableMixture computes valid Wald intervals", {
 fit <- create_mock_fit()

 # Default (95%)
 ci <- confint(fit)

 expect_equal(dim(ci), c(4, 2))
 expect_equal(colnames(ci), c("2.5 %", "97.5 %"))
 expect_equal(rownames(ci), colnames(fit$var))

 # Check calculation for first parameter (A, X): est=0.2, se=sqrt(0.01)=0.1
 # Lower: 0.2 - 1.96*0.1 = 0.004
 # Upper: 0.2 + 1.96*0.1 = 0.396
 se <- sqrt(0.01)
 est <- 0.2
 crit <- qnorm(0.975)
 expect_equal(ci[1, 1], est - crit * se)
 expect_equal(ci[1, 2], est + crit * se)
})

test_that("confint.ctableMixture truncates bounds at [0, 1]", {
 fit <- create_mock_fit()

 # Create a scenario where SE is large enough to cross 0 and 1
 # (B, Y) est=0.4. Let's pretend SE is huge in the mock object for this test
 fit$var["(B, Y)", "(B, Y)"] <- 0.5^2 # SE = 0.5

 ci <- confint(fit)

 # Lower: 0.4 - 1.96*0.5 = -0.58 -> Should be truncated to 0
 # Upper: 0.4 + 1.96*0.5 = 1.38 -> Should be truncated to 1
 expect_equal(ci["(B, Y)", 1], 0)
 expect_equal(ci["(B, Y)", 2], 1)
})

test_that("confint.ctableMixture handles 'parm' argument correctly", {
 fit <- create_mock_fit()

 # Select by name
 ci_sub <- confint(fit, parm = c("(A, X)", "(B, Y)"))
 expect_equal(nrow(ci_sub), 2)
 expect_equal(rownames(ci_sub), c("(A, X)", "(B, Y)"))

 # Select by index
 ci_idx <- confint(fit, parm = 1:2)
 expect_equal(nrow(ci_idx), 2)
 expect_equal(rownames(ci_idx), c("(A, X)", "(A, Y)"))
})

# -------------------------------------------------------------------------
# Test: summary
# -------------------------------------------------------------------------

test_that("summary.ctableMixture generates correct structure", {
 fit <- create_mock_fit()
 summ <- summary(fit)

 expect_s3_class(summ, "summary.ctableMixture")

 # Check components
 expect_true("coefficients" %in% names(summ))
 expect_true("chisq" %in% names(summ))
 expect_true("ftable" %in% names(summ))
 expect_equal(summ$m.rate, 0.15)

 # Check Coefficient Matrix
 # 4 rows (cells), 4 cols (Est, SE, z, p)
 expect_equal(dim(summ$coefficients), c(4, 4))
 expect_equal(colnames(summ$coefficients),
              c("Estimate", "Std. Error", "z value", "Pr(>|z|)"))

 # Check values passed correctly
 expect_equal(summ$coefficients["(A, X)", "Estimate"], 0.2)
})

test_that("summary.ctableMixture performs Chi-squared test on ADJUSTED table", {
 fit <- create_mock_fit()
 summ <- summary(fit)

 # Manually run chisq on the ftable
 manual_test <- suppressWarnings(chisq.test(fit$ftable))

 # Compare statistic
 expect_equal(summ$chisq$statistic, manual_test$statistic)
 expect_equal(summ$chisq$parameter, manual_test$parameter) # df
})

# -------------------------------------------------------------------------
# Test: print methods
# -------------------------------------------------------------------------

test_that("print.ctableMixture outputs expected information", {
 fit <- create_mock_fit()

 # Capture output
 out <- capture_output(print(fit))

 # Check for key headers
 expect_match(out, "Adjusted Contingency Table")
 expect_match(out, "Linkage Error Adjustment")
 expect_match(out, "Assumed Mismatch Rate.*0.15")
 expect_match(out, "Converged in 4 iterations")
})

test_that("print.summary.ctableMixture outputs statistical results", {
 fit <- create_mock_fit()
 summ <- summary(fit)

 out <- capture_output(print(summ))

 # Check structural headers
 expect_match(out, "Estimated Cell Probabilities")
 expect_match(out, "Inference for Independence")

 # Check that Chi-sq results are printed
 expect_match(out, "Pearson's Chi-squared Statistic")
 expect_match(out, "X-squared =")

 # Check that p-values are printed
 expect_match(out, "Pr(>|z|)", fixed = TRUE)
})

test_that("print methods handle non-convergence gracefully", {
 fit <- create_mock_fit()
 fit$converged <- FALSE

 out <- capture_output(print(fit))
 expect_match(out, "Status: Not converged")
})
