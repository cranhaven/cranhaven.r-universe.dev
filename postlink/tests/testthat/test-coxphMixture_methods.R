local_edition(3)

# -------------------------------------------------------------------------
# Helper: Mock Object Generation
# -------------------------------------------------------------------------
# Creates a synthetic coxphMixture object to isolate method testing
# from the fitting function's complexity and runtime.

create_mock_fit <- function() {
 # Dimensions
 n <- 100
 p <- 2 # 2 Outcome Covariates
 q <- 2 # 2 Mismatch Covariates (including intercept)

 # Mock Coefficients
 beta <- c(0.5, -0.5)
 names(beta) <- c("x1", "x2")

 gamma <- c(0.2, -0.1)
 names(gamma) <- c("z1", "z2")

 # Mock Variance-Covariance Matrix (4x4)
 # Structure: [Beta_2x2, Cov_2x2; Cov_2x2, Gamma_2x2]
 # Diagonal = 0.04 (SE = 0.2)
 vc <- diag(rep(0.04, 4))
 rownames(vc) <- colnames(vc) <- c("x1", "x2", "m.z1", "m.z2")

 # Mock Data
 set.seed(123)
 x <- matrix(rnorm(n * p), ncol = p)
 colnames(x) <- c("x1", "x2")

 # Mock Survival Outcome
 time <- sort(rexp(n, rate = 0.1)) # Sorted for convenience in checking
 status <- sample(0:1, n, replace = TRUE)
 y_surv <- Surv(time, status)

 # Mock Baseline Cumulative Hazard (Lambdahat0)
 # Monotonically increasing vector corresponding to observations (since data is sorted by time)
 lambdahat0_vec <- cumsum(runif(n, 0.01, 0.05))

 # Mock Call
 call_obj <- call("coxphMixture", formula = Surv(time, status) ~ x1 + x2)

 # Construct Object
 obj <- list(
  coefficients = beta,
  m.coefficients = gamma,
  var = vc,
  linear.predictors = as.vector(x %*% beta),
  means = colMeans(x),
  n = n,
  nevent = sum(status),
  match.prob = runif(n, 0.8, 1.0), # High correct match probability
  objective = seq(100, 90, length.out = 10),
  converged = TRUE,
  Lambdahat0 = lambdahat0_vec,
  call = call_obj,
  x = x,
  y = cbind(time = time, status = status) # Matrix representation of Surv
 )

 # Add terms and model frame for predict compatibility
 mf <- data.frame(time = time, status = status, x1 = x[,1], x2 = x[,2])
 # Ensure the formula environment is clean for testing
 f <- Surv(time, status) ~ x1 + x2
 trms <- terms(f, data = mf)

 obj$terms <- trms
 obj$model <- mf

 class(obj) <- "coxphMixture"
 return(obj)
}

# -------------------------------------------------------------------------
# Tests: vcov
# -------------------------------------------------------------------------

test_that("vcov.coxphMixture returns correct matrix structure", {
 fit <- create_mock_fit()
 vc <- vcov(fit)

 expect_true(is.matrix(vc))
 expect_equal(dim(vc), c(4, 4))
 expect_equal(rownames(vc), c("x1", "x2", "m.z1", "m.z2"))
 expect_equal(unname(diag(vc)), rep(0.04, 4))
})

# -------------------------------------------------------------------------
# Tests: confint
# -------------------------------------------------------------------------

test_that("confint.coxphMixture computes correct intervals", {
 fit <- create_mock_fit()
 ci <- confint(fit, level = 0.95)

 expect_equal(dim(ci), c(4, 2))
 expect_equal(rownames(ci), c("x1", "x2", "z1", "z2"))

 # Manual check for x1: beta=0.5, se=sqrt(0.04)=0.2
 # CI = 0.5 +/- 1.96 * 0.2 = [0.108, 0.892]
 crit <- qnorm(0.975)
 expected_lower <- 0.5 - crit * 0.2
 expected_upper <- 0.5 + crit * 0.2

 expect_equal(ci["x1", 1], expected_lower)
 expect_equal(ci["x1", 2], expected_upper)
})

test_that("confint.coxphMixture handles parameter subsetting", {
 fit <- create_mock_fit()

 # Subsetting by name
 ci_sub <- confint(fit, parm = "x2")
 expect_equal(nrow(ci_sub), 1)
 expect_equal(rownames(ci_sub), "x2")

 # Subsetting by index
 ci_idx <- confint(fit, parm = 1:2)
 expect_equal(rownames(ci_idx), c("x1", "x2"))
})

# -------------------------------------------------------------------------
# Tests: summary
# -------------------------------------------------------------------------

test_that("summary.coxphMixture produces valid output structure", {
 fit <- create_mock_fit()
 summ <- summary(fit)

 expect_s3_class(summ, "summary.coxphMixture")

 # Check components
 expect_true("coefficients" %in% names(summ))
 expect_true("m.coefficients" %in% names(summ))
 expect_true("conf.int" %in% names(summ))
 expect_true("avgcmr" %in% names(summ))

 # Check coefficient table structure
 # Cols: coef, exp(coef), se(coef), z, p
 expect_equal(ncol(summ$coefficients), 5)
 expect_equal(nrow(summ$coefficients), 2) # Only Beta

 # Check mismatch coef table
 expect_equal(nrow(summ$m.coefficients), 2) # Gamma

 # Check values
 beta_x1 <- summ$coefficients["x1", "coef"]
 expect_equal(beta_x1, 0.5)
})

# -------------------------------------------------------------------------
# Tests: print methods
# -------------------------------------------------------------------------

test_that("print.coxphMixture runs without error", {
 fit <- create_mock_fit()
 expect_output(print(fit), "Outcome Model Coefficients")
 expect_output(print(fit), "Mismatch Model Coefficients")
 expect_output(print(fit), "Likelihood ratio test.*not available")
})

test_that("print.summary.coxphMixture runs without error", {
 fit <- create_mock_fit()
 summ <- summary(fit)
 expect_output(print(summ), "Outcome Model \\(Cox PH\\)")
 expect_output(print(summ), "Hazard Ratios")
 expect_output(print(summ), "Average Estimated Correct Match Rate")
})

# -------------------------------------------------------------------------
# Tests: predict (Original Data)
# -------------------------------------------------------------------------

test_that("predict.coxphMixture: 'lp' and 'risk' types (Original Data)", {
 fit <- create_mock_fit()

 # LP
 pred_lp <- predict(fit, type = "lp")
 expect_equal(length(pred_lp), fit$n)
 expect_equal(pred_lp, fit$linear.predictors)

 # Risk
 pred_risk <- predict(fit, type = "risk")
 expect_equal(pred_risk, exp(fit$linear.predictors))

 # With SE
 pred_se <- predict(fit, type = "lp", se.fit = TRUE)
 expect_type(pred_se, "list")
 expect_true(all(c("fit", "se.fit") %in% names(pred_se)))
 expect_equal(length(pred_se$se.fit), fit$n)

 # Manual SE Check for x=0 (mean centering affects this, but here we just check positivity)
 expect_true(all(pred_se$se.fit > 0))
})

test_that("predict.coxphMixture: 'expected' and 'survival' types (Original Data)", {
 fit <- create_mock_fit()

 # Expected = Lambda0(t) * exp(lp)
 pred_exp <- predict(fit, type = "expected")
 expected_manual <- fit$Lambdahat0 * exp(fit$linear.predictors)
 expect_equal(pred_exp, expected_manual)

 # Survival = exp(-Expected)
 pred_surv <- predict(fit, type = "survival")
 expect_equal(pred_surv, exp(-expected_manual))
})

# -------------------------------------------------------------------------
# Tests: predict (New Data)
# -------------------------------------------------------------------------

test_that("predict.coxphMixture: 'lp' type (New Data)", {
 fit <- create_mock_fit()

 # Create new data
 nd <- data.frame(x1 = c(0, 1), x2 = c(0, 1))

 pred <- predict(fit, newdata = nd, type = "lp")
 expect_equal(length(pred), 2)

 # Manual Calculation:
 # Beta = c(0.5, -0.5)
 # Row 1 (0,0): 0
 # Row 2 (1,1): 0.5 - 0.5 = 0
 expect_equal(as.numeric(pred), c(0, 0))

 # Change coefficients to verify
 fit$coefficients <- c(1, 2)
 pred_mod <- predict(fit, newdata = nd, type = "lp")
 # Row 2 (1,1): 1*1 + 1*2 = 3
 expect_equal(as.numeric(pred_mod), c(0, 3))
})

test_that("predict.coxphMixture: Step function reconstruction for 'expected'", {
 fit <- create_mock_fit()

 # Setup:
 # Train times: fit$y[,"time"] (Generated as sorted rexp)
 # Train Hazard: fit$Lambdahat0 (Generated as cumsum runif)

 t_min <- fit$y[1, "time"]
 t_mid <- fit$y[50, "time"]
 t_max <- fit$y[100, "time"]
 h_mid <- fit$Lambdahat0[50]
 h_max <- fit$Lambdahat0[100]

 # Create newdata with times matching training points and outside
 # Set x1, x2 to 0 so exp(lp) = 1, isolating the baseline hazard check
 nd <- data.frame(
  time = c(t_mid, t_max, t_max + 10),
  x1 = c(0, 0, 0),
  x2 = c(0, 0, 0)
 )

 pred <- predict(fit, newdata = nd, type = "expected")

 # At t_mid, prediction should equal training hazard at index 50
 expect_equal(pred[1], h_mid)

 # At t_max, prediction should equal training hazard at index 100
 expect_equal(pred[2], h_max)

 # Extrapolation: Rule 2 (constant) implies t > t_max gets value at t_max
 expect_equal(pred[3], h_max)
})

# -------------------------------------------------------------------------
# Tests: Error Handling
# -------------------------------------------------------------------------

test_that("predict.coxphMixture errors gracefully", {
 fit <- create_mock_fit()

 # Missing time variable in newdata for type="expected"
 nd_bad <- data.frame(x1 = 1, x2 = 1) # No 'time' column
 expect_error(predict(fit, newdata = nd_bad, type = "expected"),
              "Could not identify the time variable")

 # Dimension mismatch (Coefs vs Design Matrix)
 fit_bad <- fit
 fit_bad$coefficients <- c(1, 2, 3) # 3 coefs, but x has 2 cols
 expect_error(predict(fit_bad, type = "lp"), "Dimension mismatch")
})
