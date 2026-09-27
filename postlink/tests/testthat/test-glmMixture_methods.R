local_edition(3)

# -------------------------------------------------------------------------
# Helper Function: Simulate Data for Tests
# -------------------------------------------------------------------------
simulate_mixture_data <- function(n = 200, family = "gaussian", m_rate = 0.1) {
 set.seed(123)

 # Predictors
 x <- matrix(rnorm(n), ncol = 1)
 X <- cbind(1, x) # Intercept
 colnames(X) <- c("(Intercept)", "X1")

 # Mismatch Covariates (Z)
 z <- matrix(rnorm(n, mean = 0.5), ncol = 1)
 Z <- cbind(1, z)
 colnames(Z) <- c("(Intercept)", "Z1")

 # Outcome Generation
 if (family == "gaussian") {
  beta <- c(1, 2)
  mu <- X %*% beta
  y <- rnorm(n, mean = mu, sd = 0.5)
 } else if (family == "binomial") {
  beta <- c(-0.5, 1.5)
  mu <- plogis(X %*% beta)
  y <- rbinom(n, size = 1, prob = mu)
 } else if (family == "poisson") {
  beta <- c(1, 0.5)
  mu <- exp(X %*% beta)
  y <- rpois(n, lambda = mu)
 } else if (family == "Gamma") {
  beta <- c(1, 0.5)
  mu <- exp(X %*% beta) # Log link
  shape <- 2
  y <- rgamma(n, shape = shape, rate = shape / mu)
 }

 # Introduce Mismatches (Shuffle a subset)
 n_mismatch <- floor(n * m_rate)
 idx <- sample(n, n_mismatch)
 y[idx] <- sample(y[idx]) # Shuffle y for mismatches

 # Safe matches indicator (assume we know non-shuffled ones are safe for testing)
 safe <- rep(FALSE, n)
 safe[-idx] <- TRUE

 list(x = X, y = y, z = Z, safe = safe)
}

# -------------------------------------------------------------------------
# Test S3 Methods
# -------------------------------------------------------------------------

test_that("print.glmMixture outputs correctly", {
 dat <- simulate_mixture_data(family = "gaussian")
 fit <- glmMixture(x = dat$x, y = dat$y, family = "gaussian", z = dat$z)

 # Capture output
 out <- capture.output(print(fit))

 expect_true(any(grepl("Coefficients \\(Outcome Model\\):", out)))
 expect_true(any(grepl("Coefficients \\(Mismatch Model\\):", out)))
})

test_that("summary.glmMixture produces valid summary object", {
 dat <- simulate_mixture_data(family = "gaussian")
 fit <- glmMixture(x = dat$x, y = dat$y, family = "gaussian", z = dat$z)

 summ <- summary(fit)

 expect_s3_class(summ, "summary.glmMixture")
 expect_named(summ, c("call", "family", "df.residual",
                      "coefficients",
                      "m.coefficients", "dispersion", "cov.unscaled", "match.prob"))

 # Check coefficient table structure
 expect_true("Pr(>|t|)" %in% colnames(summ$coefficients))
 expect_equal(nrow(summ$coefficients), 2)

 # Check print of summary
 out <- capture.output(print(summ))
 expect_true(any(grepl("Outcome Model Coefficients:", out)))
})

test_that("vcov.glmMixture returns correct dimensions", {
 dat <- simulate_mixture_data(family = "gaussian")
 fit <- glmMixture(x = dat$x, y = dat$y, family = "gaussian", z = dat$z)

 vc <- vcov(fit)

 # Gaussian has: 2 Beta + 1 Dispersion + 2 Gamma = 5 parameters
 expect_equal(nrow(vc), 5)
 expect_equal(ncol(vc), 5)

 # Check naming
 expect_true(any(grepl("dispersion", rownames(vc))))
 expect_true(any(grepl("m.coef", rownames(vc))))

 # Symmetry
 expect_equal(vc, t(vc))
})

test_that("confint.glmMixture computes intervals", {
 dat <- simulate_mixture_data(family = "gaussian")
 fit <- glmMixture(x = dat$x, y = dat$y, family = "gaussian", z = dat$z)

 ci <- confint(fit)

 expect_equal(ncol(ci), 2)
 # 2 Beta + 1 Dispersion + 2 Gamma = 5 rows
 expect_equal(nrow(ci), 5)

 # Lower bound should be less than upper bound
 expect_true(all(ci[,1] < ci[,2]))

 # Specific parameter selection
 ci_sub <- confint(fit, parm = 1:2)
 expect_equal(nrow(ci_sub), 2)
})

test_that("predict.glmMixture handles newdata and types correctly", {
 dat <- simulate_mixture_data(n = 200, family = "gaussian")
 fit <- glmMixture(x = dat$x, y = dat$y, family = "gaussian", z = dat$z)

 # Mock terms for test purposes (since glmMixture doesn't store them directly)
 df <- data.frame(y = dat$y, x1 = dat$x[,2])
 mf <- model.frame(y ~ x1, data = df)
 fit$terms <- terms(mf)

 # Predict on original data
 p1 <- predict(fit)
 expect_equal(length(p1), 200)
 expect_equal(p1, fit$linear.predictors) # Default type="link"

 # Predict response
 p2 <- predict(fit, type = "response")
 expect_equal(p2, fit$fitted.values)

 # Predict with newdata
 newdata <- data.frame(x1 = rnorm(10))
 p_new <- predict(fit, newdata = newdata)
 expect_equal(length(p_new), 10)

 # Standard Errors
 p_se <- predict(fit, newdata = newdata, se.fit = TRUE)
 expect_type(p_se, "list")
 expect_named(p_se, c("fit", "se.fit", "residual.scale"))
 expect_true(all(p_se$se.fit > 0))
})
