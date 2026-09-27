local_edition(3)

# ------------------------------------------------------------------------------
# Helper Function: Data Generator
# ------------------------------------------------------------------------------
generate_mixture_data <- function(n = 1000, family_type = "gaussian",
                                  link = NULL, alpha = 0.05, seed = 123) {
 set.seed(seed)

 # 1. Covariates
 X1 <- rnorm(n)
 X2 <- rbinom(n, 1, 0.5)
 X <- cbind(1, X1, X2) # Design matrix

 # 2. Parameters & Linear Predictor
 # Adjust betas to ensure valid ranges for specific links (e.g. Identity/Inverse)
 if (family_type == "poisson" && identical(link, "identity")) {
  beta <- c(10, 2, 2) # High intercept to keep lambda > 0
 } else if (family_type == "gamma" && identical(link, "inverse")) {
  beta <- c(0.5, 0.1, 0.1) # Keep linear predictor positive
 } else if (family_type == "gaussian" && identical(link, "inverse")) {
  beta <- c(2, 0.5, 0.5) # Avoid zero crossing
 } else {
  beta <- c(0.5, 1.0, -0.5)
 }

 eta <- X %*% beta

 # 3. Inverse Link to get Mu
 if (family_type == "gaussian") {
  if (identical(link, "inverse")) mu <- 1/eta else mu <- eta
 } else if (family_type == "binomial") {
  if (identical(link, "probit")) mu <- pnorm(eta) else mu <- plogis(eta)
 } else if (family_type == "poisson") {
  if (identical(link, "identity")) mu <- eta else mu <- exp(eta)
 } else if (family_type == "gamma") {
  if (identical(link, "log")) mu <- exp(eta)
  else if (identical(link, "inverse")) mu <- 1/eta
  else mu <- 1/eta # Default canonical
 }

 # 4. Generate Y
 if (family_type == "gaussian") {
  y <- rnorm(n, mean = mu, sd = 0.5)
 } else if (family_type == "binomial") {
  y <- rbinom(n, size = 1, prob = mu)
 } else if (family_type == "poisson") {
  y <- rpois(n, lambda = mu)
 } else if (family_type == "gamma") {
  shape <- 5
  y <- rgamma(n, shape = shape, rate = shape/mu)
 }

 # 5. Introduce Mismatch
 m <- rbinom(n, size = 1, prob = alpha)
 y_shuffled <- y
 if (sum(m) > 1) {
  idx <- which(m == 1)
  y_shuffled[idx] <- sample(y_shuffled[idx])
 }

 # Match indicator covariates (Intercept only)
 Z <- matrix(1, nrow = n, ncol = 1)

 list(X = X, y = y_shuffled, Z = Z, true_beta = beta, family = family_type)
}

# ------------------------------------------------------------------------------
# Test Group 1: Standard Links
# ------------------------------------------------------------------------------

test_that("Gaussian Regression (Identity Link) runs and converges", {
 dat <- generate_mixture_data(family_type = "gaussian", alpha = 0.05)

 fit <- glmMixture(x = dat$X, y = dat$y, z = dat$Z, family = "gaussian",
                   control = list(max.iter = 200, tol = 1e-3))

 expect_s3_class(fit, "glmMixture")
 expect_true(fit$converged)
 expect_equal(length(fit$coefficients), 3)

 # Coefficients should be reasonably close to truth (allowing for noise + mismatch variance)
 expect_equal(as.vector(fit$coefficients), dat$true_beta, tolerance = 0.2)
 expect_true(!is.null(fit$var)) # Variance matrix exists
 expect_true(fit$dispersion > 0)
})

test_that("Binomial Regression (Logit Link) runs and converges", {
 dat <- generate_mixture_data(family_type = "binomial", alpha = 0.05)

 fit <- suppressWarnings(
  glmMixture(x = dat$X, y = dat$y, z = dat$Z, family = "binomial",
             control = list(max.iter = 1000))
 )

 expect_true(fit$converged)
 expect_equal(length(fit$coefficients), 3)
 expect_equal(as.vector(fit$coefficients), dat$true_beta, tolerance = 0.5)
 # Binomial needs higher tolerance due to information loss from binary outcome + mismatch
})

test_that("Poisson Regression (Log Link) runs and converges", {
 dat <- generate_mixture_data(family_type = "poisson", alpha = 0.05)

 fit <- glmMixture(x = dat$X, y = dat$y, z = dat$Z, family = "poisson",
                   control = list(max.iter = 200))

 expect_true(fit$converged)
 expect_equal(as.vector(fit$coefficients), dat$true_beta, tolerance = 0.2)
})

test_that("Gamma Regression (Log Link) runs and converges", {
 dat <- generate_mixture_data(family_type = "gamma", link = "log", alpha = 0.05)

 fit <- glmMixture(x = dat$X, y = dat$y, z = dat$Z, family = Gamma(link = "log"),
                   control = list(max.iter = 200))

 expect_true(fit$converged)
 expect_equal(as.vector(fit$coefficients), dat$true_beta, tolerance = 0.2)
 expect_true(fit$dispersion > 0)
})

# ------------------------------------------------------------------------------
# Test Group 2: Non-Standard Links
# ------------------------------------------------------------------------------

test_that("Gaussian Regression with Inverse Link doesn't work", {
 dat <- generate_mixture_data(family_type = "gaussian", link = "inverse", alpha = 0.05)

 expect_error(
  glmMixture(x = dat$X, y = dat$y, z = dat$Z,
             family = gaussian(link = "inverse"),
             control = list(max.iter = 1000, tol = 0.001, init.beta = dat$true_beta)),
  "The 'gaussian' family is currently restricted to the 'identity' link."
 )

})

test_that("Poisson Regression with Identity Link works", {
 dat <- generate_mixture_data(family_type = "poisson", link = "identity", alpha = 0.05)

 fit <- glmMixture(x = dat$X, y = dat$y, z = dat$Z,
                   family = poisson(link = "identity"),
                   control = list(max.iter = 200, init.beta = dat$true_beta))
 # Providing init.beta helps identity link stability

 expect_true(fit$converged)
 expect_equal(as.vector(fit$coefficients), dat$true_beta, tolerance = 0.5)
})

test_that("Binomial Regression with Probit Link works", {
 dat <- generate_mixture_data(family_type = "binomial", link = "probit", alpha = 0.05)

 fit <- suppressWarnings(
  glmMixture(x = dat$X, y = dat$y, z = dat$Z,
             family = binomial(link = "probit"),
             control = list(max.iter = 1000, tol = 1e-3, init.beta = dat$true_beta))
 )

 expect_true(fit$converged)
 expect_equal(as.vector(fit$coefficients), dat$true_beta, tolerance = 0.5)
})

test_that("Gamma Regression with Inverse Link works", {
 dat <- generate_mixture_data(family_type = "gamma", link = "inverse", alpha = 0.05)

 fit <- glmMixture(x = dat$X, y = dat$y, z = dat$Z,
                   family = Gamma(link = "inverse"),
                   control = list(max.iter = 1000, tol = 1e-3, init.beta = dat$true_beta))

 expect_true(fit$converged)
 expect_equal(as.vector(fit$coefficients), dat$true_beta, tolerance = 0.2)
})

# ------------------------------------------------------------------------------
# Test Group 3: Constraints and Arguments
# ------------------------------------------------------------------------------

test_that("m.rate argument imposes constraint", {
 dat <- generate_mixture_data(family_type = "gaussian", alpha = 0.10)

 # Fit with known m.rate constraint
 fit_constrained <- glmMixture(x = dat$X, y = dat$y, z = dat$Z,
                               family = "gaussian",
                               m.rate = 0.10)

 # The mismatch model coefficients should reflect the constraint
 # Since Z is intercept only, gamma should be roughly -log((1-0.1)/0.1) = -2.19
 # Note: The code optimizes for -gamma in the constrained function logic
 expected_gamma <- -log((1 - 0.10) / 0.10)

 # We check if the estimated match probability aligns roughly with 1 - m.rate
 # match.prob is P(m=0|data). The prior h(Z) is what is constrained.

 # If constraint is active, the prior probability of match should be <= 0.90
 prior_prob_match <- plogis(as.numeric(fit_constrained$m.coefficients))
 # Allowing small numerical wiggle room
 expect_lte(prior_prob_match, 0.90 + 0.01)
})

test_that("safe.matches argument is respected", {
 dat <- generate_mixture_data(family_type = "gaussian", alpha = 0.2)

 # Mark the first 10 observations as safe matches
 safe_vec <- rep(FALSE, length(dat$y))
 safe_vec[1:10] <- TRUE

 fit <- glmMixture(x = dat$X, y = dat$y, z = dat$Z, family = "gaussian",
                   safe.matches = safe_vec)

 # Posterior match probability for safe matches must be exactly 1
 expect_equal(fit$match.prob[1:10], rep(1, 10))

 # Others should be estimated (likely < 1)
 expect_true(any(fit$match.prob[11:length(safe_vec)] < 1))
})

test_that("User-provided fy (Marginal Density) is accepted", {
 dat <- generate_mixture_data(family_type = "gaussian")

 # Pre-calculate a density estimate
 custom_fy_vals <- dnorm(dat$y, mean(dat$y), sd(dat$y))

 # Create a function that returns these values (simulating the approx function behavior or just pass vector)
 # The code in mixture_glm.R expects `con$fy` to be a vector of density values matching y

 fit <- glmMixture(x = dat$X, y = dat$y, z = dat$Z, family = "gaussian",
                   control = list(fy = custom_fy_vals, max.iter=50))

 expect_true(fit$converged)
})

# ------------------------------------------------------------------------------
# Test Group 4: Structure and Error Handling
# ------------------------------------------------------------------------------

test_that("Output object contains all required fields", {
 dat <- generate_mixture_data()
 fit <- glmMixture(dat$X, dat$y, family = "gaussian", z = dat$Z)

 expected_names <- c("coefficients", "m.coefficients", "residuals",
                     "fitted.values", "rank", "family", "linear.predictors",
                     "df.residual", "df.null", "converged", "match.prob",
                     "var", "objective", "dispersion")

 expect_true(all(expected_names %in% names(fit)))
 expect_s3_class(fit, c("glmMixture", "plglm", "glm", "lm"))
})

test_that("Handles invalid family gracefully", {
 dat <- generate_mixture_data()
 expect_error(glmMixture(dat$X, dat$y, z = dat$Z, family = "invalid_family"),
              "Invalid family")
})

test_that("Handles generic family function passed directly", {
 dat <- generate_mixture_data()
 fit <- glmMixture(dat$X, dat$y, z = dat$Z, family = gaussian) # Pass function, not string
 expect_true(fit$converged)
})

test_that("Handles list-based family object", {
 dat <- generate_mixture_data()
 fit <- glmMixture(dat$X, dat$y, z = dat$Z, family = gaussian()) # Pass object
 expect_true(fit$converged)
})
