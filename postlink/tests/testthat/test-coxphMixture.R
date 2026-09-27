local_edition(3)

# ==============================================================================
# DATA GENERATOR
# ==============================================================================
generate_data <- function(n = 200, mismatch_rate = 0.2, seed = 123) {
 set.seed(seed)

 # Covariates
 x1 <- rnorm(n)
 x2 <- rbinom(n, 1, 0.5)
 X <- cbind(x1, x2)

 # True Survival Model
 # We use larger coefficients so the model can distinguish signal from noise.
 beta_true <- c(2.0, -1.0)
 lin_pred <- X %*% beta_true

 # Weibull simulation (Cox-compatible)
 # Shape=2. Scale constructed so that Hazard ~ exp(beta * X)
 # Scale = exp(-linear_predictor / shape)
 time <- rweibull(n, shape = 2, scale = exp(-lin_pred / 2))

 # Censoring (~20-30%)
 cens_time <- rweibull(n, shape = 2, scale = quantile(time, 0.8))
 obs_time <- pmin(time, cens_time)
 status <- as.numeric(time <= cens_time) # 1 = event
 cens_input <- 1 - status # 1 = censored (for coxphMixture)

 # Mismatch Induction
 Z <- matrix(1, nrow = n, ncol = 1)
 n_mismatch <- floor(n * mismatch_rate)
 is_match <- rep(TRUE, n)

 if (n_mismatch > 0) {
  mismatch_idx <- sample(n, n_mismatch)

  # Shuffle Y/Cens relative to X
  shuffled_pos <- sample(mismatch_idx)
  while(length(mismatch_idx) > 1 && any(shuffled_pos == mismatch_idx)) {
   shuffled_pos <- sample(mismatch_idx)
  }

  obs_time_perm <- obs_time
  cens_input_perm <- cens_input

  obs_time_perm[mismatch_idx] <- obs_time[shuffled_pos]
  cens_input_perm[mismatch_idx] <- cens_input[shuffled_pos]

  obs_time <- obs_time_perm
  cens_input <- cens_input_perm
  is_match[mismatch_idx] <- FALSE
 }

 list(
  X = X, y = obs_time, cens = cens_input,
  Z = Z, n = n, true_beta = beta_true,
  is_match = is_match
 )
}

# ==============================================================================
# TEST SUITE
# ==============================================================================

test_that("Baseline: Matches standard CoxPH on perfect data", {
 sim <- generate_data(n = 500, mismatch_rate = 0, seed = 101)

 fit_gold <- coxph(Surv(sim$y, 1 - sim$cens) ~ sim$X)

 fit_mix <- coxphMixture(
  x = sim$X, y = sim$y, cens = sim$cens, z = sim$Z,
  control = list(max.iter = 100)
 )

 # Coefficients should be nearly identical
 expect_equal(
  as.numeric(fit_mix$coefficients),
  as.numeric(coef(fit_gold)),
  tolerance = 0.1
 )

 # Standard errors should be close
 se_gold <- sqrt(diag(vcov(fit_gold)))
 se_mix <- sqrt(diag(fit_mix$var))[1:2]
 expect_equal(as.numeric(se_mix), as.numeric(se_gold), tolerance = 0.5)
})

test_that("Performance: Reduces bias compared to Naive CoxPH on mismatched data", {
 # High N to ensure statistical separation
 sim <- generate_data(n = 2000, mismatch_rate = 0.25, seed = 999)

 # Naive Fit (Ignoring mismatch)
 fit_naive <- coxph(Surv(sim$y, 1 - sim$cens) ~ sim$X)

 # Mixture Fit
 # We use init.gamma = 1.5 (~82% match prob) to help initialization.
 # We use m.rate = 0.26 (slightly loose bound) to avoid boundary issues.
 fit_mix <- coxphMixture(
  x = sim$X, y = sim$y, cens = sim$cens, z = sim$Z,
  m.rate = 0.26,
  control = list(
   max.iter = 200,
   init.gamma = rep(1.5, ncol(sim$Z))
  )
 )

 bias_naive <- mean(abs(coef(fit_naive) - sim$true_beta))
 bias_mix <- mean(abs(fit_mix$coefficients - sim$true_beta))

 # Expect significant reduction (at least 20% improvement)
 expect_lt(bias_mix, bias_naive * 0.8)
})

test_that("Constraints: m.rate bound effectively limits mismatch probability", {
 sim <- generate_data(n = 200, mismatch_rate = 0.50, seed = 303)

 # Force a strict constraint: max 10% mismatch allowed
 # This implies match prob >= 0.90
 constraint <- 0.10

 fit_constrained <- coxphMixture(
  x = sim$X, y = sim$y, cens = sim$cens, z = sim$Z,
  m.rate = constraint,
  control = list(cmax.iter = 500)
 )

 # Calculate estimated mismatch rate: mean(1 - match_prob)
 est_mismatch_rate <- mean(1 - fit_constrained$match.prob)

 # Allow small numerical tolerance
 expect_lte(est_mismatch_rate, constraint + 0.01)
})

test_that("Safe Matches: is_flagged (safe.matches) forces probability to 1", {
 sim <- generate_data(n = 100, mismatch_rate = 0.2, seed = 404)

 # Flag first 10 observations as safe matches
 safe_flags <- rep(FALSE, sim$n)
 safe_flags[1:10] <- TRUE

 fit <- coxphMixture(
  x = sim$X, y = sim$y, cens = sim$cens, z = sim$Z,
  safe.matches = safe_flags,
  control = list(max.iter = 10)
 )

 # Check posterior probabilities for flagged records
 expect_true(all(fit$match.prob[1:10] == 1))

 # Check that non-flagged records are estimated (not all 1)
 expect_lt(min(fit$match.prob[!safe_flags]), 1)
})

test_that("Control Arguments: Max iterations stops algorithm early", {
 sim <- generate_data(n = 100, mismatch_rate = 0, seed = 505)

 # Set max.iter to 1
 fit_short <- coxphMixture(
  x = sim$X, y = sim$y, cens = sim$cens, z = sim$Z,
  control = list(max.iter = 1)
 )

 # Objective vector length implies iterations run
 expect_equal(length(fit_short$objective), 1)
 expect_false(fit_short$converged)
})

test_that("Output Structure: Returns correct list components", {
 sim <- generate_data(n = 50)
 fit <- coxphMixture(x = sim$X, y = sim$y, cens = sim$cens, z = sim$Z)

 expected_names <- c("coefficients", "m.coefficients", "var",
                     "linear.predictors", "means", "n", "nevent",
                     "match.prob", "objective", "converged",
                     "Lambdahat0", "gLambdahat0")

 expect_true(all(expected_names %in% names(fit)))
 expect_equal(dim(fit$var), c(ncol(sim$X) + ncol(sim$Z), ncol(sim$X) + ncol(sim$Z)))
})
