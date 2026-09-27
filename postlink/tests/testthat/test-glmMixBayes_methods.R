# tests/testthat/test-glmMixBayes_methods.R
# Methods tests for Bayesian GLM mixture (real Stan MCMC)
# Refactored into 4 test_that blocks + helper (glmMixture_methods style),
#
# NOTE:
# - Skipped on CRAN.
# - Opt-in by setting RUN_STAN_TESTS=true.

local_edition(3)

skip_if_no_stan <- function() {
 skip_on_cran()
 skip_if_not_installed("rstan")
 skip_if_not_installed("label.switching")
 if (!identical(Sys.getenv("RUN_STAN_TESTS"), "true")) {
  skip("Set RUN_STAN_TESTS=true to run real Stan MCMC tests.")
 }
 rstan::rstan_options(auto_write = TRUE)
 options(mc.cores = 1L)
}

# -------------------------------------------------------------------------
# Helper: keep data generation consistent with your original test-glmMixBayes
# (simple Gaussian, no special links)
# -------------------------------------------------------------------------
generate_bayesglm_mixture_data <- function(n = 100, seed = 42) {
 set.seed(seed)
 x1 <- rnorm(n)
 x2 <- rbinom(n, 1, 0.5)
 y  <- 0.5 + 1.2 * x1 - 0.8 * x2 + rnorm(n, sd = 0.7)

 dat <- data.frame(y = y, x1 = x1, x2 = x2)
 X <- stats::model.matrix(y ~ x1 + x2, dat)
 yv <- dat$y
 list(dat = dat, X = X, y = yv)
}

# Fit ONCE and reuse across tests (avoid running Stan 4 times)
fit_once <- local({
 cache <- NULL
 function() {
  if (!is.null(cache)) return(cache)

  skip_if_no_stan()
  d <- generate_bayesglm_mixture_data(n = 100, seed = 42)

  # Suppress Stan HMC warnings for unit testing purposes.
  # Divergent transitions fluctuate across OS C++ compilers and
  # are expected when running short chains on simulated data.
  suppressWarnings({
   cache <<- glmMixBayes(
    X = d$X,
    y = d$y,
    family = "gaussian",
    control = list(
     iterations = 2000,
     burnin.iterations = 1000,
     seed = 42,
     cores = 1
    )
   )
  })
  cache
 }
})

test_that("glmMixBayes print() and summary() work on a real fitted object", {
 fit <- fit_once()

 out <- utils::capture.output(print(fit))
 expect_true(length(out) > 0)

 s <- summary(fit)
 expect_s3_class(s, "summary.glmMixBayes")

 out2 <- utils::capture.output(print(s))
 expect_true(length(out2) > 0)
})

test_that("glmMixBayes vcov() returns a p x p matrix", {
 fit <- fit_once()
 d <- generate_bayesglm_mixture_data(n = 100, seed = 42)
 X <- d$X

 V <- stats::vcov(fit)
 expect_true(is.matrix(V))
 expect_equal(nrow(V), ncol(X))
 expect_equal(ncol(V), ncol(X))
})

test_that("glmMixBayes confint() returns a p x 2 matrix with ordered bounds", {
 fit <- fit_once()
 d <- generate_bayesglm_mixture_data(n = 100, seed = 42)
 X <- d$X

 CI <- stats::confint(fit)
 expect_true(is.matrix(CI))
 expect_equal(nrow(CI), ncol(X))
 expect_equal(ncol(CI), 2L)
 expect_true(all(CI[, 1] <= CI[, 2]))
})

test_that("glmMixBayes predict() works and mi_with() methods exist + run", {
 fit <- fit_once()
 d <- generate_bayesglm_mixture_data(n = 100, seed = 42)
 X <- d$X

 # predict(): should accept newx as a matrix
 newx <- X[1:5, , drop = FALSE]
 pr <- stats::predict(fit, newx = newx, type = "link")

 # Allow either numeric vector OR list with $fit (depending on your predict impl)
 expect_true(is.numeric(pr) || is.list(pr))
 if (is.list(pr) && "fit" %in% names(pr)) {
  expect_equal(length(pr$fit), nrow(newx))
 }

 # mi_with.glmMixBayes exists (requires explicit formula by design)
 wm_glm <- getS3method("mi_with", "glmMixBayes", optional = TRUE)
 expect_true(is.function(wm_glm))

 # Your mi_with.glmMixBayes requires formula (no longer inferred)
 # Keep formula consistent with model.matrix construction: y ~ x1 + x2
 res_glm <- mi_with(fit, data = d$dat, formula = y ~ x1 + x2)
 # res_glm <- mi_with(fit, data = d$dat)
 expect_true(!is.null(res_glm))

})
