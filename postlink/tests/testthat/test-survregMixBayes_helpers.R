# tests/testthat/test-survregMixBayes_helpers.R
# Unit tests for internal Bayesian survival mixture helper utilities
# These tests are fast and do not run Stan.

local_edition(3)

test_that(".validate_survreg_dist lowercases valid input and rejects invalid shape", {
 expect_equal(postlink:::.validate_survreg_dist("weibull"), "weibull")
 expect_equal(postlink:::.validate_survreg_dist("Gamma"), "gamma")
 expect_equal(postlink:::.validate_survreg_dist("WEIBULL"), "weibull")

 expect_error(
  postlink:::.validate_survreg_dist(),
  "`dist` must be a single character string."
 )

 expect_error(
  postlink:::.validate_survreg_dist(1),
  "`dist` must be a single character string."
 )

 expect_error(
  postlink:::.validate_survreg_dist(c("gamma", "weibull")),
  "`dist` must be a single character string."
 )
})

test_that(".normalize_surv_y works for list input", {
 y <- list(
  time = c(1.2, 2.5, 3.1),
  event = c(1, 0, 3)
 )

 out <- postlink:::.normalize_surv_y(y)

 expect_type(out, "list")
 expect_equal(names(out), c("time", "event"))
 expect_equal(out$time, c(1.2, 2.5, 3.1))
 expect_equal(out$event, c(1L, 0L, 1L))
})

test_that(".normalize_surv_y works for matrix input", {
 y <- cbind(
  time = c(0.5, 1.5, 2.5),
  event = c(0, 1, 2)
 )

 out <- postlink:::.normalize_surv_y(y)

 expect_equal(out$time, c(0.5, 1.5, 2.5))
 expect_equal(out$event, c(0L, 1L, 1L))
})

test_that(".normalize_surv_y rejects invalid inputs", {
 expect_error(
  postlink:::.normalize_surv_y(c(1, 2, 3)),
  "`y` must be a 2-column matrix"
 )

 expect_error(
  postlink:::.normalize_surv_y(matrix(c(1, 0, 2), ncol = 1)),
  "`y` must be a 2-column matrix"
 )

 expect_error(
  postlink:::.normalize_surv_y(list(time = c(1, 2, 3))),
  "`y` must be a 2-column matrix"
 )

 expect_error(
  postlink:::.normalize_surv_y(list(time = c(1, 0, 2), event = c(1, 0, 1))),
  "Survival times must be positive and finite."
 )

 expect_error(
  postlink:::.normalize_surv_y(list(time = c(1, -2, 3), event = c(1, 0, 1))),
  "Survival times must be positive and finite."
 )

 expect_error(
  postlink:::.normalize_surv_y(list(time = c(1, Inf, 3), event = c(1, 0, 1))),
  "Survival times must be positive and finite."
 )
})

test_that("generate_stan_surv returns gamma survival Stan code", {
 pri <- postlink:::fill_defaults(
  priors = NULL,
  p_family = "gamma",
  model_type = "survival"
 )

 code <- postlink:::generate_stan_surv(
  components = c("gamma", "gamma"),
  priors = pri
 )

 code_compact <- gsub("\\s+", "", code)

 expect_true(is.character(code))
 expect_length(code, 1L)
 expect_match(code_compact, "gamma_lpdf", fixed = TRUE)
 expect_match(code_compact, "gamma_lccdf", fixed = TRUE)
 expect_match(code_compact, "phi1~exponential(1);", fixed = TRUE)
 expect_match(code_compact, "phi2~exponential(1);", fixed = TRUE)
 expect_match(code_compact, "target+=log_mix(theta,lp1,lp2);", fixed = TRUE)
})

test_that("generate_stan_surv returns weibull survival Stan code", {
 pri <- postlink:::fill_defaults(
  priors = NULL,
  p_family = "weibull",
  model_type = "survival"
 )

 code <- postlink:::generate_stan_surv(
  components = c("weibull", "weibull"),
  priors = pri
 )

 code_compact <- gsub("\\s+", "", code)

 expect_true(is.character(code))
 expect_length(code, 1L)
 expect_match(code_compact, "weibull_lpdf", fixed = TRUE)
 expect_match(code_compact, "weibull_lccdf", fixed = TRUE)
 expect_match(code_compact, "shape1~gamma(2,1);", fixed = TRUE)
 expect_match(code_compact, "shape2~gamma(2,1);", fixed = TRUE)
 expect_match(code_compact, "scale1~gamma(2,1);", fixed = TRUE)
 expect_match(code_compact, "scale2~gamma(2,1);", fixed = TRUE)
})

test_that("generate_stan_surv includes injected transformed-data and function definitions", {
 pri <- postlink:::fill_defaults(
  priors = list(
   mu = c(1, 2),
   Sigma = matrix(c(1, 0.1, 0.1, 1), nrow = 2),
   myfun = postlink:::stan_func("real myfun(real x) { return x; }")
  ),
  p_family = "gamma",
  model_type = "survival"
 )

 code <- suppressWarnings(
  postlink:::generate_stan_surv(
   components = c("gamma", "gamma"),
   priors = pri
  )
 )

 code_compact <- gsub("\\s+", "", code)

 expect_match(code_compact, "realmyfun(realx){returnx;}", fixed = TRUE)
 expect_match(code_compact, "vector[2]mu=[1,2]';", fixed = TRUE)
 expect_match(code_compact, "cov_matrix[2]Sigma;", fixed = TRUE)
})

test_that("generate_stan_surv rejects invalid component specifications", {
 pri <- postlink:::fill_defaults(
  priors = NULL,
  p_family = "gamma",
  model_type = "survival"
 )

 expect_error(
  postlink:::generate_stan_surv(
   components = "gamma",
   priors = pri
  ),
  "`components` must be a length-2 character vector."
 )

 expect_error(
  postlink:::generate_stan_surv(
   components = c("gamma", "weibull"),
   priors = pri
  ),
  "Only Gamma-Gamma and Weibull-Weibull components are supported for survival."
 )

 expect_error(
  postlink:::generate_stan_surv(
   components = c("poisson", "poisson"),
   priors = pri
  ),
  "Only Gamma-Gamma and Weibull-Weibull components are supported for survival."
 )
})
