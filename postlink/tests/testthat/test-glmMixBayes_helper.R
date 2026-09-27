# tests/testthat/test-glmMixBayes_helpers.R
# Unit tests for internal Bayesian mixture helper utilities
# These tests are fast and do not run Stan.

local_edition(3)

test_that("fill_defaults fills glm defaults and preserves user overrides", {
 res <- postlink:::fill_defaults(
  priors = list(beta1 = "normal(0,3)"),
  p_family = "gaussian",
  model_type = "glm"
 )

 expect_type(res, "list")
 expect_equal(res$beta1, "normal(0,3)")
 expect_equal(res$beta2, "normal(0,5)")
 expect_equal(res$sigma1, "cauchy(0,2.5)")
 expect_equal(res$sigma2, "cauchy(0,2.5)")
 expect_equal(res$theta, "beta(1,1)")
})

test_that("fill_defaults returns survival defaults and errors on invalid inputs", {
 res <- postlink:::fill_defaults(
  priors = NULL,
  p_family = "weibull",
  model_type = "survival"
 )

 expect_equal(res$beta1, "normal(0,2)")
 expect_equal(res$beta2, "normal(0,2)")
 expect_equal(res$shape1, "gamma(2,1)")
 expect_equal(res$shape2, "gamma(2,1)")
 expect_equal(res$scale1, "gamma(2,1)")
 expect_equal(res$scale2, "gamma(2,1)")
 expect_equal(res$theta, "beta(1,1)")

 expect_error(
  postlink:::fill_defaults(priors = 1, p_family = "gaussian", model_type = "glm"),
  "must be a named list"
 )

 expect_error(
  postlink:::fill_defaults(priors = list(), p_family = "badfamily", model_type = "glm"),
  "must be one of"
 )

 expect_error(
  postlink:::fill_defaults(priors = list(), p_family = "gaussian", model_type = "badtype"),
  "Unknown model_type"
 )
})

test_that("parse_prior_string parses valid priors and rejects malformed strings", {
 p1 <- postlink:::parse_prior_string("normal(0, 5)")
 expect_equal(p1$dist, "normal")
 expect_equal(p1$args, c(0, 5))

 p2 <- postlink:::parse_prior_string("beta(2,2)")
 expect_equal(p2$dist, "beta")
 expect_equal(p2$args, c(2, 2))

 expect_error(
  postlink:::parse_prior_string("normal 0,5"),
  "must be in format"
 )

 expect_error(
  postlink:::parse_prior_string("normal(a,5)"),
  "Could not parse numeric arguments"
 )
})

test_that("prepare_stan_priors returns expected flat prior data for glm gaussian", {
 pri <- list(
  beta1 = "normal(1,2)",
  beta2 = "normal(3,4)",
  sigma1 = "cauchy(0,1.5)",
  sigma2 = "cauchy(0,2.5)",
  theta = "beta(2,3)"
 )

 out <- postlink:::prepare_stan_priors(pri, family = "gaussian", model_type = "glm")

 expect_type(out, "list")
 expect_equal(out$prior_beta1_mu, 1)
 expect_equal(out$prior_beta1_sd, 2)
 expect_equal(out$prior_beta2_mu, 3)
 expect_equal(out$prior_beta2_sd, 4)
 expect_equal(out$prior_theta_alpha, 2)
 expect_equal(out$prior_theta_beta, 3)
 expect_equal(out$prior_sigma1_loc, 0)
 expect_equal(out$prior_sigma1_scale, 1.5)
 expect_equal(out$prior_sigma2_loc, 0)
 expect_equal(out$prior_sigma2_scale, 2.5)
})

test_that("prepare_stan_priors handles survival gamma exponential priors correctly", {
 pri <- list(
  beta1 = "normal(0,5)",
  beta2 = "normal(0,5)",
  theta = "beta(1,1)",
  phi1 = "exponential(2)",
  phi2 = "exponential(3)"
 )

 out <- postlink:::prepare_stan_priors(pri, family = "gamma", model_type = "survival")

 # exponential(rate) is converted to gamma(shape = 1, rate = rate)
 expect_equal(out$prior_phi1_alpha, 1)
 expect_equal(out$prior_phi1_beta, 2)
 expect_equal(out$prior_phi2_alpha, 1)
 expect_equal(out$prior_phi2_beta, 3)
})

test_that("prepare_stan_priors errors when prior argument length is wrong", {
 pri <- list(
  beta1 = "normal(0,5)",
  beta2 = "normal(0,5)",
  theta = "beta(1,1)",
  sigma1 = "cauchy(0)",
  sigma2 = "cauchy(0,2.5)"
 )

 expect_error(
  postlink:::prepare_stan_priors(pri, family = "gaussian", model_type = "glm"),
  "expected 2 arguments"
 )
})

test_that("stan_func and is_valid_stan_func validate injected Stan functions", {
 valid_fun <- "real myfun(real x) { return x; }"

 expect_true(postlink:::is_valid_stan_func(valid_fun))

 sf <- postlink:::stan_func(valid_fun)
 expect_s3_class(sf, "stan_function_string")
 expect_true(postlink:::is_func(sf))

 expect_false(postlink:::is_valid_stan_func("x <- 1"))
 expect_error(postlink:::stan_func("x <- 1"), "invalid stan function")
 expect_error(postlink:::stan_func(123), "must be a single string")
})

test_that("validate_args accepts valid priors and rejects invalid ones", {
 good_priors <- list(
  beta1 = "normal(0,5)",
  beta2 = "normal(0,5)",
  theta = "beta(1,1)"
 )

 expect_invisible(postlink:::validate_args(good_priors, p_family = "gaussian"))
 expect_invisible(postlink:::validate_args(NULL, p_family = "poisson"))

 expect_error(
  postlink:::validate_args(good_priors, p_family = "badfamily"),
  "must be one of"
 )

 expect_error(
  postlink:::validate_args(list("normal(0,5)"), p_family = "gaussian"),
  "must be a named list"
 )

 expect_error(
  postlink:::validate_args(list(beta1 = "normal(0,)"), p_family = "gaussian"),
  "Missing values in list of hyperparameters"
 )

 expect_error(
  postlink:::validate_args(
   list(beta1 = "normal(mu,5)", beta2 = "normal(0,5)", theta = "beta(1,1)"),
   p_family = "gaussian"
  ),
  "variable used in a prior string is not defined"
 )

 expect_warning(
  postlink:::validate_args(
   list(beta1 = "student_t(3,0,2)", beta2 = "normal(0,5)", theta = "beta(1,1)"),
   p_family = "gaussian"
  ),
  "untested distribution"
 )
})

test_that("process_variable handles scalar, vector, matrix, prior string, and stan function", {
 scalar_out <- postlink:::process_variable(3.5, "alpha")
 expect_equal(scalar_out$declaration, "")
 expect_match(scalar_out$definition, "real alpha = 3.5;", fixed = TRUE)
 expect_equal(scalar_out$stan_func, "")

 vector_out <- postlink:::process_variable(c(1, 2, 3), "mu")
 expect_equal(vector_out$declaration, "")
 expect_match(vector_out$definition, "vector[3] mu = [1, 2, 3]';", fixed = TRUE)

 mat <- matrix(c(1, 0.2, 0.2, 1), nrow = 2)
 matrix_out <- postlink:::process_variable(mat, "Sigma")
 expect_match(matrix_out$declaration, "cov_matrix[2] Sigma;", fixed = TRUE)
 expect_match(matrix_out$definition, "Sigma[1, 1] = 1;", fixed = TRUE)
 expect_match(matrix_out$definition, "Sigma[2, 2] = 1;", fixed = TRUE)

 prior_string_out <- postlink:::process_variable("normal(0,5)", "beta1")
 expect_equal(prior_string_out$declaration, "")
 expect_equal(prior_string_out$definition, "")
 expect_equal(prior_string_out$stan_func, "")

 sf <- postlink:::stan_func("real myfun(real x) { return x; }")
 expect_warning(
  fun_out <- postlink:::process_variable(sf, "f"),
  "stan function definition"
 )
 expect_equal(fun_out$declaration, "")
 expect_equal(fun_out$definition, "")
 expect_match(fun_out$stan_func, "return x;", fixed = TRUE)

 bad_mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
 expect_error(
  postlink:::process_variable(bad_mat, "Bad"),
  "Non-square or unsymmetric matrix"
 )

 expect_error(
  postlink:::process_variable(list(1, 2), "bad"),
  "invalid type or form"
 )
})

test_that("get_stan_definitions collects declarations, definitions, and function defs", {
 pri <- list(
  beta1 = "normal(mu, 5)",
  mu = c(1, 2),
  Sigma = matrix(c(1, 0.1, 0.1, 1), nrow = 2),
  f = postlink:::stan_func("real myfun(real x) { return x; }")
 )

 defs <- suppressWarnings(postlink:::get_stan_definitions(pri))

 expect_type(defs, "list")
 expect_true(all(c("variable_defs", "function_defs") %in% names(defs)))
 expect_match(defs$variable_defs, "vector[2] mu = [1, 2]';", fixed = TRUE)
 expect_match(defs$variable_defs, "cov_matrix[2] Sigma;", fixed = TRUE)
 expect_match(defs$function_defs, "real myfun(real x) { return x; }", fixed = TRUE)
})

test_that("generate_stan returns family-specific Stan code and rejects invalid components", {
 gaussian_code <- postlink:::generate_stan(
  components = c("gaussian", "gaussian"),
  priors = postlink:::fill_defaults(NULL, "gaussian", "glm")
 )
 expect_true(is.character(gaussian_code))
 expect_match(gaussian_code, "normal_lpdf", fixed = TRUE)
 expect_match(gaussian_code, "sigma1 ~", fixed = TRUE)
 expect_match(gaussian_code, "cauchy(0,2.5)", fixed = TRUE)

 poisson_code <- postlink:::generate_stan(
  components = c("poisson", "poisson"),
  priors = postlink:::fill_defaults(NULL, "poisson", "glm")
 )
 expect_match(poisson_code, "poisson_log_lpmf", fixed = TRUE)

 gamma_code <- postlink:::generate_stan(
  components = c("gamma", "gamma"),
  priors = postlink:::fill_defaults(NULL, "gamma", "glm")
 )
 expect_match(gamma_code, "gamma_lpdf", fixed = TRUE)

 binomial_code <- postlink:::generate_stan(
  components = c("binomial", "binomial"),
  priors = postlink:::fill_defaults(NULL, "binomial", "glm")
 )
 expect_match(binomial_code, "bernoulli_logit_lpmf", fixed = TRUE)

 expect_error(
  postlink:::generate_stan(
   components = c("weibull", "weibull"),
   priors = list()
  ),
  "Invalid mixture inputs"
 )
})
