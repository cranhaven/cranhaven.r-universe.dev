# tests/testthat/test-priors.R

test_that("extract_priors.lm returns et_prior_spec with correct structure", {
  set.seed(1)
  df  <- data.frame(y = rnorm(30), x1 = rnorm(30), x2 = rnorm(30))
  fit <- lm(y ~ x1 + x2, data = df)
  ps  <- extract_priors(fit, multiplier = 2, min_sd = 0.1)

  expect_s3_class(ps, "et_prior_spec")
  expect_equal(ps$method, "lm")
  expect_equal(sort(ps$pred_names), sort(c("x1", "x2")))
  expect_named(ps$coefs, ps$pred_names)
  expect_true(all(names(ps$coefs) %in% c("x1", "x2")))
  expect_equal(ps$multiplier, 2)
  expect_equal(ps$min_sd, 0.1)
  expect_s3_class(ps$prior, "brmsprior")
})

test_that("extract_priors.lm min_sd floor is enforced", {
  # Near-zero coefficient — prior SD should be min_sd, not multiplier * |coef|
  set.seed(42)
  df  <- data.frame(y = rnorm(50), x1 = rnorm(50), x2 = rnorm(50))
  # Force x2 coefficient close to zero by making it weakly related to y
  df$y <- df$x1 * 2 + rnorm(50, 0, 5)
  fit <- lm(y ~ x1 + x2, data = df)
  ps  <- extract_priors(fit, multiplier = 2, min_sd = 0.5)

  # Extract SDs from brms prior object and check floor
  prior_df <- as.data.frame(ps$prior)
  beta_rows <- prior_df[prior_df$class == "b" & prior_df$coef != "", ]

  # All SDs should be >= min_sd
  extract_sd <- function(prior_str) {
    m <- regmatches(prior_str, regexpr("normal\\([^,]+,\\s*([0-9.]+)\\)", prior_str))
    if (length(m) == 0) return(NA)
    as.numeric(sub(".*,\\s*([0-9.]+)\\).*", "\\1", m))
  }
  sds <- sapply(beta_rows$prior, extract_sd)
  expect_true(all(sds >= 0.5, na.rm = TRUE))
})

test_that("extract_priors.glm works for gaussian family", {
  set.seed(2)
  df  <- data.frame(y = rnorm(40), x1 = rnorm(40), x2 = rnorm(40))
  fit <- glm(y ~ x1 + x2, data = df, family = gaussian())
  ps  <- extract_priors(fit)

  expect_s3_class(ps, "et_prior_spec")
  expect_equal(ps$method, "glm")
  expect_true(all(c("x1", "x2") %in% ps$pred_names))
})

test_that("extract_priors.lm print method runs without error", {
  set.seed(3)
  df  <- data.frame(y = rnorm(20), x1 = rnorm(20))
  fit <- lm(y ~ x1, data = df)
  ps  <- extract_priors(fit)
  expect_output(print(ps), "ErrorTracer prior specification")
  expect_output(print(ps), "lm")
})

test_that("extract_priors.lm errors with intercept-only model", {
  df  <- data.frame(y = rnorm(10))
  fit <- lm(y ~ 1, data = df)
  expect_error(extract_priors(fit), "No non-intercept predictors")
})

test_that("extract_priors dispatches correctly based on class", {
  set.seed(10)
  df  <- data.frame(y = rnorm(30), x = rnorm(30))
  lm_fit  <- lm(y ~ x, data = df)
  glm_fit <- glm(y ~ x, data = df, family = gaussian())

  ps_lm  <- extract_priors(lm_fit)
  ps_glm <- extract_priors(glm_fit)

  expect_equal(ps_lm$method, "lm")
  expect_equal(ps_glm$method, "glm")
})

test_that("extract_priors.glmnet works when glmnet is available", {
  skip_if_not_installed("glmnet")
  set.seed(5)
  x   <- matrix(rnorm(100 * 5), 100, 5,
                 dimnames = list(NULL, paste0("x", 1:5)))
  y   <- x[, 1] * 2 + x[, 3] * (-1) + rnorm(100)
  cvf <- glmnet::cv.glmnet(x, y, alpha = 0.5)
  ps  <- extract_priors(cvf)

  expect_s3_class(ps, "et_prior_spec")
  expect_equal(ps$method, "glmnet")
  expect_true(length(ps$pred_names) > 0)
  expect_true(all(ps$pred_names %in% paste0("x", 1:5)))
})

test_that("extract_priors.ranger works when ranger is available", {
  skip_if_not_installed("ranger")
  set.seed(7)
  df  <- data.frame(y = rnorm(80), x1 = rnorm(80), x2 = rnorm(80), x3 = rnorm(80))
  df$y <- df$x1 * 1.5 + df$x2 * (-0.5) + rnorm(80)
  rf  <- ranger::ranger(y ~ x1 + x2 + x3, data = df,
                        importance = "permutation")
  ps  <- extract_priors(rf, multiplier = 2, min_sd = 0.1)

  expect_s3_class(ps, "et_prior_spec")
  expect_equal(ps$method, "ranger")
  expect_null(ps$coefs)   # ranger has no signed coefficients
  expect_true(length(ps$pred_names) > 0)
})
