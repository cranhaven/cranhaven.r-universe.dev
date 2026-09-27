local_edition(3)

# Helper function to simulate GLM data with ELE linkage errors
# Adapted from Chambers (2009) simulation logic
generate_ele_glm_data <- function(n = 200, m_rate = 0.1, family = "gaussian") {
 set.seed(42)

 # 1. Design Matrix and True Parameters
 x1 <- rnorm(n)
 x2 <- rbinom(n, 1, 0.5)
 X <- cbind(Intercept = 1, x1 = x1, x2 = x2)
 true_beta <- c(1.5, 0.8, -0.5)

 # 2. Generate Response based on family
 eta <- as.vector(X %*% true_beta)

 if (family == "gaussian") {
  y <- eta + rnorm(n, sd = 0.5)
  fam_obj <- gaussian()
 } else if (family == "binomial") {
  prob <- plogis(eta)
  y <- rbinom(n, 1, prob)
  fam_obj <- binomial()
 } else if (family == "poisson") {
  mu <- exp(eta)
  y <- rpois(n, mu)
  fam_obj <- poisson()
 }

 # 3. Induce Linkage Errors (ELE model within a single block)
 # Shuffling a portion of the response vector to simulate mismatches
 mis_idx <- sample(1:n, size = round(m_rate * n))
 linked_y <- y
 if (length(mis_idx) > 1) {
  linked_y[mis_idx] <- y[sample(mis_idx)]
 }

 return(list(
  X = X,
  y = linked_y,
  true_beta = true_beta,
  family = family,
  fam_obj = fam_obj
 ))
}

test_that("glmELE engine fits all weighting methods for Gaussian", {
 dat <- generate_ele_glm_data(n = 300, m_rate = 0.15, family = "gaussian")

 # Test with "all" weight matrices
 fit <- glmELE(
  x = dat$X,
  y = dat$y,
  family = "gaussian",
  m.rate = 0.15,
  blocks = rep(1, 300),
  weight.matrix = "all"
 )

 expect_s3_class(fit, "glmELE")
 expect_equal(nrow(fit$coefficients), 3) # ratio, LL, BLUE
 expect_true(all(c("ratio", "LL", "BLUE") %in% rownames(fit$coefficients)))
 expect_true(all(diag(fit$var$BLUE) > 0)) # Check variance matrix
})

test_that("glmELE works for Binomial and Poisson families", {
 # Binomial
 dat_b <- generate_ele_glm_data(n = 200, m_rate = 0.1, family = "binomial")
 fit_b <- glmELE(dat_b$X, dat_b$y, family = "binomial", m.rate = 0.1, blocks = rep(1, 200))
 expect_equal(fit_b$family$family, "binomial")

 # Poisson
 dat_p <- generate_ele_glm_data(n = 200, m_rate = 0.1, family = "poisson")
 fit_p <- glmELE(dat_p$X, dat_p$y, family = "poisson", m.rate = 0.1, blocks = rep(1, 200))
 expect_equal(fit_p$family$family, "poisson")
})

test_that("S3 methods (print, summary, vcov, confint) behave as expected", {
 dat <- generate_ele_glm_data(n = 150)

 adj <- adjELE(linked.data = data.frame(dat$y, dat$X),
               m.rate = 0.1,
               weight.matrix = "all")

 fit <- plglm(dat.y ~ x1 + x2, data = data.frame(dat.y = dat$y, dat$X),
              family = "gaussian", adjustment = adj)

 expect_output(print(fit))
 s_fit <- summary(fit)
 expect_s3_class(s_fit, "summary.glmELE")

 expect_true("BLUE" %in% names(s_fit$coefficients))

 expect_equal(dim(vcov(fit, weight.matrix = "BLUE")), c(3, 3))

 ci <- confint(fit, level = 0.95, weight.matrix = "LL")
 expect_equal(nrow(ci), 3)
 expect_equal(colnames(ci), c("2.5 %", "97.5 %"))
})

test_that("Predict method handles new data and standard errors", {
 dat <- generate_ele_glm_data(n = 100)
 adj <- adjELE(linked.data = data.frame(dat.y = dat$y, x1 = dat$X[,2], x2 = dat$X[,3]), m.rate = 0.1)
 fit <- plglm(dat.y ~ x1 + x2, data = data.frame(dat.y = dat$y, x1 = dat$X[,2], x2 = dat$X[,3]),
              family = "gaussian", adjustment = adj)

 # In-sample prediction
 pred_in <- predict(fit, type = "response")
 expect_length(pred_in, 100)

 # New data with SE and intervals
 new_df <- data.frame(x1 = c(0, 0.5), x2 = c(1, 0))
 pred_new <- predict(fit, newdata = new_df, se.fit = TRUE, interval = "confidence")

 expect_equal(nrow(pred_new$fit), 2)
 expect_equal(colnames(pred_new$fit), c("fit", "lwr", "upr"))
 expect_length(pred_new$se.fit, 2)
 expect_true(pred_new$residual.scale > 0)
})

test_that("glmELE handles multi-block m.rates and audit.size", {
 # Simulate 2 blocks with different rates
 n <- 200
 blocks <- c(rep(1, 100), rep(2, 100))
 dat <- generate_ele_glm_data(n = n)

 # m.rate as a vector
 fit <- glmELE(
  x = dat$X, y = dat$y,
  m.rate = c(0.1, 0.05),
  blocks = blocks,
  audit.size = c(20, 20) # Variance inflation from estimated alpha
 )

 expect_equal(nrow(fit$coefficients), 3)
 expect_true(!is.null(fit$var$BLUE))
})

test_that("fitglm.adjELE correctly synchronizes subsets and row names", {
 n <- 100
 dat <- generate_ele_glm_data(n = n)
 df <- data.frame(y = dat$y, x1 = dat$X[,2], x2 = dat$X[,3])
 rownames(df) <- paste0("ID_", 1:n)

 adj <- adjELE(linked.data = df, m.rate = 0.1, weight.matrix = "all")

 # Fit on a subset
 # fitglm.adjELE must find row names ID_11 to ID_60 in the linked.data
 fit_sub <- plglm(y ~ x1 + x2, data = df, subset = 11:60, adjustment = adj)

 expect_equal(fit_sub$df.residual, 50 - 3)
 expect_equal(length(fit_sub$residuals["BLUE",]), 50)
 expect_equal(names(fit_sub$residuals["BLUE",])[1], "ID_11")
})

test_that("Error handling for invalid inputs", {
 dat <- generate_ele_glm_data(n = 20)

 # Dimension mismatch
 expect_error(glmELE(x = dat$X[1:10,], y = dat$y, m.rate = 0.1, blocks = rep(1, 20)))

 # Binomial response not 0/1
 expect_error(glmELE(x = dat$X, y = runif(20, 2, 5), family = "binomial", m.rate = 0.1, blocks = rep(1, 20)))

 # Unsupported family
 expect_error(glmELE(x = dat$X, y = dat$y, family = "inverse.gaussian", m.rate = 0.1, blocks = rep(1, 20)))
})

test_that("summary.glmELE handles missing variance and non-Gaussian families correctly", {
 dat_b <- generate_ele_glm_data(n = 100, m_rate = 0.1, family = "binomial")
 adj_b <- adjELE(linked.data = data.frame(y = dat_b$y, dat_b$X),
                 m.rate = 0.1, weight.matrix = "BLUE")
 fit_b <- plglm(y ~ x1 + x2, data = data.frame(y = dat_b$y, dat_b$X),
                family = "binomial", adjustment = adj_b)

 # 1. Test non-Gaussian z-value logic and pnorm branch
 s_fit_b <- summary(fit_b)
 expect_true("z value" %in% colnames(s_fit_b$coefficients$BLUE))
 expect_true("Pr(>|z|)" %in% colnames(s_fit_b$coefficients$BLUE))

 # 2. Test defensive fallback for missing variance matrix
 fit_b_broken <- fit_b
 fit_b_broken$var$BLUE <- NULL

 expect_warning(s_fit_broken <- summary(fit_b_broken),
                "Variance matrix not found for method: BLUE")
 # Assert SEs, z-values, and p-values became NA correctly, but estimates remain
 expect_true(all(is.na(s_fit_broken$coefficients$BLUE[, "Std. Error"])))
 expect_true(all(is.na(s_fit_broken$coefficients$BLUE[, "z value"])))
 expect_true(!any(is.na(s_fit_broken$coefficients$BLUE[, "Estimate"])))
})

test_that("print.summary.glmELE output matches exact snapshot formatting", {
 dat <- generate_ele_glm_data(n = 50, family = "gaussian")
 adj <- adjELE(linked.data = data.frame(y = dat$y, dat$X),
               m.rate = 0.1, weight.matrix = "BLUE")
 fit <- plglm(y ~ x1 + x2, data = data.frame(y = dat$y, dat$X),
              family = "gaussian", adjustment = adj)

 s_fit <- summary(fit)

 # expect_snapshot tests print methods for formatting,
 # dispersion values, stars, and call layout.
 expect_snapshot(print(s_fit))
})

test_that("vcov and confint handle defaults, numeric params, validations, and qnorm branches", {
 dat <- generate_ele_glm_data(n = 50, family = "gaussian")
 adj <- adjELE(linked.data = data.frame(y = dat$y, dat$X),
               m.rate = 0.1, weight.matrix = "all")
 fit <- plglm(y ~ x1 + x2, data = data.frame(y = dat$y, dat$X),
              family = "gaussian", adjustment = adj)

 # 1. Test weight.matrix default fallback in vcov
 v_default <- vcov(fit)
 v_explicit <- vcov(fit, weight.matrix = "ratio") # first method in adj
 expect_identical(v_default, v_explicit)

 # 2. Test weight.matrix default fallback in confint
 ci_default <- confint(fit)
 ci_explicit <- confint(fit, weight.matrix = "ratio")
 expect_identical(ci_default, ci_explicit)

 # 3. Test invalid weight.matrix stops
 expect_error(confint(fit, weight.matrix = "INVALID"),
              "Method INVALID not found in object coefficients.")

 # 4. Test numeric `parm` parsing in confint
 ci_num <- confint(fit, parm = c(2, 3))
 expect_equal(rownames(ci_num), c("x1", "x2"))
 expect_equal(nrow(ci_num), 2)

 # 5. Test confint qnorm branch (Binomial)
 dat_b <- generate_ele_glm_data(n = 50, family = "binomial")
 adj_b <- adjELE(linked.data = data.frame(y = dat_b$y, dat_b$X),
                 m.rate = 0.1, weight.matrix = "BLUE")
 fit_b <- plglm(y ~ x1 + x2, data = data.frame(y = dat_b$y, dat_b$X),
                family = "binomial", adjustment = adj_b)

 ci_bin <- confint(fit_b)
 # Ensure confidence bounds are symmetric around estimate on the link scale using qnorm
 est <- fit_b$coefficients["BLUE", 1]
 se <- sqrt(fit_b$var$BLUE[1, 1])
 crit <- stats::qnorm(0.975)
 expect_equal(ci_bin[1, "2.5 %"], est - crit * se)
})

test_that("predict.glmELE correctly handles missing components, in-sample reconstructions, and warnings", {
 dat <- generate_ele_glm_data(n = 50)
 adj <- adjELE(linked.data = data.frame(y = dat$y, dat$X),
               m.rate = 0.1, weight.matrix = "BLUE")
 fit <- plglm(y ~ x1 + x2, data = data.frame(y = dat$y, dat$X),
              family = "gaussian", adjustment = adj)

 # 1. Invalid weight.matrix
 expect_error(predict(fit, weight.matrix = "INVALID"),
              "Method INVALID not found in object coefficients.")

 # 2. Missing model component
 fit_no_mod <- fit
 fit_no_mod$model <- NULL
 expect_error(predict(fit_no_mod),
              "Ensure the model was fitted with 'model = TRUE'")

 # 3. In-sample prediction with SE/intervals forces reconstruction of X
 p_in <- predict(fit, se.fit = TRUE, interval = "confidence")
 expect_equal(nrow(p_in$fit), 50)
 expect_true(all(c("fit", "lwr", "upr") %in% colnames(p_in$fit)))
 expect_length(p_in$se.fit, 50)

 # 4. Missing variance warning in predict
 fit_no_var <- fit
 fit_no_var$var$BLUE <- NULL
 expect_warning(p_no_var <- predict(fit_no_var, se.fit = TRUE),
                "Variance matrix missing for this method. SEs set to NA.")
 expect_true(all(is.na(p_no_var$se.fit)))
})

test_that("predict.glmELE computes mathematical offsets correctly", {
 dat <- generate_ele_glm_data(n = 50)
 df <- data.frame(y = dat$y, x1 = dat$X[,2], x2 = dat$X[,3], off_val = rep(1, 50))
 adj <- adjELE(linked.data = df, m.rate = 0.1, weight.matrix = "BLUE")

 # Fit model with an offset
 fit_off <- plglm(y ~ x1 + x2 + offset(off_val), data = df,
                  family = "gaussian", adjustment = adj)

 # Mathematical validation of offset logic in new data
 nd_base <- data.frame(x1 = 0, x2 = 0, off_val = 0)
 nd_shifted <- data.frame(x1 = 0, x2 = 0, off_val = 5)

 p_base <- predict(fit_off, newdata = nd_base)
 p_shifted <- predict(fit_off, newdata = nd_shifted)

 # The shifted prediction should be EXACTLY 5 units higher
 expect_equal(unname(p_shifted), unname(p_base) + 5)
})

test_that("predict.glmELE computes napredict padding correctly", {
 dat <- generate_ele_glm_data(n = 50)
 df <- data.frame(y = dat$y, x1 = dat$X[,2], x2 = dat$X[,3])
 adj <- adjELE(linked.data = df, m.rate = 0.1, weight.matrix = "BLUE")

 fit <- plglm(y ~ x1 + x2, data = df, family = "gaussian", adjustment = adj)

 # NA action and padding validation (na.exclude adds NAs to outputs)
 nd_na <- data.frame(x1 = c(0, NA, 0), x2 = c(0, 0, 0))

 # Simple predict
 p_na_simple <- predict(fit, newdata = nd_na, na.action = na.exclude)
 expect_length(p_na_simple, 3)
 expect_true(is.na(p_na_simple[2]))
 expect_false(is.na(p_na_simple[1]))

 # Complex predict (matrix + list)
 p_na_complex <- predict(fit, newdata = nd_na, na.action = na.exclude,
                         se.fit = TRUE, interval = "confidence")

 expect_equal(nrow(p_na_complex$fit), 3)
 expect_true(is.na(p_na_complex$fit[2, "fit"]))
 expect_true(is.na(p_na_complex$fit[2, "lwr"]))
 expect_true(is.na(p_na_complex$se.fit[2]))
 expect_false(is.na(p_na_complex$se.fit[3]))
})

test_that("predict.glmELE non-Gaussian confidence intervals and response scaling are correct", {
 dat_p <- generate_ele_glm_data(n = 50, family = "poisson")
 adj_p <- adjELE(linked.data = data.frame(y = dat_p$y, dat_p$X),
                 m.rate = 0.1, weight.matrix = "BLUE")
 fit_p <- plglm(y ~ x1 + x2, data = data.frame(y = dat_p$y, dat_p$X),
                family = "poisson", adjustment = adj_p)

 # 1. Link scale interval (uses qnorm)
 p_link <- predict(fit_p, interval = "confidence", type = "link",
                   newdata = data.frame(x1 = 0, x2 = 0))

 expect_true(p_link$fit[1, "lwr"] < p_link$fit[1, "fit"])
 expect_true(p_link$fit[1, "upr"] > p_link$fit[1, "fit"])

 # 2. Response scale interval (linkinv transformation)
 p_resp <- predict(fit_p, interval = "confidence", type = "response",
                   newdata = data.frame(x1 = 0, x2 = 0))

 # For Poisson (log link), exp(link) == response
 expect_equal(p_resp$fit[1, "fit"], exp(p_link$fit[1, "fit"]))
 expect_equal(p_resp$fit[1, "lwr"], exp(p_link$fit[1, "lwr"]))
 expect_equal(p_resp$fit[1, "upr"], exp(p_link$fit[1, "upr"]))
})
