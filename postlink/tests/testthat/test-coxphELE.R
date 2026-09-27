local_edition(3)

# Helper function to simulate CoxPH data with ELE linkage errors
# Based on Vo et al. (2024) logic
generate_ele_cox_data <- function(n = 200, m_rate = 0.15, num_blocks = 1) {
 set.seed(123)

 # 1. Simulate Covariates and Survival Times
 x1 <- rnorm(n)
 x2 <- rbinom(n, 1, 0.5)
 true_beta <- c(x1 = 0.8, x2 = -0.5)

 true_hazard <- exp(0.8 * x1 - 0.5 * x2)
 true_time <- rexp(n, rate = true_hazard)

 # Add censoring (approx 30%)
 cens_time <- rexp(n, rate = 0.3)
 obs_time <- pmin(true_time, cens_time)
 obs_status <- as.numeric(true_time <= cens_time)

 # 2. Assign Blocks
 blocks <- sort(rep(1:num_blocks, length.out = n))

 # 3. Induce Linkage Errors (shuffling covariates within blocks)
 linked_x1 <- x1
 linked_x2 <- x2

 for (v in 1:num_blocks) {
  idx <- which(blocks == v)
  n_v <- length(idx)
  mis_size <- round(m_rate * n_v)

  if (mis_size > 0) {
   mis_idx <- sample(idx, mis_size)
   shuff_idx <- sample(mis_idx)
   linked_x1[mis_idx] <- x1[shuff_idx]
   linked_x2[mis_idx] <- x2[shuff_idx]
  }
 }

 dat <- data.frame(
  time = obs_time,
  status = obs_status,
  x1 = linked_x1,
  x2 = linked_x2,
  blocks = blocks
 )

 return(list(data = dat, true_beta = true_beta))
}

test_that("coxphELE engine runs and produces valid output", {
 sim <- generate_ele_cox_data(n = 300, m_rate = 0.1)

 fit <- coxphELE(
  x = as.matrix(sim$data[, c("x1", "x2")]),
  y = sim$data$time,
  cens = 1 - sim$data$status, # Routine uses 1 = censored
  m.rate = 0.1,
  blocks = sim$data$blocks
 )

 expect_s3_class(fit, "coxphELE")
 expect_equal(length(fit$coefficients), 2)
 expect_true(all(!is.na(fit$coefficients)))
 expect_true(all(diag(fit$var) > 0))
})

test_that("S3 Methods (print, summary, vcov, confint) work correctly", {
 sim <- generate_ele_cox_data(n = 200)
 adj <- adjELE(linked.data = sim$data, m.rate = 0.15)
 fit <- plcoxph(Surv(time, status) ~ x1 + x2, adjustment = adj)

 # vcov
 expect_equal(dim(vcov(fit)), c(2, 2))

 # summary
 s_fit <- summary(fit)
 expect_s3_class(s_fit, "summary.coxphELE")
 expect_equal(nrow(s_fit$coefficients), 2)
 expect_equal(colnames(s_fit$coefficients), c("coef", "exp(coef)", "se(coef)", "z", "Pr(>|z|)"))

 # confint
 ci <- confint(fit, level = 0.9)
 expect_equal(dim(ci), c(2, 2))
 expect_equal(colnames(ci), c("5 %", "95 %"))

 # print (capturing output to ensure no errors)
 expect_output(print(fit))
 expect_output(print(s_fit))
})

test_that("Predict method handles lp and risk types", {
 sim <- generate_ele_cox_data(n = 100)
 adj <- adjELE(linked.data = sim$data, m.rate = 0.15)
 fit <- plcoxph(Surv(time, status) ~ x1 + x2, adjustment = adj)

 # Original data
 lp <- predict(fit, type = "lp")
 expect_length(lp, 100)

 risk <- predict(fit, type = "risk")
 expect_true(all(risk > 0))
 expect_equal(risk, exp(lp))

 # New data
 new_dat <- data.frame(x1 = c(0, 1), x2 = c(1, 0))
 lp_new <- predict(fit, newdata = new_dat)
 expect_length(lp_new, 2)
})

test_that("coxphELE handles audit.size for variance inflation", {
 # When audit.size is provided, V1 component of variance is added
 sim <- generate_ele_cox_data(n = 400, m_rate = 0.2, num_blocks = 2)

 # Fit without audit (V1 = 0)
 fit_no_audit <- coxphELE(
  x = as.matrix(sim$data[, c("x1", "x2")]),
  y = sim$data$time,
  cens = 1 - sim$data$status,
  m.rate = 0.2,
  blocks = sim$data$blocks
 )

 # Fit with audit (V1 > 0)
 fit_audit <- coxphELE(
  x = as.matrix(sim$data[, c("x1", "x2")]),
  y = sim$data$time,
  cens = 1 - sim$data$status,
  m.rate = 0.2,
  blocks = sim$data$blocks,
  audit.size = 20 # 20 per block
 )

 # Variances should generally be larger when accounting for m.rate estimation error
 expect_true(all(diag(fit_audit$var) >= diag(fit_no_audit$var)))
})

test_that("fitcoxph.adjELE correctly subsets and aligns data", {
 sim <- generate_ele_cox_data(n = 200)
 # Create some NAs in a variable NOT in the model to test alignment
 sim$data$unused_var <- c(rep(NA, 10), rep(1, 190))

 adj <- adjELE(linked.data = sim$data, m.rate = 0.15)

 # plcoxph internally calls fitcoxph.adjELE
 # Test with a subset to ensure row names/indices align properly
 fit_sub <- plcoxph(Surv(time, status) ~ x1 + x2,
                    data = sim$data,
                    subset = 51:200,
                    adjustment = adj)

 expect_equal(fit_sub$n, 150)
 expect_equal(length(fit_sub$linear.predictors), 150)
})

test_that("Routine throws appropriate errors for bad inputs", {
 sim <- generate_ele_cox_data(n = 50)

 # Mismatch in m.rate and blocks
 expect_error(
  coxphELE(x = as.matrix(sim$data[,3:4]), y = sim$data$time, cens = 1-sim$data$status,
           m.rate = c(0.1, 0.2), blocks = rep(1, 50))
 )

 # Bad response type in wrapper
 adj <- adjELE(linked.data = sim$data, m.rate = 0.1)
 expect_error(
  fitcoxph.adjELE(x = as.matrix(sim$data[,3:4]), y = sim$data$time, adjustment = adj)
 )
})

test_that("coxphELE handles missing blocks, explicit init.beta, and missing var_names", {
 sim <- generate_ele_cox_data(n = 50)

 # Strip column names to trigger: if (is.null(var_names)) var_names <- paste0("V", 1:p)
 x_no_names <- as.matrix(sim$data[, c("x1", "x2")])
 colnames(x_no_names) <- NULL

 # Omit blocks to trigger: blocks <- rep(1, n) ...
 # Provide init.beta to trigger: init.beta <- control$init.beta
 fit <- coxphELE(
  x = x_no_names,
  y = sim$data$time,
  cens = 1 - sim$data$status,
  m.rate = 0.1,
  control = list(init.beta = c(0.1, 0.1))
 )

 expect_equal(names(fit$coefficients), c("V1", "V2"))
 expect_equal(fit$n, 50)
})

test_that("coxphELE handles nleqslv non-convergence and singular Jacobian", {
 sim <- generate_ele_cox_data(n = 50)

 # Create a perfectly collinear matrix to guarantee a singular Jacobian
 x_bad <- cbind(sim$data$x1, sim$data$x1)

 # Expect two warnings
 expect_warning(
  expect_warning(
   fit_bad <- coxphELE(
    x = x_bad,
    y = sim$data$time,
    cens = 1 - sim$data$status,
    m.rate = 0.1,
    blocks = sim$data$blocks,
    control = list(init.beta = c(0, 0))
    # Prevent survival::coxph from assigning NA
   ),
   "algorithm may not have fully converged"
  ),
  "Jacobian is singular"
 )

 # Verify the resulting Variance matrix is populated with NAs
 expect_true(all(is.na(fit_bad$var)))
})

test_that("fitcoxph.adjELE correctly handles missing data and row mismatch errors", {
 sim <- generate_ele_cox_data(n = 50)

 # Trigger: "The 'adjustment' object does not contain linked data..."
 adj_nodata <- list(data_ref = list(data = NULL))
 expect_error(
  fitcoxph.adjELE(x = matrix(1), y = Surv(1, 1), adjustment = adj_nodata, control = list(init.beta = NULL)),
  "does not contain linked data"
 )

 # Trigger: "Row mismatch: Model matrix 'x' has no row names and its length..."
 x_no_rownames <- matrix(1:10, nrow = 5, ncol = 2)
 mock_full_data <- data.frame(id = 1:10)
 rownames(mock_full_data) <- 1:10
 adj_mismatch_len <- list(data_ref = list(data = mock_full_data), blocks = rep(1, 10), m.rate = 0.1)
 expect_error(
  fitcoxph.adjELE(x = x_no_rownames, y = Surv(1:5, rep(1, 5)), adjustment = adj_mismatch_len, control = list(init.beta = NULL)),
  "occurred without row names"
 )

 # Trigger: "Row mismatch: Some observations in the model matrix could not be matched..."
 x_wrong_rownames <- matrix(1:20, nrow = 10, ncol = 2)
 rownames(x_wrong_rownames) <- 11:20
 expect_error(
  fitcoxph.adjELE(x = x_wrong_rownames, y = Surv(1:10, rep(1, 10)), adjustment = adj_mismatch_len, control = list(init.beta = NULL)),
  "could not be matched"
 )
})

test_that("fitcoxph.adjELE properly processes varied lengths of m.rate and audit.size", {
 sim <- generate_ele_cox_data(n = 60, num_blocks = 2)
 full_dat <- sim$data
 rownames(full_dat) <- 1:nrow(full_dat)
 x_mat <- as.matrix(full_dat[, c("x1", "x2")])
 rownames(x_mat) <- rownames(full_dat)
 y_surv <- Surv(full_dat$time, full_dat$status)

 n_full <- nrow(full_dat)
 n_unique_full <- length(unique(full_dat$blocks))

 # Trigger: if (length(m_rate_in) == n_full) & if (length(audit_in) == n_full)
 adj_nfull <- list(
  data_ref = list(data = full_dat),
  blocks = full_dat$blocks,
  m.rate = rep(0.1, n_full),
  audit.size = rep(10, n_full)
 )
 fit_nfull <- fitcoxph.adjELE(x = x_mat, y = y_surv, adjustment = adj_nfull, control = list(init.beta = NULL))
 expect_s3_class(fit_nfull, "coxphELE")

 # Trigger: if (length(m_rate_in) == n_unique_full) &
 # if (length(audit_in) == n_unique_full)
 adj_nunique <- list(
  data_ref = list(data = full_dat),
  blocks = full_dat$blocks,
  m.rate = c(0.1, 0.15),
  audit.size = c(10, 15)
 )
 fit_nunique <- fitcoxph.adjELE(x = x_mat, y = y_surv, adjustment = adj_nunique, control = list(init.beta = NULL))
 expect_s3_class(fit_nunique, "coxphELE")

 # Trigger: audit.size length == 1 logic
 adj_audit_1 <- list(
  data_ref = list(data = full_dat),
  blocks = full_dat$blocks,
  m.rate = 0.1,
  audit.size = 10
 )
 fit_audit_1 <- fitcoxph.adjELE(x = x_mat, y = y_surv, adjustment = adj_audit_1, control = list(init.beta = NULL))
 expect_s3_class(fit_audit_1, "coxphELE")

 # Trigger error: Dimensions of 'm.rate'
 adj_badm <- list(
  data_ref = list(data = full_dat),
  blocks = full_dat$blocks,
  m.rate = c(0.1, 0.15, 0.2)
 )
 expect_error(
  fitcoxph.adjELE(x = x_mat, y = y_surv, adjustment = adj_badm, control = list(init.beta = NULL)),
  "inconsistent with data dimensions"
 )
})

test_that("fitcoxph.adjELE correctly handles edge cases in Surv objects and coefficient names", {
 sim <- generate_ele_cox_data(n = 50)
 rownames(sim$data) <- 1:50
 x_mat <- as.matrix(sim$data[, c("x1", "x2")])
 rownames(x_mat) <- rownames(sim$data)

 adj <- list(data_ref = list(data = sim$data), blocks = sim$data$blocks, m.rate = 0.1)

 # Trigger warning: "The current implementation of 'coxphELE' assumes right-censored data..."
 y_left <- Surv(sim$data$time, sim$data$status, type = "left")
 expect_warning(
  fitcoxph.adjELE(x = x_mat, y = y_left, adjustment = adj, control = list(init.beta = NULL)),
  "assumes right-censored data"
 )

 # Trigger: if (is.null(type)) type <- "right"
 y_notype <- Surv(sim$data$time, sim$data$status)
 attr(y_notype, "type") <- NULL
 fit_notype <- fitcoxph.adjELE(x = x_mat, y = y_notype, adjustment = adj, control = list(init.beta = NULL))
 expect_s3_class(fit_notype, "coxphELE")
})
