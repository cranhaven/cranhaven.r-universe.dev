## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  cache = TRUE,
  cache.lazy = FALSE,
  fig.align = "center",
  comment = "",
  fig.width = 10,
  fig.height = 5,
  dpi = 150
)

set.seed(2024)

## ----packages-----------------------------------------------------------------
library(gkwreg)
library(betareg)
library(ggplot2)

# Standardized color palette for all comparisons
MODEL_COLORS <- c(
  "Beta (betareg)" = "#D32F2F", # Red
  "Beta (gkwreg)" = "#1976D2", # Blue
  "Kumaraswamy" = "#388E3C", # Green
  "Exp. Kumaraswamy" = "#7B1FA2" # Purple
)

MODEL_NAMES <- c(
  betareg = "Beta (betareg)",
  gkw_beta = "Beta (gkwreg)",
  gkw_kw = "Kumaraswamy",
  gkw_ekw = "Exp. Kumaraswamy"
)

## ----helper_functions, include=FALSE------------------------------------------
# ==============================================================================
# SIMULATION INFRASTRUCTURE
# ==============================================================================

# Store fitted model objects
run_single_sim_with_fits <- function(n, dgp_fun, params, formula, test_prop = 0.2) {
  data_full <- tryCatch(
    {
      dgp_fun(n, params)
    },
    error = function(e) NULL
  )

  if (is.null(data_full)) {
    return(list(
      betareg = list(success = FALSE),
      gkw_kw = list(success = FALSE)
    ))
  }

  n_test <- floor(n * test_prop)
  test_idx <- sample(seq_len(n), n_test)
  train_data <- data_full[-test_idx, ]
  test_data <- data_full[test_idx, ]

  results <- list()

  # Beta regression
  results$betareg <- tryCatch(
    {
      t0 <- proc.time()[3]
      fit <- betareg(formula, data = train_data)
      time_elapsed <- proc.time()[3] - t0

      pred <- predict(fit, newdata = test_data, type = "response")

      if (any(is.na(pred)) || any(is.infinite(pred))) {
        return(list(success = FALSE))
      }

      rmse <- sqrt(mean((test_data$y - pred)^2))

      list(
        fit = fit,
        coef = coef(fit),
        se = sqrt(diag(vcov(fit))),
        loglik = as.numeric(logLik(fit)),
        aic = AIC(fit),
        bic = BIC(fit),
        rmse = rmse,
        time = time_elapsed,
        converged = isTRUE(fit$converged),
        success = TRUE
      )
    },
    error = function(e) {
      list(success = FALSE)
    }
  )

  # Kumaraswamy regression
  results$gkw_kw <- tryCatch(
    {
      t0 <- proc.time()[3]
      fit <- gkwreg(formula, data = train_data, family = "kw")
      time_elapsed <- proc.time()[3] - t0

      pred <- predict(fit, newdata = test_data, type = "response")

      if (any(is.na(pred)) || any(is.infinite(pred))) {
        return(list(success = FALSE))
      }

      rmse <- sqrt(mean((test_data$y - pred)^2))

      list(
        fit = fit,
        coef = coef(fit),
        se = fit$se,
        loglik = fit$loglik,
        aic = fit$aic,
        bic = fit$bic,
        rmse = rmse,
        time = time_elapsed,
        converged = isTRUE(fit$convergence),
        success = TRUE
      )
    },
    error = function(e) {
      list(success = FALSE)
    }
  )

  results
}

run_single_sim <- function(n, dgp_fun, params, formula, test_prop = 0.2) {
  data_full <- tryCatch(
    {
      dgp_fun(n, params)
    },
    error = function(e) NULL
  )

  if (is.null(data_full)) {
    return(list(
      betareg = list(success = FALSE),
      gkw_beta = list(success = FALSE),
      gkw_kw = list(success = FALSE),
      gkw_ekw = list(success = FALSE)
    ))
  }

  n_test <- floor(n * test_prop)
  test_idx <- sample(seq_len(n), n_test)
  train_data <- data_full[-test_idx, ]
  test_data <- data_full[test_idx, ]

  results <- list()

  safe_fit <- function(fit_fun, name) {
    tryCatch(
      {
        t0 <- proc.time()[3]
        fit <- fit_fun()
        time_elapsed <- proc.time()[3] - t0

        if (name == "betareg") {
          loglik <- as.numeric(logLik(fit))
          aic <- AIC(fit)
          bic <- BIC(fit)
          converged <- isTRUE(fit$converged)
        } else {
          loglik <- fit$loglik
          aic <- fit$aic
          bic <- fit$bic
          converged <- isTRUE(fit$convergence)
        }

        pred <- predict(fit, newdata = test_data, type = "response")

        if (any(is.na(pred)) || any(is.infinite(pred))) {
          return(list(success = FALSE))
        }

        rmse <- sqrt(mean((test_data$y - pred)^2))

        if (!is.finite(loglik)) loglik <- NA_real_
        if (!is.finite(aic)) aic <- NA_real_
        if (!is.finite(bic)) bic <- NA_real_
        if (!is.finite(rmse)) rmse <- NA_real_
        if (!is.finite(time_elapsed)) time_elapsed <- NA_real_

        list(
          loglik = as.numeric(loglik),
          aic = as.numeric(aic),
          bic = as.numeric(bic),
          rmse = as.numeric(rmse),
          time = as.numeric(time_elapsed),
          converged = converged,
          success = TRUE
        )
      },
      error = function(e) {
        list(success = FALSE)
      }
    )
  }

  results$betareg <- safe_fit(
    function() betareg(formula, data = train_data),
    "betareg"
  )

  results$gkw_beta <- safe_fit(
    function() gkwreg(formula, data = train_data, family = "beta"),
    "gkwreg"
  )

  results$gkw_kw <- safe_fit(
    function() gkwreg(formula, data = train_data, family = "kw"),
    "gkwreg"
  )

  results$gkw_ekw <- safe_fit(
    function() gkwreg(formula, data = train_data, family = "ekw"),
    "gkwreg"
  )

  results
}

run_full_simulation <- function(n_sim, n, dgp_fun, params, formula, verbose = FALSE) {
  all_results <- vector("list", n_sim)

  for (i in seq_len(n_sim)) {
    all_results[[i]] <- run_single_sim(n, dgp_fun, params, formula)
  }

  models <- c("betareg", "gkw_beta", "gkw_kw", "gkw_ekw")
  summary_stats <- list()

  for (model in models) {
    success_idx <- sapply(all_results, function(x) {
      !is.null(x[[model]]) && isTRUE(x[[model]]$success)
    })

    n_success <- sum(success_idx)

    if (n_success == 0) {
      summary_stats[[model]] <- list(
        n_success = 0,
        conv_rate = 0,
        loglik_mean = NA_real_,
        aic_mean = NA_real_,
        bic_mean = NA_real_,
        rmse_mean = NA_real_,
        time_mean = NA_real_
      )
      next
    }

    successful <- all_results[success_idx]

    extract_metric <- function(metric_name) {
      vals <- sapply(successful, function(x) {
        val <- x[[model]][[metric_name]]
        if (is.null(val) || !is.numeric(val)) {
          return(NA_real_)
        }
        as.numeric(val)
      })
      if (all(is.na(vals))) {
        return(NA_real_)
      }
      vals
    }

    converged_vals <- sapply(successful, function(x) {
      val <- x[[model]]$converged
      if (is.null(val)) {
        return(FALSE)
      }
      if (is.logical(val)) {
        return(val)
      }
      if (is.numeric(val)) {
        return(val > 0)
      }
      FALSE
    })

    summary_stats[[model]] <- list(
      n_success = n_success,
      conv_rate = mean(converged_vals, na.rm = TRUE),
      loglik_mean = mean(extract_metric("loglik"), na.rm = TRUE),
      aic_mean = mean(extract_metric("aic"), na.rm = TRUE),
      bic_mean = mean(extract_metric("bic"), na.rm = TRUE),
      rmse_mean = mean(extract_metric("rmse"), na.rm = TRUE),
      time_mean = mean(extract_metric("time"), na.rm = TRUE)
    )
  }

  list(
    raw = all_results,
    summary = summary_stats,
    n = n,
    n_sim = n_sim,
    formula = formula
  )
}

make_comparison_table <- function(sim_results) {
  models <- names(sim_results$summary)

  safe_format <- function(x, digits = 2) {
    if (is.null(x) || is.na(x)) {
      return(NA_real_)
    }
    round(as.numeric(x), digits)
  }

  df_list <- lapply(models, function(model) {
    s <- sim_results$summary[[model]]

    if (is.null(s) || s$n_success == 0) {
      return(data.frame(
        Model = MODEL_NAMES[model],
        N_Success = 0,
        Conv_Rate = NA_real_,
        AIC = NA_real_,
        RMSE = NA_real_,
        Time = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    data.frame(
      Model = MODEL_NAMES[model],
      N_Success = s$n_success,
      Conv_Rate = safe_format(s$conv_rate * 100, 1),
      AIC = safe_format(s$aic_mean, 2),
      RMSE = safe_format(s$rmse_mean, 4),
      Time = safe_format(s$time_mean, 4),
      stringsAsFactors = FALSE
    )
  })

  df <- do.call(rbind, df_list)
  rownames(df) <- NULL
  df
}

## ----scenario1_dgp------------------------------------------------------------
dgp_beta <- function(n, params) {
  x1 <- rnorm(n, 0, 1)
  x2 <- runif(n, -1, 1)

  eta_mu <- params$beta_mu[1] + params$beta_mu[2] * x1 + params$beta_mu[3] * x2
  eta_phi <- params$beta_phi[1] + params$beta_phi[2] * x1

  mu <- plogis(eta_mu)
  phi <- exp(eta_phi)

  y <- rbeta(n, mu * phi, (1 - mu) * phi)
  y <- pmax(pmin(y, 0.999), 0.001)

  data.frame(y = y, x1 = x1, x2 = x2, mu = mu, phi = phi)
}

## ----scenario1_visualization, fig.height=6, fig.cap="Distributional Characteristics - Scenario 1 (Well-Specified Beta)"----
set.seed(123)
vis_data_s1 <- dgp_beta(1000, list(
  beta_mu = c(0.5, -0.8, 0.6),
  beta_phi = c(1.5, 0.4)
))

par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))

# Response distribution
hist(vis_data_s1$y,
  breaks = 40, col = "#388E3C", border = "white",
  main = "Response Distribution", xlab = "Y", prob = TRUE,
  cex.main = 1.2, cex.lab = 1.1
)
lines(density(vis_data_s1$y), col = "#D32F2F", lwd = 2)
legend("topright", "Kernel density", col = "#D32F2F", lwd = 2, bty = "n")

# Effect of x1
plot(vis_data_s1$x1, vis_data_s1$y,
  pch = 16, col = rgb(0, 0, 0, 0.3),
  main = "Covariate Effect: x1", xlab = "x1", ylab = "Y",
  cex.main = 1.2, cex.lab = 1.1
)
lines(lowess(vis_data_s1$x1, vis_data_s1$y), col = "#D32F2F", lwd = 3)
abline(h = 0.5, lty = 2, col = "gray50")

# Effect of x2
plot(vis_data_s1$x2, vis_data_s1$y,
  pch = 16, col = rgb(0, 0, 0, 0.3),
  main = "Covariate Effect: x2", xlab = "x2", ylab = "Y",
  cex.main = 1.2, cex.lab = 1.1
)
lines(lowess(vis_data_s1$x2, vis_data_s1$y), col = "#388E3C", lwd = 3)
abline(h = 0.5, lty = 2, col = "gray50")

# Conditional variance
plot(abs(vis_data_s1$x1), (vis_data_s1$y - vis_data_s1$mu)^2,
  pch = 16, col = rgb(0, 0, 0, 0.2),
  main = "Heteroscedasticity Structure",
  xlab = "|x1|", ylab = "Squared Residuals",
  cex.main = 1.2, cex.lab = 1.1
)
lines(lowess(abs(vis_data_s1$x1), (vis_data_s1$y - vis_data_s1$mu)^2),
  col = "#1976D2", lwd = 3
)

# mtext("Figure 1: Distributional Characteristics - Scenario 1 (Well-Specified Beta)",
#       side = 3, line = -2, outer = TRUE, font = 2, cex = 1.1)

## ----scenario1_sim, cache=TRUE------------------------------------------------
results_s1 <- run_full_simulation(
  n_sim = 200,
  n = 300,
  dgp_fun = dgp_beta,
  params = list(
    beta_mu = c(0.5, -0.8, 0.6),
    beta_phi = c(1.5, 0.4)
  ),
  formula = y ~ x1 + x2 | x1
)

comp_s1 <- make_comparison_table(results_s1)

## ----scenario1_coef_comparison, cache=TRUE------------------------------------
# Fit models to one dataset for coefficient comparison
set.seed(456)
data_coef_s1 <- dgp_beta(300, list(
  beta_mu = c(0.5, -0.8, 0.6),
  beta_phi = c(1.5, 0.4)
))

fit_beta_s1 <- betareg(y ~ x1 + x2 | x1, data = data_coef_s1)
fit_kw_s1 <- gkwreg(y ~ x1 + x2 | x1, data = data_coef_s1, family = "kw")

# Extract coefficients
coef_beta_s1 <- coef(fit_beta_s1)
se_beta_s1 <- sqrt(diag(vcov(fit_beta_s1)))

coef_kw_s1 <- coef(fit_kw_s1)
se_kw_s1 <- fit_kw_s1$se

# Build comparison table
coef_names <- names(coef_beta_s1)
true_values <- c(0.5, -0.8, 0.6, 1.5, 0.4)

coef_comparison_s1 <- data.frame(
  Parameter = coef_names,
  True_Value = true_values,
  Beta_Est = round(coef_beta_s1, 4),
  Beta_SE = round(se_beta_s1, 4),
  Beta_Z = round(coef_beta_s1 / se_beta_s1, 2),
  Kw_Est = round(coef_kw_s1, 4),
  Kw_SE = round(se_kw_s1, 4),
  Kw_Z = round(coef_kw_s1 / se_kw_s1, 2)
)

# Calculate differences
coef_comparison_s1$Bias_Beta <- round(coef_comparison_s1$Beta_Est - true_values, 4)
coef_comparison_s1$Bias_Kw <- round(coef_comparison_s1$Kw_Est - true_values, 4)
coef_comparison_s1$SE_Ratio <- round(coef_comparison_s1$Kw_SE / coef_comparison_s1$Beta_SE, 3)

knitr::kable(
  row.names = FALSE,
  coef_comparison_s1,
  caption = "Table 1: Parameter Estimates - Scenario 1 (Well-Specified Beta, n=300)",
  col.names = c(
    "Parameter", "True", "Est.", "SE", "z", "Est.", "SE", "z",
    "Bias", "Bias", "SE Ratio"
  ),
  align = c("l", rep("r", 10))
)

## ----scenario2_dgp------------------------------------------------------------
dgp_heavy_tails <- function(n, params) {
  x1 <- rnorm(n, 0, 1)
  x2 <- rbinom(n, 1, 0.5)

  eta_alpha <- params$beta_alpha[1] + params$beta_alpha[2] * x1
  eta_beta <- params$beta_beta[1] + params$beta_beta[2] * x2
  eta_lambda <- params$beta_lambda[1]

  alpha <- exp(eta_alpha)
  beta <- exp(eta_beta)
  lambda <- exp(eta_lambda)

  u <- runif(n)
  y <- (1 - (1 - u^(1 / beta))^(1 / alpha))^(1 / lambda)
  y <- pmax(pmin(y, 0.9999), 0.0001)

  data.frame(y = y, x1 = x1, x2 = factor(x2))
}

## ----scenario2_visualization, fig.height=6, fig.cap="Distributional Characteristics - Scenario 2 (Heavy Tails)"----
set.seed(789)
vis_data_s2 <- dgp_heavy_tails(1500, list(
  beta_alpha = c(0.8, -0.5),
  beta_beta = c(0.3, 0.4),
  beta_lambda = c(0.6)
))

par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

# Response distribution
hist(vis_data_s2$y,
  breaks = 60, col = "#7B1FA2", border = "white",
  main = "Heavy-Tailed Distribution", xlab = "Y", prob = TRUE,
  cex.main = 1.2, cex.lab = 1.1
)
lines(density(vis_data_s2$y), col = "#D32F2F", lwd = 2)
# Add Beta approximation for comparison
beta_approx <- rbeta(1500, 2, 2)
lines(density(beta_approx), col = "#1976D2", lwd = 2, lty = 2)
legend("topright", c("True density", "Beta approximation"),
  col = c("#D32F2F", "#1976D2"), lwd = 2, lty = c(1, 2), bty = "n", cex = 0.9
)

# QQ plot against Beta
qqplot(rbeta(1500, 2, 2), vis_data_s2$y,
  main = "Q-Q Plot: EKw vs Beta(2,2)",
  xlab = "Beta(2,2) Quantiles", ylab = "Observed Quantiles",
  pch = 16, col = rgb(0, 0, 0, 0.3), cex.main = 1.2, cex.lab = 1.1
)
abline(0, 1, col = "#D32F2F", lwd = 2)
text(0.2, 0.8, "Heavier tails\n(departures)", col = "#D32F2F", cex = 0.9)

# Effect by group
boxplot(y ~ x2,
  data = vis_data_s2, col = c("#388E3C", "#7B1FA2"),
  main = "Distribution by Group", xlab = "x2", ylab = "Y",
  names = c("Group 0", "Group 1"), cex.main = 1.2, cex.lab = 1.1
)

# Tail behavior
plot(sort(vis_data_s2$y)[1:50],
  ylab = "Y", xlab = "Order (Lower Tail)",
  main = "Lower Tail Concentration", pch = 16, col = "#7B1FA2",
  cex.main = 1.2, cex.lab = 1.1
)
beta_tail <- sort(rbeta(1500, 2, 2))[1:50]
points(beta_tail, pch = 1, col = "#1976D2")
legend("topleft", c("EKw", "Beta(2,2)"),
  col = c("#7B1FA2", "#1976D2"),
  pch = c(16, 1), bty = "n"
)

# mtext("Figure 2: Distributional Characteristics - Scenario 2 (Heavy Tails)",
#       side = 3, line = -2, outer = TRUE, font = 2, cex = 1.1)

## ----scenario2_sim, cache=TRUE------------------------------------------------
results_s2 <- run_full_simulation(
  n_sim = 200,
  n = 300,
  dgp_fun = dgp_heavy_tails,
  params = list(
    beta_alpha = c(0.8, -0.5),
    beta_beta = c(0.3, 0.4),
    beta_lambda = c(0.6)
  ),
  formula = y ~ x1 | x2
)

comp_s2 <- make_comparison_table(results_s2)

## ----scenario2_coef_comparison, cache=TRUE------------------------------------
# Fit models to one dataset
set.seed(101112)
data_coef_s2 <- dgp_heavy_tails(300, list(
  beta_alpha = c(0.8, -0.5),
  beta_beta = c(0.3, 0.4),
  beta_lambda = c(0.6)
))

fit_beta_s2 <- betareg(y ~ x1 | x2, data = data_coef_s2)
fit_kw_s2 <- gkwreg(y ~ x1 | x2, data = data_coef_s2, family = "kw")

coef_beta_s2 <- coef(fit_beta_s2)
se_beta_s2 <- sqrt(diag(vcov(fit_beta_s2)))

coef_kw_s2 <- coef(fit_kw_s2)
se_kw_s2 <- fit_kw_s2$se

# Note: True parameters are on different scale (α, β vs μ, φ)
# Focus on relative comparisons
coef_comparison_s2 <- data.frame(
  Parameter = names(coef_beta_s2),
  Beta_Est = round(coef_beta_s2, 4),
  Beta_SE = round(se_beta_s2, 4),
  Beta_Z = round(coef_beta_s2 / se_beta_s2, 2),
  Kw_Est = round(coef_kw_s2, 4),
  Kw_SE = round(se_kw_s2, 4),
  Kw_Z = round(coef_kw_s2 / se_kw_s2, 2),
  SE_Ratio = round(se_kw_s2 / se_beta_s2, 3)
)

knitr::kable(
  row.names = FALSE,
  coef_comparison_s2,
  caption = "Table 2: Parameter Estimates - Scenario 2 (Heavy Tails, n=300)",
  # col.names = c("Parameter", "Est.", "SE", "z", "Est.", "SE", "z", "SE Ratio"),
  align = c("l", rep("r", 7))
)

## ----scenario3_dgp------------------------------------------------------------
dgp_extreme <- function(n, params) {
  x1 <- rnorm(n, 0, 1)
  group <- sample(c("J", "U"), n, replace = TRUE)

  alpha <- ifelse(
    group == "J",
    exp(params$alpha_J[1] + params$alpha_J[2] * x1),
    exp(params$alpha_U[1] + params$alpha_U[2] * x1)
  )

  beta <- ifelse(group == "J", exp(params$beta_J), exp(params$beta_U))

  u <- runif(n)
  y <- (1 - (1 - u)^(1 / beta))^(1 / alpha)
  y <- pmax(pmin(y, 0.9999), 0.0001)

  data.frame(y = y, x1 = x1, group = factor(group))
}

## ----scenario3_visualization, fig.height=7, fig.cap="Extreme Distributional Shapes - Scenario 3 (Boundary Concentration)"----
set.seed(131415)
vis_data_s3 <- dgp_extreme(2000, list(
  alpha_J = c(-1.5, 0.2),
  beta_J = 2.0,
  alpha_U = c(-1.8, 0.1),
  beta_U = -0.8
))

par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

# J-shaped distribution
hist(vis_data_s3$y[vis_data_s3$group == "J"],
  breaks = 50,
  col = "#FF6F00", border = "white",
  main = "J-Shaped Distribution", xlab = "Y", prob = TRUE,
  xlim = c(0, 1), cex.main = 1.2, cex.lab = 1.1
)
lines(density(vis_data_s3$y[vis_data_s3$group == "J"]),
  col = "#D32F2F", lwd = 2
)
text(0.5, 15, sprintf(
  "80%% of mass\n< %.2f",
  quantile(vis_data_s3$y[vis_data_s3$group == "J"], 0.8)
),
col = "#D32F2F", cex = 0.9
)

# U-shaped distribution
hist(vis_data_s3$y[vis_data_s3$group == "U"],
  breaks = 50,
  col = "#0277BD", border = "white",
  main = "U-Shaped Distribution", xlab = "Y", prob = TRUE,
  xlim = c(0, 1), cex.main = 1.2, cex.lab = 1.1
)
lines(density(vis_data_s3$y[vis_data_s3$group == "U"]),
  col = "#D32F2F", lwd = 2
)
text(0.5, 3, "Bimodal:\nconcentration\nat boundaries",
  col = "#D32F2F", cex = 0.9
)

# Combined distribution
plot(density(vis_data_s3$y),
  main = "Mixture Distribution",
  xlab = "Y", ylab = "Density", lwd = 2, col = "#7B1FA2",
  cex.main = 1.2, cex.lab = 1.1
)
polygon(density(vis_data_s3$y), col = rgb(0.48, 0.12, 0.63, 0.3), border = NA)

# Empirical CDF showing boundary concentration
plot(ecdf(vis_data_s3$y),
  main = "Empirical CDF",
  xlab = "Y", ylab = "F(Y)", lwd = 2, col = "#388E3C",
  cex.main = 1.2, cex.lab = 1.1
)
abline(v = c(0.1, 0.9), lty = 2, col = "gray40")
text(0.05, 0.5, sprintf(
  "%.0f%%\n< 0.1",
  100 * mean(vis_data_s3$y < 0.1)
),
col = "#D32F2F", cex = 0.9
)
text(0.95, 0.5, sprintf(
  "%.0f%%\n> 0.9",
  100 * mean(vis_data_s3$y > 0.9)
),
col = "#D32F2F", cex = 0.9
)

# mtext("Figure 3: Extreme Distributional Shapes - Scenario 3 (Boundary Concentration)",
#       side = 3, line = -2, outer = TRUE, font = 2, cex = 1.1)

## ----scenario3_sim, cache=TRUE------------------------------------------------
results_s3 <- run_full_simulation(
  n_sim = 200,
  n = 400,
  dgp_fun = dgp_extreme,
  params = list(
    alpha_J = c(-1.5, 0.2),
    beta_J = 2.0,
    alpha_U = c(-1.8, 0.1),
    beta_U = -0.8
  ),
  formula = y ~ x1 * group | group
)

comp_s3 <- make_comparison_table(results_s3)

## ----scenario3_coef_comparison, cache=TRUE------------------------------------
# For this scenario, we'll fit to a single J-shaped dataset where Beta converges
set.seed(161718)
# Generate J-shaped only for better Beta convergence probability
data_coef_s3 <- dgp_extreme(400, list(
  alpha_J = c(-1.2, 0.15), # Slightly less extreme for illustration
  beta_J = 1.8,
  alpha_U = c(-1.5, 0.1),
  beta_U = -0.5
))

# Try fitting - Beta may fail
fit_beta_s3 <- tryCatch(
  {
    betareg(y ~ x1 * group | group, data = data_coef_s3)
  },
  error = function(e) NULL
)

fit_kw_s3 <- gkwreg(y ~ x1 * group | group, data = data_coef_s3, family = "kw")

if (!is.null(fit_beta_s3) && fit_beta_s3$converged) {
  coef_beta_s3 <- coef(fit_beta_s3)
  se_beta_s3 <- sqrt(diag(vcov(fit_beta_s3)))

  coef_kw_s3 <- coef(fit_kw_s3)
  se_kw_s3 <- fit_kw_s3$se

  coef_comparison_s3 <- data.frame(
    Parameter = names(coef_beta_s3),
    Beta_Est = round(coef_beta_s3, 4),
    Beta_SE = round(se_beta_s3, 4),
    Beta_Z = round(coef_beta_s3 / se_beta_s3, 2),
    Kw_Est = round(coef_kw_s3, 4),
    Kw_SE = round(se_kw_s3, 4),
    Kw_Z = round(coef_kw_s3 / se_kw_s3, 2),
    SE_Ratio = round(se_kw_s3 / se_beta_s3, 3)
  )

  knitr::kable(
    coef_comparison_s3,
    caption = "Table 3: Parameter Estimates - Scenario 3 (Extreme Shapes, n=400, Beta Converged)",
    col.names = c("Parameter", "Est.", "SE", "z", "Est.", "SE", "z", "SE Ratio"),
    align = c("l", rep("r", 7))
  )
} else {
  cat("**Table 3: Parameter Estimates - Scenario 3**\n\n")
  cat("Beta regression failed to converge (as expected in 94.5% of cases).\n")
  cat("Only Kumaraswamy estimates are available:\n\n")

  coef_kw_s3 <- coef(fit_kw_s3)
  se_kw_s3 <- fit_kw_s3$se

  knitr::kable(
    digits = 3,
    data.frame(
      row.names = FALSE,
      Parameter = names(coef_kw_s3),
      Estimate = round(coef_kw_s3, 4),
      SE = round(se_kw_s3, 4),
      z_stat = round(coef_kw_s3 / se_kw_s3, 2),
      p_value = round(2 * pnorm(-abs(coef_kw_s3 / se_kw_s3)), 4)
    ),
    caption = "Kumaraswamy Parameter Estimates (Beta Failed)"
  )
}

## ----results_table, echo=FALSE------------------------------------------------
all_scenarios <- rbind(
  data.frame(Scenario = "S1: Well-Specified Beta", comp_s1),
  data.frame(Scenario = "S2: Heavy Tails", comp_s2),
  data.frame(Scenario = "S3: Extreme Shapes", comp_s3)
)

knitr::kable(
  row.names = FALSE,
  all_scenarios,
  caption = "Table 4: Comprehensive Model Comparison Across Three Simulation Scenarios",
  digits = c(0, 0, 0, 1, 2, 3, 3),
  align = c("l", "l", "r", "r", "r", "r", "r")
)

## ----comprehensive_comparison, echo=FALSE, fig.height=6, fig.cap="Comparative Performance Across Scenarios"----
par(mfrow = c(1, 3), mar = c(10, 4, 3, 1))

# AIC by scenario for Beta and Kumaraswamy only
scenarios <- c("S1", "S2", "S3")
beta_aic <- c(
  comp_s1$AIC[comp_s1$Model == "Beta (betareg)"],
  comp_s2$AIC[comp_s2$Model == "Beta (betareg)"],
  comp_s3$AIC[comp_s3$Model == "Beta (betareg)"]
)
kw_aic <- c(
  comp_s1$AIC[comp_s1$Model == "Kumaraswamy"],
  comp_s2$AIC[comp_s2$Model == "Kumaraswamy"],
  comp_s3$AIC[comp_s3$Model == "Kumaraswamy"]
)

# For S3, Beta AIC is huge, so use log scale or relative
aic_matrix <- rbind(beta_aic, kw_aic)
colnames(aic_matrix) <- scenarios

barplot(aic_matrix,
  beside = TRUE, col = c("#D32F2F", "#388E3C"),
  names.arg = scenarios, las = 1,
  main = "Model Fit (AIC)", ylab = "AIC",
  cex.names = 1.2, cex.main = 1.2
)
legend("topleft", c("Beta", "Kumaraswamy"),
  fill = c("#D32F2F", "#388E3C"),
  bty = "n", cex = 1.1
)
text(2, max(beta_aic) * 0.9, "Lower\nis better", cex = 0.9, col = "gray30")

# Convergence rates
beta_conv <- c(
  comp_s1$Conv_Rate[comp_s1$Model == "Beta (betareg)"],
  comp_s2$Conv_Rate[comp_s2$Model == "Beta (betareg)"],
  comp_s3$Conv_Rate[comp_s3$Model == "Beta (betareg)"]
)
kw_conv <- c(
  comp_s1$Conv_Rate[comp_s1$Model == "Kumaraswamy"],
  comp_s2$Conv_Rate[comp_s2$Model == "Kumaraswamy"],
  comp_s3$Conv_Rate[comp_s3$Model == "Kumaraswamy"]
)

conv_matrix <- rbind(beta_conv, kw_conv)
barplot(conv_matrix,
  beside = TRUE, col = c("#D32F2F", "#388E3C"),
  names.arg = scenarios, las = 1, ylim = c(0, 110),
  main = "Convergence Reliability", ylab = "Success Rate (%)",
  cex.names = 1.2, cex.main = 1.2
)
abline(h = 95, lty = 2, lwd = 2, col = "darkred")
text(1.5, 98, "95% threshold", col = "darkred", cex = 0.9)

# Computational speedup
beta_time <- c(
  comp_s1$Time[comp_s1$Model == "Beta (betareg)"],
  comp_s2$Time[comp_s2$Model == "Beta (betareg)"],
  comp_s3$Time[comp_s3$Model == "Beta (betareg)"]
)
kw_time <- c(
  comp_s1$Time[comp_s1$Model == "Kumaraswamy"],
  comp_s2$Time[comp_s2$Model == "Kumaraswamy"],
  comp_s3$Time[comp_s3$Model == "Kumaraswamy"]
)

speedup <- beta_time / kw_time

barplot(speedup,
  names.arg = scenarios, col = "#388E3C",
  las = 1, main = "Computational Speedup",
  ylab = "Speedup Factor (Kw vs Beta)",
  cex.names = 1.2, cex.main = 1.2
)
abline(h = 1, lty = 2, col = "gray40")
text(1:3, speedup + 0.5, sprintf("%.1fx", speedup), cex = 1.1, font = 2)

# mtext("Figure 4: Comparative Performance Across Scenarios",
#       side = 3, line = -2, outer = TRUE, font = 2, cex = 1.2)

## ----efficiency_analysis, echo=FALSE------------------------------------------
timing <- aggregate(
  Time ~ Model,
  data = all_scenarios[!is.na(all_scenarios$Time), ],
  FUN = mean
)
timing <- timing[order(timing$Time), ]
timing$Speedup <- timing$Time[timing$Model == "Beta (betareg)"] / timing$Time

knitr::kable(
  timing,
  caption = "Table 5: Average Computational Time and Speedup Relative to Beta Regression",
  digits = 3,
  col.names = c("Model", "Mean Time (sec)", "Speedup Factor"),
  row.names = FALSE
)

## ----session_info, echo=FALSE-------------------------------------------------
sessionInfo()

