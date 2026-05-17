## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
set.seed(42)

## ----setup--------------------------------------------------------------------
library(PRA)

## -----------------------------------------------------------------------------
task_means <- c(10, 15, 20) # Expected duration for each task (weeks)
task_vars <- c(4, 9, 16) # Variance of each task duration
cor_mat <- matrix(c(
  1.0, 0.5, 0.3,
  0.5, 1.0, 0.4,
  0.3, 0.4, 1.0
), nrow = 3, byrow = TRUE)

## ----results='asis'-----------------------------------------------------------
result <- smm(task_means, task_vars, cor_mat)
cat("Total Mean Duration:    ", round(result$total_mean, 2), "weeks\n")
cat("Total Variance:         ", round(result$total_var, 2), "\n")
cat("Total Std Deviation:    ", round(result$total_std, 2), "weeks\n")

## ----results='asis'-----------------------------------------------------------
total_mean <- result$total_mean
total_sd <- result$total_std
ci_lower <- total_mean - 1.96 * total_sd
ci_upper <- total_mean + 1.96 * total_sd
cat("95% CI: [", round(ci_lower, 1), ",", round(ci_upper, 1), "] weeks\n")

## -----------------------------------------------------------------------------
x_range <- seq(total_mean - 4 * total_sd, total_mean + 4 * total_sd, length.out = 300)
y_range <- dnorm(x_range, mean = total_mean, sd = total_sd)

plot(x_range, y_range,
  type = "l", lwd = 2, col = "steelblue",
  main = "SMM - Implied Project Duration Distribution",
  xlab = "Total Duration (weeks)", ylab = "Density"
)

# Shade 95% CI region
x_ci <- x_range[x_range >= ci_lower & x_range <= ci_upper]
y_ci <- dnorm(x_ci, mean = total_mean, sd = total_sd)
polygon(c(ci_lower, x_ci, ci_upper), c(0, y_ci, 0),
  col = "lightblue", border = NA
)

abline(v = total_mean, col = "black", lty = 2, lwd = 1.5)
legend("topright",
  legend = c("Normal density", "95% CI", "Mean"),
  col = c("steelblue", "lightblue", "black"),
  lty = c(1, NA, 2), lwd = c(2, NA, 1.5),
  pch = c(NA, 15, NA), pt.cex = 1.5,
  bty = "n"
)

## -----------------------------------------------------------------------------
# Represent each task as a normal distribution for MCS comparison (independent case)
task_dists_for_mcs <- list(
  list(type = "normal", mean = task_means[1], sd = sqrt(task_vars[1])),
  list(type = "normal", mean = task_means[2], sd = sqrt(task_vars[2])),
  list(type = "normal", mean = task_means[3], sd = sqrt(task_vars[3]))
)

# Run MCS without correlation (identity = fully independent)
mcs_result <- mcs(10000, task_dists_for_mcs)

## -----------------------------------------------------------------------------
# SMM variance without correlation = sum of individual variances
smm_var_nocor <- sum(task_vars)

comparison <- data.frame(
  Method          = c("SMM (independent)", "Monte Carlo (10,000 runs)"),
  Total_Mean      = round(c(result$total_mean, mcs_result$total_mean), 2),
  Total_Variance  = round(c(smm_var_nocor, mcs_result$total_variance), 2),
  Total_StdDev    = round(c(sqrt(smm_var_nocor), mcs_result$total_sd), 2)
)
knitr::kable(comparison, caption = "SMM vs. Monte Carlo Comparison (independent tasks)")

