## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
set.seed(42)

## ----setup--------------------------------------------------------------------
library(PRA)

## -----------------------------------------------------------------------------
num_simulations <- 10000
task_distributions <- list(
  list(type = "normal", mean = 10, sd = 2), # Task A
  list(type = "triangular", a = 5, b = 10, c = 15), # Task B
  list(type = "uniform", min = 8, max = 12) # Task C
)

## -----------------------------------------------------------------------------
correlation_matrix <- matrix(c(
  1.0, 0.5, 0.3,
  0.5, 1.0, 0.4,
  0.3, 0.4, 1.0
), nrow = 3, byrow = TRUE)

## -----------------------------------------------------------------------------
results <- mcs(num_simulations, task_distributions, correlation_matrix)

## ----results='asis'-----------------------------------------------------------
cat("Mean Total Duration:     ", round(results$total_mean, 2), "weeks\n")
cat("Variance of Duration:    ", round(results$total_variance, 2), "\n")
cat("Std Dev of Duration:     ", round(results$total_sd, 2), "weeks\n")

## -----------------------------------------------------------------------------
hist_data <- results$total_distribution

hist(hist_data,
  breaks = 50, freq = FALSE,
  main = "Monte Carlo Simulation - Total Project Duration",
  xlab = "Total Duration (weeks)", col = "steelblue", border = "white"
)
lines(density(hist_data), col = "tomato", lwd = 2)
abline(v = results$total_mean, col = "black", lty = 2, lwd = 1.5)
legend("topright",
  legend = c("Density", paste0("Mean = ", round(results$total_mean, 1), " wks")),
  col = c("tomato", "black"), lty = c(1, 2), lwd = 2, bty = "n"
)

## -----------------------------------------------------------------------------
knitr::kable(
  data.frame(
    Percentile = c("P5", "P50 (Median)", "P95"),
    Duration = round(results$percentiles, 1),
    Meaning = c(
      "5% chance of finishing this fast or faster",
      "Equal chance of finishing above or below this",
      "95% chance of finishing by this date"
    )
  ),
  caption = "Simulation Percentiles"
)

## ----results='asis'-----------------------------------------------------------
contingency_val <- contingency(results, phigh = 0.95, pbase = 0.50)
cat("Schedule contingency (P95 − P50):", round(contingency_val, 2), "weeks\n")
cat(
  "There is a 95% chance the project will finish within",
  round(results$percentiles["95%"], 1), "weeks.\n"
)

## -----------------------------------------------------------------------------
sensitivity_results <- sensitivity(task_distributions, correlation_matrix)

sens_data <- data.frame(
  Task        = c("Task A (Normal)", "Task B (Triangular)", "Task C (Uniform)"),
  Sensitivity = sensitivity_results
)

p <- ggplot2::ggplot(
  sens_data,
  ggplot2::aes(x = Sensitivity, y = reorder(Task, Sensitivity))
) +
  ggplot2::geom_col(fill = "steelblue") +
  ggplot2::geom_text(ggplot2::aes(label = round(Sensitivity, 3)),
    hjust = -0.1, size = 3.5
  ) +
  ggplot2::labs(
    title = "Tornado Chart - Task Sensitivity",
    x     = "Sensitivity Coefficient",
    y     = NULL
  ) +
  ggplot2::xlim(0, max(sensitivity_results) * 1.2) +
  ggplot2::theme_minimal()

print(p)

