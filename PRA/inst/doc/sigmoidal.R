## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(PRA)

## -----------------------------------------------------------------------------
data <- data.frame(
  time       = 1:9,
  completion = c(5, 15, 40, 60, 70, 75, 80, 85, 90)
)

## -----------------------------------------------------------------------------
fit <- fit_sigmoidal(data, "time", "completion", "logistic")

## -----------------------------------------------------------------------------
summary(fit)

## -----------------------------------------------------------------------------
plot_sigmoidal(
  fit, data, "time", "completion", "logistic",
  conf_level = 0.95,
  main = "Logistic Learning Curve - Completion Forecast",
  xlab = "Week",
  ylab = "Completion (%)"
)

## -----------------------------------------------------------------------------
future_times <- seq(1, 12, length.out = 100)
predictions <- predict_sigmoidal(fit, future_times, "logistic", conf_level = 0.95)

knitr::kable(
  tail(round(predictions, 1), 5),
  caption = "Predicted completion (final 5 points)",
  row.names = FALSE
)

## ----warning=FALSE------------------------------------------------------------
fit_logistic <- fit_sigmoidal(data, "time", "completion", "logistic")
fit_pearl <- fit_sigmoidal(data, "time", "completion", "pearl")
fit_gompertz <- fit_sigmoidal(data, "time", "completion", "gompertz")

## -----------------------------------------------------------------------------
# Residual standard errors for comparison
rse <- function(fit) summary(fit)$sigma

comparison <- data.frame(
  Model = c("Logistic", "Pearl", "Gompertz"),
  Residual_StdError = round(c(rse(fit_logistic), rse(fit_pearl), rse(fit_gompertz)), 3)
)
knitr::kable(comparison, caption = "Model Fit Comparison (lower RSE = better fit)")

## -----------------------------------------------------------------------------
x_seq <- seq(1, 12, length.out = 200)

pred_log <- predict_sigmoidal(fit_logistic, x_seq, "logistic")
pred_prl <- predict_sigmoidal(fit_pearl, x_seq, "pearl")
pred_gom <- predict_sigmoidal(fit_gompertz, x_seq, "gompertz")

# Base plot with observed data
plot(data$time, data$completion,
  pch = 16, xlim = c(1, 12), ylim = c(0, 105),
  main = "Learning Curve: Model Comparison",
  xlab = "Week", ylab = "Completion (%)"
)
lines(pred_log$x, pred_log$pred, col = "steelblue", lwd = 2)
lines(pred_prl$x, pred_prl$pred, col = "tomato", lwd = 2, lty = 2)
lines(pred_gom$x, pred_gom$pred, col = "darkgreen", lwd = 2, lty = 3)
legend("bottomright",
  legend = c("Logistic", "Pearl", "Gompertz"),
  col = c("steelblue", "tomato", "darkgreen"),
  lty = c(1, 2, 3), lwd = 2, bty = "n"
)

