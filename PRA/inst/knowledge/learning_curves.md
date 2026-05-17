# Learning Curves for Project Risk Analysis

## Overview

Learning curves model how performance improves with experience. In project risk analysis, they are used to forecast production costs, construction durations, and other metrics that improve as workers and processes gain experience.

## Sigmoidal Models

PRA implements three sigmoidal (S-curve) models that capture the typical pattern of slow initial progress, rapid acceleration, and eventual plateau:

### Pearl Model
y = K / (1 + exp(-r * (x - x0)))

- K: Upper asymptote (maximum capacity/completion)
- r: Growth rate
- x0: Midpoint (inflection point)
- Best for: Symmetric S-curves where growth and saturation are balanced

### Gompertz Model
y = K * exp(-exp(-r * (x - x0)))

- Same parameters as Pearl
- Best for: Asymmetric curves with slow initial growth and rapid mid-phase acceleration
- Common in biological and production learning contexts

### Logistic Model
y = K / (1 + ((K - y0) / y0) * exp(-r * x))

- K: Upper asymptote
- r: Growth rate
- y0: Initial value
- Best for: Cases where the initial value is known and growth is logistic

## Fitting and Prediction

1. **Fit**: Provide a data.frame with x (time/unit) and y (cost/completion) columns, plus the model type. Uses nonlinear least squares (Levenberg-Marquardt algorithm).
2. **Predict**: Use the fitted model to forecast y values for new x values. Optionally compute confidence intervals at a specified confidence level (e.g., 0.95 for 95% CI).
3. **Plot**: Visualize the fitted curve against the original data, with optional confidence bands.

## Choosing a Model

- If the data shows symmetric growth: try Pearl or Logistic
- If early growth is slow relative to mid-phase acceleration: try Gompertz
- Fit all three and compare residuals to select the best fit
- Use confidence intervals to assess prediction uncertainty

## References

- ICEAA Learning Curve Analysis Guide.
- DAU Learning Curve Workshop materials.
