<!-- README.md is generated from README.Rmd. Please edit that file -->
The SSM package provides functions to fit, plot and predict using smooth supersaturated models. It defines an S4 class called "SSM", and methods for plotting and predicting them. The fitting function is highly customizable and provides optional sensitivity analysis and the provision to estimate metamodel error using a Gaussian process.

------------------------------------------------------------------------

The following code will fit a smooth supersaturated model to a 20 point design in four factors. Note the design should be held in a matrix, not a data.frame, and all entries must be numeric. The options `SA`, `GP` and `validation` turn on automated sensitivity analysis, Gaussian process metamodel error estimation and Leave-One-Out cross-validation respectively. The `plot` method plots the main effects of the model while the `predict` method gives the model prediction at a point and also a 95% credible interval if a metamodel error GP has been fit.

``` r
X <- matrix(runif(80, -1, 1), ncol = 4)
Y <- apply(apply(X, 1, "^", 1:4), 2, sum)
s <- fit.ssm(X, Y, SA = TRUE, GP = TRUE, validation = TRUE)
s
plot(s, yrange="yrange")
predict(s, rep(0.5, 4))
sensitivity.plot(s)
```

------------------------------------------------------------------------

To install the most up-to-date SSM package through GitHub use `devtools::install_github("peterrobertcurtis/SSM")`.

------------------------------------------------------------------------

More details on how to use the SSM can be found in the vignette and help pages.
