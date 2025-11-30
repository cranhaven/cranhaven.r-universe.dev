
# gkwreg: Generalized Kumaraswamy Regression Models for Bounded Data <img src="man/figures/gkwreg.png" align="right" height="138" />

[![CRAN
status](https://www.r-pkg.org/badges/version/gkwreg)](https://CRAN.R-project.org/package=gkwreg)
[![R-CMD-check](https://github.com/evandeilton/gkwreg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/evandeilton/gkwreg/actions/workflows/R-CMD-check.yaml)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/gkwreg)](https://cran.r-project.org/package=gkwreg)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

The **gkwreg** package provides a comprehensive and computationally
efficient framework for regression modeling of data restricted to the
standard unit interval $(0, 1)$, including proportions, rates,
fractions, percentages, and bounded indices.

While Beta regression is the traditional approach for such data,
**gkwreg** focuses on the **Generalized Kumaraswamy (GKw)** distribution
family. This offers exceptional flexibility by encompassing seven
important bounded distributions—including Beta and Kumaraswamy—as
special or limiting cases.

The package enables full **distributional regression**, where *all*
relevant parameters can be modeled as functions of covariates through
flexible link functions. Maximum Likelihood estimation is performed
efficiently via the **Template Model Builder (TMB)** framework,
leveraging **Automatic Differentiation (AD)** for superior computational
speed, numerical accuracy, and optimization stability.

## Table of Contents

- [Key Features](https://www.google.com/search?q=%23key-features)
- [Installation](https://www.google.com/search?q=%23installation)
- [Quick Start](https://www.google.com/search?q=%23quick-start)
- [Real Data
  Example](https://www.google.com/search?q=%23real-data-example)
- [Advanced
  Features](https://www.google.com/search?q=%23advanced-features)
- [Mathematical
  Background](https://www.google.com/search?q=%23mathematical-background)
- [Comparison with Other
  Packages](https://www.google.com/search?q=%23comparison-with-other-packages)
- [Citation](https://www.google.com/search?q=%23citation)

------------------------------------------------------------------------

## Key Features

### Flexible Distribution Hierarchy

Model bounded data using the **5-parameter Generalized Kumaraswamy
(GKw)** distribution and its **seven nested subfamilies**:

| Distribution | Code | Parameters Modeled | Fixed Parameters | \# Params |
|:---|:---|:---|:---|:---|
| Generalized Kumaraswamy | `gkw` | $\alpha, \beta, \gamma, \delta, \lambda$ | None | 5 |
| Beta-Kumaraswamy | `bkw` | $\alpha, \beta, \gamma, \delta$ | $\lambda = 1$ | 4 |
| Kumaraswamy-Kumaraswamy | `kkw` | $\alpha, \beta, \delta, \lambda$ | $\gamma = 1$ | 4 |
| Exponentiated Kumaraswamy | `ekw` | $\alpha, \beta, \lambda$ | $\gamma = 1, \delta = 0$ | 3 |
| McDonald (Beta Power) | `mc` | $\gamma, \delta, \lambda$ | $\alpha = 1, \beta = 1$ | 3 |
| Kumaraswamy | `kw` | $\alpha, \beta$ | $\gamma = 1, \delta = 0, \lambda = 1$ | 2 |
| Beta | `beta` | $\gamma, \delta$ | $\alpha = 1, \beta = 1, \lambda = 1$ | 2 |

Each family offers distinct flexibility-parsimony tradeoffs. Start
simple (`kw` or `beta`) and compare nested models using likelihood ratio
tests or information criteria.

### Advanced Regression Modeling

- **Extended formula syntax** for parameter-specific linear predictors:

  ``` r
  y ~ alpha_predictors | beta_predictors | gamma_predictors | delta_predictors | lambda_predictors
  ```

  *Example:* `yield ~ batch + temp | temp | 1 | temp | batch`

- **Multiple link functions** with optional scaling:

  - **Positive parameters** ($\alpha, \beta, \gamma, \lambda$): `log`
    (default), `sqrt`, `inverse`, `identity`
  - **Probability parameters** ($\delta \in (0,1)$): `logit` (default),
    `probit`, `cloglog`, `cauchy`
  - **Link scaling**: Control transformation intensity via `link_scale`
    (useful for numerical stability)

- **Flexible control** via `gkw_control()`:

  - Multiple optimizers: `nlminb` (default), `BFGS`, `Nelder-Mead`,
    `CG`, `SANN`, `L-BFGS-B`
  - Custom starting values, convergence tolerances, iteration limits
  - Fast fitting mode (disable Hessian computation for point estimates
    only)

### Computational Efficiency

- **TMB-powered estimation**: Compiled C++ templates with automatic
  differentiation.

  - Exact gradients and Hessians (machine precision).
  - **10-100× faster** than numerical differentiation.
  - Superior convergence stability.

- **Performance optimizations**:

  - Intelligent caching of intermediate calculations.
  - Vectorized operations via Eigen/Armadillo.
  - Memory-efficient for large datasets ($n > 100,000$).

### Comprehensive Inference Tools

**Standard R Methods** (familiar workflow):

- `summary()`, `print()`, `coef()`, `vcov()`, `confint()`
- `logLik()`, `AIC()`, `BIC()`, `nobs()`
- `fitted()`, `residuals()`, `predict()`
- `anova()` for nested model comparisons

**Advanced Prediction** (`predict.gkwreg`):

- Multiple prediction types: `"response"`, `"parameter"`, `"link"`,
  `"variance"`, `"density"`, `"probability"`, `"quantile"`.
- Predictions under alternative distributional assumptions.

### Sophisticated Diagnostics

**6 Diagnostic Plot Types** (`plot.gkwreg`):

1.  **Residuals vs Indices**: Detect autocorrelation.
2.  **Cook’s Distance**: Identify influential observations.
3.  **Leverage vs Fitted**: Flag high-leverage points.
4.  **Residuals vs Linear Predictor**: Check
    linearity/heteroscedasticity.
5.  **Half-Normal Plot with Envelope**: Assess distributional adequacy.
6.  **Predicted vs Observed**: Overall goodness-of-fit.

------------------------------------------------------------------------

## Installation

You can install the stable version from CRAN (once accepted) or the
development version from GitHub.

``` r
# Install from CRAN (stable release):
install.packages("gkwreg")

# Install companion distribution package:
install.packages("gkwdist")

# Or install development versions from GitHub:
# install.packages("remotes")
remotes::install_github("evandeilton/gkwdist")
remotes::install_github("evandeilton/gkwreg")
```

------------------------------------------------------------------------

## Quick Start

### Basic Regression

``` r
library(gkwreg)
library(gkwdist)

# Simulate data
set.seed(123)
n <- 500
x1 <- runif(n, -2, 2)
x2 <- rnorm(n)

# True parameters (log link)
alpha_true <- exp(0.8 + 0.3 * x1)
beta_true <- exp(1.2 - 0.2 * x2)

# Generate response from Kumaraswamy distribution
y <- rkw(n, alpha = alpha_true, beta = beta_true)
y <- pmax(pmin(y, 1 - 1e-7), 1e-7) # Ensure strict bounds
df <- data.frame(y = y, x1 = x1, x2 = x2)

# Fit Kumaraswamy regression
# Formula: alpha ~ x1, beta ~ x2 (intercept-only models also supported)
fit_kw <- gkwreg(y ~ x1 | x2, data = df, family = "kw")

# View results
summary(fit_kw)
```

### Advanced Prediction

``` r
# Create prediction grid
newdata <- data.frame(
  x1 = seq(-2, 2, length.out = 100),
  x2 = 0
)

# Predict different quantities
pred_mean <- predict(fit_kw, newdata, type = "response") # E(Y|X)
pred_var <- predict(fit_kw, newdata, type = "variance") # Var(Y|X)
pred_alpha <- predict(fit_kw, newdata, type = "alpha") # α parameter
pred_params <- predict(fit_kw, newdata, type = "parameter") # All parameters

# Evaluate density at y = 0.5 for each observation
dens_values <- predict(fit_kw, newdata, type = "density", at = 0.5)

# Compute quantiles (10th, 50th, 90th percentiles)
quantiles <- predict(fit_kw, newdata,
  type = "quantile",
  at = c(0.1, 0.5, 0.9), elementwise = FALSE
)
```

### Model Comparison

``` r
# Fit nested models
fit0 <- gkwreg(y ~ 1, data = df, family = "kw") # Null model
fit1 <- gkwreg(y ~ x1, data = df, family = "kw") # + x1
fit2 <- gkwreg(y ~ x1 | x2, data = df, family = "kw") # + x2 on beta

# Information criteria comparison
AIC(fit0, fit1, fit2)

# Likelihood ratio tests
anova(fit0, fit1, fit2, test = "Chisq")
```

### Diagnostic Plots

``` r
# All diagnostic plots (base R graphics)
par(mfrow = c(3, 2))
plot(fit_kw, ask = FALSE)

# Select specific plots with customization
plot(fit_kw,
  which = c(2, 5, 6), # Cook's distance, Half-normal, Pred vs Obs
  type = "quantile", # Quantile residuals (recommended)
  caption = list(
    "2" = "Influential Points",
    "5" = "Distributional Check"
  ),
  nsim = 200, # More accurate envelope
  level = 0.95
) # 95% confidence

# Modern ggplot2 version with grid arrangement
plot(fit_kw,
  use_ggplot = TRUE,
  arrange_plots = TRUE,
  theme_fn = ggplot2::theme_bw
)

# Extract diagnostic data for custom analysis
diag <- plot(fit_kw, save_diagnostics = TRUE)
head(diag$data) # Access Cook's distance, leverage, residuals, etc.
```

------------------------------------------------------------------------

## Real Data Example

``` r
# Food Expenditure Data (proportion spent on food)
data("FoodExpenditure")
food <- FoodExpenditure
food$prop <- food$food / food$income

# Fit different distributional families
fit_beta <- gkwreg(prop ~ income + persons, data = food, family = "beta")
fit_kw <- gkwreg(prop ~ income + persons, data = food, family = "kw")
fit_ekw <- gkwreg(prop ~ income + persons, data = food, family = "ekw")

# Compare families
comparison <- data.frame(
  Family = c("Beta", "Kumaraswamy", "Exp. Kumaraswamy"),
  LogLik = c(logLik(fit_beta), logLik(fit_kw), logLik(fit_ekw)),
  AIC = c(AIC(fit_beta), AIC(fit_kw), AIC(fit_ekw)),
  BIC = c(BIC(fit_beta), BIC(fit_kw), BIC(fit_ekw))
)
print(comparison)

# Visualize best fit
best_fit <- fit_kw
plot(food$income, food$prop,
  xlab = "Income", ylab = "Food Proportion",
  main = "Food Expenditure Pattern", pch = 16, col = "gray40"
)
income_seq <- seq(min(food$income), max(food$income), length = 100)
pred_df <- data.frame(income = income_seq, persons = median(food$persons))
lines(income_seq, predict(best_fit, pred_df), col = "red", lwd = 2)
```

------------------------------------------------------------------------

## Advanced Features

### Custom Optimization Control

``` r
library(gkwreg)
library(gkwdist)

# Simulate data
set.seed(123)
n <- 500
x <- runif(n, 1, 5)
x1 <- runif(n, -2, 2)
x2 <- rnorm(n)
x3 <- rnorm(n, 1, 4)

# True parameters (log link)
alpha_true <- exp(0.8 + 0.3 * x1)
beta_true <- exp(1.2 - 0.2 * x2)

# Generate response from Kumaraswamy distribution
y <- rkw(n, alpha = alpha_true, beta = beta_true)
y <- pmax(pmin(y, 1 - 1e-7), 1e-7) # Ensure strict bounds
df <- data.frame(y = y, x = x, x1 = x1, x2 = x2, x3 = x3)

# Default control (used automatically)
fit <- gkwreg(y ~ x1, data = df, family = "kw")

# Increase iterations for difficult problems
fit_robust <- gkwreg(y ~ x1,
  data = df, family = "kw",
  control = gkw_control(maxit = 1000, trace = 1)
)

# Try alternative optimizer
fit_bfgs <- gkwreg(y ~ x1,
  data = df, family = "kw",
  control = gkw_control(method = "BFGS")
)

# Fast fitting without standard errors (exploratory analysis)
fit_fast <- gkwreg(y ~ x1,
  data = df, family = "kw",
  control = gkw_control(hessian = FALSE)
)

# Custom starting values
fit_custom <- gkwreg(y ~ x1 + x2 | x3,
  data = df, family = "kw",
  control = gkw_control(
    start = list(
      alpha = c(0.5, 0.2, -0.1), # Intercept + 2 slopes
      beta  = c(1.0, 0.3) # Intercept + 1 slope
    )
  )
)
```

### Link Functions and Scaling

``` r
# Default: log link for all parameters
fit_default <- gkwreg(y ~ x | x, data = df, family = "kw")

# Custom link functions per parameter
fit_links <- gkwreg(y ~ x | x,
  data = df, family = "kw",
  link = list(alpha = "sqrt", beta = "log")
)

# Link scaling (control transformation intensity)
# Larger scale = gentler transformation, smaller = steeper
fit_scaled <- gkwreg(y ~ x | x,
  data = df, family = "kw",
  link_scale = list(alpha = 5, beta = 15)
)
```

### Working with Large Datasets

``` r
# Large dataset example
set.seed(456)
n_large <- 100000
x_large <- rnorm(n_large)
y_large <- rkw(n_large, alpha = exp(0.5 + 0.2 * x_large), beta = exp(1.0))
df_large <- data.frame(y = y_large, x = x_large)

# Fast fitting
fit_large <- gkwreg(y ~ x,
  data = df_large, family = "kw",
  control = gkw_control(hessian = FALSE)
)

# Diagnostic plots with sampling (much faster)
plot(fit_large,
  which = c(1, 2, 4, 6), # Skip computationally intensive plot 5
  sample_size = 5000
) # Use random sample of 5000 obs
```

------------------------------------------------------------------------

## Mathematical Background

### The Generalized Kumaraswamy Distribution

The GKw distribution is a five-parameter family for variables on
$(0, 1)$ with cumulative distribution function:

$$F(x; \alpha, \beta, \gamma, \delta, \lambda) = I_{[1-(1-x^{\alpha})^{\beta}]^{\lambda}}(\gamma, \delta)$$

where $I_z(a,b)$ is the regularized incomplete beta function. The
probability density function is:

<!-- $$f(x; \alpha, \beta, \gamma, \delta, \lambda) = \frac{\lambda \alpha \beta x^{\alpha-1}}{B(\gamma, \delta)} (1-x^{\alpha})^{\beta-1} \left[1-(1-x^{\alpha})^{\beta}\right]^{\gamma\lambda-1} \left\{1-\left[1-(1-x^{\alpha})^{\beta}\right]^{\lambda} \right \}^{\delta-1}$$ -->

<!-- $$f(x; \alpha, \beta, \gamma, \delta, \lambda) = \frac{\lambda \alpha \beta x^{\alpha-1}}{B(\gamma, \delta)} (1-x^{\alpha})^{\beta-1} \left[1-(1-x^{\alpha})^{\beta}\right]^{\gamma\lambda-1} \left\{1-\left[1-(1-x^{\alpha})^{\beta}\right]^{\lambda} \right\}^{\delta-1}$$ -->

$$f(x; \alpha, \beta, \gamma, \delta, \lambda) = \frac{\lambda \alpha \beta x^{\alpha-1}}{B(\gamma, \delta)} (1-x^{\alpha})^{\beta-1} \left[1-(1-x^{\alpha})^{\beta}\right]^{\gamma\lambda-1} \\{1-\left[1-(1-x^{\alpha})^{\beta}\right]^{\lambda}\\}^{\delta-1}$$

**Parameter Roles**:

- **$\alpha, \beta$**: Control basic shape (inherited from Kumaraswamy)
- **$\gamma, \delta$**: Govern tail behavior and concentration
- **$\lambda$**: Additional flexibility for skewness and peaks

### Regression Framework

For response $y_i \in (0,1)$ following a GKw family distribution, each
parameter
$\theta_{ip} \in \{\alpha_i, \beta_i, \gamma_i, \delta_i, \lambda_i\}$
depends on covariates via link functions:

$$g_p(\theta_{ip}) = \eta_{ip} = \mathbf{x}_{ip}^\top \boldsymbol{\beta}_p$$

Maximum likelihood estimation maximizes:

$$\ell(\Theta; \mathbf{y}, \mathbf{X}) = \sum_{i=1}^{n} \log f(y_i; \theta_i(\Theta))$$

TMB computes exact gradients $\nabla \ell$ and Hessian $\mathbf{H}$ via
automatic differentiation, enabling fast and stable optimization.

------------------------------------------------------------------------

## Computational Engine: TMB

**Template Model Builder (TMB)** translates statistical models into
optimized C++ code with automatic differentiation:

**Under the Hood**:

    R Formula -> TMB C++ Template -> Automatic Differentiation -> 
    Compiled Object -> Fast Optimization (nlminb/optim) -> 
    Standard Errors (Hessian inversion)

------------------------------------------------------------------------

## Comparison with Other Packages

| Feature | gkwreg | betareg | gamlss | brms |
|:---|:---|:---|:---|:---|
| **Distribution Family** | GKw hierarchy (7) | Beta | 100+ | 50+ |
| **Estimation** | MLE (TMB/AD) | MLE | GAMLSS | Bayesian MCMC |
| **Parameter Modeling** | All parameters | Mean, precision | All parameters | All parameters |
| **Speed (n=10k)** | **Fast (~1s)** | Fast (~1s) | Moderate (~5s) | Slow (~5min) |
| **Link Functions** | 9 options + scaling | Fixed | Many | Many |
| **Optimization** | `gkw_control()` (detailed) | Basic | Moderate | Extensive |
| **Diagnostic Plots** | 6 types, dual graphics | 4 types | Extensive | Via bayesplot |
| **Dependencies** | gkwdist, TMB, Formula | Minimal | Many | Stan, many |

**When to use gkwreg**:

- Need flexible bounded distributions beyond Beta.
- Large datasets requiring fast computation.
- All parameters depend on covariates.
- Frequentist inference preferred.

------------------------------------------------------------------------

## Documentation and Support

- **Reference Manual**: `help(package = "gkwreg")`
- **Vignettes**: `browseVignettes("gkwreg")`
- **Function Help**: `?gkwreg`, `?predict.gkwreg`, `?plot.gkwreg`,
  `?gkw_control`
- **GitHub Issues**: [Report bugs or request
  features](https://github.com/evandeilton/gkwreg/issues)

------------------------------------------------------------------------

## Contributing

Contributions are welcome! Please see our [Contributing
Guidelines](https://github.com/evandeilton/gkwreg/blob/main/CONTRIBUTING.md)
and [Code of
Conduct](https://github.com/evandeilton/gkwreg/blob/main/CODE_OF_CONDUCT.md).

### How to Contribute

- Report bugs or request features via [GitHub
  Issues](https://github.com/evandeilton/gkwreg/issues)
- Submit pull requests for bug fixes or new features
- Improve documentation or add examples
- Share your use cases and feedback

## Community Guidelines

This project follows a [Code of
Conduct](https://github.com/evandeilton/gkwreg/blob/main/CODE_OF_CONDUCT.md).
Please read it before contributing.

------------------------------------------------------------------------

## Citation

If you use **gkwreg** in your research, please cite:

``` r
citation("gkwreg")
```

Or use the BibTeX entry:

``` bibtex
@Manual{,
  title = {gkwreg: Generalized Kumaraswamy Regression Models for Bounded Data},
  author = {José Evandeilton Lopes},
  year = {2025},
  note = {R package version 2.1.4},
  url = {https://github.com/evandeilton/gkwreg},
}
```

## License

This package is licensed under the **MIT License**. See the
[LICENSE](https://github.com/evandeilton/gkwreg/blob/main/LICENSE) file
for details.

------------------------------------------------------------------------

## Author and Maintainer

**José Evandeilton Lopes (Lopes, J. E.)** \| <evandeilton@gmail.com> \|
[GitHub](https://github.com/evandeilton) \|
[ORCID](https://orcid.org/0009-0007-5887-4084)

LEG - Laboratório de Estatística e Geoinformação \| UFPR - Universidade
Federal do Paraná, Brazil

------------------------------------------------------------------------
