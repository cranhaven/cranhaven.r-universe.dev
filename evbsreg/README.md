# evbsreg <img src="man/figures/logo.png" align="right" height="139" alt="evbsreg website" />

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/Raydonal/evbsreg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Raydonal/evbsreg/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**Local influence diagnostics for the Extreme-Value Birnbaum–Saunders
(EVBS) regression model.**

This package implements the methodology of:

> Ospina, R., Lima, J. I. C., Barros, M., and Macêdo, A. M. S. (2026).
> *Local influence diagnostics for the extreme-value Birnbaum–Saunders
> regression model: methodology, validation, and application to anomalous
> wind gusts.* Submitted.

It provides joint maximum likelihood estimation, conformal normal curvature
(CNC) diagnostics under three perturbation schemes, randomized quantile
residuals with simulation envelope, Monte Carlo utilities, and
publication-quality density and diagnostic plots. Since 1.1.0 it also
provides the full EVBS distribution family (density, distribution, quantile),
the finite upper endpoint and return levels, moving-block bootstrap standard
errors, and the corresponding local influence diagnostics for the
generalized extreme-value (GEV) regression model, used throughout as a
benchmark. Since 1.2.0 it provides `evbs_monitor()`, a prospective control
chart that combines the curvature diagnostic with the finite upper endpoint
into an index of endpoint identifiability with a natural control limit. See
`NEWS.md` for the full changelog.

## Installation

From a local source tarball:

```r
install.packages("evbsreg_1.2.0.tar.gz", repos = NULL, type = "source")
```

Or from GitHub:

```r
# install.packages("remotes")
remotes::install_github("Raydonal/evbsreg")
```

Dependencies: `SpatialExtremes` (GEV random number generation) and
`ggplot2` (density plots). `gamlss` is needed only to fit the optional
`logEVBS()` family with the `gamlss` package itself.

## Quick start

```r
library(evbsreg)
data(itajai)

# 1. Fit the EVBS regression model
X   <- cbind(1, itajai$pressure)
fit <- evbsreg.fit(X, itajai$wind)
round(fit$coeff, 4)

# 2. Local influence diagnostics
diag <- cnc_diagnostics(fit)
plot_cnc(diag, q = 7)

# 3. Most influential observation
which(diag$Bj[7, ] > diag$bq[7])

# 4. Refit without it and measure the impact
fit82 <- evbsreg.fit(X[-82, ], itajai$wind[-82])
round(100 * (fit82$coeff - fit$coeff) / abs(fit$coeff), 2)
```

The tail-shape parameter changes by about **−73.67%** when the catastrophic
event of 26 April 2017 (observation 82) is removed, while the regression
structure remains stable.

## Main functions

| Function | Purpose |
|---|---|
| `evbsreg.fit()` | Joint maximum likelihood fit of the EVBS regression model |
| `cnc_diagnostics()` | Conformal normal curvature diagnostics |
| `plot_cnc()` | Two-panel diagnostic figure (eigenvalues + contributions) |
| `rqrandomized()`, `rcoxsnell()` | Quantile and Cox–Snell residuals |
| `envelope_qq()` | Normal probability plot with simulation envelope |
| `revbs()`, `devbs()`, `pevbs()`, `qevbs()` | EVBS density, distribution, quantile, and random generation |
| `evbs_endpoint()` | Finite upper endpoint of the fitted model (Weibull domain) |
| `evbs_return_level()` | Return levels and expected shortfall from the EVBS quantile function |
| `evbs_block_boot()` | Moving-block bootstrap standard errors for serially dependent series |
| `evbsreg.fit.mc()` | Monte Carlo simulation study |
| `plot_evbs_alpha()` … | Density plots (Figures 1–2 of the paper) |
| `gevreg.fit()`, `gev_scores()`, `cnc_diagnostics_gev()` | GEV regression fit and matching local influence diagnostics |
| `logEVBS()`, `dlogEVBS()`, `plogEVBS()`, `qlogEVBS()` | `gamlss.family` for a log-EVBS model with covariate-dependent shape |
| `evbs_monitor()`, `plot.evbs_monitor()` | Prospective endpoint-identifiability control chart |

See `vignette("evbsreg")` for the full worked example.

## Reproducing the paper

Five standalone scripts reproduce every figure, table, and simulation:

```r
source(system.file("scripts/script_01_density_figures.R",    package = "evbsreg"))
source(system.file("scripts/script_02_itajai_application.R", package = "evbsreg"))
source(system.file("scripts/script_03_simulation_scenario1.R", package = "evbsreg"))
source(system.file("scripts/script_04_simulation_scenario2.R", package = "evbsreg"))
source(system.file("scripts/script_05_simulation_scenario3.R", package = "evbsreg"))
```

Each simulation script defaults to `m = 5000` replicates (matching the
paper). Set `m <- 500` at the top of a script for a quick check.

## Documentation

Full documentation, including the reference index and the "Get started"
vignette, is available at the package website:
<https://raydonal.github.io/evbsreg/>.

## License

MIT © Raydonal Ospina