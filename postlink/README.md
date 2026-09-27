
# **postlink** <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/postlink)](https://CRAN.R-project.org/package=postlink)
[![R-CMD-check](https://github.com/postlink-group/postlink/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/postlink-group/postlink/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/postlink-group/postlink/graph/badge.svg)](https://app.codecov.io/gh/postlink-group/postlink)
<!-- badges: end -->

The `postlink` package is dedicated to providing a unified suite of
statistical tools designed to rigorously account for record linkage
errors in post-linkage data analysis.

Record linkage is often error-prone, particularly when identifiers used
for matching records are noisy or non-unique. Mismatches (false matches)
act as a contaminant in the linked data, typically leading to attenuated
estimates in downstream analysis. The `postlink` R package currently
supports three statistical frameworks to account for potential mismatch
errors during downstream regression modeling:

- **Weighting**: Solves adjusted estimating equations using
  block-specific proportions of correct links. It assumes an
  Exchangeable Linkage Error (ELE) model (Chambers, 2009; Chambers et
  al., 2023; Vo et al., 2024).
- **Mixture Modeling**: Uses an Expectation-Maximization (EM) algorithm
  and infers the latent correct match status using a two-component
  mixture model (Slawski et al., 2025).
- **Bayesian Mixture Modeling**: Performs posterior inference via data
  augmentation, alternating between imputing the latent match status and
  updating model parameters to propagate linkage uncertainty (Gutman et
  al., 2016).

The `postlink` package currently focuses on methods for secondary
analysis, where the individual files that were linked are not
accessible. For the primary analysis setting, when individual files are
accessible, methods that perform record linkage and analysis jointly
with direct propagation of uncertainty would be more suitable.

The long-term goal of `postlink` is to extend support for a wide array
of linkage and post-linkage analysis scenarios.

## **Package Design**

The package is built on a modular, object-oriented S3 architecture that
decouples the specification of linkage error from the substantive
statistical modeling. This provides a familiar, standard formula based
modeling interface.

**Phase 1**: Adjustment Specification

First, we define the linked data and the chosen adjustment methodology
using a constructor function. These constructors validate the data and
return a lightweight S3 adjustment object.

- `adjELE()`: Specifies the Exchangeable Linkage Error (ELE) model,
  using known or audited mismatch rates.
- `adjMixture()`: Specifies a frequentist mixture model approach that
  treats match status as a latent variable, estimating linkage error
  rates directly from data (e.g., using an overall mismatch rate, safe
  matches, predictors of match status, or predicted correct match
  probabilities). If no record linkage information is available, a
  constant mismatch rate is assumed.
- `adjMixBayes()`: Specifies a Bayesian mixture model approach, with
  default or informative priors, enabling parameter estimation and
  multiple imputation of latent match statuses using Stan.

**Phase 2**: Estimation & Inference

The adjustment object is subsequently passed to a standard modeling
wrapper, integrating the linkage error correction into the familiar R
modeling syntax:

- `plglm()` for generalized linear models (linear, logistic, Poisson,
  Gamma).
- `plcoxph()` for Cox proportional hazards regression.
- `plsurvreg()` for parametric survival models.
- `plctable()` for contingency table analysis.

Estimation and inference supported for each type of adjustment object
vary. Please refer to the `adj*` or `pl*` documentation for models
currently supported.

Standard R workflows (e.g., `summary()`, `predict()`, `vcov()`, and
`confint()`) can be used for display and processing of results. These
methods specially are derived to consider the additional steps
introduced by the linkage error adjustment.

**Note**: While the two-phase workflow is recommended for standard
analyses, the package’s architecture isolates the core logic of each
method into independent internal routines. If preferred, the underlying
computational functions can be used directly by supplying pre-computed
design matrices and response vectors (e.g., `coxphELE()`,
`glmMixture()`, `survregMixBayes()`)

## **Installation**

The package can be installed from
[CRAN](https://CRAN.R-project.org/package=postlink):

``` r
install.packages("postlink")
```

The active development version can be installed from
[GitHub](https://github.com/postlink-group/postlink):

``` r
# Using remotes:
# install.packages("remotes")
remotes::install_github("postlink-group/postlink")

# Or, using pak:
# install.packages("pak")
pak::pkg_install("postlink-group/postlink")
```

## **System Requirements**

Because **postlink** package relies on compiled C++ code and Stan for
Bayesian mixture modeling, depending on your operating system and how
you install the package, additional system tools may be needed to
compile this code.

When installing the version from CRAN:

- Windows and macOS users: In most cases, you do not need any extra
  tools. `install.packages("postlink")` will download a pre-compiled
  binary directly from CRAN.

  *(Note: If a new version was just released on CRAN, binaries may take
  a few days to build. If R prompts you to compile from source during
  this window, you can either say “no” to install the older binary, or
  say “yes” and ensure you have the tools listed below).*

- Linux users: CRAN does not provide binaries for Linux. You will always
  compile from source and must install the system requirements listed
  below.

When installing the development version from GitHub, all users will be
compiling from source and must have a working C++ development
environment and `GNU make` installed.

**Required Tools for Compiling from Source:**

If you are compiling from source (GitHub, Linux, or requesting source
from CRAN), please ensure your system is set up with a C++11 compatible
compiler:

- Windows: Install the version of
  [Rtools](https://cran.r-project.org/bin/windows/Rtools/) that matches
  your current R version.
- macOS: Install the Xcode Command Line Tools by opening your terminal
  and running: `xcode-select --install`.
- Linux: Ensure you have the standard C++ compiler and R development
  tools installed (e.g., `sudo apt-get install r-base-dev` on
  Ubuntu/Debian).

## **Quick Start**

Below is a brief example illustrating the typical workflow using
`postlink`.

We analyze the relationship between age at death and year of birth
linked using historical records from the LIFE-M project. The linked
dataset contains a mix of hand-linked records (assumed correct) and
purely machine-linked records subject to an approximate 5% mismatch
rate. Instead of fitting the standard glm model ignoring the mismatch
errors, we use the `postlink` to adjust for potential mismatches using
the entire linked dataset.

``` r
library(postlink)

# Load the LIFE-M demo dataset
data(lifem)

# Phase 1: Adjustment Specification
# We model the correct match indicator via logistic regression using 
# name commonness scores (commf, comml) and a 5% expected mismatch rate.
adj_object <- adjMixture(
  linked.data = lifem,
  m.formula = ~ commf + comml,
  m.rate = 0.05,
  safe.matches = hndlnk
)

# Check specified adjustment
print(adj_object)

# Phase 2: Estimation & Inference
# Fit a Gaussian regression model utilizing a cubic polynomial for year of birth.
fit <- plglm(
  age_at_death ~ poly(unit_yob, 3, raw = TRUE),
  family = "gaussian",
  adjustment = adj_object
)

# View model results
summary(fit)
confint(fit)
```

## **References**

Chambers, R. (2009). Regression analysis of probability-linked data. ,
4, 1-15.

Chambers, R. L., Fabrizi, E., Ranalli, M. G., Salvati, N., & Wang, S.
(2023). Robust regression using probabilistically linked data. , 15(2),
e1596.

Gutman, R., Sammartino, C., Green, T., & Montague, B. (2016). Error
adjustments for file linking methods using encrypted unique client
identifier (eUCI) with application to recently released prisoners who
are HIV+. , 35(1), 115–129.

Slawski, M., West, B. T., Bukke, P., Wang, Z., Diao, G., & Ben-David, E.
(2025). A general framework for regression with mismatched data based on
mixture modelling. , 188(3), 896-919.

Vo, T. H., Garès, V., Zhang, L. C., Happe, A., Oger, E., Paquelet, S., &
Chauvet, G. (2024). Cox regression with linked data. , 43(2), 296-314.
