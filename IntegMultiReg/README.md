# IntegMultiReg

**Integrative Bayesian Multiple Regression for Multi-Platform Biomarkers.**

`IntegMultiReg` implements the integrative multi-regression (IMR) model of
Chekouo, Stingo, Doecke and Do (2017, *Biometrics*) and extends it from
time-to-event outcomes to continuous (Gaussian) and binary (probit) outcomes.

Given several molecular platforms measured on overlapping but partially missing
sets of subjects, IMR partitions subjects into the availability subgroups of a
Venn diagram, fits one regression per subgroup, and shares information across
availability subgroups through

* **non-local (product moment) priors** on the regression coefficients, and
* a **Markov random field (MRF) prior** on the variable-selection indicators,

so that no subject with partially observed platforms is discarded and the same
biomarkers tend to be selected across availability subgroups.

## Installation

The package contains C code that links against the
[GNU Scientific Library (GSL)](https://www.gnu.org/software/gsl/), which must be
installed first:

* macOS: `brew install gsl`
* Debian/Ubuntu: `sudo apt-get install libgsl-dev`
* Windows: GSL is provided by Rtools.

Once available on CRAN, install the package with:

```r
install.packages("IntegMultiReg")
```

Alternatively, install a local source tarball:

```r
install.packages("IntegMultiReg_0.1.1.tar.gz", repos = NULL, type = "source")
```

The CRAN checking tools `checkbashisms` and `qpdf` are not runtime
dependencies. Package users do not need them. Maintainers running
`R CMD check --as-cran` locally can install them with
`brew install checkbashisms qpdf` on macOS or
`sudo apt-get install devscripts qpdf` on Debian/Ubuntu.

## Quick start

```r
library(IntegMultiReg)
data("simIMR")

fit <- imr(
  platform_data_list = simIMR$platforms,
  outcome            = simIMR$outcome.binary,
  cov                = simIMR$covariates,
  type_outcome       = "binary",
  nu                 = c(-4, -3, -4),
  sample_mcmc        = c(2000, 1000),
  ssize              = 30,
  seed               = 1
)

fit                       # short summary
summary(fit)              # selected biomarkers per platform
coef(fit)                 # per-platform mPIP matrices
plot(fit, type = "selection")
plot_top_features(fit)    # ranked biomarker bar chart
predict(fit, newdata = simIMR$platforms[1:2], covariates = simIMR$covariates)
cv_imr(fit)               # fold-split predictive assessment using fitted samples
```

## Real-data example

`kircIMR` is a reduced public UCSC Xena TCGA-KIRC survival example aligned with
the Biometrics kidney cancer case study: mRNA expression, miRNA expression, DNA
methylation, clinical covariates and right-censored survival.  It is derived
from public UCSC Xena TCGA-KIRC sampleMap files, not from controlled-access
TCGA/GDC files, and contains only a reduced Cox-screened feature panel.

The package replaces TCGA barcodes with package-internal IDs such as `KIRC001`
and does not distribute a barcode mapping.  Users should not attempt
participant re-identification or linkage to external resources.

```r
data("kircIMR")
sapply(kircIMR$platforms, dim)
kircIMR$model_subgroup_sizes

kirc_fit <- imr(
  kircIMR$platforms,
  kircIMR$outcome.survival,
  cov = kircIMR$covariates,
  type_outcome = "right.censored",
  nu = c(-4, -3, -4),
  sample_mcmc = c(4000, 1000),
  ssize = 30,
  seed = 1
)
```

See the package vignette `vignette("IntegMultiReg")` for a complete walk-through.

## Reference

Chekouo T, Stingo FC, Doecke JD, Do K-A (2017). "A Bayesian Integrative
Approach for Multi-Platform Genomic Data: A Kidney Cancer Case Study."
*Biometrics*, **73**(2), 615–624. <https://doi.org/10.1111/biom.12587>

When using `kircIMR`, please also acknowledge TCGA, the National Cancer
Institute Genomic Data Commons, and UCSC Xena as the public data sources.
