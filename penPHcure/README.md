
<!-- README.md is generated from README.Rmd. Please edit that file -->

# penPHcure

Contrary to standard survival analysis models, which rely on the
assumption that the entire population will eventually experience the
event of interest, mixture cure models allow to split the population in
susceptible and non-susceptible (cured) individuals.

In this R package, we implement the semi-parametric proportional-hazards
(PH) cure model of Sy and Taylor (2000) extended to time-varying
covariates. In particular, the **penPHcure** function allows to:

  - estimate the regression coefficients and the baseline hazard
    function (conditional on being susceptible);

  - compute confidence intervals for the estimated regression
    coefficients using the basic/percentile bootstrap method;

  - perform variable selection based on the SCAD-penalized likelihood,
    as in Beretta and Heuchenne (2019).

Moreover, the **penPHcure.simulate** function allows to simulate data
from a PH cure model, where the event-times are generated on a
continuous scale from a piecewise exponential distribution conditional
on time-varying covariates, using a method similar to the one described
in Hendry (2014).

## Installation

### CRAN

To install the latest release from CRAN:

``` r
install.packages(penPHcure)
```

### GitHub

To install the latest devel version from GitHub:

``` r
library(devtools)
install_github("a-beretta/penPHcure")
```

## Contact

  - If you discover a bug or you have a suggestion to improve the
    package: <https://github.com/a-beretta/penPHcure/issues>

  - For help or more information contact: <a.beretta@uliege.be>

## References

<div id="refs" class="references">

<div id="ref-Beretta_Heuchenne_2019">

Beretta, Alessandro, and Cédric Heuchenne. 2019. “Variable Selection in
Proportional Hazards Cure Model with Time-Varying Covariates,
Application to Us Bank Failures.” *Journal of Applied Statistics* 46
(9): 1529–49. <https://doi.org/10.1080/02664763.2018.1554627>.

</div>

<div id="ref-Hendry_2014">

Hendry, David J. 2014. “Data Generation for the Cox Proportional Hazards
Model with Time-Dependent Covariates: A Method for Medical Researchers.”
*Statistics in Medicine* 33 (3): 436–54.
<https://doi.org/10.1002/sim.5945>.

</div>

<div id="ref-Sy_Taylor_2000">

Sy, Judy P, and Jeremy MG Taylor. 2000. “Estimation in a Cox
Proportional Hazards Cure Model.” *Biometrics* 56 (1): 227–36.
<https://doi.org/10.1111/j.0006-341X.2000.00227.x>.

</div>

</div>
