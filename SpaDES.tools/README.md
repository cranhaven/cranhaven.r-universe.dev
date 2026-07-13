<!-- badges: start -->
[![R build status](https://github.com/PredictiveEcology/SpaDES.tools/workflows/R-CMD-check/badge.svg)](https://github.com/PredictiveEcology/SpaDES.tools/actions)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/SpaDES.tools)](https://cran.r-project.org/package=SpaDES.tools)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/SpaDES.tools)](https://cran.r-project.org/package=SpaDES.tools)
[![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/SpaDES.tools/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/SpaDES.tools?branch=master)
<!-- badges: end -->

<img align="right" width="80" pad="20" src="https://github.com/PredictiveEcology/SpaDES/raw/master/man/figures/SpaDES.png">

# SpaDES.tools

Additional modelling tools for Spatial Discrete Event Simulation (`SpaDES`) module development.

Provides GIS/map utilities and additional modeling tools for developing cellular automata and agent based models in `SpaDES`.

**Website:** [https://SpaDES.PredictiveEcology.org](https://SpaDES.PredictiveEcology.org)

**Wiki:** [https://github.com/PredictiveEcology/SpaDES/wiki](https://github.com/PredictiveEcology/SpaDES/wiki)

## Installation

Building packages from source requires the appropriate development libraries for your operating system (*e.g.*, Windows users should install [Rtools](https://cran.r-project.org/bin/windows/Rtools/)).

### Current stable release

**Install from CRAN:**

```r
install.packages("SpaDES.tools")
```

**Install from GitHub:**
    
```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES.tools", dependencies = TRUE) # master
```

### Development version (unstable)

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES.tools", ref = "development", dependencies = TRUE)
```

## Contributions

Please see `CONTRIBUTING.md` for information on how to contribute to this project.
