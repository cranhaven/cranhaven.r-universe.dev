# PBOX <img src="./man/figures/pboxIcon.png" align="right" height="138"/>

<!-- badges: 
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/bayesplot?color=blue)](https://cran.r-project.org/web/packages/bayesplot)
[![Downloads](https://cranlogs.r-pkg.org/badges/bayesplot?color=blue)](https://cran.rstudio.com/package=bayesplot)
[![R-CMD-check](https://github.com/stan-dev/bayesplot/workflows/R-CMD-check/badge.svg)](https://github.com/stan-dev/bayesplot/actions)
[![codecov](https://codecov.io/gh/stan-dev/bayesplot/branch/master/graph/badge.svg)](https://codecov.io/gh/stan-dev/bayesplot)
<!-- badges: end -->

### Version 0.1.8 (BETA)

## Overview

The [pbox](https://github.com/athammad/pbox) R package is designed for risk assessment and management. It is an advanced statistical library that excels in exploring probability distributions within a given dataset. The tool offers a method to encapsulate and query the probability space effortlessly. Its distinctive feature lies in the ease with which users can navigate and analyze marginal, joint, and conditional probabilities while taking into account the underlying correlation structure inherent in the data. This unique capability empowers users to delve into intricate relationships and dependencies within a dataset, providing a solid foundation for making well-informed decisions in the context of risk management scenarios. With `pbox` is straightforward to answer questions like:

-   What is the probability of experiencing extreme heat waves in Indonesia with temperatures above 32 degrees?

-   What is the probability of simultaneous extreme heat waves in Vietnam with temperatures above than 31 degrees and the average regional temperature being above than 26 degrees?

-   Given that the average regional temperature is 26 degrees, what is the probability of experiencing extreme heat waves in both Vietnam and Indonesia with temperatures above 33 degrees?

## Features

**Generate a `pbox` object from data**

```{r, echo=TRUE, eval=FALSE}
data("SEAex")
pbx<-set_pbox(SEAex)
pbx
```

**Access the data and the copula object**

```{r, echo=TRUE, eval=FALSE}
pbx@data
pbc@copula
```

**Access the results of the automated selection for both the marginal distribution and the copula**

```{r, echo=TRUE, eval=FALSE}
pbx@fit
```

**Explore the probabilistic space**

```{r, echo=TRUE, eval=FALSE}

#Get marginal distribution
qpbox(pbx,mj = "Malaysia:33")

#Get Joint distribution
qpbox(pbx,mj = "Malaysia:33 & Vietnam:34")

# Conditional distribution distribution with Pr(X <= x, Y <= y) / Pr(Y = y)
qpbox(pbx,mj = "Malaysia:33 & median:c(Vietnam,Thailand)", co="mean:c(avgRegion)", fixed=TRUE)

# Estimate confidence intervals
qpbox(pbx,mj = "Vietnam:31 & avgRegion:26", co="Malaysia:32",CI=T)


```

**Map the probabilistic space with a gird of quantile values**

```{r, echo=TRUE, eval=FALSE}

grid_pbox(pbx, mj = c("Vietnam", "Malaysia"))


```

**Query the probabilistic space under different scenarios with different combinations of parameters for a single query**

```{r, echo=TRUE, eval=FALSE}

scenario_pbox(pbx,mj = "Vietnam:31 & avgRegion:26", param_list = list(Vietnam="mu"))

```

## Installation
The `pbox` package is currently under active development with the aim of being available on CRAN soon. The development version can be installed from GitHub, and once it is available on CRAN, you can install it directly from CRAN. Below are the installation instructions for both the development version and the CRAN version:

### Development Version
To install the development version from GitHub, you first need to install the remotes package if you haven't already. Then, use `remotes::install_github` to install the `pbox` package from GitHub:

```
install.packages("remotes")
remotes::install_github("athammad/pbox")

```

### CRAN Version
Once the `pbox` package is available on CRAN, you can install it directly from CRAN with the following command:

```
install.packages("pbox")
```

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/athammad/pbox/issues/).

## Author
`pbox` is written by [Ahmed T. Hammad](https://athsas.com/) and is under active development. Please feel free to contribute by submitting any issues or requests—or by solving any current issues!


<!---



# Citation
```
@article{hammad2024obox,
  author = {Ahmed T. Hammad},
  title = {pbox: Exploring multivariate spaces with Probability Boxes},
  journal = {Journal of Statistical Software},
  year = {2024}
}
```
## TO DO


-   Finalise Vignette
-   Improve documentation

1) Errors from fitdist should be stored somewhere and accesible to the user. How?

2) Some copula families are not included yet!
.copula_families <- list(
  # Archimedean copula families # "amh",
  archmCopula = c("clayton", "frank", "gumbel", "joe"),
  # Extreme-Value copula families #"tawn" #"tev"
  evCopula = c("galambos", "gumbel", "huslerReiss"),
  # Elliptical copula families # "t"
  ellipCopula = c("normal")
)

3) Currently the user cannot change the copulas and families beign tested!
-->

