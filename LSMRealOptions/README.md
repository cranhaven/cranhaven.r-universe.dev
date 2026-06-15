# LSMRealOptions
Value American-style and Real Options Through LSM Simulation

<!-- badges: start -->
<!-- badges: end -->

The least-squares Monte Carlo (LSM) simulation method is a popular method for the approximation of the value of early and multiple exercise options. 'LSMRealOptions' provides implementations of the LSM simulation method to value American option products and capital investment projects through real options analysis. 'LSMRealOptions' values capital investment projects with cash flows dependent upon underlying state variables that are stochastically evolving, providing analysis into the timing and critical values at which investment is optimal. 'LSMRealOptions' provides flexibility in the stochastic processes followed by underlying assets, the number of state variables, basis functions and underlying asset characteristics to allow a broad range of assets to be valued through the LSM simulation method. Real options projects are further able to be valued whilst considering construction periods, time-varying initial capital expenditures and path-dependent operational flexibility including the ability to temporarily shutdown or permanently abandon projects after initial investment has occurred.

## Installation

You can install the released version of LSMRealOptions from [CRAN](https://CRAN.R-project.org) with:

```
install.packages("LSMRealOptions")
```

And the development version from [GitHub](https://github.com/) with:

```
devtools::install_github("TomAspinall/LSMRealOptions")
```
which contains source code for the package starting with version 0.1.0.
