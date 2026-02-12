
# MOSAlloc

<!-- badges: start -->
<!-- badges: end -->

MOSAlloc provides a framework for multipurpose optimal resource allocation in survey sampling, extending the classical optimal allocation principles introduced by Tschuprow (1923) and Neyman (1934) to multidomain and multivariate allocation problems. Conic quadratic problem representations are parsed to the Embedded Conic Solver from the `ECOSolveR` package. See Willems (2025, <doi:10.25353/ubtr-9200-484c-5c89>) for a detailed description of the theory behind `MOSAlloc`.

## Installation

You can install the development version of MOSAlloc from GitLab using the `remotes` package:

``` r
# install.packages("remotes")
remotes::install_gitlab("willemsf/mosalloc")
```


## Citation

Cite package as:

  Willems, F. (2025). _A Framework for Multiobjective and Uncertain Resource Allocation Problems in Survey Sampling based on Conic Optimization_. Ph.D. thesis, Trier University, Trier, Germany. <https://doi.org/10.25353/ubtr-9200-484c-5c89>.


## Licensing

This package is licensed under the GNU General Public License, version 3 or later (GPL-3.0-or-later).


## Author / Maintainer

**Felix Willems**, Trier University
Email: willemsf@uni-trier.de

Maintainer: Felix Willems <mail.willemsf+MOSAlloc@gmail.com>  

Supervised by **Prof. Dr. Ralf Münnich**, Trier University.


## References

Neyman, J. (1934). _On the Two Different Aspects of the Representative Method: The Method of Stratified Sampling and the Method of Purposive Selection_. Journal of the Royal Statistical Society, 97(4), 558–625.

Tschuprow, A.A. (1923). _On the Mathematical Expectation of the Moments of Frequency Distribution in the Case of Correlated Observations_. Metron, 2(3,4), 461-493, 646-683.

Willems, F. (2025). _A Framework for Multiobjective and Uncertain Resource Allocation Problems in Survey Sampling based on Conic Optimization (Doctoral dissertation)_. Trier University. <https://doi.org/10.25353/ubtr-9200-484c-5c89>.