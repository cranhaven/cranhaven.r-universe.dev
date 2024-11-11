<!-- badges: start -->
[![R-CMD-check](https://github.com/ericdunipace/approxOT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ericdunipace/approxOT/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/approxOT)](https://CRAN.R-project.org/package=approxOT)
<!-- badges: end -->

# approxOT: Approximate and Exact Optimal Transport Distances

This [R](https://www.r-project.org) package performs the computation of approximate and exact optimal transport distances 
through a variety of algorithms. We also provide header files
in C++ to allow other packages to use these methods.

## Installation
To install this package, download or clone this repository and install with `devtools::install("approxOT")` or install directly
from Github using `devtools::install_github`. Alternatively, a version is available from [CRAN](https://CRAN.R-project.org/package=approxOT).

## Algorithms
The package currently supports the following algorithms to calculate optimal transport distances
1.  "exact" or "networkflow": utilizes a network flow algorithm to calculate *exact* optimal transport distances
2. "shortsimplex": uses the shortlist method of Gottschlich
and Schuhmacher (2014) to calculate an *exact* optimal transport distance.
3. "univariate": uses the exact method special to the univariate case.
4.  "sinkhorn": use the Sinkhorn distance method of Cuturi (2013) 
to calculate entropically regularized optimal transport distances.
An unbiased option also allows the calculation of Sinkhorn divergences (removing the entropic bias).
5. "greenkhorn": use the Greenkhorn algorithm of Altschuler et al. (2017).
6. "hilbert": utilizes Hilbert sorting to perform *very fast* optimal transport distances. Relies on the CGAL header library provided by the [RcppCGAL](https://github.com/ericdunipace/RcppCGAL) package.
7. "sliced": uses the sliced optimal transport distances as described in Bonneel et al. (2015).
8. "ranks": use the average ranks of each column to perform quick optimal transport distances.
9. "swapping": calculates the swapping optimal transport distance
to improve approximate solutions.

Algorithms 1--3 are exact algorithms. Algorithms 4+ are approximate

## Author
Eric Dunipace

## License
GPL 3
