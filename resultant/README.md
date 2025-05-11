# resultant

*Utilities for polynomials with rational coefficients.*

<!-- badges: start -->
[![R-CMD-check](https://github.com/stla/resultant/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stla/resultant/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

___

This package deals with polynomials, univariate and multivariate, with 
rational coefficients. Features include:

- resultant of two multivariate polynomials

- subresultants of two multivariate polynomials

- greatest common divisor of two multivariate polynomials

- integral division (aka division without remainder) of two multivariate polynomials

- Sturm-Habicht sequence of a multivariate polynomial

- square-free factorization of a multivariate polynomial

- number of real roots of a univariate polynomial in a given interval

- division with remainder of two univariate polynomials

See [this blog post](https://laustep.github.io/stlahblog/posts/resultant-algebraicCurves.html) for an illustration of the resultant.

___

## License

This package is provided under the GPL-3 license but it uses the C++ library 
[CGAL](https://www.cgal.org/) which requires a license from the 
[GeometryFactory](https://geometryfactory.com) if you wish to use it for 
commercial purposes.

