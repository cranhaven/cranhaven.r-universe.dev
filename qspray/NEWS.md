# qspray 3.1.0

- The package no longer depends on **RcppArmadillo**.

- The functions `permuteVariables` and `swapVariables` generated an error for a constant `qspray` or a `qspray` having only one term.

- New function `isHomogeneousQspray`, to check whether a `qspray` polynomial is homogeneous.

- The `HallInnerProduct` function has been greatly improved, and it is now possible to get the result of the Hall inner product with a symbolic parameter (this result is a univariate `qspray` whose variable represents the parameter).


# qspray 3.0.0

- The C++ code has now a large header file that can be used in other packages (by setting `LinkingTo: qspray, Rcpp, RcppArmadillo` in the DESCRIPTION file). It is used by two upcoming packages: **ratioOfQsprays** (fractions of multivariate polynomials) and **symbolicQspray** (multivariate polynomials with symbolic parameters). Moreover, this code is templated. For example, multivariate polynomials with numeric (`double`) coefficients can be represented by the objects of type `Qspray<double>`, and instantiating this type automatically makes available the arithmetic operations for these polynomials.

- The `show` method of `qspray` objects has been changed. The monomial previously printed as `"x^(2, 0, 3)"` is now printed by default as `"x^2.z^3"` if there are no more than three variables in the polynomial, otherwise it is printed as `"x1^2.x3^3"`. But it is now possible to control the way a `qspray` is printed with the help of the function `showQsprayOption<-`. Helper functions to construct a custom `show` method are provided.

- The **README** has been updated. In particular, it contains a large section about the show options.

- New function `substituteQspray`, to substitute some values to a subset of the variables of a `qspray` polynomial.

- New function `composeQspray`, to get the polynomial obtained by substituting the variables of a polynomial with polynomials. The new function `changeVariables` is an alias of `composeQspray`.

- New function `isPolynomialOf`, to check whether a `qspray` polynomial can be written as a polynomial of some given `qspray` polynomials, and to get this polynomial in case this is true. This uses Gröbner bases and thus this can be slow.
 
- New function `isSymmetricQspray`, to check whether the polynomial defined by a `qspray` object is symmetric. 

- There was an error in `qdivision` (without severe consequences).

- The function `as.function.qspray` now has a Boolean argument `N`; if set to `TRUE`, the function returns a numerical approximation of the result.

- New function `PSFpoly`, which computes the power sum polynomials.

- New function `MSPcombination`, to get a symmetric `qspray` polynomial as a linear combination of the monomial symmetric polynomials. 

- New function `compactSymmetricQspray` which prints a symmetric `qspray` polynomial as a linear combination of the monomial symmetric polynomials.

- New function `qsprayDivision`, returning the quotient and the remainder of the division of a `qspray` polynomial by a `qspray` polynomial.

- New function `HallInnerProduct`, which computes the Hall inner product between two symmetric `qspray` polynomials. Very inefficient (will be hopefully improved in the future).

- New function `characteristicPolynomial`, to get the characteristic polynomial of a square matrix as a `qspray` polynomial.


# qspray 2.1.1

- Unit tests.

- The polynomials are now printed with the lexicographic order of the monomials.


# qspray 2.1.0

- New function `implicitization`, to transform a system of parametric equations to an implicit equation. This is based on Gröbner bases. This doesn't always work (use the 'giacR' package if needed).

- The division step in the Buchberger algorithm is now performed in C++.

- The `groebner` function has been considerably improved, but it still can be very slow.


# qspray 2.0.0

- The implementation of the opposite polynomial was wrong (it did nothing).

- The pretty form of a polynomial returned by `prettyQspray` has been improved: now there are spaces around '+' and '-'.

- The division of a polynomial by a `bigq` number did not always work.

- New function `groebner`, to compute a Gröbner basis.


# qspray 1.1.1

Fixed a small error in the C++ code.


# qspray 1.1.0

Differentiation of `qspray` polynomials (functions `derivQspray` and `dQspray`).


# qspray 1.0.0

The package does not depend on 'RcppCGAL' neither 'RcppEigen' anymore. 
It now imports 'RationalMatrix' (to compute the determinant of a rational 
matrix).


# qspray 0.1.1

Removed a useless function in the C++ code.


# qspray 0.1.0

First release.
