The ‘symbolicQspray’ package
================
Stéphane Laurent
2024-07-26

***Multivariate polynomials with symbolic parameters.***

<!-- badges: start -->

[![R-CMD-check](https://github.com/stla/symbolicQspray/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stla/symbolicQspray/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

------------------------------------------------------------------------

These notes about the **symbolicQspray** package assume that the reader
is a bit familiar with the [**qspray**
package](https://github.com/stla/qspray "the 'qspray' package on Github")
and with the [**ratioOfQsprays**
package](https://github.com/stla/ratioOfQsprays "the 'ratioOfQsprays' package on Github").

A `symbolicQspray` object represents a multivariate polynomial whose
coefficients are fractions of polynomials with rational coefficients.
Actually (see our discussion in the next section), a `symbolicQspray`
object represents a *multivariate polynomial with parameters*. The
parameters are the variables of the fractions of polynomials, and so
they are symbolically represented.

To construct a `symbolicQspray` polynomial, use `qlone` (from the
**qspray** package) to introduce the parameters and use `Qlone` to
introduce the variables of the polynomial:

``` r
library(symbolicQspray)
f <- function(a1, a2, X1, X2, X3) {
  (a1/(a2^2+1)) * X1^2*X2  +  (a2+1) * X3  +  a1/a2
}
# parameters, the variables occurring in the coefficients:
a1 <- qlone(1)
a2 <- qlone(2)
# variables:
X1 <- Qlone(1)
X2 <- Qlone(2)
X3 <- Qlone(3)
# the 'symbolicQspray':
( Qspray <- f(a1, a2, X1, X2, X3) )
## { [ a1 ] %//% [ a2^2 + 1 ] } * X^2.Y  +  { [ a2 + 1 ] } * Z  +  { [ a1 ] %//% [ a2 ] }
```

The fractions of polynomials such as the first coefficient `a1/(a2^2+1)`
in the above example are
[**ratioOfQsprays**](https://github.com/stla/ratioOfQsprays "the 'ratioOfQsprays' package on Github")
objects, and the numerator and the denominator of a `ratioOfQsprays` are
[**qspray**](https://github.com/stla/qspray "the 'qspray' package on Github")
objects.

Arithmetic on `symbolicQspray` objects is available:

``` r
Qspray^2
## { [ a1^2 ] %//% [ a2^4 + 2*a2^2 + 1 ] } * X^4.Y^2  +  { [ 2*a1.a2 + 2*a1 ] %//% [ a2^2 + 1 ] } * X^2.Y.Z  +  { [ 2*a1^2 ] %//% [ a2^3 + a2 ] } * X^2.Y  +  { [ a2^2 + 2*a2 + 1 ] } * Z^2  +  { [ 2*a1.a2 + 2*a1 ] %//% [ a2 ] } * Z  +  { [ a1^2 ] %//% [ a2^2 ] }
Qspray - Qspray
## 0
(Qspray - 1)^2
## { [ a1^2 ] %//% [ a2^4 + 2*a2^2 + 1 ] } * X^4.Y^2  +  { [ 2*a1.a2 + 2*a1 ] %//% [ a2^2 + 1 ] } * X^2.Y.Z  +  { [ 2*a1^2 - 2*a1.a2 ] %//% [ a2^3 + a2 ] } * X^2.Y  +  { [ a2^2 + 2*a2 + 1 ] } * Z^2  +  { [ 2*a1.a2 + 2*a1 - 2*a2^2 - 2*a2 ] %//% [ a2 ] } * Z  +  { [ a1^2 - 2*a1.a2 + a2^2 ] %//% [ a2^2 ] }
Qspray^2 - 2*Qspray + 1
## { [ a1^2 ] %//% [ a2^4 + 2*a2^2 + 1 ] } * X^4.Y^2  +  { [ 2*a1.a2 + 2*a1 ] %//% [ a2^2 + 1 ] } * X^2.Y.Z  +  { [ 2*a1^2 - 2*a1.a2 ] %//% [ a2^3 + a2 ] } * X^2.Y  +  { [ a2^2 + 2*a2 + 1 ] } * Z^2  +  { [ 2*a1.a2 + 2*a1 - 2*a2^2 - 2*a2 ] %//% [ a2 ] } * Z  +  { [ a1^2 - 2*a1.a2 + a2^2 ] %//% [ a2^2 ] }
```

## Evaluating a `symbolicQspray`

Substituting the “exterior” variables (the variables occurring in the
ratios of polynomials, also called the *parameters* - see below) yields
a `qspray` object:

``` r
a <- c(2, "3/2")
( qspray <- evalSymbolicQspray(Qspray, a = a) )
## 8/13*X^2.Y + 5/2*Z + 4/3
```

Substituting the “main” variables yields a `ratioOfQsprays` object:

``` r
X <- c(4, 3, "2/5")
( ratioOfQsprays <- evalSymbolicQspray(Qspray, X = X) )
## [ a1.a2^2 + 48*a1.a2 + a1 + 2/5*a2^4 + 2/5*a2^3 + 2/5*a2^2 + 2/5*a2 ] %//% [ a2^3 + a2 ]
```

There is a discutable point here. A `symbolicQspray` object represents a
polynomial with `ratioOfQsprays` coefficients. So one could consider
that the polynomial variables `X`, `Y` and `Z` represent some
indeterminate `ratioOfQsprays` fractions, and that it should be possible
to replace them with `ratioOfQsprays` objects. However this is not
allowed. We will discuss that, just after checking the consistency:

``` r
evalSymbolicQspray(Qspray, a = a, X = X)
## Big Rational ('bigq') :
## [1] 1243/39
evalQspray(qspray, X)
## Big Rational ('bigq') :
## [1] 1243/39
evalRatioOfQsprays(ratioOfQsprays, a)
## Big Rational ('bigq') :
## [1] 1243/39
a <- gmp::as.bigq(a); X <- gmp::as.bigq(X)
f(a[1], a[2], X[1], X[2], X[3])
## Big Rational ('bigq') :
## [1] 1243/39
```

Now let’s turn to our promised discussion. Why is replacing the values
of the polynomial variables with some `ratioOfQsprays` objects not
allowed?

Actually my motivation to do this package was inspired by the [**Jack
polynomials**](https://github.com/stla/jackR "the 'jack' package on Github").
In the context of Jack polynomials, the variables `X`, `Y` and `Z`
represent indeterminate *numbers*, and the coefficients are *numbers
depending on a parameter* (the Jack parameter), and it turns out that
they are fractions of polynomials of this parameter. So I consider that
a `symbolicQspray` is *not* a polynomial on the field of fractions of
polynomials: I consider it is a polynomial with *rational coefficients
depending on some parameters*.

Also note that evaluating the `ratioOfQsprays` object
`evalSymbolicQspray(Qspray, X = X)` at `a` would make no sense if we
took some `ratioOfQsprays` objects for the values of `X`.

## Querying a `symbolicQspray`

The package provides some functions to perform elementary queries on a
`symbolicQspray`:

``` r
numberOfVariables(Qspray)
## [1] 3
numberOfParameters(Qspray)
## [1] 2
numberOfTerms(Qspray)
## [1] 3
getCoefficient(Qspray, c(2, 1)) # coefficient of X^2.Y
## [ a1 ] %//% [ a2^2 + 1 ]
getConstantTerm(Qspray)
## [ a1 ] %//% [ a2 ]
isUnivariate(Qspray)
## [1] FALSE
isConstant(Qspray)
## [1] FALSE
```

## Transforming a `symbolicQspray`

You can differentiate a `symbolicQspray` polynomial:

``` r
derivSymbolicQspray(Qspray, 2) # derivative w.r.t. Y
## { [ a1 ] %//% [ a2^2 + 1 ] } * X^2
```

You can permute its variables:

``` r
swapVariables(Qspray, 2, 3) == f(a1, a2, X1, X3, X2)
## [1] TRUE
```

You can perform polynomial transformations of its variables:

``` r
changeVariables(Qspray, list(X1+1, X2^2, X1+X2+X3)) == 
  f(a1, a2, X1+1, X2^2, X1+X2+X3)
## [1] TRUE
```

You can also perform polynomial transformations of its parameters:

``` r
changeParameters(Qspray, list(a1^2, a2^2)) == f(a1^2, a2^2, X1, X2, X3)
## [1] TRUE
```

## Showing a `symbolicQspray`

You can change the way a `symbolicQspray` is printed by using
`showSymbolicQsprayOption`:

``` r
showSymbolicQsprayOption(Qspray, "a") <- "x"
showSymbolicQsprayOption(Qspray, "showMonomial") <- 
  showMonomialXYZ(c("A", "B", "C"))
showSymbolicQsprayOption(Qspray, "quotientBar") <- " / "
Qspray
## { [ x1 ] / [ x2^2 + 1 ] } * A^2.B  +  { [ x2 + 1 ] } * C  +  { [ x1 ] / [ x2 ] }
```

When this is possible, the result of an arithmetic operation between two
`symbolicQspray` objects inherits the show options of the first operand:

``` r
set.seed(421)
( Q <- rSymbolicQspray() ) # a random symbolicQspray
## { [ -a1^2.a3^3 - 5/2*a1^2 - 5/2*a3 ] %//% [ a1^4.a3^3 - 3/2*a2^4 ] } * X^3.Y  +  { [ 5/3*a1^4.a3^4 - 1/3*a1^2.a2.a3^3 ] %//% [ a1^2 - a2^2.a3^3 ] } * Y^3
Qspray + Q
## { [ -x1^2.x3^3 - 5/2*x1^2 - 5/2*x3 ] / [ x1^4.x3^3 - 3/2*x2^4 ] } * A^3.B  +  { [ x1 ] / [ x2^2 + 1 ] } * A^2.B  +  { [ 5/3*x1^4.x3^4 - 1/3*x1^2.x2.x3^3 ] / [ x1^2 - x2^2.x3^3 ] } * B^3  +  { [ x2 + 1 ] } * C  +  { [ x1 ] / [ x2 ] }
```

This behavior is the same as the ones implemented in **qspray** and in
**ratioOfQsprays**. You should be familiar with these two packages in
order to use **symbolicQspray**.

## Application: Jacobi polynomials

The [Jacobi
polynomials](https://en.wikipedia.org/wiki/Jacobi_polynomials "Jacobi polynomials on Wikipedia")
are univariate polynomials depending on two parameters that we will
denote by `alpha` and `beta`. They are implemented in this package:

``` r
JP <- JacobiPolynomial(2)
isUnivariate(JP)
## [1] TRUE
numberOfParameters(JP)
## [1] 2
showSymbolicQsprayOption(JP, "showRatioOfQsprays") <-
  showRatioOfQspraysXYZ(c("alpha", "beta"))
JP
## { [ 1/8*alpha^2 + 1/4*alpha.beta + 7/8*alpha + 1/8*beta^2 + 7/8*beta + 3/2 ] } * X^2  +  { [ 1/4*alpha^2 + 3/4*alpha - 1/4*beta^2 - 3/4*beta ] } * X  +  { [ 1/8*alpha^2 - 1/4*alpha.beta - 1/8*alpha + 1/8*beta^2 - 1/8*beta - 1/2 ] }
```

The implementation constructs these polynomials by using the [recurrence
relation](https://en.wikipedia.org/wiki/Jacobi_polynomials#Recurrence_relations "recurrence relation between Jacobi polynomials").
This is a child game, one just has to copy the first two terms and this
recurrence relation:

``` r
JacobiPolynomial <- function(n) {
  stopifnot(isPositiveInteger(n))
  if(n == 0) {
    Qone()
  } else if(n == 1) {
    alpha <- qlone(1)
    beta  <- qlone(2)
    X     <- Qlone(1)
    (alpha + 1) + (alpha + beta + 2) * (X - 1)/2
  } else {
    alpha <- qlone(1)
    beta  <- qlone(2)
    X     <- Qlone(1)
    a <- n + alpha
    b <- n + beta
    c <- a + b
    K <- 2 * n * (c - n) * (c - 2)
    lambda1 <- ((c - 1) * (c * (c - 2) * X + (a - b) * (c - 2*n))) / K
    lambda2 <- (2 * (a - 1) * (b - 1) * c) / K
    (lambda1 * JacobiPolynomial(n - 1) - lambda2 * JacobiPolynomial(n - 2))
  }
}
```

It is clearly visible from the recurrence relation that the coefficients
of the Jacobi polynomials are indeed fractions of polynomials in `alpha`
and `beta`. But they actually are *polynomials* in `alpha` and `beta`.
Actually I don’t know, this is a conjecture I made because I observed
this fact for some small values of `n`. We can check it with the
function `hasPolynomialCoefficientsOnly`:

``` r
JP <- JacobiPolynomial(7)
hasPolynomialCoefficientsOnly(JP)
## [1] TRUE
```

Up to a factor, the [Gegenbauer
polynomials](https://en.wikipedia.org/wiki/Gegenbauer_polynomials "Gegenbauer polynomials on Wikipedia")
with parameter `alpha` coincide with the Jacobi polynomials with
parameters `alpha - 1/2` and `alpha - 1/2`. Let’s derive them from the
Jacobi polynomials, as an exercise. The factor can be implemented as
follows (see Wikipedia for its formula):

``` r
risingFactorial <- function(theta, n) {
  toMultiply <- c(theta, lapply(seq_len(n-1), function(i) theta + i))
  Reduce(`*`, toMultiply)
}
theFactor <- function(alpha, n) {
  risingFactorial(2*alpha, n) / risingFactorial((2*alpha + 1)/2, n)
}
```

Now let’s apply the formula given in the Wikipedia article:

``` r
GegenbauerPolynomial <- function(n) {
  alpha <- qlone(1)
  P <- changeParameters(
    JacobiPolynomial(n), list(alpha - "1/2", alpha - "1/2")
  )
  theFactor(alpha, n) * P
}
```

Let’s check that the recurrence relation given in the Wikipedia article
is fulfilled:

``` r
n <- 5
alpha <- qlone(1)
X <- Qlone(1)
(n + 1) * GegenbauerPolynomial(n+1) == 
  2*(n + alpha) * X * GegenbauerPolynomial(n) - 
    (n + 2*alpha - 1) * GegenbauerPolynomial(n-1)
## [1] TRUE
```

## Application to Jack polynomials

The **symbolicQspray** package is used in the [**jack**
package](https://github.com/stla/jackR "the 'jack' package on Github")
to compute the Jack polynomials with a symbolic Jack parameter. The Jack
polynomials exactly fit to the polynomials represented by the
`symbolicQspray` objects: their coefficients are fractions of
polynomials by definition, of one variable: the Jack parameter.

<!-- -------------------- links -------------------- -->
