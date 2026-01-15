The ‘ratioOfQsprays’ package
================
Stéphane Laurent
2024-07-26

***Fractions of multivariate polynomials with rational coefficients.***

<!-- badges: start -->

[![R-CMD-check](https://github.com/stla/ratioOfQsprays/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stla/ratioOfQsprays/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

------------------------------------------------------------------------

The [**qspray** package](https://github.com/stla/qspray) allows
arithmetic (and more) on multivariate polynomials with rational
coefficients. Based on this one, the **ratioOfQsprays** package allows
to manipulate *fractions* of multivariate polynomials with rational
coefficients.

These notes about the **ratioOfQsprays** package assume that the reader
is a bit familiar with the **qspray** package.

## Creating a `ratioOfQsprays`

A `ratioOfQsprays` object represents a fraction of two multivariate
polynomial with rational coefficients. Such polynomials are represented
by `qspray` objects. The easiest way to create a `ratioOfQsprays` is to
introduce the variables of the polynomials with the `qlone` function
(from the **qspray** package), and then to build a `qspray` numerator
and a `qspray` denominator with the arithmetic operations. For example:

``` r
library(ratioOfQsprays)
f <- function(x1, x2, x3) {
  (2*x1^2 + x2*x3) / (4*x1 - 3*x3 + 1)
}
# variables:
x1 <- qlone(1)
x2 <- qlone(2)
x3 <- qlone(3)
# the 'ratioOfQsprays':
( roq <- f(x1, x2, x3) )
## [ 1/2*x^2 + 1/4*y.z ]  %//%  [ x - 3/4*z + 1/4 ]
```

The denominator of a `ratioOfQsprays` fraction of polynomials is always
*monic*. That means it is a polynomial whose leading coefficient is 1.

Arithmetic on `ratioOfQsprays` objects is available:

``` r
roq^2
## [ 1/4*x^4 + 1/4*x^2.y.z + 1/16*y^2.z^2 ]  %//%  [ x^2 - 3/2*x.z + 1/2*x + 9/16*z^2 - 3/8*z + 1/16 ]
roq - roq
## [ 0 ]
1 / roq
## [ 2*x - 3/2*z + 1/2 ]  %//%  [ x^2 + 1/2*y.z ]
2*roq + (x2 + x3)/x1
## [ x^3 + 1/2*x.y.z + x.y + x.z - 3/4*y.z + 1/4*y - 3/4*z^2 + 1/4*z ]  %//%  [ x^2 - 3/4*x.z + 1/4*x ]
```

You don’t like my quotient bar `%//%`? Be patient, we will see how to
change it later. I adopted this large quotient bar because it is more
easy to find it than a single slash `/` in a `ratioOfQsprays` having a
long expression.

Rational numbers and `qspray` polynomials are coercible to
`ratioOfQsprays` objects, and then you can also perform arithmetic
operations between a `ratioOfQsprays` and such an object:

``` r
2 * roq
## [ x^2 + 1/2*y.z ]  %//%  [ x - 3/4*z + 1/4 ]
"1/2" * roq
## [ 1/4*x^2 + 1/8*y.z ]  %//%  [ x - 3/4*z + 1/4 ]
roq + gmp::as.bigq("7/3") 
## [ 1/2*x^2 + 7/3*x + 1/4*y.z - 7/4*z + 7/12 ]  %//%  [ x - 3/4*z + 1/4 ]
x1 + roq + x3^2
## [ 3/2*x^2 + x.z^2 - 3/4*x.z + 1/4*x + 1/4*y.z - 3/4*z^3 + 1/4*z^2 ]  %//%  [ x - 3/4*z + 1/4 ]
```

The result of an arithmetic operation is always an irreducible fraction.
To perform this step, the C++ library **CGAL** is used to compute a
greatest common divisor of the numerator and the denominator of the
possibly non-reduced fraction resulting from the arithmetic operation,
and then to divide both of them by this greatest common divisor. This is
very efficient in general.

## Evaluating a `ratioOfQsprays`

Use `evalRatioOfQsprays` to evaluate a `ratioOfQsprays`. This function
returns a `bigq` number:

``` r
library(gmp) # rational numbers
x <- c("4", "3", "2/5")
evalRatioOfQsprays(roq, x)
## Big Rational ('bigq') :
## [1] 166/79
x <- as.bigq(x)
evalRatioOfQsprays(roq, x)
## Big Rational ('bigq') :
## [1] 166/79
f(x[1], x[2], x[3])
## Big Rational ('bigq') :
## [1] 166/79
```

It is also possible to substitute some values to only a subset of the
variables, with the help of the function `substituteRatioOfQsprays`. You
have to indicate the variables you don’t want to replace with `NA`:

``` r
x <- c(NA, "3", "2/5")
substituteRatioOfQsprays(roq, x)
## [ 1/2*x^2 + 3/10 ]  %//%  [ x - 1/20 ]
x <- as.bigq(x)
f(x1, x[2], x[3])
## [ 1/2*x^2 + 3/10 ]  %//%  [ x - 1/20 ]
```

And it is possible to convert a `ratioOfQsprays` to a function which is
evaluated by **Ryacas**:

``` r
fyac <- as.function(roq)
fyac("4", "3", "2/5") # = evalRatioOfQsprays(roq, c("4", "3", "2/5"))
## [1] "166/79"
```

Actually you can pass some literal variables to this function:

``` r
fyac("x", "3", "2/5") # = substituteRatioOfQsprays(roq, c(NA, "3", "2/5"))
## [1] "(2*(5*x^2+3))/(20*x-1)"
fyac("x", "y", "z")   # = roq
## [1] "(z*y+2*x^2)/(4*x-3*z+1)"
fyac("x", "x", "x")
## [1] "(3*x^2)/(x+1)"
```

Complex numbers and allowed; the imaginary unit is denoted by `I`. See
the [**Yacas** documentation](https://yacas.readthedocs.io/en/latest/)
for more information.

``` r
fyac("Sqrt(2)", "2 + 2*I", "3")
## [1] "Complex(10/(Sqrt(32)-8),6/(Sqrt(32)-8))"
```

You can get numerical approximations by setting the option `N=TRUE` in
`as.function`:

``` r
fyacN <- as.function(roq, N = TRUE)
fyacN("4", "3", "2/5") 
## [1] 2.101266
fyacN("x", "3", "2/5")
## expression((2 * (5 * x^2 + 3))/(20 * x - 1))
fyacN("Sqrt(2)", "2 + 2*I", "3")
## [1] -4.267767-2.56066i
```

## Querying a `ratioOfQsprays`

A couple of functions to query a `ratioOfQsprays` are available:

``` r
getNumerator(roq)
## 1/2*x^2 + 1/4*y.z
getDenominator(roq)
## x - 3/4*z + 1/4
numberOfVariables(roq)
## [1] 3
isConstant(roq)
## [1] FALSE
isConstant(roq / roq)
## [1] TRUE
isUnivariate(roq)
## [1] FALSE
isUnivariate(x1 / (x1^2 + 1))
## [1] TRUE
isPolynomial(roq)
## [1] FALSE
isPolynomial((x1^2 - x2^2) / (x1 - x2))
## [1] TRUE
```

## Showing a `ratioOfQsprays`

As you have seen, the variables of `roq` are denoted by `x`, `y`, `z`.
This is the default way of printing a `ratioOfQsprays` which has no more
than three variables. If it has more than three variables, then they are
denoted by `x1`, `x2`, `x3`, …:

``` r
x4 <- qlone(4)
roq / x4
## [ 1/2*x1^2 + 1/4*x2.x3 ]  %//%  [ x1.x4 - 3/4*x3.x4 + 1/4*x4 ]
```

It is possible to control the way a `ratioOfQsprays` is printed. For
example, let’s say you want to print `roq` by using `a1`, `a2`, `a3` for
the variables and you want to change the symbol for the quotient bar:

``` r
showRatioOfQspraysOption(roq, "x") <- "a"
showRatioOfQspraysOption(roq, "quotientBar") <- " / " 
roq
## [ 1/2*a1^2 + 1/4*a2.a3 ] / [ a1 - 3/4*a3 + 1/4 ]
```

Now, if you perform an arithmetic operation between `roq` *at first
position* and an another `ratioOfQsprays`, these show options are passed
to the result if possible:

``` r
roq + (x1 + 1)/x2
## [ 1/2*a1^2.a2 + a1^2 - 3/4*a1.a3 + 5/4*a1 + 1/4*a2^2.a3 - 3/4*a3 + 1/4 ] / [ a1.a2 - 3/4*a2.a3 + 1/4*a2 ]
```

If you perform an arithmetic operation between `roq` and an object
coercible to a `ratioOfQsprays` object but which is not a
`ratioOfQsprays` object, such as a `bigq` number or a `qspray` object,
the show options of `roq` are passed to the result, even if `roq` is not
at the first position:

``` r
x1 * roq
## [ 1/2*a1^3 + 1/4*a1.a2.a3 ] / [ a1 - 3/4*a3 + 1/4 ]
```

An obvious example of a situation in which it is not always possible to
transfer the show options is when you use three letters for the
variables, e.g.

``` r
showRatioOfQspraysOption(roq, "showQspray") <- showQsprayXYZ(c("A", "B", "C"))
roq
## [ 1/2*A^2 + 1/4*B.C ] / [ A - 3/4*C + 1/4 ]
```

but then you add to `roq` a `ratioOfQsprays` containing the fourth
variable:

``` r
roq + x4/(x4 + 1)
## [ 1/2*A1^2.A4 + 1/2*A1^2 + A1.A4 + 1/4*A2.A3.A4 + 1/4*A2.A3 - 3/4*A3.A4 + 1/4*A4 ] / [ A1.A4 + A1 - 3/4*A3.A4 - 3/4*A3 + 1/4*A4 + 1/4 ]
```

Obviously it is not possible to denote the resulting fraction of
polynomials with the letters `A`, `B` and `C`. The solution I adopted
consists in taking the first of these letters and to index it. The same
method is used for the `qspray` polynomials.

## Transforming a `ratioOfQsprays`

Let’s take a `ratioOfQsprays` fraction of polynomials:

``` r
f <- function(x, y, z) {
  (2*x^2 + y*z) / (4*x - 3*z + 1)
}
x <- qlone(1); y <- qlone(2); z <- qlone(3)
roq <- f(x, y, z)
```

You can differentiate it:

``` r
derivRatioOfQsprays(roq, 2) # derivative w.r.t. y
## [ 1/4*z ]  %//%  [ x - 3/4*z + 1/4 ]
```

You can permute its variables:

``` r
swapVariables(roq, 2, 3) == f(x, z, y)
## [1] TRUE
```

You can perform some polynomial changes of its variables:

``` r
changeVariables(roq, list(x+1, y^2, x+y+z)) == f(x+1, y^2, x+y+z)
## [1] TRUE
```
