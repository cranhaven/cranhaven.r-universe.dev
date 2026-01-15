The ‘jack’ package: Jack and other symmetric polynomials
================

***Jack, zonal, Schur, and other symmetric polynomials.***

<!-- badges: start -->

[![R-CMD-check](https://github.com/stla/jackR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stla/jackR/actions/workflows/R-CMD-check.yaml)
[![R-CMD-check-valgrind](https://github.com/stla/jackR/actions/workflows/R-CMD-check-valgrind.yaml/badge.svg)](https://github.com/stla/jackR/actions/workflows/R-CMD-check-valgrind.yaml)
<!-- badges: end -->

``` r
library(jack)
```

*Schur polynomials* have applications in combinatorics and *zonal
polynomials* have applications in multivariate statistics. They are
particular cases of [*Jack
polynomials*](https://en.wikipedia.org/wiki/Jack_function "Jack polynomials on Wikipedia"),
which are some multivariate symmetric polynomials. The original purpose
of this package was the evaluation and the computation in symbolic form
of these polynomials. Now it contains much more stuff dealing with
multivariate symmetric polynomials.

## Breaking change in version 6.0.0

In version 6.0.0, each function whose name ended with the suffix `CPP`
(`JackCPP`, `JackPolCPP`, etc.) has been renamed by removing this
suffix, and the functions `Jack`, `JackPol`, etc. have been renamed by
adding the suffix `R` to their name: `JackR`, `JackPolR`, etc. The
reason of these changes is that a name like `Jack` is more appealing
than `JackCPP` and it is more sensible to assign the more appealing
names to the functions implemented with **Rcpp** since they are highly
more efficient. The interest of the functions `JackR`, `JackPolR`, etc.
is meager.

## Getting the polynomials

The functions `JackPol`, `ZonalPol`, `ZonalQPol` and `SchurPol`
respectively return the Jack polynomial, the zonal polynomial, the
quaternionic zonal polynomial, and the Schur polynomial.

Each of these polynomials is given by a positive integer, the number of
variables (the `n` argument), and an integer partition (the `lambda`
argument); the Jack polynomial has a parameter in addition, the `alpha`
argument, a number called the *Jack parameter*.

Actually there are four possible Jack polynomials for a given Jack
parameter and a given integer partition: the $J$-polynomial, the
$P$-polynomial, the $Q$-polynomial and the $C$-polynomial. You can
specify which one you want with the `which` argument, which is set to
`"J"` by default. These four polynomials differ only by a constant
factor.

To get a Jack polynomial with `JackPol`, you have to supply the Jack
parameter as a `bigq` rational number or anything coercible to a `bigq`
number by an application of the `as.bigq` function of the **gmp**
package, such as a character string representing a fraction,
e.g. `"2/5"`:

``` r
jpol <- JackPol(2, lambda = c(3, 1), alpha = "2/5")
jpol
## 98/25*x^3.y + 28/5*x^2.y^2 + 98/25*x.y^3
```

This is a `qspray` object, from the [**qspray**
package](https://github.com/stla/qspray "the 'qspray' package on Github").
Here is how you can evaluate this polynomial:

``` r
evalQspray(jpol, c("2", "3/2"))
## Big Rational ('bigq') :
## [1] 1239/10
```

It is also possible to convert a `qspray` polynomial to a function whose
evaluation is performed by the **Ryacas** package:

``` r
jyacas <- as.function(jpol)
```

You can provide the values of the variables of this function as numbers
or character strings:

``` r
jyacas(2, "3/2")
## [1] "1239/10"
```

You can even pass a variable name to this function:

``` r
jyacas("x", "x")
## [1] "(336*x^4)/25"
```

If you want to substitute a complex number to a variable, use a
character string which represents this number, with `I` denoting the
imaginary unit:

``` r
jyacas("2 + 2*I", "2/3 + I/4")
## [1] "Complex((-158921)/2160,101689/2160)"
```

It is also possible to evaluate a `qspray` polynomial for some complex
values of the variables with `evalQspray`. You have to separate the real
parts and the imaginary parts:

``` r
evalQspray(jpol, values_re = c("2", "2/3"), values_im = c("2", "1/4"))
## Big Rational ('bigq') object of length 2:
## [1] -158921/2160 101689/2160
```

## Direct evaluation of the polynomials

If you just have to evaluate a Jack polynomial, you don’t need to resort
to a `qspray` polynomial: you can use the functions `Jack`, `Zonal`,
`ZonalQ` or `Schur`, which directly evaluate the polynomial; this is
much more efficient than computing the `qspray` polynomial and then
applying `evalQspray`.

``` r
Jack(c("2", "3/2"), lambda = c(3, 1), alpha = "2/5")
## Big Rational ('bigq') :
## [1] 1239/10
```

However, if you have to evaluate a Jack polynomial for several values,
it could be better to resort to the `qspray` polynomial.

## Skew Jack polynomials

As of version 6.0.0, the package is able to compute the skew Schur
polynomials with the function `SkewSchurPol`, and the general skew Jack
polynomial is available as of version 6.1.0 (function `SkewJackPol`).

## Symbolic Jack parameter

As of version 6.0.0, it is possible to get a Jack polynomial with a
symbolic Jack parameter in its coefficients, thanks to the
[**symbolicQspray**
package](https://github.com/stla/symbolicQspray "the 'symbolicQspray' package on Github").

``` r
( J <- JackSymPol(2, lambda = c(3, 1)) )
## { [ 2*a^2 + 4*a + 2 ] } * X^3.Y  +  { [ 4*a + 4 ] } * X^2.Y^2  +  { [ 2*a^2 + 4*a + 2 ] } * X.Y^3
```

This is a `symbolicQspray` object, from the **symbolicQspray** package.

A `symbolicQspray` object corresponds to a multivariate polynomial whose
coefficients are fractions of polynomials with rational coefficients.
The variables of these fractions of polynomials can be seen as some
parameters. The Jack polynomials fit into this category: from their
definition, their coefficients are fractions of polynomials in the Jack
parameter. However you can see in the above output that for this
example, the coefficients are *polynomials* in the Jack parameter (`a`):
there’s no fraction. Actually this fact is always true for the Jack
$J$-polynomials. This is an established fact and it is not obvious (it
is a consequence of the [Knop & Sahi
formula](https://en.wikipedia.org/wiki/Jack_function#Combinatorial_formula "Knop & Sahi formula")).

You can substitute a value to the Jack parameter with the help of the
`substituteParameters` function:

``` r
( J5 <- substituteParameters(J, 5) )
## 72*X^3.Y + 24*X^2.Y^2 + 72*X.Y^3
```

``` r
J5 == JackPol(2, lambda = c(3, 1), alpha = "5")
## [1] TRUE
```

Note that you can change the letters used to denote the variables. By
default, the Jack parameter is denoted by `a` and the variables are
denoted by `X`, `Y`, `Z` if there are no more than three variables,
otherwise they are denoted by `X1`, `X2`, … Here is how to change these
symbols:

``` r
showSymbolicQsprayOption(J, "a") <- "alpha"
showSymbolicQsprayOption(J, "X") <- "x"
J
## { [ 2*alpha^2 + 4*alpha + 2 ] } * x1^3.x2  +  { [ 4*alpha + 4 ] } * x1^2.x2^2  +  { [ 2*alpha^2 + 4*alpha + 2 ] } * x1.x2^3
```

If you want to have the variables denoted by `x` and `y`, do:

``` r
showSymbolicQsprayOption(J, "showMonomial") <- showMonomialXYZ(c("x", "y"))
J
## { [ 2*alpha^2 + 4*alpha + 2 ] } * x^3.y  +  { [ 4*alpha + 4 ] } * x^2.y^2  +  { [ 2*alpha^2 + 4*alpha + 2 ] } * x.y^3
```

The skew Jack polynomials with a symbolic Jack parameter are available
too, as of version 6.1.0.

## Compact expression of Jack polynomials

The expression of a Jack polynomial in the canonical basis can be long.
Since these polynomials are symmetric, one can get a considerably
shorter expression by writing the polynomial as a linear combination of
the monomial symmetric polynomials. This is what the function
`compactSymmetricQspray` does:

``` r
( J <- JackPol(3, lambda = c(4, 3, 1), alpha = "2") )
## 3888*x^4.y^3.z + 2592*x^4.y^2.z^2 + 3888*x^4.y.z^3 + 3888*x^3.y^4.z + 4752*x^3.y^3.z^2 + 4752*x^3.y^2.z^3 + 3888*x^3.y.z^4 + 2592*x^2.y^4.z^2 + 4752*x^2.y^3.z^3 + 2592*x^2.y^2.z^4 + 3888*x.y^4.z^3 + 3888*x.y^3.z^4
```

``` r
compactSymmetricQspray(J) |> cat()
## 3888*M[4, 3, 1] + 2592*M[4, 2, 2] + 4752*M[3, 3, 2]
```

The function `compactSymmetricQspray` is also applicable to a
`symbolicQspray` object, like a Jack polynomial with symbolic Jack
parameter.

It is easy to figure out what is a monomial symmetric polynomial:
`M[i, j, k]` is the sum of all monomials `x^p.y^q.z^r` where `(p, q, r)`
is a permutation of `(i, j, k)`.

The “compact expression” of a Jack polynomial with `n` variables does
not depend on `n` if `n >= sum(lambda)`:

``` r
lambda <- c(3, 1)
alpha <- "3"
J4 <- JackPol(4, lambda, alpha)
J9 <- JackPol(9, lambda, alpha)
compactSymmetricQspray(J4) |> cat()
## 32*M[3, 1] + 16*M[2, 2] + 28*M[2, 1, 1] + 24*M[1, 1, 1, 1]
```

``` r
compactSymmetricQspray(J9) |> cat()
## 32*M[3, 1] + 16*M[2, 2] + 28*M[2, 1, 1] + 24*M[1, 1, 1, 1]
```

In fact I’m not sure the Jack polynomial makes sense when
`n < sum(lambda)`.

## Hall inner product

The **qspray** package provides a function to compute the Hall inner
product of two symmetric polynomials, namely `HallInnerProduct`. This is
the generalized Hall inner product, the one with a parameter $\alpha$.
It is known that the Jack polynomials with parameter $\alpha$ are
orthogonal for the Hall inner product with parameter $\alpha$. Let’s
give a try:

``` r
alpha <- "3"
J1 <- JackPol(4, lambda = c(3, 1), alpha, which = "P")
J2 <- JackPol(4, lambda = c(2, 2), alpha, which = "P")
HallInnerProduct(J1, J2, alpha)
## Big Rational ('bigq') :
## [1] 0
```

``` r
HallInnerProduct(J1, J1, alpha)
## Big Rational ('bigq') :
## [1] 135/8
```

``` r
HallInnerProduct(J2, J2, alpha)
## Big Rational ('bigq') :
## [1] 63/5
```

If you set `alpha=NULL` in `HallInnerProduct`, you get the Hall inner
product with symbolic parameter $\alpha$:

``` r
HallInnerProduct(J1, J1, alpha = NULL)
## 3/128*alpha^4 + 1/4*alpha^3 + 63/128*alpha^2 + 81/64*alpha
```

This is a `qspray` object. The Hall inner product is always polynomial
in $\alpha$.

It is also possible to get the Hall inner product of two
`symbolicQspray` polynomials. Take for example a Jack polynomial with
symbolic parameter:

``` r
J <- JackSymPol(4, lambda = c(3, 1), which = "P")
showSymbolicQsprayOption(J, "a") <- "t"
HallInnerProduct(J, J, alpha = 2)
## [ 20*t^4 - 24*t^3 + 92*t^2 - 48*t + 104 ] %//% [ t^4 + 4*t^3 + 6*t^2 + 4*t + 1 ]
```

We use `t` to display the Jack parameter and not `alpha` so that there
is no confusion between the Jack parameter and the parameter of the Hall
product.

Now, what happens if we compute the symbolic Hall inner product of this
Jack polynomial with itself, that is, if we run
`HallInnerProduct(J, J, alpha = NULL)`? Let’s see:

``` r
( Hip <- HallInnerProduct(J, J, alpha = NULL) )
## { [ 6 ] %//% [ t^4 + 4*t^3 + 6*t^2 + 4*t + 1 ] } * alpha^4  +  { [ 9*t^2 - 6*t + 1 ] %//% [ t^4 + 4*t^3 + 6*t^2 + 4*t + 1 ] } * alpha^3  +  { [ 3*t^4 - 6*t^3 + 5*t^2 ] %//% [ t^4 + 4*t^3 + 6*t^2 + 4*t + 1 ] } * alpha^2  +  { [ 4*t^4 ] %//% [ t^4 + 4*t^3 + 6*t^2 + 4*t + 1 ] } * alpha
```

We get the Hall inner product of the Jack polynomial with itself, with
two symbolic parameters: the Jack parameter displayed as `t` and the
parameter of the Hall product displayed as `alpha`. This is a
`symbolicQspray` object.

Now one could be interested in the symbolic Hall inner product of the
Jack polynomial with itself for the case when the Jack parameter and the
parameter of the Hall product coincide, that is, to set `alpha=t` in the
`symbolicQspray` polynomial that we named `Hip`. One can get it as
follows:

``` r
changeVariables(Hip, list(qlone(1)))
## [ 3*t^4 + t^3 ] %//% [ t^2 + 2*t + 1 ]
```

This is rather a trick. The `changeVariables` function allows to replace
the variables of a `symbolicQspray` polynomial with the new variables
given as a list in its second argument. The `Hip` polynomial has only
one variable, `alpha`, and it has one parameter, `t`. This parameter `t`
is the polynomial variable of the `ratioOfQsprays` coefficients of
`Hip`. Technically this is a `qspray` object: this is `qlone(1)`. So we
provided `list(qlone(1))` as the list of new variables. This corresponds
to set `alpha=t`. The usage of the `changeVariables` is a bit deflected,
because `qlone(1)` is not a new variable for `Hip`, this is a constant.

## Laplace-Beltrami operator

Just to illustrate the possibilities of the packages involved in the
**jack** package (**qspray**, **ratioOfQsprays**, **symbolicQspray**),
let us check that the Jack polynomials are eigenpolynomials for the
[Laplace-Beltrami
operator](https://math.mit.edu/~rstan/pubs/pubfiles/73.pdf "Laplace-Beltrami operator")
on the space of homogeneous symmetric polynomials.

``` r
LaplaceBeltrami <- function(qspray, alpha) {
  n <- numberOfVariables(qspray)
  derivatives1 <- lapply(seq_len(n), function(i) {
    derivQspray(qspray, i)
  })
  derivatives2 <- lapply(seq_len(n), function(i) {
    derivQspray(derivatives1[[i]], i)
  })
  x <- lapply(seq_len(n), qlone) # x_1, x_2, ..., x_n
  # first term
  out1 <- 0L
  for(i in seq_len(n)) {
    out1 <- out1 + alpha * x[[i]]^2 * derivatives2[[i]]
  }
  # second term
  out2 <- 0L
  for(i in seq_len(n)) {
    for(j in seq_len(n)) {
      if(i != j) {
        out2 <- out2 + x[[i]]^2 * derivatives1[[i]] / (x[[i]] - x[[j]])
      }
    }
  }
  # at this step, `out2` is a `ratioOfQsprays` object, because of the divisions
  # by `x[[i]] - x[[j]]`; but actually its denominator is 1 because of some
  # simplifications and then we extract its numerator to get a `qspray` object
  out2 <- getNumerator(out2)
  out1/2 + out2
}
```

``` r
alpha <- "3"
J <- JackPol(4, c(2, 2), alpha)
collinearQsprays(
  qspray1 = LaplaceBeltrami(J, alpha), 
  qspray2 = J
)
## [1] TRUE
```

## Other symmetric polynomials

Many other symmetric multivariate polynomials have been introduced in
version 6.1.0. Let’s see a couple of them.

### Skew Jack polynomials

The skew Jack polynomials are now available. They generalize the skew
Schur polynomials. In order to specify the skew integer partition
$\lambda/\mu$, one has to provide the outer partition $\lambda$ and the
inner partition $\mu$. The skew Schur polynomial associated to some skew
partition is the skew Jack $P$-polynomial with Jack parameter $\alpha=1$
associated to the same skew partition:

``` r
n <- 3
lambda <- c(3, 3)
mu <- c(2, 1)
skewSchurPoly <- SkewSchurPol(n, lambda, mu)
skewJackPoly <- SkewJackPol(n, lambda, mu, alpha = 1, which = "P")
skewSchurPoly == skewJackPoly
## [1] TRUE
```

### $t$-Schur polynomials

The $t$-Schur polynomials depend on a single parameter usually denoted
by $t$ and their coefficients are polynomials in this parameter. They
yield the Schur polynomials when substituting $t$ with $0$:

``` r
n <- 3
lambda <- c(2, 2)
tSchurPoly <- tSchurPol(n, lambda)
substituteParameters(tSchurPoly, values = 0) == SchurPol(n, lambda)
## [1] TRUE
```

### Hall-Littlewood polynomials

Similarly to the $t$-Schur polynomials, the Hall-Littlewood polynomials
depend on a single parameter usually denoted by $t$ and their
coefficients are polynomials in this parameter. The Hall-Littlewood
$P$-polynomials yield the Schur polynomials when substituting $t$ with
$0$:

``` r
n <- 3
lambda <- c(2, 2)
hlPoly <- HallLittlewoodPol(n, lambda, which = "P")
substituteParameters(hlPoly, values = 0) == SchurPol(n, lambda)
## [1] TRUE
```

### Macdonald polynomials

The Macdonald polynomials depend on two parameters usually denoted by
$q$ and $t$. Their coefficients are not polynomials in $q$ and $t$ in
general, they are ratios of polynomials in $q$ and $t$. These
polynomials yield the Hall-Littlewood polynomials when substituting $q$
with $0$:

``` r
n <- 3
lambda <- c(2, 2)
macPoly <- MacdonaldPol(n, lambda)
hlPoly <- HallLittlewoodPol(n, lambda)
changeParameters(macPoly, list(0, qlone(1))) == hlPoly
## [1] TRUE
```

## Kostka numbers and Kostka polynomials

The ordinary Kostka numbers are usually denoted by $K_{\lambda,\mu}$
where $\lambda$ and $\mu$ denote two integer partitions. The Kostka
number $K_{\lambda,\mu}$ is then associated to the two integer
partitions $\lambda$ and $\mu$, and it is the coefficient of the
monomial symmetric polynomial $m_{\mu}$ in the expression of the Schur
polynomial $s_{\lambda}$ as a linear combination of monomial symmetric
polynomials. It is always a non-negative integer. It is possible to
compute these Kostka numbers with the **jack** package. They are also
available in the [**syt**
package](https://github.com/stla/syt "the 'syt' package on Github").
There is more in the **jack** package. Since the Schur polynomials are
the Jack $P$-polynomials with Jack parameter $\alpha=1$, one can more
generally define the *Kostka-Jack number* $K_{\lambda,\mu}(\alpha)$ as
the coefficient of the monomial symmetric polynomial $m_{\mu}$ in the
expression of the Jack $P$-polynomial $P_{\lambda}(\alpha)$ with Jack
parameter $\alpha$ as a linear combination of monomial symmetric
polynomials. The **jack** package allows to compute these numbers. Note
that I call them “Kostka-Jack numbers” here as well as in the
documentation of the package but I don’t know whether this wording is
standard (probably not).

The Kostka numbers are also generalized by the *Kostka-Foulkes
polynomials*, or *$t$-Kostka polynomials*, which are provided in the
**jack** package. These are univariate polynomials whose variable is
denoted by $t$, and their value at $t=1$ are the Kostka numbers. These
polynomials are used in the computation of the Hall-Littlewood
polynomials.

Finally, the Kostka numbers are also generalized by the
*Kostka-Macdonald polynomials*, or *$qt$-Kostka polynomials*, also
provided in the **jack** package. Actually these polynomials even
generalize the Kostka-Foulkes polynomials. They have two variables,
denoted by $q$ and $t$, and one obtains the Kostka-Foulkes polynomials
by replacing $q$ with $0$. Currently the Kostka-Macdonald polynomials
are not used in the **jack** package.

The skew generalizations are also available in the **jack** package:
skew Kostka-Jack numbers, skew Kostka-Foulkes polynomials, and skew
Kostka-Macdonald polynomials.

## References

- I.G. Macdonald. *Symmetric Functions and Hall Polynomials*. Oxford
  Mathematical Monographs. The Clarendon Press Oxford University Press,
  New York, second edition, 1995.

- J. Demmel and P. Koev. *Accurate and efficient evaluation of Schur and
  Jack functions*. Mathematics of computations, vol. 75, n. 253,
  223-229, 2005.

- The symmetric functions catalog.
  <https://www.symmetricfunctions.com/index.htm>.

<!-- -------------------- links -------------------- -->
