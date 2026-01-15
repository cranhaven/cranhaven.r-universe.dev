The ‘giacR’ package
================

*R interface to ‘Giac’*

<!-- badges: start -->

[![R-CMD-check](https://github.com/stla/giacR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stla/giacR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

------------------------------------------------------------------------

Giac is a general purpose symbolic algebra software. It powers the
graphical interface Xcas. This package allows to execute Giac commands
in R. You can find the [documentation of Giac
here](https://www-fourier.ujf-grenoble.fr/~parisse/giac/doc/en/cascmd_en/cascmd_en.html).

## Installation

``` r
remotes::install_github("rstudio/chromote")
remotes::install_github("stla/giacR")
```

## Initialisation of a Giac session

The ‘chromote’ package is used to create a Giac session. If the
`find_chrome()` function of ‘chromote’ returns `NULL`, you can set the
path to the Chrome executable (or Chromium, Brave, etc) to the
environment variable `CHROMOTE_CHROME`. Or you can pass it to the the
`Giac$new` function. Since the Chrome executable is in my system path, I
can use `Sys.which("chrome")`.

``` r
library(giacR)
giac <- Giac$new(Sys.which("chrome"))
```

## Examples

### Elementary calculus

``` r
giac$execute("2 + 3/7")
## [1] "17/7=2.42857142857"
```

### Gröbner basis

``` r
giac$execute("gbasis([x^3 - 2*x*y, x^2*y - 2*y^2 + x], [x, y])")
## [1] "[x^2,x*y,2*y^2-x]"
```

### Antiderivative

``` r
giac$execute("integrate(ln(x))")
## [1] "x*ln(x)-x"
```

### Infinite sum

``` r
giac$execute("sum(1/(n^2), n, 1, +(infinity))")
## [1] "1/6*pi^2=1.64493406685"
```

### Exact rational roots of a polynomial

``` r
giac$execute("crationalroot(2*x^3 - 3*x^2 + 8*x - 12)")
## [1] "[2*i,3/2,-2*i]"
```

### Solve a system of equations (and simplify the solutions)

``` r
giac$execute(
  "apply(simplify, solve([x^2+y+z=1, x+y^2+z=1, x+y+z^2=1], [x, y, z]))"
)
## [1] "list[[0,1,0],[1,0,0],[0,0,1],[sqrt(2)-1,sqrt(2)-1,sqrt(2)-1],[-sqrt(2)-1,-sqrt(2)-1,-sqrt(2)-1]]"
```

### Determinant of a matrix with symbolic entries

``` r
giac$execute("det([[1, 2, 3], [3/4, a, b], [c, 4, 5]])")
## [1] "(-6*a*c+10*a+4*b*c-8*b+3)/2"
```

### Check whether a variable occurs in an expression

``` r
giac$execute("has(x*y + u^2*z, u)")
## [1] "3"
```

``` r
giac$execute("has(x*y + u^2*z, w)")
## [1] "0"
```

## Close session

``` r
giac$close()
## [1] TRUE
```

## Blog posts

- [Gröbner implicitization and the ‘giacR’
  package](https://laustep.github.io/stlahblog/posts/giacR01.html)

- [Using implicitization to split a
  ball](https://laustep.github.io/stlahblog/posts/giacR02.html)
