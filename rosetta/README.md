
<!-- badges: start -->

[![Pipeline
status](https://gitlab.com/r-packages/rosetta/badges/prod/pipeline.svg)](https://gitlab.com/r-packages/rosetta/-/commits/prod)
[![Coverage
status](https://codecov.io/gl/r-packages/rosetta/branch/prod/graph/badge.svg)](https://app.codecov.io/gl/r-packages/rosetta?branch=prod)
[![Version on
CRAN](https://www.r-pkg.org/badges/version/rosetta?color=brightgreen)](https://cran.r-project.org/package=rosetta)
[![Version on
CRAN](https://cranlogs.r-pkg.org/badges/last-month/rosetta?color=brightgreen)](https://cran.r-project.org/package=rosetta)
[![Version on
CRAN](https://cranlogs.r-pkg.org/badges/grand-total/rosetta?color=brightgreen)](https://cran.r-project.org/package=rosetta)
<!-- badges: end -->

# <img src='man/figures/logo.png' align="right" height="120" /> rosetta 📦

rosetta: Parallel Use of Statistical Packages in Teaching

The pkgdown website for this project is located at
<https://r-packages.gitlab.io/rosetta/>.

<!--------------------------------------------->
<!-- Start of a custom bit for every package -->
<!--------------------------------------------->

When teaching statistics, it can often be desirable to uncouple the
content from specific software packages. To ease such efforts, the
Rosetta Stats website (<https://rosettastats.com>) allows comparing
analyses in different packages. This package is the companion to the
Rosetta Stats website, aiming to provide functions that produce output
that is similar to output from other statistical packages, thereby
facilitating ‘software-agnostic’ teaching of statistics.

Rosetta Stats is a statistics chrestomathy, conceptually based on
Rosetta Code; this is its R companion package. Rosetta Stats is meant to
illustrate how common analyses can be conducted in a variety of
statistical software packages. It was founded mainly to facilitate using
SPSS and R in parallel or to facilitate switching from SPSS to R. It is
geared towards application of statistics to psychological science.

<!--------------------------------------------->
<!--  End of a custom bit for every package  -->
<!--------------------------------------------->

## Installation

You can install the released version of `rosetta` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages('rosetta');
```

You can install the development version of `rosetta` from
[GitLab](https://about.gitlab.com) with:

``` r
remotes::install_gitlab('r-packages/rosetta');
```

If you want the even more cutting edge version, you can install from the
`dev` branch (as opposed to the default branch, `prod`) with:

``` r
remotes::install_gitlab('r-packages/rosetta@dev');
```

(This is assuming you have the `remotes` packages installed; otherwise,
install that first using the `install.packages` function.)
