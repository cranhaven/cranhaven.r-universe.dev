
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Pipeline
status](https://gitlab.com/r-packages/rock/badges/prod/pipeline.svg)](https://gitlab.com/r-packages/rock/-/commits/prod)
[![Coverage
status](https://codecov.io/gl/r-packages/rock/branch/prod/graph/badge.svg)](https://app.codecov.io/gl/r-packages/rock?branch=prod)
[![Version on
CRAN](https://www.r-pkg.org/badges/version/rock?color=brightgreen)](https://cran.r-project.org/package=rock)
[![Version on
CRAN](https://cranlogs.r-pkg.org/badges/last-month/rock?color=brightgreen)](https://cran.r-project.org/package=rock)
[![Version on
CRAN](https://cranlogs.r-pkg.org/badges/grand-total/rock?color=brightgreen)](https://cran.r-project.org/package=rock)
<!-- [![Dependency status](https://tinyverse.netlify.com/badge/rock)](https://CRAN.R-project.org/package=rock) -->
<!-- badges: end -->

# <img src='img/hex-logo.png' align="right" height="200" /> rock 📦

## Reproducible Open Coding Kit

The pkgdown website for this project is located at
<https://rock.opens.science>. If there is a development version of the
documentation, that is available at <https://r-packages.gitlab.io/rock>.

<!--------------------------------------------->
<!-- Start of a custom bit for every package -->
<!--------------------------------------------->

The Reproducible Open Coding Kit (ROCK, and this package, `rock`) was
developed to facilitate reproducible and open coding, specifically
geared towards qualitative research methods. Although it is a
general-purpose toolkit, three specific applications have been
implemented, specifically an interface to the `rENA` package that
implements Epistemic Network Analysis (ENA), means to process notes from
Cognitive Interviews (CIs), and means to work with a decentralized
construct taxonomy (DCT).

More ROCK-related resources are available through
<https://rock.science>.

<!--------------------------------------------->
<!--  End of a custom bit for every package  -->
<!--------------------------------------------->

## Installation

You can install the released version of `rock` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages('rock');
```

You can install the development version of `rock` from
[GitLab](https://about.gitlab.com/) with:

``` r
remotes::install_gitlab('r-packages/rock');
```

(assuming you have `remotes` installed; otherwise, install that first
using the `install.packages` function)

If you really want to install the most cutting edge version, install the
‘dev’ branch of this repository:

``` r
remotes::install_gitlab('r-packages/rock@dev');
```

<!--------------------------------------------->
<!-- Start of a custom bit for every package -->
<!--------------------------------------------->
<!-- ## References -->
<!-- van Woerkum, C. and Aarts, N. (2012), ‘Accountability: New challenges, new forms’, *Journal of Organizational Transformation & Social Change*, 9, pp. 271–283, \doi{10.1386/jots.9.3.271_1}. -->
<!--------------------------------------------->
<!--  End of a custom bit for every package  -->
<!--------------------------------------------->
<!--  https://stackoverflow.com/questions/4822471/count-number-of-lines-in-a-git-repository    -->
<!--  cloc $(git ls-files) -->
