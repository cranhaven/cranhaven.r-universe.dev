\name{bacteria}
\alias{bacteria}
\docType{data}
\title{Bacteria Data}
\description{
Tests of the presence of the bacteria H. influenzae in children with otitis media in the Northern
Territory of Australia (Ripley, et al., 2016).
}
\usage{data("bacteria")}
\format{
  A data frame with 220 observations on the following 6 variables.
  \describe{
    \item{\code{y}}{a factor with levels \code{n} (absence of bacteria) and \code{y} (preence of bacteria)}
    \item{\code{ap}}{a factor with levels \code{a} (active) and \code{p} (placebo)}
    \item{\code{hilo}}{a factor with levels \code{hi} (high compliance) and \code{lo} (low compliance)}
    \item{\code{week}}{week of test}
    \item{\code{ID}}{subjects identifier}
    \item{\code{trt}}{a factor with levels \code{placebo}, \code{drug} and \code{drug+}}
  }
}

\references{
Ripley, B., Venables, B., Bates, D.M., Hornik, K., Gebhardt, A. and Firsth, D. (2016). MASS: Support Functions and Datasets for Venables and Ripley's MASS. R package version 7.3-45.
}

