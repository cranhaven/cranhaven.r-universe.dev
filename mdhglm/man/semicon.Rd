\name{semicon}
\alias{semicon}
\docType{data}
\title{
Semiconductor Data
}
\description{
This example is taken from Myers et al. (2002). It involves a designed
experiment in a semiconductor plant. Six factors are employed, and it
is of interest to study the curvature or camber of the substrate devices
produced in the plant. There is a lamination process, and the camber
measurement is made four times on each device produced. The goal is
to model the camber taken in 10E-04 in. as a function of the design
variables. Each design variable is taken at two levels and the design is
a 2^(6-2) fractional factorial. The camber measurement is known to be
nonnormal.
}
\usage{data("semicon")}
\format{
  A data frame with 64 observations on the following 8 variables.
  \describe{
    \item{\code{Device}}{Devise number}
    \item{\code{x1}}{a x1 variable with two values \code{-1} and \code{1}}
    \item{\code{x2}}{a x2 variable with two values \code{-1} and \code{1}}
    \item{\code{x3}}{a x3 variable with two values \code{-1} and \code{1}}
    \item{\code{x4}}{a x4 variable with two values \code{-1} and \code{1}}
    \item{\code{x5}}{a x5 variable with two values \code{-1} and \code{1}}
    \item{\code{x6}}{a x6 variable with two values \code{-1} and \code{1}}
    \item{\code{y}}{camber measurement}
  }
}
\references{
Myers, P.H., Montgomery, D.C., and Vining, G.G. (2002). Generalized
Linear Models with Applications in Engineering and the Sciences.
John Wiley & Sons, New York.
}

