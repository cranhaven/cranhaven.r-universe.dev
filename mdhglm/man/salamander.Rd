\name{salamander}
\alias{salamander}
\docType{data}
\title{
Salamander Data
}
\description{
McCullagh and Nelder (1989) presented a data set on salamander mating.
Three experiments were conducted: two were done with the same
salamanders in the summer and autumn and another one in the autumn
of the same year using different salamanders. The response variable is binary,
indicating success of mating. In each experiment, 20 females and 20
males from two populations called whiteside, denoted by W, and rough
butt, denoted by R, were paired six times for mating with individuals
from their own and the other population, resulting in 120 observations
in each experiment.
}
\usage{data("salamander")}
\format{
  A data frame with 360 observations on the following 7 variables.
  \describe{
    \item{\code{Season}}{a factor with levels \code{Fall} and \code{Summer}}
    \item{\code{Experiment}}{experiment number}
    \item{\code{TypeM}}{a factor for male with levels \code{R} and \code{W}}
    \item{\code{TypeF}}{a factor for female with levels \code{R} and \code{W}}
    \item{\code{Male}}{males identifier for 20 males}
    \item{\code{Female}}{females idenfier for females}
    \item{\code{Mate}}{1=success of mating, 0=failness of mating}
  }
}
\source{
McCullagh, P. and Nelder, J.A. (1989). Generalized Linear Models.
Chapman and Hall, London.
}

