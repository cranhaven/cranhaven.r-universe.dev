\name{injection}
\alias{injection}
\docType{data}
\title{Injection Moulding data
}
\description{
Engel (1992) presented data from an injection-moulding experiment. 
An industrial Taguchi experiment was performed to study
the in uence of several controllable factors on the mean value and the
variation in the percentage of shrinkage of products made by injection
moulding. The responses y were percentages of shrinkage of products
made by injection moulding. There are seven controllable factors (A-G),
in a 2^(7-4) fractional factorial design. There are 8 combinations of
levels for seven controllable factors (A-G) to allow the estimates of seven
main effects for controllable factors. At each setting of the controllable
factors, 4 observations were obtained from a 2^(3-1) fractional factorial
with three noise factors (M-O).
}
\usage{data("injection")}
\format{
  A data frame with 32 observations on the following 11 variables.
  \describe{
    \item{\code{y}}{percentages of shrinkage of products}
    \item{\code{a}}{a controllable factor A with levels \code{1} and \code{2}}
    \item{\code{b}}{a controllable factor B with levels \code{1} and \code{2}}
    \item{\code{c}}{a controllable factor C with levels \code{1} and \code{2}}
    \item{\code{d}}{a controllable factor D with levels \code{1} and \code{2}}
    \item{\code{e}}{a controllable factor E with levels \code{1} and \code{2}}
    \item{\code{f}}{a controllable factor F with levels \code{1} and \code{2}}
    \item{\code{g}}{a controllable factor G with levels \code{1} and \code{2}}
    \item{\code{m}}{a noise factor M with levels \code{1} and \code{2}}
    \item{\code{n}}{a noise factor N with levels \code{1} and \code{2}}
    \item{\code{o}}{a noise factor O with levels \code{1} and \code{2}}
  }
}
\references{
Engel, J. (1992). Modelling variation in industrial experiments. Applied
Statistics, 41, 579--593.
}

