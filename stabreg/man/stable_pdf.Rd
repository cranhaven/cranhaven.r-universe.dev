
\name{stable_pdf}
\alias{stable_pdf}
\title{Compute values of a (normalized) stable density}
\usage{
stable_pdf(x, a, b)
}
\arguments{
    \item{x}{values at which to evaluate the density}
    \item{a}{alpha value. Either a scalar or vector of length equal to \code{x} if different values for each observations are assumed}
    \item{b}{beta value. Either a scalar or vector of length equal to \code{x} if different values for each observations are assumed}
}
\description{
    Compute stable pdf values. Admissible range for beta is -1 to 1. If \code{b} is not 0, admissible values of alpha are \code{0.5 < a < 0.9} and \code{1.1 < a <= 2}.
}
