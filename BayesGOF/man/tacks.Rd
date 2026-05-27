\name{tacks}
\alias{tacks}
\docType{data}
\title{ Rolling Tacks Data
}
\description{
An experiment that requires a common thumbtack to be "flipped" \eqn{n=9} times.  Out of these total number of flips, \eqn{y} is the total number of times that the thumbtack landed point up.
}
\usage{data("tacks")}
\format{
  A data frame of \eqn{(y_i, n_i)} for \eqn{i = 1,...,320}.
  \describe{
    \item{\code{y}}{number of times a thumbtack landed point up in the \eqn{i^{th}} trial}
    \item{\code{n}}{total number of flips for the thumbtack in the \eqn{i^{th}} trial}
  }
}
\source{
Beckett, L. and Diaconis, P., 1994. "Spectral analysis for discrete longitudinal data," \emph{Advances in Mathematics, 103}(1), pp. 107-128.
}

\keyword{datasets}
