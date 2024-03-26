\name{sim.xy}
\docType{data}
\alias{sim.xy}
\title{High-5 and Low setting data}
\description{
  Simmulated data for the high-5-sized scenario and low-sized. It contains a list
  with the who cases, each of them with the following fields:
  * \code{x} X matrix for training set
  * \code{y} Y vector for training set
  * \code{xval} X matrix for validation set
  * \code{yval} Y vector for validation set
  * \code{beta} true beta array
}
\usage{data("sim.xy")}
\source{https://github.com/ryantibs/best-subset}
\references{
  Hastie, Trevor, Robert Tibshirani, and Ryan J. Tibshirani. "Extended comparisons of best subset selection, forward stepwise selection, and the lasso." arXiv preprint arXiv:1707.08692 (2017).
}
\keyword{datasets}