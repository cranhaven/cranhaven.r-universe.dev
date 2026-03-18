\name{fechner-package}
\alias{fechner-package}
\concept{Fechnerian scaling}
\docType{package}
\title{
  Fechnerian Scaling of Discrete Object Sets: The R Package fechner
}
\description{
  Fechnerian scaling is a procedure for constructing a metric on a set
  of objects (e.g., symbols, X-ray films).  The constructed Fechnerian
  metric represents subjective dissimilarities among the objects as
  perceived by a system (e.g., person, technical device).  The package
  \pkg{fechner} provides functions and example datasets for performing
  and illustrating Fechnerian scaling of discrete object sets in \R.
}
\details{
  \tabular{ll}{
    Package: \tab \pkg{fechner}\cr
    Type: \tab Package\cr
    Version: \tab 1.0-3\cr
    Date: \tab 2016-06-05\cr
    License: \tab \acronym{GPL} (>= 2)
  }
  Fechnerian scaling of discrete object (or stimulus) sets provides a
  theoretical framework for deriving, so-called Fechnerian, distances
  among objects representing subjective dissimilarities.  A Fechnerian
  metric on a set of stimuli is constructed from the probabilities
  with which the objects are discriminated from each other by a
  perceiving system.  In addition to the oriented and overall
  Fechnerian distances, the package \pkg{fechner} also computes such
  related information as the points of subjective equality, the
  psychometric increments, the geodesic chains and loops with their
  corresponding lengths, and the generalized Shepardian
  dissimilarities (or \eqn{S}-index).  Moreover, the package
  \pkg{fechner} provides functions for checking the required data
  format and the fundamental regular minimality/maximality condition.
  These concepts are explained in detail in the paper about the
  \pkg{fechner} package by \cite{Uenlue, Kiefer, and Dzhafarov
  (2009)}, and in the theoretical papers by \cite{Dzhafarov and
  Colonius (2006, 2007)} (see \sQuote{References}).

  The package \pkg{fechner} is implemented based on the S3 system.  It
  comes with a namespace, and consists of three external functions
  (functions the package exports): \code{\link{check.data}},
  \code{\link{check.regular}}, and the main function of this package,
  \code{\link{fechner}}.  It also contains six internal functions
  (functions not exported by the package), which are \code{plot},
  \code{print}, and \code{summary} methods for objects of the class
  \code{fechner}, a \code{print} method for objects of the class
  \code{summary.fechner}, and two functions for computing intermediate
  graph-theoretic information: \code{\link{plot.fechner}},
  \code{\link{print.fechner}}, \code{\link{summary.fechner}},
  \code{\link{print.summary.fechner}}, and \link{fechner-internal}.
  The features of the package \pkg{fechner} are illustrated with
  accompanying two real datasets, \code{\link{morse}} and
  \code{\link{wish}}, and two artificial datasets,
  \code{\link{regMin}} and \code{\link{noRegMin}}.
}
\author{
  Thomas Kiefer, Ali Uenlue.  Based on original MATLAB source by
  Ehtibar N. Dzhafarov.

  Maintainer: Ali Uenlue <ali.uenlue@tum.de>
}
\references{
  Dzhafarov, E. N. and Colonius, H. (2006) Reconstructing distances
  among objects from their discriminability.  \emph{Psychometrika},
  \bold{71}, 365--386.

  Dzhafarov, E. N. and Colonius, H. (2007) Dissimilarity cumulation
  theory and subjective metrics.  \emph{Journal of Mathematical
  Psychology}, \bold{51}, 290--304.

  Uenlue, A. and Kiefer, T. and Dzhafarov, E. N.
  (2009) Fechnerian scaling in \R: The package \pkg{fechner}.
  \emph{Journal of Statistical Software}, \bold{31}(6), 1--24.
  URL \url{http://www.jstatsoft.org/v31/i06/}.
}
\keyword{package}
