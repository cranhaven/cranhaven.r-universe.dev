\name{noRegMin}
\alias{noRegMin}
\docType{data}
\title{Artificial Data: Regular Minimality Violated}
\description{
  Artificial data of fictitious \sQuote{discrimination probabilities}
  among \eqn{10} fictitious stimuli.
}
\usage{
noRegMin
}
\format{
  The \code{noRegMin} data frame consists of \eqn{10} rows and
  \eqn{10} columns, representing the fictitious stimuli presented in
  the first and second observation area, respectively.  Each number, a
  numeric, in the data frame is assumed to give the relative frequency
  of perceivers scoring \sQuote{different} to the row stimulus
  \sQuote{followed} by the column stimulus.
}
\note{
  This dataset is artificial and included for illustrating regular
  minimality being violated.  It differs from the artificial data
  \code{\link{regMin}} only in the entry in row \#\eqn{9} and column
  \#\eqn{10}.
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
\seealso{
  \code{\link{regMin}} for the other artificial data satisfying
  regular minimality in non-canonical form; \code{\link{check.data}}
  for checking data format; \code{\link{check.regular}} for checking
  regular minimality/maximality; \code{\link{fechner}}, the main
  function for Fechnerian scaling.  See also \code{\link{morse}} for
  Rothkopf's Morse code data, \code{\link{wish}} for Wish's
  Morse-code-like data, and \code{\link{fechner-package}} for general
  information about this package.
}
\examples{
## dataset noRegMin violates regular minimality
noRegMin
check.regular(noRegMin, type = "reg.minimal")
}
\keyword{datasets}
