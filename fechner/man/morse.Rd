\name{morse}
\alias{morse}
\docType{data}
\title{Rothkopf's Morse Code Data}
\description{
  \cite{Rothkopf's (1957)} Morse code data of discrimination
  probabilities among \eqn{36} auditory Morse code signals for the
  letters \eqn{A}, \eqn{B}, \ldots, \eqn{Z} and the digits \eqn{0},
  \eqn{1}, \ldots, \eqn{9}.
}
\usage{
morse
}
\format{
  The \code{morse} data frame consists of \eqn{36} rows and \eqn{36}
  columns, representing the Morse code signals for the letters and
  digits \eqn{A}, \ldots, \eqn{Z}, \eqn{0}, \ldots, \eqn{9} presented
  first and second, respectively.  Each number, an integer, in the
  data frame gives the percentage of subjects who responded
  \sQuote{same} to the row signal followed by the column signal.
}
\details{
  Each signal consists of a sequence of dots and dashes.  A chart of
  the Morse code letters and digits can be found at
  \url{http://en.wikipedia.org/wiki/Morse_code}.

  \cite{Rothkopf's (1957)} \eqn{36\times 36}{36x36} Morse code data
  gives the same-different judgements of \eqn{598} subjects in
  response to the \eqn{36\times 36}{36x36} auditorily presented pairs
  of Morse codes.  Subjects who were not familiar with Morse code
  listened to a pair of signals constructed mechanically and separated
  by a pause of approximately \eqn{1.4} seconds.  Each subject was
  required to state whether the two signals presented were the same or
  different.  Each number in the \code{morse} data frame is the
  percentage of roughly \eqn{150} subjects.
}
\note{
  The original \cite{Rothkopf's (1957)} \eqn{36\times 36}{36x36}
  dataset does not satisfy regular maximality.  There are two maximal
  entries in row \#\eqn{2}, of value \eqn{84}, which are
  \eqn{p_{BB}}{p\_BB} and \eqn{p_{BX}}{p\_BX}.  Following the
  argument in \cite{Dzhafarov and Colonius (2006)}, a statistically
  compatible dataset is obtained by replacing the value of
  \eqn{p_{BX}}{p\_BX} with \eqn{83} and leaving the rest of the data
  unchanged.  The latter is the dataset accompanying the package
  \code{fechner}.

  For typographic reasons, it may be useful to consider only a small
  subset of the stimulus set, best, chosen to form a
  \sQuote{self-contained} subspace: a geodesic loop for any two of the
  subset's elements (computed using the complete dataset) is contained
  within the subset.  For instance, a particular self-contained
  \eqn{10}-code subspace of the \eqn{36} Morse codes consists of the
  codes for the letter \eqn{B} and the digits \eqn{0}, \eqn{1},
  \eqn{2}, \eqn{4}, \ldots, \eqn{9} (see \code{\link{fechner}}).
}
\source{
  Rothkopf, E. Z. (1957) A measure of stimulus similarity and errors
  in some paired-associate learning tasks.  \emph{Journal of
  Experimental Psychology}, \bold{53}, 94--101.
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
  \code{\link{check.data}} for checking data format;
  \code{\link{check.regular}} for checking regular
  minimality/maximality; \code{\link{fechner}}, the main function for
  Fechnerian scaling.  See also \code{\link{wish}} for Wish's
  Morse-code-like data, and \code{\link{fechner-package}} for general
  information about this package.
}
\keyword{datasets}
