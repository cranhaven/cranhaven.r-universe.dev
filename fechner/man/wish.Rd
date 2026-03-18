\name{wish}
\alias{wish}
\docType{data}
\title{Wish's Morse-code-like Data}
\description{
  \cite{Wish's (1967)} Morse-code-like data of discrimination
  probabilities among \eqn{32} auditory Morse-code-like signals.
}
\usage{
wish
}
\format{
  The \code{wish} data frame consists of \eqn{32} rows and \eqn{32}
  columns, representing the Morse-code-like signals (see
  \sQuote{Details}) presented first and second, respectively.  Each
  number, a numeric, in the data frame gives the relative frequency of
  subjects who responded \sQuote{different} to the row signal followed
  by the column signal.
}
\details{
  The \eqn{32} Morse-code-like signals in \cite{Wish's (1967)} study
  were \eqn{5}-element sequences
  \eqn{T_1P_1T_2P_2T_3}{T\_1P_\1T\_2P\_2T\_3}, where \eqn{T} stands
  for a tone (short or long) and \eqn{P} stands for a pause (\eqn{1}
  or \eqn{3} units long).  As in \cite{Dzhafarov and Colonius (2006)},
  the stimuli are labeled \eqn{A}, \eqn{B}, \ldots, \eqn{Z}, \eqn{0},
  \eqn{1}, \ldots, \eqn{5}, in the order they are presented in
  \cite{Wish's (1967)} article.

  \cite{Wish's (1967)} \eqn{32\times 32}{32x32} Morse-code-like data
  gives the same-different judgements of subjects in response to the
  \eqn{32\times 32}{32x32} auditorily presented pairs of codes.
}
\note{
  The original \cite{Wish's (1967)} \eqn{32\times 32}{32x32} dataset
  does not satisfy regular minimality.  There is the entry
  \eqn{p_{TV} = 0.03}{p\_TV = 0.03}, which is the same as
  \eqn{p_{VV}}{p\_VV} and smaller than
  \eqn{p_{TT} = 0.06}{p\_TT = 0.06}.  Following the argument in
  \cite{Dzhafarov and Colonius (2006)}, a statistically compatible
  dataset is obtained by replacing the value of \eqn{p_{TV}}{p\_TV}
  with \eqn{0.07} and leaving the rest of the data unchanged.  The
  latter is the dataset accompanying the package \code{fechner}.

  For typographic reasons, it may be useful to consider only a small
  subset of the stimulus set, best, chosen to form a
  \sQuote{self-contained} subspace: a geodesic loop for any two of the
  subset's elements (computed using the complete dataset) is contained
  within the subset.  For instance, a particular self-contained
  \eqn{10}-code subspace of the \eqn{32} Morse-code-like signals
  consists of \eqn{S}, \eqn{U}, \eqn{W}, \eqn{X}, \eqn{0}, \eqn{1},
  \ldots, \eqn{5} (see \code{\link{fechner}}).
}
\source{
  Wish, M. (1967) A model for the perception of Morse code-like
  signals.  \emph{Human Factors}, \bold{9}, 529--540.
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
  Fechnerian scaling.  See also \code{\link{morse}} for Rothkopf's
  Morse code data, and \code{\link{fechner-package}} for general
  information about this package.
}
\keyword{datasets}
