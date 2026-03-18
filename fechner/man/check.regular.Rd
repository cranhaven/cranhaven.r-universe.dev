\name{check.regular}
\alias{check.regular}
\concept{regular minimality}
\concept{regular maximality}
\concept{probability different data format}
\concept{percent same data format}
\title{Check for Regular Minimality/Maximality}
\description{
  \code{check.regular} is used to check whether the data satisfy
  regular minimality/maximality.
}
\usage{
check.regular(X, type = c("probability.different", "percent.same",
                          "reg.minimal", "reg.maximal"))
}
\arguments{
  \item{X}{a required square matrix or data frame of numeric
    data.  No \code{NA}, \code{NaN}, \code{Inf}, or \code{-Inf}
    values are allowed.}

  \item{type}{an optional character string giving the type of check to
    be performed.  This must be one of \code{"probability.different"},
    \code{"percent.same"}, \code{"reg.minimal"}, or
    \code{"reg.maximal"}, with default \code{"probability.different"},
    and may be abbreviated to a unique prefix.}
}
\details{
  The \code{type} argument specifies whether regular minimality or
  regular maximality is to be checked.  \code{"probability.different"}
  and \code{"percent.same"} are for datasets in the
  probability-different and percent-same formats, and imply regular
  minimality and regular maximality checks, respectively.
  \code{"reg.minimal"} and \code{"reg.maximal"} can be specified to
  force checking for regular minimality and regular maximality,
  respectively, independent of the used dataset.  In particular,
  \code{"reg.minimal"} and\code{"reg.maximal"} are to be used for
  datasets that are properly in the general format.

  \code{check.regular} calls \code{\link{check.data}}.  In
  particular, the rows and columns of the canonical
  representation matrix (see \sQuote{Value}) are canonically
  relabeled based on the labeling provided by
  \code{\link{check.data}}.  That is, using the
  \code{\link{check.data}} labeling, the pairs of points of subjective
  equality (PSEs) are assigned identical labels, leaving intact the
  labeling of the rows and relabeling the columns with their
  corresponding PSEs.  If the data \code{X} do not satisfy regular
  minimality/maximality, \code{check.regular} produces respective
  messages.  The latter give information about parts of \code{X}
  violating that condition.

  Regular minimality/maximality is a fundamental property of
  discrimination and means that 
  \enumerate{
    \item every row contains a single minimal/maximal entry;
    \item every column contains a single minimal/maximal entry;
    \item an entry \eqn{p_{ij}}{p\_ij} of \code{X} which is 
      minimal/maximal in the \eqn{i}{i}th row is also minimal/maximal
      in the \eqn{j}{j}th column, and vice versa.
  }

  If \eqn{p_{ij}}{p\_ij} is the entry which is minimal/maximal in
  the \eqn{i}{i}th row and in the \eqn{j}{j}th column, the
  \eqn{i}{i}th row object (in one, the first, observation area) and
  the \eqn{j}{j}th column object (in the other, the second,
  observation area) are called each other's PSEs.  In psychophysical
  applications, for instance, observation area refers to the two fixed
  and perceptually distinct areas in which the stimuli are presented
  pairwise; for example, spatial arrangement (left versus right) or
  temporal order (first versus second).
}
\value{
  If the data do satisfy regular minimality/maximality,
  \code{check.regular} returns a named list consisting of the
  following four components:

  \item{canonical.representation}{a matrix giving the
    representation of \code{X} in which regular 
    minimality/maximality is satisfied in the canonical form.
    That is, the single minimal/maximal entries of the rows and
    columns lie on the main diagonal (of the canonical
    representation).  In addition, the rows and columns are
    canonically relabeled.}
  \item{canonical.transformation}{a data frame giving the
    permutation of the columns of \code{X} used to produce the
    canonical representation of \code{X}.  The first and second
    variables of this data frame, \code{observation.area.1} and
    \code{observation.area.2}, respectively, represent the pairs
    of PSEs.  The third variable, \code{common.label}, lists the
    identical labels assigned to the pairs of PSEs.}
  \item{check}{a character string giving the check that was
    performed.  This is either \code{"regular minimality"} or
    \code{"regular maximality"}.}
  \item{in.canonical.form}{logical.  If \code{TRUE}, the
    permutation of the columns used to obtain the canonical
    representation of \code{X} is the identity; that is, the
    original data \code{X} are already in the canonical form.}
}
\author{
  Thomas Kiefer, Ali Uenlue.  Based on original MATLAB source by Ehtibar N. Dzhafarov.
}
\references{
  Dzhafarov, E. N. and Colonius, H. (2006) Reconstructing
  distances among objects from their discriminability.
  \emph{Psychometrika}, \bold{71}, 365--386.

  Dzhafarov, E. N. and Colonius, H. (2007) Dissimilarity
  cumulation theory and subjective metrics. \emph{Journal of
  Mathematical Psychology}, \bold{51}, 290--304.

  Uenlue, A. and Kiefer, T. and Dzhafarov, E. N.
  (2009) Fechnerian scaling in \R: The package \pkg{fechner}.
  \emph{Journal of Statistical Software}, \bold{31}(6), 1--24.
  URL \url{http://www.jstatsoft.org/v31/i06/}.
}
\seealso{
  \code{\link{check.data}} for checking data format;
  \code{\link{fechner}}, the main function for Fechnerian scaling.
  See also \code{\link{fechner-package}} for general information
  about this package.
}
\examples{
## dataset \link{wish} satisfies regular minimality in canonical form
check.regular(wish)

## dataset \link{regMin} satisfies regular minimality in non-canonical
## form and so is canonically transformed and relabeled
regMin
check.regular(regMin)

## dataset \link{noRegMin} does satisfy neither regular minimality nor
## regular maximality
check.regular(noRegMin, type = "probability.different")
check.regular(noRegMin, type = "reg.maximal")

## dataset \link{morse} satisfies regular maximality in canonical form
check.regular(morse, type = "percent.same")
## part of \link{morse} data satisfies regular maximality
check.regular(morse[c(2, 27:36), c(2, 27:36)], type = "reg.maximal")
}
\keyword{manip}
\keyword{utilities}
