\name{summary.fechner}
\alias{summary.fechner}
\title{Summary Method for Objects of Class fechner}
\description{
  S3 method to summarize objects of the class \code{fechner}.
}
\usage{
\method{summary}{fechner}(object, level = 2, \dots)
}
\arguments{
  \item{object}{a required object of class \code{fechner}, obtained
    from a call to the function \code{\link{fechner}}.}

  \item{level}{an optional numeric, integer-valued and greater than or
    equal to \eqn{2}, giving the level of comparison of the
    \eqn{S}-index and the overall Fechnerian distance \eqn{G}.}

  \item{\dots}{further arguments to be passed to or from other
    methods.  They are ignored in this function.}
}
\details{
  The \code{summary} method outlines the results obtained from
  Fechnerian scaling analyses.  It computes the Pearson correlation
  coefficient and the \eqn{C}-index (see \cite{Uenlue, Kiefer, and
  Dzhafarov (2009)})
  \deqn{C = \frac{2\sum (S - G)^2}{\sum S^2 + \sum G^2}}{%
        C = (2 * sum((S - G)^2)) / (sum(S^2) + sum(G^2))}
  for specific (controlled by the argument \code{level}) stimuli pairs
  with their corresponding \eqn{S}-index and \eqn{G} values.

  The \code{level} of comparison refers to the minimum number of links
  in geodesic loops.  That is, choosing level \eqn{n} means that
  comparison involves only those \eqn{S}-index and \eqn{G} values that
  have geodesic loops containing not less than \eqn{n} links.

  If there are no (off-diagonal) pairs of stimuli with geodesic loops
  containing at least \code{level} links (in this case a summary is
  not possible), \code{summary.fechner} stops with an error message.

  The function \code{summary.fechner} returns an object of the class
  \code{summary.fechner} (see \sQuote{Value}), for which a
  \code{print} method, \code{\link{print.summary.fechner}}, is
  provided.  Specific summary information details such as individual
  stimuli pairs and their corresponding \eqn{S}-index and \eqn{G}
  values can be accessed through assignment (see \sQuote{Examples}).
}
\value{
  If the arguments \code{object} and \code{level} are of required
  types, and if there are (off-diagonal) pairs of stimuli with
  geodesic loops containing at least \code{level} links,
  \code{summary.fechner} returns a named list, of the class
  \code{summary.fechner}, consisting of the following four components:

  \item{pairs.used.for.comparison}{a data frame giving the pairs
    of stimuli (first variable \code{stimuli.pairs}) and their
    corresponding \eqn{S}-index (second variable \code{S.index}) and
    \eqn{G} (third variable \code{Fechnerian.distance.G}) values used
    for comparison.}
  \item{Pearson.correlation}{a numeric giving the value of the Pearson
    correlation coefficient if it exists, or a character string saying
    \emph{\dQuote{Pearson's correlation coefficient is not defined}}
    if it does not exist.}
  \item{C.index}{a numeric giving the value of the \eqn{C}-index.}
  \item{comparison.level}{a numeric giving the \code{level} of
    comparison used.}
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
  \code{\link{plot.fechner}}, the S3 method for plotting objects of
  the class \code{fechner}; \code{\link{print.fechner}}, the S3 method
  for printing objects of the class \code{fechner};
  \code{\link{print.summary.fechner}}, the S3 method for printing
  objects of the class \code{summary.fechner}; \code{\link{fechner}},
  the main function for Fechnerian scaling, which creates objects of
  the class \code{fechner}.  See also \code{\link{fechner-package}}
  for general information about this package.
}
\examples{
## Fechnerian scaling of dataset \link{wish}
f.scal.wish <- fechner(wish)

## results are summarized for comparison levels 2 and 5
summary(f.scal.wish)
summary(f.scal.wish, level = 5)

## accessing detailed summaries through assignment
str(detailed.summary.l1 <- summary(f.scal.wish))
detailed.summary.l5 <- summary(f.scal.wish, level = 5)
detailed.summary.l5$pairs.used.for.comparison[1, ]

## to verify the obtained summaries
f.scal.wish$geodesic.loops
f.scal.wish$S.index
f.scal.wish$overall.Fechnerian.distances
}
\keyword{methods}
\keyword{print}
\keyword{univar}
