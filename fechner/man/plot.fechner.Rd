\name{plot.fechner}
\alias{plot.fechner}
\title{Plot Method for Objects of Class fechner}
\description{
  S3 method to plot objects of the class \code{fechner}.
}
\usage{
\method{plot}{fechner}(x, level = 2, \dots)
}
\arguments{
  \item{x}{a required object of class \code{fechner}, obtained from a
    call to the function \code{\link{fechner}}.}

  \item{level}{an optional numeric, integer-valued and greater than or
    equal to \eqn{2}, giving the level of comparison of the
    \eqn{S}-index and the overall Fechnerian distance \eqn{G}.}

  \item{\dots}{further arguments to be passed to or from other
    methods.  They are ignored in this function.}
}
\details{
  The \code{plot} method graphs the results obtained from Fechnerian
  scaling analyses.  It produces a scatterplot of the overall
  Fechnerian distance \eqn{G} versus the \eqn{S}-index, with rugs
  added to the axes and jittered (\code{amount = 0.01} of noise) to
  accommodate ties in the \eqn{S}-index and \eqn{G} values.  The
  diagonal line \eqn{y = x} is for visual inspection of the deviations
  of the two types of values.

  The \code{level} of comparison refers to the minimum number of links
  in geodesic loops.  That is, choosing level \eqn{n} means that
  comparison involves only those \eqn{S}-index and \eqn{G} values that
  have geodesic loops containing not less than \eqn{n} links.

  If there are no (off-diagonal) pairs of stimuli with geodesic loops
  containing at least \code{level} links (in this case a plot is not
  possible), \code{plot.fechner} stops with an error message.
}
\value{
  If the arguments \code{x} and \code{level} are of required types,
  and if there are (off-diagonal) pairs of stimuli with geodesic loops
  containing at least \code{level} links, \code{plot.fechner} produces
  a plot, and invisibly returns \code{NULL}.
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
  \code{\link{print.fechner}}, the S3 method for printing objects of
  the class \code{fechner}; \code{\link{summary.fechner}}, the S3
  method for summarizing objects of the class \code{fechner}, which
  creates objects of the class \code{summary.fechner};
  \code{\link{print.summary.fechner}}, the S3 method for printing
  objects of the class \code{summary.fechner}; \code{\link{fechner}},
  the main function for Fechnerian scaling, which creates objects of
  the class \code{fechner}.  See also \code{\link{fechner-package}}
  for general information about this package.
}
\examples{
## Fechnerian scaling of dataset \link{wish}
f.scal.wish <- fechner(wish)

## results are plotted for comparison levels 2 and 5
plot(f.scal.wish)
plot(f.scal.wish, level = 5)
}
\keyword{hplot}
\keyword{methods}
\keyword{print}
