\name{print.fechner}
\alias{print.fechner}
\title{Print Method for Objects of Class fechner}
\description{
  S3 method to print objects of the class \code{fechner}.
}
\usage{
\method{print}{fechner}(x, \dots)
}
\arguments{
  \item{x}{a required object of class \code{fechner}, obtained from a
    call to the function \code{\link{fechner}}.}

  \item{\dots}{further arguments to be passed to or from other
    methods.  They are ignored in this function.}
}
\details{
  The \code{print} method prints the main results obtained from
  Fechnerian scaling analyses, which are the overall Fechnerian
  distances and the geodesic loops.
}
\value{
  If the argument \code{x} is of required type, \code{print.fechner}
  prints the overall Fechnerian distances and the geodesic loops, and
  invisibly returns \code{x}.
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
## overall Fechnerian distances and geodesic loops are printed
(f.scal.wish <- fechner(wish))
}
\keyword{methods}
\keyword{print}
