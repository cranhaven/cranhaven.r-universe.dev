\name{print.summary.fechner}
\alias{print.summary.fechner}
\title{Print Method for Objects of Class summary.fechner}
\description{
  S3 method to print objects of the class \code{summary.fechner}.
}
\usage{
\method{print}{summary.fechner}(x, \dots)
}
\arguments{
  \item{x}{a required object of class \code{summary.fechner}, obtained
    from a call to the function \code{\link{summary.fechner}} (through
    generic function \code{\link[base]{summary}}).}

  \item{\dots}{further arguments to be passed to or from other
    methods.  They are ignored in this function.}
}
\details{
  The \code{print} method prints the summary information about objects
  of the class \code{fechner} computed by
  \code{\link{summary.fechner}}, which are the number of stimuli pairs
  used for comparison, a summary of the corresponding \eqn{S}-index
  values, a summary of the corresponding Fechnerian distance \eqn{G}
  values, the Pearson correlation, the \eqn{C}-index, and the
  comparison level.  Specific summary information details such as
  individual stimuli pairs and their corresponding \eqn{S}-index and
  \eqn{G} values can be accessed through assignment (see
  \sQuote{Examples}).
}
\value{
  If the argument \code{x} is of required type,
  \code{print.summary.fechner} prints the afore mentioned summary
  information in \sQuote{Details}, and invisibly returns \code{x}.
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
  \code{\link{summary.fechner}}, the S3 method for summarizing objects
  of the class \code{fechner}, which creates objects of the class
  \code{summary.fechner}; \code{\link{fechner}}, the main function for
  Fechnerian scaling, which creates objects of the class
  \code{fechner}.  See also \code{\link{fechner-package}} for general
  information about this package.
}
\examples{
## Fechnerian scaling of dataset \link{morse}
## summary information about the Fechnerian scaling object are printed
## accessing detailed summary through assignment
(detailed.summary <- summary(fechner(morse, format = "percent.same")))
str(detailed.summary)
detailed.summary$pairs.used.for.comparison[3, ]
}
\keyword{methods}
\keyword{print}
