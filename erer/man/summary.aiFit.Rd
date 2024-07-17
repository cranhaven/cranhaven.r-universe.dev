\name{summary.aiFit}
\alias{summary.aiFit}
\title{Summary of Results from Static or Dynamic Models}
\description{This summarizes the main results from AIDS models.}
\usage{\method{summary}{aiFit}(object, digits=3, \dots)}
\arguments{
  \item{object}{an object of class \code{aiFit} from the function of \code{aiStaFit} or \code{aiDynFit}.}
  \item{digits}{number of digits for rounding outputs}
  \item{\dots}{additional arguments to be passed.}
}
\details{This wraps up the coefficents and statistics from aiFit by equation.}
\value{A data frame object with coefficients and related statistics by equation.}

\author{Changyou Sun (\email{cs258@msstate.edu})}
\seealso{\code{\link{aiStaFit}} and \code{\link{aiDynFit}}.}

\examples{
# see the examples for 'aiDynFit'.
}
\keyword{methods}
