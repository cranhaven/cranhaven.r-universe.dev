\name{aiElas}
\alias{aiElas}
\title{Computing Elasticity for Static or Dynamic AIDS Models}
\description{Calculate expenditure elasticity, Marshalllian price elasticity, Hicksian price elasticity, and their variances for static or dynamic AIDS Models.}

\usage{aiElas(z, ...)}
\arguments{
  \item{z }{ an object of class \code{aiFit} from the function of \code{aiStaFit} or \code{aiDynFit}.}
  \item{\dots }{ additional arguments to be passed to \code{bsTab}, e.g., digits = 3.}
}
\details{Calculate expenditure elasticity, Marshalllian price elasticity, and Hicksian price elasticity for static or dynamic AIDS Models. The related variance, t-ratio, p-value, and significance are also reported.}

\value{
Return a list object with the following components:
  \item{name }{ name of the share variables; the omitted share name is the last one.}
  \item{expen }{ expenditure elasticity and related statistics.}
  \item{marsh }{ Marshalllian price elasticity and related statistics.}
  \item{hicks }{ Hicksian price elasticity and related statistics.}
}

\references{Wan, Y., C. Sun, and D.L. Grebner. 2010. Analysis of import demand for wooden beds in the United States. Journal of Agricultural and Applied Economics 42(4):643-658.}
\author{Changyou Sun (\email{cs258@msstate.edu})}
\seealso{\code{\link{aiStaFit}}; \code{\link{aiDynFit}}.}

\examples{
# see the examples for 'aiDynFit'.
}
\keyword{regression}