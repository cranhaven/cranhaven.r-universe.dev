\name{aiDiag}
\alias{aiDiag}
\title{Diagnostic Statitics for Static or Dynamic AIDS Model}
\description{Report a set of diagnostic statistics for static or dynamic AIDS models}
\usage{aiDiag(x, digits = 3, ...)}
\arguments{
  \item{x }{ an object of class \code{aiFit} from the function of \code{aiStaFit} or \code{aiDynFit}.}
  \item{digits }{ number of digits used in rounding outputs.}
  \item{\dots }{ additional arguments to be passed.}
}

\details{Compute several diagnostic statistics for each equation in a AIDS model. Tests includes are BG, BP, RESET, and JB. See the reference paper for detail.}
\value{Return a data frame object with the statistics and p values for the four tests by equation.}

\references{Wan, Y., C. Sun, and D.L. Grebner. 2010. Analysis of import demand for wooden beds in the United States. Journal of Agricultural and Applied Economics 42(4):643-658.}

\author{Changyou Sun (\email{cs258@msstate.edu})}
\seealso{\code{\link{aiStaFit}}; \code{\link{aiDynFit}}.}

\examples{
# see the examples for 'aiDynFit'.
}
\keyword{regression}