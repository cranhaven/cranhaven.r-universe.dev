\name{rippl}
\alias{rippl}
\title{
Rippl's method
}
\description{
Computes the Rippl-no-failure storage for given set of discharges and target.
}
\usage{rippl(discharge,target,plot=TRUE)}
\arguments{
\item{discharge}{a vector of natural discharge at the reservoir site.}
\item{target}{a vector of demand time series with length equal that of \code{discharge}. If the time scale doesn't match, the \code{target} will be cycled or truncated.}
\item{plot}{logical: whether plot the Rippl's method process or merely report the result.}
}
\value{
no-failure storage value for the given time series, \code{discharge} and \code{target}.
}
\references{
Rippl, Wengel. The capacity of storage reservoirs for water supply. Van Nostrand's Engineering Magazine (1879-1886) 29.175 (1883): 67.
}
\seealso{\code{\link[WRSS]{sim}}}
\examples{
\dontrun{
rippl(Nile,mean(Nile)*0.95)
}
}
\keyword{ plot }
\keyword{ matrix }