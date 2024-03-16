\name{acf2y}
\alias{acf2y}
\title{Plots of the ACF and PACF of a time series}
\description{Plots of the ACF and PACF at the same lags}
\usage{
acf2y(y, lag.max = 40, numer = TRUE)
}
\arguments{
 \item{y}{A time series object}
 \item{lag.max}{An integer, the maximum lag} 
 \item{numer}{A boolean; if TRUE the ACF and PACF are printed}
}
\details{
The ACF and  PACF are plotted with the same scale.}

\value{
if numer=TRUE, it returns the values of ACF and PACF for each lag
}

\author{Yves Aragon, Thibault Laurent}
\seealso{\code{\link{xy.acfb}}}

\references{Shumway R. and Stoffer D., Time Series Analysis and Its Applications - With R Examples, 2nd ed., 2006,
Springer.}

\examples{
data("nottem")
acf2y(nottem)
}
\keyword{ts}
