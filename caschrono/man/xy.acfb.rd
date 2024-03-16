\name{xy.acfb}
\alias{xy.acfb}
\title{Representation of a time series and its  ACF and  PACF}
\description{\code{xy.acfb} plots a  time series and its  ACF and  PACF at the same lags.}
\usage{
xy.acfb(y, lag.max=40, numer=TRUE)
}
\arguments{
 \item{y}{A time series object}
 \item{lag.max}{An integer, the value of the maximum lag} 
 \item{numer}{A boolean, \code{=TRUE} for printing the value of ACF and PACF by lag}
}
\details{
We keep the same scale for the ACF and the PACF}

\references{Shumway R. and Stoffer D., Time Series Analysis and Its Applications - With R Examples, 2nd ed., 2006,
Springer.}

\value{
if \code{numer=TRUE}, it prints the values of ACF and PACF for each lag
}

\author{Yves Aragon and Thibault Laurent}
\seealso{\code{\link{acf2y}}}

\examples{
data(nottem)
xy.acfb(nottem)
}
\keyword{ts}
