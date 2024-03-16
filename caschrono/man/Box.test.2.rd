\name{Box.test.2}
\alias{Box.test.2}
\title{Portemanteau tests}
\description{\code{Box.test.2} computes at different lags, a Portemanteau statistic 
for testing that a time series is a white noise.}
\usage{
Box.test.2(x, nlag, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0, decim = 8)
}
\arguments{
 \item{x}{a time series object}
 \item{nlag}{a vector of integers: the lags where the statistic are computed}
 \item{type}{test to be performed}
 \item{fitdf}{number of degrees of freedom to be subtracted if x is a series of residuals}
 \item{decim}{an integer, the precision of the results}
}
\details{This function uses the \code{Box.test}.}
\value{
It returns a matrix of size \code{nlag} x 2 with the statistics and the p-value 
}

\author{Yves Aragon}

\examples{
set.seed(123)
y1 <- arima.sim(n = 100, list(ar = -.7), sd = sqrt(4))
a1 <- Box.test.2(y1, nlag = c(3, 6, 9, 12), type = "Ljung-Box", decim = 4)
}
\keyword{ts}
