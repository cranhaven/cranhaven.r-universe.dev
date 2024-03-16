\name{plot2acf}
\alias{plot2acf}
\title{ACF plots of two series}
\description{Plots the ACF of two series at the same lags}
\usage{
plot2acf(y1, y2, lag.max = 40, main = c("", ""))
}
\arguments{
 \item{y1}{A time series object}
 \item{y2}{A time series object}
 \item{lag.max}{An integer, the value of the maximum lag} 
 \item{main}{A vector of character, the title of the plot}
}
\details{
We keep the same scale for the two graphs}

\value{
no value
}

\author{Yves Aragon and Thibault Laurent}

\examples{
data(nottem)
set.seed(2561)
innov1 <- rnorm(290,sd=4.18)
y <- arima.sim(list(order = c(12, 0, 1), ma = -.7, ar = c(rep(0, 11), .9)),
               innov = innov1, n.start = 50, n = 240) + 50
plot2acf(nottem, y, main = c("ACF nottem", "ACF SAR"))
}
\keyword{ts}
