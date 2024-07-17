\name{head}
\alias{head.ts}
\alias{tail.ts}
\title{Return the first or last part of time series data}
\description{Return the first of last parts of an object of time series data.}

\usage{
  \method{head}{ts}(x, n = 5, \dots)
  \method{tail}{ts}(x, n = 5, \dots)
}
\arguments{
  \item{x }{ input time seires data.}
  \item{n }{ a single integer for the length or row of returned data}
  \item{\dots }{ additional arguments to be passed.}
}
\details{ The data can be an univariate or multivariate time series data.}
\value{ An object like x but generally smaller.}

\author{Changyou Sun (\email{cs258@msstate.edu})}

\examples{
h1 <- ts(data=cbind(1:24), start=c(2001, 1), frequency=12)
h2 <- ts(data=cbind(1:24, 25:48), start=c(2001, 1), frequency=12)
h3 <- ts(data=cbind(1:4, 5:8, 9:12), start=c(2001, 1), frequency=4)
colnames(h2) <- c("aa", "bb")
colnames(h3) <- c("cc", "dd", "ee")
h1; h2; h3

h1; head(h1); tail(h1, 28)
h2; head(h2); tail(h2, 50)
h3; head(h3, 2); tail(h3); tail(h3, 8)

data(daBed); head(daBed); tail(daBed)
}
\keyword{methods}