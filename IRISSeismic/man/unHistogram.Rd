\name{unHistogram}

\alias{unHistogram}

\title{Histogram to Vector}

\description{
If \code{vec} represents a set of binned counts of incrementing values (ascending)
return a vector of associated bin values with the proper count of each value. Intended 
for internal use.
}

\usage{
unHistogram(vec, startVal, incr)
}

\arguments{
  \item{vec}{a histogram vector or ordered set of binned counts}
  \item{startVal}{the initial value of the first bin element}
  \item{incr}{the increment rate of each subsequent bin value}
}

%% \details{ }

\value{
A vector of bin values with appropriate counts of each.
}

%% \references{ }

\author{
Rob Casey \email{rob@iris.washington.edu}
}

%% \note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{ }

\keyword{methods}
