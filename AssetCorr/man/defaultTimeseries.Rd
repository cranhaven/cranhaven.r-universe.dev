\name{defaultTimeseries}
\alias{defaultTimeseries}

\title{Creating a hypothetical Default Time Series.

}
\description{This function return a time series of defaults.

}
\usage{
defaultTimeseries(N, AC, Years, PD)
}

\arguments{
  \item{N}{ Number of obligors for each point in time.

}
  \item{AC}{ Desired asset correlation.

}
  \item{Years}{ Number of points in time, which corresponds to the length of the default time series.

}
  \item{PD}{ Uniform probability of default assumed for each obligor.

}
}
\details{
This function can be used to draw a random default time series, assuming a specific length of the time series, number of obligors, a uniform asset correlation and a uniform probability of default.


}
\value{
The output contains a Nx1-vector with simulated defaults for each point in time.

}

\examples{

D1<-defaultTimeseries(1000,0.1,10,0.01)
}
\keyword{ defaultimeseries }

