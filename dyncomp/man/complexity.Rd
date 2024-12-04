\name{complexity}
\alias{complexity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate dynamic complexity of time series
%%  ~~function to do ... ~~
}
\description{
A function to calculate the dynamic complexity of a series of observations, resulting from the degree of fluctuation and the degree of scattering. This measure is calculated in moving windows of a specified width, resulting in a series of values of a length equal to the length of the series of observations.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~

}

\usage{complexity(x, scaleMin, scaleMax, width = 7, measure = "complexity", rescale = FALSE)}

\arguments{
  \item{x}{
  The data to be used (representing a series of observations).
%%     ~~Describe \code{x} here~~
}
  \item{scaleMin}{
  Theoretical minimum of the data. Will default to the observed minimum of x.
%%     ~~Describe \code{scaleMin} here~~
}
  \item{scaleMax}{
  Theoretical maximum of the data. Will default to the observed maximum of x.
%%     ~~Describe \code{scaleMax} here~~
}
  \item{width}{
  Width of the moving window. Default is 7.
%%     ~~Describe \code{width} here~~
}
  \item{measure}{
  Either "complexity", "fluctuation" or "distribution". Indicates which value should be returned. Default is "complexity".
%%     ~~Describe \code{measure} here~~
}

\item{rescale}{
If TRUE, rescales the returned values to scale minimum and maximum. This is sometimes useful for graphical interpretation or plotting. Default: FALSE.
}

}

\author{
Tim Kaiser <Tim.Kaiser@sbg.ac.at>
%%  ~~who you are~~
}

\references{Kaiser, T. (2017). dyncomp: an R package for Estimating the Complexity of Short Time Series. DOI 10.17605/OSF.IO/GWTKX}

\examples{
t <- runif(100, 0, 10)
c <- complexity(x = t, scaleMin = 0, scaleMax = 10, width = 5, measure = "complexity",
rescale = TRUE)
plot(t, type = "l")
lines(c, col = "red", lty = 4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
