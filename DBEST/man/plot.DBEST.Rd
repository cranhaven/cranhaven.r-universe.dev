\name{plot.DBEST}
\alias{plot.DBEST}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Methods for objects of class "DBEST".
}
\description{
Plot methods for objects of class "DBEST".
}
\usage{
%%## S3 method for class 'DBEST':
\method{plot}{DBEST}(x, figure = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
DBEST object.
}
  \item{figure}{
If omitted, the plot function displays 'figure 1' and 'figure 2'. If this option is set to 1, the plot function will display the input data and the estimated trend, plus the trend local change (figure 1). If this option is set to 2, the plot function will display a graph with the decomposition of the time-series, including the actual data, the trend, the seasonal component and the remainder.
}
  \item{...}{further arguments passed to the \code{\link[graphics]{plot}} function.}
}

\author{
Hristo Tomov
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also as \code{\link[DBEST]{DBEST}}
}
\examples{
\dontrun{
# Display figure 1 - the input data and the estimated trend, plus the trend local change.
plot(DBEST.obj, figure=1)

# Display figure 2 - the decomposition of the time-series, including the actual data, 
# the trend, the seasonal component and the remainder.
plot(DBEST.obj, figure=2)

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}% use one of  RShowDoc("KEYWORDS")
