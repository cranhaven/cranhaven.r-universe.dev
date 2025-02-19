\encoding{UTF-8}
\name{unemployment}
\alias{unemployment}
\title{US Unemployment Rate}
\description{
  US monthly unemployment rate from January 1967 to November 2009; 
  the 515 monthly observations are seasonally adjusted.
}
\usage{unemployment}
\format{
  A time series with 515 observations; the object is of class \code{ts}.
}
\references{
  de Carvalho, M., Turkman, K. F. and Rua, A. (2013)
  Dynamic threshold modelling and the US business cycle.
  \emph{Journal of the Royal Statistical Society, Ser.} C, \bold{62}, 535-550.
}
\seealso{
  \url{https://www.maths.ed.ac.uk/~mdecarv/decarvalho2013ash.html}
}
\source{
  Bureau of Labor Statistics.
}
\examples{
## de Carvalho et al (2013; Fig. 1)
data(unemployment)
plot(unemployment, xlab = "Time", ylab = "Unemployment Rate")
}

\keyword{Economics}
