\encoding{UTF-8}
\name{claims}
\alias{claims}
\title{Initial Claims of Unemployment}
\description{
  Weekly number (in thousands) of unemployment insurance claims in the
  US from 7 Jan 1967 until 28 Nov 2009.
}
\usage{claims}
\format{
  A time series with 515 observations; the object is of class \code{tis}
  (time-indexed series).
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
  United States Department of Labor---Employment & Training Administration.
}
\examples{
## de Carvalho et al (2013; Fig 1)
data(claims)
plot(time(claims), claims, type = "l",
     xlab = "Time", ylab = "Initial Claims (in Thousands)")
}
\keyword{Economics}
