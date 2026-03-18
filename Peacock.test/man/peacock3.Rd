\name{peacock3}
\alias{peacock3}
\title{Three Dimensional Kolmogorov-Smirnov/Peacock Two-Sample test}
\description{
This function implements the original definition of the three-dimensional Kolmogorov-Smirnov test by Peacock (1983).
This test is not the widely used Fasano-Franceschini test (1987). The latter is a variant the Peacock test. 
}
\usage{
peacock3(x, y)
}
\arguments{
  \item{x}{
         x is the object representing the first sample. x should be able to be converted a matrix, 
         where each row represents a sample point. If the object cannot be converted to a matrix, 
         the function will stop and throw an error message. Pleaste note that only the first three columns
         of the matrix will be used, and the rest columns are just ingored.
         }
  \item{y}{Similar to x,  y is the object representing the second sample.}
}

\value{the value of the test statistic}
\author{Yuanhui Xiao}
\examples{
    x <- matrix(rnorm(12, 0, 1), ncol=3)
    y <- matrix(rnorm(18, 0, 1), ncol=3)
    ks <- peacock3(x, y)
    ks 
}
\keyword{Three-dimensional Kolmogorov-Smirnov/Peacock Two-Sample test}
\keyword{Three-dimensional Fasano-Franceschini Two-Sample test}