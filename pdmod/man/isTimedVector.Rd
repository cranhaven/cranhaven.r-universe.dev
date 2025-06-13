\name{isTimedVector}
\alias{isTimedVector}
\title{Is TimedVector}
\usage{
  isTimedVector(tv)
}
\arguments{
  \item{tv}{Potential \code{TimedVector} object}
}
\value{
  \code{TRUE}, if the object inherits \code{TimedVector}
  \code{FALSE}, otherwise
}
\description{
  Determines if an object inherits
  \code{\link{TimedVector}}
}
\examples{
# A TimedVector
tv = TimedVector(rep(1, 10), 1:10)
isTimedVector(tv)

# Not a TimedVector
isTimedVector(1:10)
isTimedVector(time(tv))
}
\author{
  Chloe Bracis
}
\seealso{
  \code{\link{TimedVector}},
  \code{\link{verifyTimedVector}}
}

