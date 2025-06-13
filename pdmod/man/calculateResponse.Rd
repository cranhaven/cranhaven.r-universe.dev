\name{calculateResponse}
\alias{calculateResponse}
\title{Calculate response from the estimate}
\usage{
  calculateResponse(k, rmax, est)
}
\arguments{
  \item{k}{Response rate parameter}

  \item{rmax}{Maximum response}

  \item{est}{Vector of estimates}
}
\value{
  Vector of responses
}
\description{
  Given an estiamtes probability of reward beween 0 and 1,
  calculates a response rate (i.e. the measured response of
  the animal such as visits to the food delivery system)
}
\examples{
calculateResponse(0.8, 10, runif(20))
}
\author{
  Chloe Bracis
}
\seealso{
  \code{\link{Constants}}, \code{\link{isTimedVector}},
  \code{\link{verifyTimedVector}}
}

