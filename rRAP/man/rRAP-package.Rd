\name{rRAP-package}
\alias{rRAP-package}
\alias{rRAP}
\docType{package}
\title{
Real-Time Adaptive Penalization for Streaming Lasso Models
}
\description{
This package provides an implementation of the Real-time adaptive penalization (RAP) algorithm through which to iteratively update a regularization parameter in a streaming context.  
}
\details{
\tabular{ll}{
Package: \tab rRAP\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2016-09-29\cr
License: \tab GPL-2\cr
}
}
\author{
Ricardo Pio Monti
Maintainer: Ricardo Pio Monti <ricardo.monti08@gmail.com>
}
\references{
See Monti et al, "A framework for adaptive regularization in streaming Lasso models", 2016
}
\keyword{ package }
\seealso{
\code{\link{RAP}},  \code{\link{update.RAP}},  \code{\link{predict.RAP}}
}
\examples{
  # Recreate Figure 1 from Monti et al 2016
  library(lars)
  data(diabetes)
  Data = cbind(diabetes$y, diabetes$x)
  # initialize RAP object
  R = RAP(X = matrix(diabetes$x[1,], nrow=1), y = diabetes$y[1], r = .995, eps = 0.0005, l0 = .1)
  # iteratively update:
  \dontrun{
  for (i in 2:nrow(Data)){
    R = update.RAP(RAPobj=R, Ynew = diabetes$y[i], Xnew=matrix(diabetes$x[i,], nrow=1))
  }
  }
}
