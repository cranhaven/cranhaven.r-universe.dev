\name{predict.RAP}
\alias{predict.RAP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict method for RAP objects
}
\description{
Obtain prediction based on current estimate of sparse linear regression model
}
\usage{
\method{predict}{RAP}(object, Xnew, ...)
}
\arguments{
  \item{object}{
  Current RAP object
}
  \item{Xnew}{
  New observation from which to predict
}
  \item{...}{
    Additional arguments 
  }
}
\value{
Produces a matrix of predicted values
}
\references{
Monti et al, "A framework for adaptive regularization in streaming Lasso models", 2016
}
\author{
Ricardo Pio Monti 
}
\seealso{
\code{\link{RAP}}, \code{\link{update.RAP}}
}
\examples{
  # 
  library(lars)
  data(diabetes)
  Data = cbind(diabetes$y, diabetes$x)
  # initialize RAP object with a burn in of 50 observations
  R = RAP(X = matrix(diabetes$x[1:50,], nrow=50),
    y = diabetes$y[1:50], r = .995, eps = 0.0005, l0 = .1)
  # make predictions:
  #predict.RAP(object = R, Xnew = diabetes$x[50:70,])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
