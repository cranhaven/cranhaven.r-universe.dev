\name{update.RAP}
\alias{update.RAP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update sparsity parameter and regression coefficients
}
\description{
Update regularization parameter and the associated Lasso regression coefficients,
Updates can either be mini-batch or single observations.
}
\usage{
\method{update}{RAP}(object, Ynew, Xnew, ...)
}
\arguments{
  \item{object}{
 Current RAP object
}
  \item{Ynew}{
New response. In the case of mini-batch updates a vector should be provided.
}
  \item{Xnew}{
New covariates. This should be a matrix.
}
  \item{...}{
    Additional arguments 
  }
}
\details{
See Monti et al 2016
}
\value{
A RAP objecti is returned where the regularization parameter and the estimated regression coefficients have been updated.
}
\references{
See Monti et al, "A framework for adaptive regularization in streaming Lasso models", 2016
}
\author{
Ricardo Pio Monti
}
\note{
Warning that this implementation uses the shooting algorithm (co-ordinate gradient descent) to update regression coefficients. 
A more efficient implementation would employ stochastic gradient descent. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{RAP}}, \code{\link{predict.RAP}}
}
\examples{
  # Recreate Figure 1 from 
  library(lars)
  data(diabetes)
  Data = cbind(diabetes$y, diabetes$x)
  # initialize RAP object
  R = RAP(X = matrix(diabetes$x[1,], nrow=1), y = diabetes$y[1], r = .995, eps = 0.0005, l0 = .1)
  # iteratively update:
  \dontrun{
  for (i in 2:nrow(Data)){
    R = update.RAP(object=R, Ynew = diabetes$y[i], Xnew=matrix(diabetes$x[i,], nrow=1))
  }
  }
}

