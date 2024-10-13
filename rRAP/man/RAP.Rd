\name{RAP}
\alias{RAP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Initialization of a RAP object
}
\description{
This function initializes and RAP object. This contains a Lasso regression model together with methods to iteratively update the regularization parameter.
}
\usage{
RAP(X, y, r = 0.95, eps = 0.01, l0 = 0.1, Approx = FALSE)
}
\arguments{
  \item{X}{
Burn in training data. Can either be a single observation (in this case a matrix with 1 row) or several.
This must be a matrix.
}
  \item{y}{
Burn in response data
}
  \item{r}{
Fixed forgetting factor used to update 
}
  \item{eps}{
Fixed stepsize used to update regularization parameter
}
  \item{l0}{
Initial guess for regularization parameter
}
  \item{Approx}{
Boolean indicating whether exact or approximate gradient should be calculated when updating regularization parameter.
}
}
\details{
See Monti et al, "A framework for adaptive regularization in streaming Lasso models", 2016
}
\value{
A RAP object is returned with the following elements:
\item{r}{Fixed forgetting factor}
\item{eps}{Stepsize used to update regularization parameter}
\item{w}{Current measure of effective sample size}
\item{xbar}{}
\item{St}{}
\item{regParam}{Current estimate of regularization parameter}
\item{l1Track}{Vector storing all past estimates of regularization parameter}
\item{beta}{Current estimate of regression coefficients}
\item{Approx}{Boolean indicating if exact or approximate gradients where employed}

The object has the following methods:
\item{update}{Update regularization parameters and regression coefficients based on new data}
\item{predict}{Predict based on current model}
}
\references{
Monti et al, "A framework for adaptive regularization in streaming Lasso models", 2016
}
\author{
Ricardo Pio Monti
}
\note{
Warning that this implementation uses the shooting algorithm (co-ordinate gradient descent) to update regression coefficients. 
A more efficient implementation would employ stochastic gradient descent. 
}
\seealso{
\code{\link{update.RAP}}, \code{\link{update.RAP}}
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
    R = update.RAP(RAPobj=R, Ynew = diabetes$y[i], Xnew=matrix(diabetes$x[i,], nrow=1))
  }
  }
}

