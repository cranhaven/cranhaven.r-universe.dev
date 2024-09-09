\name{sureEst}
\alias{sureEst}
\title{
SURE model parameter estimation
}
\description{
Estimate parameters in a SURE model.
}
\usage{
sureEst(y, x, v, sigma, iter=10000, tol=1e-12)
}
\arguments{
  \item{y}{
a n by p matrix, whose columns are dependent variables.
}
  \item{x}{
a n by m matrix, whose columns are predictor variables to select from.
}
  \item{v}{
a list; v[[j]] indicates which x's in the model for y[,j].
}
  \item{sigma}{
initial residual variance-covarance matrix (if given).
}
  \item{iter}{
maximum number of iterations in a numerical process to estimate model parameters.
}
  \item{tol}{
convergence tolerance.
}
}

\value{
a list with the following components:
  \item{loglik}{log-likelihood of the model}
  \item{b}{estimates of model coefficients}
  \item{sigma}{estimated residual variance-covariance}
  \item{fitted.values}{fitted mean values}
}

\examples{
data(etrait)
x<- as.matrix(mdat-1/2)
y<- as.matrix(traits)[,1:3]
v<- list(c(1,25,50),numeric(0),3)
\dontrun{
o<- sureEst(y, x, v=v, iter=250, tol=1e-12)
}
}

