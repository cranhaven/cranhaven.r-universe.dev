\name{sumSqerror}
\alias{sumSqerror}
\title{Calculate sum of squared prediction errors}
\description{

Calculate a weighted sum squared prediction errors for a parameterization.
}
\usage{
    sumSqerror(coefficients, model=NULL, data=NULL, error.weights=NULL)}
\arguments{
    \item{coefficients}{A vector of coefficients (parameters).}
    \item{model}{an object of class TSmodel which gives the structure 
      of the model for which coefficients are used. \code{coef(model)} should 
      be the same length as coefficients.}
    \item{data}{an object of class TSdata which gives the data with 
       which the model is to be evaluated.}
    \item{error.weights}{a vector of weights to be applied to the 
       squared prediction errors.}
}
\value{
The value of the sum squared errors for a prediction horizon given by the 
length of error.weights. Each period ahead is weighted by the corresponding 
weight in error.weights.
}
\details{
This function is primarily for use in parameter optimization,
which requires that an objective function be specified by a vector
of parameters.It returns only the sum of the weighted squared 
errors (eg.for optimization). 
The sample size is determined by TobsOutput(data).
}
\seealso{
\code{\link{l}}
\code{\link{l.SS}}
\code{\link{l.ARMA}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
model <- estVARXls(eg1.DSE.data.diff)
sumSqerror(1e-10 + coef(model), model=TSmodel(model), 
        data=TSdata(model), error.weights=c(1,1,10))
}
\concept{DSE}
\keyword{ts}

