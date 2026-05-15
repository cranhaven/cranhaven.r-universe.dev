\name{estimate.unknownsampvar}
\alias{estimate.unknownsampvar}

\title{
Estimates of variance component, unknown sampling variance, regression coefficients and small area means in Fay Herriot model with unknown sampling variance.
}
\description{
The function returns a list of 5 elements. The first element is  an estimate of the variance component , the second element is an estimate of the parameter related to sampling variance, the third element is a vector of estimates of the regression coefficients  in the Fay-Herriot model, the fourth element is a vector of the predictors pf the small area means and last element is the design matrix, the first column being a column of ones and the remaining columns represent the values of the covariates for different small areas. See details below.
}
\usage{
estimate.unknownsampvar(response, mat.design, sample.size)
}

\arguments{
  \item{response}{
A numeric vector. It represents the response or the direct survey based estimators in the Fay Herriot Model
}
  \item{mat.design}{
A numeric matrix. The first column is a column of ones(also called the intercept). 
The other columns consist of observations of each of the covariates or the explanatory 
variable in Fay Herriot Model.
}
  \item{sample.size}{
A numeric vector. The known sample sizes used to calculate the direct survey based estimators.
}
}
\details{
For more details please see the package vignette.
}
\value{
\item{ psi.hat }{Estimate of the variance component}
\item{ del.hat }{Estimate of the parameter for sampling variance}
\item{ beta.hat }{Estimate of the unknown regression coefficients}
\item{ theta.hat }{Predictors of the small area means}
\item{ mat.design }{design matrix}
}
\references{
On measuring the variability of small area estimators under a basic area level model. 
Datta, Rao, Smith. Biometrika(2005),92, 1,pp. 183-196
Large Sample Techniques for Statistics, Springer Texts in Statistics. Jiming Jiang.
Chapters - 4,12 and 13.
}
\author{
Abhishek Nandy
}

\seealso{
\code{\link{prasadraoest}}
\code{\link{fayherriot}}
}
\examples{
set.seed( 55 )                  # setting a random seed
require(MASS)                   # the function mvrnorm requires MASS
x1 <- rep( 1, 10 )              # vector of ones representing intercept
x2 <- rnorm( 10 )               # vector of covariates randomly generated
x <- cbind( x1, x2 )            # design matrix
x <- as.matrix( x )             # coercing into class matrix
n <- rbinom (10, 20, 0.4)       # generating sample sizes for each small area
psi <- 1                        # true value of the psi parameter
delta <- 1                      # true value of the delta parameter
beta <- c( 1, 0.5 )             # true values of the regression parameters
theta <- mvrnorm( 1, as.vector( x \%*\% beta ), diag(10) )  # true small area means
y <- mvrnorm( 1, as.vector( theta ), diag( delta/n ) )  # design based estimators
estimate.unknownsampvar( y, x, n )      

}
\keyword{ fay Herriot }
\keyword{ small area estimation }
\keyword{ variance component }
\keyword{ unknown sampling variance }
