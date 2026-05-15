\name{maximlikelihood}
\alias{maximlikelihood}
\title{
Maximum likelihood estimates of the variance components and the
unknown regression coefficients in Fay Herriot Model.
}
\description{
This function returns a list of three elements the first one is the maximum
likelihood estimate of the variance component,the second one is a vector 
of the maximum likelihood estimate of the unknown regression coefficients 
the first one being the coefficient of the intecept and the remaining ones 
are in the same order as the columns of the design matrix and the 
last one being the value of the 
maximized loglikelihood function in Fay Herriot model. it uses the 
optim in the stats package and the BFGS algorithm to minimize the 
negative loglikelihood. The initial value for this iterative 
proceedure of maximization are chosen as follows. The initial value 
for the variance component is the fay Prasad-rao estimate of the variance 
component, the initial value for the regression coefficients are the 
estimates of the regression coefficients using the multiple linear 
regression and ignoring the random effects.(For more details see 
vignette). Note that our function does not accept any missing values.   
}
\usage{
maximlikelihood(response, designmatrix, sampling.var)
}

\arguments{
  \item{response}{
A numeric vector. It represents the response or the direct survey based estimators in the Fay Herriot Model
}
  \item{designmatrix}{
A numeric matrix. The first column is a column of ones(also called the intercept). 
The other columns consist of observations of each of the covariates or the explanatory 
variable in Fay Herriot Model.
}
  \item{sampling.var}{
A numeric vector consisting of the known sampling variances of each of the small area levels.
}
}
\details{
For more details please see the package vignette
}
\value{

\item{ estimate }{Maximum likelihood estimate of the variance component}
\item{ reg.coefficients }{Maximum likelihood estimate of the unknown regression coefficients}
\item{ loglikeli.optimum }{The maximized value of the log likelihood function}
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
\code{\link{resimaxilikelihood}}
}
\examples{
response=c(1,2,3,4,5)
designmatrix=cbind(c(1,1,1,1,1),c(1,2,4,4,1),c(2,1,3,1,5))
randomeffect.var=c(0.5,0.7,0.8,0.4,0.5)
maximlikelihood(response,designmatrix,randomeffect.var)
}

\keyword{ fay Herriot }
\keyword{ small area estimation }
\keyword{ variance component }
\keyword{ maximum likelihood }