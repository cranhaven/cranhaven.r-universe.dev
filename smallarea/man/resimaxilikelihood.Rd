\name{resimaxilikelihood}
\alias{resimaxilikelihood}
\title{
Estimate of the variance component in Fay Herriot Model using Residual
Maximum Likelihood, REML.
}
\description{
This function returns a list with one element in it which is the estimate of the 
variance component in the Fay Herriot Model using residual maximum likelihood
method. The estimates are obtained as a solution of equations known as REML 
equations. The solution is obtained numerically using Fisher-scoring algorithm.
For more details please see the package vignette and the references. Note that 
our function does not accept any missing values.
}
\usage{
resimaxilikelihood(response, designmatrix, sampling.var,maxiter)
}

\arguments{
  \item{response}{
a numeric vector. It represents the response or the observed value in the Fay Herriot
Model
}
  \item{designmatrix}{
a numeric matrix. The first column is a column of ones(also called the intercept). 
The other columns consist of observations of each of the covariates or the explanatory 
variable in Fay Herriot Model.
}
  \item{sampling.var}{
a numeric vector consisting of the known sampling variances of each of the small area levels.
}
  \item{maxiter}{
maximum number of iterations of fisher scoring
}
}
\details{
For more details see the package vignette
}
\value{

\item{estimate}{estimate of the variance component}

}
\references{
On measuring the variability of small area estimators under a basic area level model. 
Datta, Rao, Smith. Biometrika(2005),92, 1,pp. 183-196
Large Sample Techniques for Statistics, Springer Texts in Statistics. Jiming Jiang.
Chapters - 4,12 and 13.
Small Area Estimation, JNK Rao,Wiley 2003
Variance Components, Wiley Series in Probability and Statistics,2006
Searle, Casella, Mc-Culloh
}
\author{
Abhishek Nandy
}

\seealso{
\code{\link{prasadraoest}}
\code{\link{maximlikelihood}}
\code{\link{fayherriot}}
}
\examples{
response=c(1,2,3,4,5)
designmatrix=cbind(c(1,1,1,1,1),c(1,2,4,4,1),c(2,1,3,1,5))
randomeffect.var=c(0.5,0.7,0.8,0.4,0.5)
resimaxilikelihood(response,designmatrix,randomeffect.var,100)
}
\keyword{ fay Herriot }
\keyword{ small area estimation }
\keyword{ variance component }
\keyword{ REML }
\keyword{ residual maximum likelihood }
