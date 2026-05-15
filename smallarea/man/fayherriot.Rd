\name{fayherriot}
\alias{fayherriot}
\title{
Estimate of the variance component in Fay Herriot Model using Fay Herriot Method
}
\description{
This function returns a list with one element in it which is the estimate of the 
variance component in the Fay Herriot Model. The estimate is found by solving an 
equation (for details see vignette) and is due to Fay Herriot. The uniroot in the 
stats package is used to find the root. uniroot searches for a root of that equation 
in a particular interval the lower bound is 0 and the upper bound is set to estimate 
of the variance component using Prasad Rao method + three times the square root of 
the number of observation. It depends on the function prasadraoest in the same package.
Note that our function does not accept missing values.
}
\usage{
fayherriot(response, designmatrix, sampling.var)
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
}
\details{
For more details please see the attached vignette
}
\value{

\item{estimate}{estimate of the variance component}

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
\code{\link{maximlikelihood}}
\code{\link{resimaxilikelihood}}
}
\examples{
response=c(1,2,3,4,5)
designmatrix=cbind(c(1,1,1,1,1),c(1,2,4,4,1),c(2,1,3,1,5))
randomeffect.var=c(0.5,0.7,0.8,0.4,0.5)
fayherriot(response,designmatrix,randomeffect.var)
}
\keyword{ fay Herriot }
\keyword{ small area estimation }
\keyword{ variance component }
