\name{predAccuracy}
\alias{predAccuracy}
\title{
A function for accuracy assessment between an array of two variables
}
\description{
This function calculates statistical indices for accuracy between an array of two variables such as calibration and validation vectors. The indices are Bias, RMSE, R-squared, and NSE
}
\usage{
predAccuracy(x,y)
}

\arguments{
  \item{x}{
a numeric vector of first variable of the two variables for accuracy assessment
}
 \item{y}{
a numeric vector of second variable of the two variables for accuracy assessment
}
}
\details{
The function calculates four indices for accuracy: bias, root mean square error (RMSE), r-squared, Nash-sutcliffe efficiency (NSE)
}
\value{
A table of four variables: bias, RMSE, Rsquared, and NSE
}
\references{
Becker, R. A., Chambers, J. M. and Wilks, A. R. 1988. The New S Language. Wadsworth & Brooks/Cole.

Nash, J. E.; Sutcliffe, J. V. (1970). "River flow forecasting through conceptual models part I — A discussion of principles". Journal of Hydrology. 10 (3): 282–290
}
\author{
Christian Thine Omuto
}

\seealso{
\code{\link{predUncertain}}, \code{\link{featureRep}}
}
\examples{
xy=data.frame(a=c(2,3,4,5,6,7,8,9),b=c(1,1.5,8,10,12,3.5,NA,18))
predAccuracy(xy$a,xy$b)$Rsquared
}
