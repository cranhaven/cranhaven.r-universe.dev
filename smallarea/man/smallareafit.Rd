\name{smallareafit}
\alias{smallareafit}
\title{
Fits a Fay Herriot Model to data
}
\description{
Fits a Fay Herriot model to the data and returns a list of items which area
 estimates of different paramaters and mse of the estimates of the small area 
means the details of which are provided in  the value section.
}
\usage{
smallareafit(formula, data, method)
}

\arguments{
  \item{formula}{
an object of class formula. a formula similar in appearance to that of in lm function in R.
It has to be ascertained that the data contains a column of the sampling variances, and that
while specifying the formula the the name of the variable that contains the sampling variances
should preceede the variables which are the covariates. e.g 
response~D+x1+x2 is a correct way of specifying the formula where as 
response~x1+D+x2 is not.(note D is the variabe that contains the values of
sampling variances and x1 and x2 are covariates). In general the first of the variables
on the right hand side of ~ will be treated as the vector of sampling variance. Note that
our function does not accept any missing values.
}
  \item{data}{
an optional data.frame. containg the variable names and data. in absence of this arguments
 the function will accept the corresponding things from the global environment.
}
  \item{method}{
method can be from "PR", "FH", "ML", "REML"
}
}
\details{
for more details see the vignette
}
\value{

\item{smallmean.est}{Estimates of the small area mean}
\item{smallmean.mse }{Mean Square Prediction error of the estimates of the small area mead}
\item{var.comp}{ an estimate of the variance components}
\item{est.coef}{ an estimate of the regression coefficients}
}
\references{
On measuring the variability of small area estimators under a basic area level model. 
Datta, Rao, Smith. Biometrika(2005),92, 1,pp. 183-196
Large Sample Techniques for Statistics, Springer Texts in Statistics. Jiming Jiang.
Chapters - 4,12 and 13.
Small Area Estimation, JNK Rao, Wiley 2003
Variance Components, Wiley Series in Probability and Statistics,2006
Searle, Casella, Mc-Culloh
}
\author{
Abhishek Nandy
}

\seealso{
\code{\link{prasadraoest}}
\code{\link{fayherriot}}
\code{\link{resimaxilikelihood}}
\code{\link{maximlikelihood}}
}
\examples{
data=data.frame(response=c(1,2,3,4,8),D=c(0.2,0.5,0.1,0.9,1))
data
ans=smallareafit(response~D,data,method="FH")
ans1=smallareafit(response~D,data,method="REML")
ans2=smallareafit(response~D,data,method="PR")
ans3=smallareafit(response~D,data,method="ML")
}
\keyword{ fay herriot}
\keyword{ small area estimation}
\keyword{ mean square error }
