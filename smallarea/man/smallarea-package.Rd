\name{smallarea-package}
\alias{smallarea-package}
\alias{smallarea}
\docType{package}
\title{
Fits a Fay Herriot model
}
\description{
It has some useful functions which the users might find convenient
for fitting Fay Herriot Model. Details are included in package vignette.
}
\details{
\tabular{ll}{
Package: \tab smallarea\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2013-04-25\cr
License: \tab GPL(>=2)\cr
}
}
\author{
Abhishek Nandy

Maintainer: Abhishek nandy<nandy006@umn.edu>

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

\keyword{ package }
\keyword{ fay Herriot }
\keyword{ small area estimation }
\keyword{ variance component }
\keyword{ mean square error }
\examples{
data=data.frame(response=c(1,2,3,4,8),D=c(0.2,0.5,0.1,0.9,1))
data
ans=smallareafit(response~D,data,method="FH")
ans1=smallareafit(response~D,data,method="REML")
ans2=smallareafit(response~D,data,method="PR")
ans3=smallareafit(response~D,data,method="ML")
}
