\name{predict.orcutt}
\alias{predict.orcutt}
\title{
Predict method for Cochrane-Orcutt Estimation
}
\description{
Predicted values based on orcutt object.
}
\usage{
\method{predict}{orcutt}(object, ...)
}
\arguments{
\item{object}{An "orcutt" object build with Cochrane-Orcutt fuction}
\item{\dots}{further arguments passed to or from other methods.}
}
\references{
Verbeek M. (2004) \emph{A guide to modern econometrics}, John Wiley & Sons Ltd
}
\author{
Stefano Spada
}
\examples{
\donttest{
data(icecream, package="orcutt")
lm = lm(cons ~ price + income + temp, data=icecream)
coch = cochrane.orcutt(lm)
predict.coch = predict(coch)
}
}
\keyword{ cochrane.orcutt }

