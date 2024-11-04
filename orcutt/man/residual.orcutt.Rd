\name{residual.orcutt}
\alias{residual.orcutt}
\title{
Accessing Cochrane-Orcutt Fits
}
\description{
Residual for Cochrane-Orcutt Estimation
}
\usage{
residual.orcutt(object, ...)
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
residuals(coch)
}
}
\keyword{ cochrane.orcutt }
