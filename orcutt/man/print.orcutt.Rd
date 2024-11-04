\name{print.orcutt}
\alias{print.orcutt}
\title{
Print Cochrane-Orcutt Estimation
}
\description{
Print Cochrane-Orcutt Estimation
}
\usage{
\method{print}{orcutt}(x, ...)
}
\arguments{ 
\item{x}{an orcutt object}
\item{\dots}{additional arguments for specific methods.}
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
coch
}
}
\keyword{ orcutt }
