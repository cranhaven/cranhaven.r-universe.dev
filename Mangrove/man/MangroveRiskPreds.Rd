\name{MangroveRiskPreds}
\alias{MangroveRiskPreds}
\alias{calcORs}
\alias{applyORs}
\alias{print.MangroveRiskPreds}
\alias{summary.MangroveRiskPreds}
\alias{plot.MangroveRiskPreds}
\title{
The MangroveRiskPreds class
}
\description{
Methods to generate, summarise and apply MangroveRiskPreds objects
}
\usage{
calcORs(ped,ORs)
applyORs(riskpred,K)

\method{print}{MangroveRiskPreds}(x, ...)
\method{summary}{MangroveRiskPreds}(object, ...)
\method{plot}{MangroveRiskPreds}(x, ...)
}
\arguments{
  \item{ped}{
	A \code{\link[Mangrove]{MangrovePed}} object
}
\item{ORs}{
	A \code{\link[Mangrove]{MangroveORs}} object
}
 \item{riskpred}{
	A \code{MangroveRiskPreds} object
}
 \item{object}{
	A \code{MangroveRiskPreds} object
}
 \item{x}{
	A \code{MangroveRiskPreds} object
}
 \item{K}{
	The prevalence of the disease being predicted.
}
  \item{\dots}{
	Arguments to be passed on to downstream methods.
}
}
\details{
\code{MangroveRiskPreds} objects are used to hold risk predictions for a number of individuals. This object contains the odds ratio for each individual of developing the disease, relative to the population average. You can generate risk predictions from a pedigree object and some per-variant odds ratios using \code{calcORs}, and you can use these to calculate posterior probabilities of developing the disease given a prevalence using \code{applyORs}.

The \code{print} method writes the odds ratio for each individual to screen. The \code{summary} method gives some quantile information, and the \code{plot} method prints a histogram of log odds ratios, which should be approximately normal.
}
\value{
\code{calcORs} returns a \code{MangroveRiskPreds} object, \code{applyORs} returns a numeric vector containing posterior probabilities. The methods do not return anything.
}
\seealso{
\code{\link[Mangrove]{MangroveTree}} for doing risk prediction for a whole family. \code{\link[Mangrove]{MangroveContPreds}} for doing quantitative trait prediction.
}
\examples{
data(ccped)
data(exampleORs)
ccors <- calcORs(ccped,exampleORs)

summary(ccors)
plot(ccors)

ccposts <- applyORs(ccors,0.02)

}

\keyword{classes}
\keyword{methods}
