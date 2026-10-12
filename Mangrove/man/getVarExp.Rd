\name{getVarExp}
\alias{getVarExp}
\alias{getVarExpSim}
\title{
Calculating liability-scale variance explained by genetic risk variants
}
\description{
These functions calculate the variance explained on the liability scale by the genetic risk variants described by a \code{\link[Mangrove]{MangroveORs}} object. 
}
\usage{
getVarExp(ORs, K)
getVarExpSim(ORs,K,iter=1000)
}
\arguments{
  \item{ORs}{
	An object of class \code{\link[Mangrove]{MangroveORs}}	
}
  \item{K}{
	The prevalence of the disease.
}
  \item{iter}{
	The number of individuals sampled for variance estimation.
}
}
\details{
\code{getVarExp} calculates the variance explained analytically, by converting the odds ratios to liability-scale units, and combining them together additively. \code{getVarExpSim} simulates genotypes from the allele frequencies, performs risk prediction on them, converts the resulting posteriors to liabilities, and directly measures the variance of the sample.
}
\value{
The proportion of variance explained, as a single numeric value.}
\examples{
data(exampleORs)
getVarExp(exampleORs,0.02)
getVarExpSim(exampleORs,0.02)
}
\keyword{misc}
