\name{createCase}
\alias{createCase}
\title{Simulate Cases}
\description{Numerical feature simulation for positive samples. Called by \code{\link{createData}}.}
\usage{
createCase(subtype, upper.mean, lower.mean, upper.sd, lower.sd, n.features, 
subtype1.feats = 1:5, subtype2.feats = 6:10, subtype3.feats = 11:15)
}
\arguments{
  \item{subtype}{Numeric number indicating which subtype to simulate. Currently supports three subtype: 1, 2 or 3.}
  \item{upper.mean}{The mean of the upper component of the bimodal Gaussian distribution from which features are simulated.}
  \item{lower.mean}{The mean of the lower component of the bimodal Gaussian distribution from which features are simulated.}
  \item{upper.sd}{The standard deviation of the upper component of the bimodal Gaussian distribution from which features are simulated.}
  \item{lower.sd}{The standard deviation of the lower component of the bimodal Gaussian distribution from which features are simulated.}
  \item{n.features}{Number of features to simulate.}
  \item{subtype1.feats}{Numeric vector representing the indices of features that define subtype 1.}
  \item{subtype2.feats}{Numeric vector representing the indices of features that define subtype 2.}
  \item{subtype3.feats}{Numeric vector representing the indices of features that define subtype 3.}
}
\details{
Simulations support 3 subtypes, each defined by 5 different features. 

Subtype 1 is defined as having the first 3 \code{subtype1.feats} and at least one of the next 2 \code{subtype1.feats} simulated from the upper component of the bimodal Gaussian distribution. 

Subtype 2 is defined as having all 5 \code{subtype2.feats} simulated from the upper component. 

Subtype 3 is defined as having the first 4 \code{subtype3.feats} simulated from the upper component and and the last \code{subtype3.feats} simulated from the lower component.  
}
\value{Returns a vector of simulated features}
\references{ADD REFERENCE}
\author{Michelle Saul}
\note{\code{createCase} is not meant to be called alone. It is designed as a helper function for \code{\link{createData}}.}
\seealso{\code{\link{createData}}}
\examples{
# Toy Example 
case <- createCase(subtype = 1, upper.mean = 13, lower.mean = 5, 
upper.sd = 1, lower.sd = 1, n.features = 20, 
                        subtype1.feats = 1:5,
                        subtype2.feats = 6:10,
                        subtype3.feats = 11:15)
}
