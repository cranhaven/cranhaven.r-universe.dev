\name{createControl}
\alias{createControl}
\title{Simulate Control}
\description{Numerical feature simulation for negative samples. Called by \code{\link{createData}}.}
\usage{
createControl(upper.mean, lower.mean, upper.sd, lower.sd, n.features, 
subtype1.feats = 1:5, subtype2.feats = 6:10, subtype3.feats = 11:15)
}
\arguments{
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
Simulates data such that none of the 3 subtypes defined in \code{\link{createCase}} are represented. 

To ensure subtype 1 is not represented, at least one of the first three \code{subtype1.feats} and/or both of the next 2 \code{subtype1.feats} are simulated from the lower component of the Gaussian distribution. 

To ensure subtype 2 is not represented, at least one of the five \code{subtype2.feats} is simulated from the lower component. 

To ensure subtype 3 is not represented, at least one of the first 4 \code{subtype3.feats} is simulated from the lower component and/or the last \code{subtype3.feats} is simulated from the upper component. 
}
\value{Returns a vector of simulated features}
\references{ADD REFERENCE}
\author{Michelle Saul}
\note{\code{createControl} is not meant to be called alone. It is designed as a helper function for \code{\link{createData}}.}
\seealso{\code{\link{createData}}}
\examples{
# Toy Example 
control <- createControl(upper.mean = 13, lower.mean = 5, 
upper.sd = 1, lower.sd = 1, n.features = 20, 
                        subtype1.feats = 1:5,
                        subtype2.feats = 6:10,
                        subtype3.feats = 11:15)
}