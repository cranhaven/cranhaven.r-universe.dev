\encoding{utf8}
\name{sphere.boot.bw}
\alias{sphere.boot.bw}
 \title{
Spherical smoothing parameter for HDRs estimation}

\description{
This function provides the specific smoothing parameter for spherical HDRs estimation proposed in Saavedra-Nieves and Crujeiras (2021).}
\usage{
sphere.boot.bw(sample,bw="none",tau=0.5,ngrid=500,
               B=50,nborder=500,upper=NULL)
}
 \arguments{
  \item{sample}{A matrix whose rows represent points on the unit sphere in Cartesian coordinates. If a row norm is different from one, a message appears indicating that they must be standardized.}
  \item{bw}{Pilot smoothing parameter to be used. According to \code{Directional} package, this can be either \code{"none"} for cross validation or \code{"rot"} for the rule of thumb suggested by García-Portugués (2013). Default \code{bw="none"}.}
  \item{tau}{Numeric probability. According to Saavedra-Nieves and Crujeiras (2021), \code{1-tau} represents the probability coverage required for HDR. Default \code{tau=0.5}.}
  \item{ngrid}{Resolution of the density calculation. Default \code{ngrid=500}.}
  \item{B}{Integer string indicating the number of bootstrap resamples. Default \code{B=50}.}
  \item{nborder}{Maximum number of HDRs boundary points to be represented. Default \code{nborder=500}.}
\item{upper}{Numerical upper value for bounding the optimization procedure. Default \code{upper=NULL}. In this case, the upper bound is equal to \code{ 1.5bw}.}
}
\details{
Saavedra-Nieves and Crujeiras (2021) propose a specific smoothing parameter for HDRs estimation based on the minimization of the Hausdorff distance between the boundaries of the theoretical HDR and the plug-in estimator.}
\value{
A numeric value corresponding to the selected smoothing parameter.

}
\references{
García-Portugués, E. (2013). Exact risk improvement of bandwidth selectors for kernel density
estimation with directional data. Electronic Journal of Statistics, 7, 1655-1685.\cr
Saavedra-Nieves, P. and Crujeiras, R. M. (2021). Nonparametric estimation of directional highest density regions. Advances in Data Analysis and Classification, 1-36.
}
\author{
Paula Saavedra-Nieves and Rosa M. Crujeiras.
}


\examples{
\donttest{
# HDR selector from a sample of size 1000 of model 4 in library HDiR
set.seed(1)
sample=rspheremix(500,model=4)
sphere.boot.bw(sample,tau=0.8,B=2)
}
}
