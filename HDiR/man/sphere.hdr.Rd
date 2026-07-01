\encoding{utf8}
\name{sphere.hdr}
\alias{sphere.hdr}
 \title{
Computation of HDRs for a spherical density and of general level sets for spherical real-valued functions}
\description{
This function computes HDRs of a spherical density and general level sets for real-valued functions defined on the unit sphere.
}
\usage{
sphere.hdr(f,tau=NULL,level=NULL,nborder=1000,tol=0.1,
           mesh=40,deg=6,plot.hdr=TRUE,col=NULL)
}
 \arguments{
  \item{f}{Object of class \code{"function"} that represents a spherical density function or any general real-valued function defined on the unit sphere. Matrix parametrisation of \code{f} must be established. Each row of the matrix corresponds to the cartesian coordinates of a point on the unit sphere. Additionally, if \code{f} is not a density function, the argument \code{tau} cannot be specified and only the value of \code{level} must be provided by the user.}
 \item{tau}{Numeric probability. According to Saavedra-Nieves and Crujeiras (2021), \code{1-tau} represents the probability coverage required for HDR. If \code{tau=NULL}, a value for the threshold \code{level} must be provided.}
    \item{level}{Numeric threshold of the HDR or of the general level set provided by the user. If \code{level} is larger than the maximum value of the function f, the HDR or the level set are equal to the emptyset. If \code{level} is smaller than the minimum of f, its support is obtained. If \code{level=NULL}, f must be a density function and a value for the probability coverage \code{1-tau} of the HDR must be provided.}
    \item{nborder}{Maximum number of HDRs boundary points to be represented. Default \code{nborder=} \code{1000}.}
  \item{tol}{
Tolerance parameter to determinate the boundary of HDRs. Default \code{tol=0.1}.}
  \item{mesh}{
A numeric value 10, 20 or 40 indicating the 3D cartesian mesh used for numerical integration on the unit shere. Default \code{mesh=40} considering a total of 32000 triangular cells on the sphere. If \code{mesh=20} or \code{mesh=10}, 8000 triangular cells or 2000 are considered, respectively.
}
  \item{deg}{Integer string indicating the degree (from 0 to 6) of the quadrature rules for triangles on the sphere for numerical integration. Default \code{deg=6}.
}
  \item{plot.hdr}{
Logical string. If \code{TRUE}, the boundary of the level set or the HDR are represented graphically. Default \code{plot.density=TRUE}.}
  \item{col}{
Color number for plotting the boundary of the HDR. Default \code{"darkgray"}.}
}
\details{
A detailed definition of directional HDRs for a density is given in Saavedra-Nieves and Crujeiras (2021). Note that numerical integration on the sphere is used to compute the threshold of HDR when \code{tau} is provided.}
\value{
If \code{tau} is provided, a list with the next components:%%  If it is a LIST, use
\item{hdr}{A matrix of rows of points on the HDR boundary.}
\item{prob.content}{Probability coverage \code{1-tau}.}
\item{level}{Threshold associated to the probability content \code{1-tau}.}
If \code{level} is provided, a list with the next components:%%  If it is a LIST, use
\item{levelset}{A matrix of rows of points on the level set boundary.}
\item{level}{Threshold of the level set.}
}
\references{
Saavedra-Nieves, P. and Crujeiras, R. M. (2021). Nonparametric estimation of directional highest density regions. Advances in Data Analysis and Classification, 1-36.}
\author{
Paula Saavedra-Nieves, Rosa M. Crujeiras and Andrés Prieto.
}

\examples{
\donttest{
#HDR of model 8 in library HDiR
f1<-function(x){return(dspheremix(x,model=8))}
sphere.hdr(f1,tau=0.5,mesh=20,deg=3)
}
\donttest{
# Density level set plug-in reconstruction from a sample
# of size 500 (model 8) by using a kernel density
# estimator with uniform kernel
library(DirStats)
f2<-function(x){
  set.seed(1)
  sample<-rspheremix(500, model = 3)
  return(kde_dir(x, data = sample, h = 0.4,
  L = function(x) dunif(x)))
}
sphere.hdr(f2,level=0.3)
}
}

