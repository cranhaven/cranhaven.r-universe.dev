\encoding{utf8}
\name{circ.boot.bw}
\alias{circ.boot.bw}
\title{Circular smoothing parameter for HDRs estimation}
\description{
This function provides the specific smoothing parameter for circular HDRs estimation proposed in Saavedra-Nieves and Crujeiras (2021).}
\usage{circ.boot.bw(sample, bw = bw.CV(circular(sample),
upper=100), tau = 0.5, B = 50, upper = 1.5 * bw)}
\arguments{
  \item{sample}{Numeric vector of angles in radians.}
  \item{bw}{Pilot soothing parameter to be used. Following Oliveira et al. (2014), the value of the smoothing parameter can be chosen by using the functions \code{bw.rt}, \code{bw.CV}, \code{bw.pi} or \code{bw.boot}. Default \code{bw=bw.CV} providing a cross-validation bandwidth.}
  \item{tau}{Numeric probability. According to Saavedra-Nieves and Crujeiras (2021), \code{1-tau} represents the probability coverage required for HDR. Default \code{tau=0.5}.}
  \item{B}{Integer value indicating the number of bootstrap resamples. Default \code{B=50}.}
  \item{upper}{Numerical upper value for bounding the optimization procedure. Default \code{ 1.5bw}.}
}
\details{
Saavedra-Nieves and Crujeiras (2021) propose a specific smoothing parameter for HDRs estimation based on the minimization of the Hausdorff distance between the boundaries of the theoretical HDR and the plug-in estimator.}
\value{A numeric value corresponding to the selected smoothing parameter.}
\references{
Saavedra-Nieves, P. and Crujeiras, R. M.  (2021). Nonparametric estimation of directional highest density regions. Advances in Data Analysis and Classification, 1-36.}
\author{Paula Saavedra-Nieves and Rosa M. Crujeiras.}

\examples{
# HDR selector from a sample of size 500 of model 5 in NPCirc
library(NPCirc)
set.seed(1)
sample<- rcircmix(500, model=5)
circ.boot.bw(sample,tau=0.4,B=2)
}
