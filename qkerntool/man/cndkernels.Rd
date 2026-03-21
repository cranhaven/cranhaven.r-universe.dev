\name{cnds}
\alias{cnds}
\alias{nonlcnd}
\alias{polycnd}
\alias{rbfcnd}
\alias{laplcnd}
\alias{anocnd}
\alias{raticnd}
\alias{multcnd}
\alias{invcnd}
\alias{wavcnd}
\alias{powcnd}
\alias{logcnd}
\alias{caucnd}
\alias{chicnd}
\alias{studcnd}
\alias{norcnd}


\title{CND Kernel Functions}
\description{
  The kernel generating functions provided in qkerntool. \cr
  The Non Linear Kernel \eqn{k(x,y) = [exp(\alpha ||x||^2)+exp(\alpha||y||^2)-2exp(\alpha x'y)]/2}.\cr
  The Polynomial kernel \eqn{k(x,y) = [(\alpha ||x||^2+c)^d+(\alpha ||y||^2+c)^d-2(\alpha x'y+c)^d]/2}.\cr
  The Gaussian kernel \eqn{k(x,y) = 1-exp(-||x-y||^2/\gamma)}.\cr
  The Laplacian Kernel \eqn{k(x,y) = 1-exp(-||x-y||/\gamma)}.\cr
  The ANOVA Kernel  \eqn{k(x,y) = n-\sum exp(-\sigma (x-y)^2)^d}. \cr
  The Rational Quadratic Kernel \eqn{k(x,y) = ||x-y||^2/(||x-y||^2+c)}. \cr
  The Multiquadric Kernel \eqn{k(x,y) = \sqrt{(||x-y||^2+c^2)-c}}. \cr
  The Inverse Multiquadric Kernel \eqn{k(x,y) = 1/c-1/\sqrt{||x-y||^2+c^2}}.\cr
  The Wave Kernel \eqn{k(x,y) = 1-\frac{\theta}{||x-y||}\sin\frac{||x-y||}{\theta}}.\cr
  The d Kernel \eqn{k(x,y) = ||x-y||^d}.\cr
  The Log Kernel \eqn{k(x,y) = \log(||x-y||^d+1)}.\cr
  The Cauchy Kernel \eqn{k(x,y) = 1-1/(1+||x-y||^2/\gamma)}.\cr
  The Chi-Square Kernel \eqn{k(x,y) = \sum{2(x-y)^2/(x+y)}}.\cr
  The Generalized T-Student Kernel \eqn{k(x,y) = 1-1/(1+||x-y||^d)}.\cr
  The normal Kernel \eqn{k(x,y) = ||x-y||^2}.\cr
}
\usage{
nonlcnd(alpha = 1)
polycnd(d = 2, alpha = 1, c = 1)
rbfcnd(gamma = 1)
laplcnd(gamma = 1)
anocnd(d = 2, sigma = 1)
raticnd(c = 1)
multcnd(c = 1)
invcnd(c = 1)
wavcnd(theta = 1)
powcnd(d = 2)
logcnd(d = 2)
caucnd(gamma = 1)
chicnd( )
studcnd(d = 2)
norcnd()
}

  \arguments{
   \item{alpha}{for the Non Linear cndkernel function "nonlcnd" and the Polynomial cndkernel function "polycnd".}
   \item{gamma}{for the Radial Basis cndkernel function "rbfcnd" and the Laplacian cndkernel function "laplcnd" and the Cauchy cndkernel function "caucnd".}
   \item{sigma}{for the ANOVA cndkernel function "anocnd".}
   \item{theta}{for the Wave cndkernel function "wavcnd".}
   \item{c}{for the Rational Quadratic cndkernel function "raticnd", the Polynomial cndkernel function "polycnd", the Multiquadric
   cndkernel function "multcnd" and the Inverse Multiquadric cndkernel function  "invcnd".}
   \item{d}{for the Polynomial cndkernel function "polycnd", the ANOVA cndkernel function "anocnd", the cndkernel function "powcnd", the Log cndkernel function "logcnd" and the Generalized T-Student cndkernel function "studcnd".}
  }
\details{
  The kernel generating functions are used to initialize a kernel
  function which calculates the kernel function value between two feature vectors in a
  Hilbert Space. These functions can be passed as a \code{qkernel} argument on almost all
  functions in \pkg{qkerntool}.
  }
\value{
 Return an S4 object of class \code{cndkernel} which extents the
 \code{function} class. The resulting function implements the given
 kernel calculating the kernel function value between two vectors.
 \item{qpar}{a list containing the kernel parameters (hyperparameters)
   used.}
 The kernel parameters can be accessed by the \code{qpar} function.
 }

\author{Yusen Zhang\cr
  \email{yusenzhang@126.com}}


\seealso{\code{\link{cndkernmatrix}}, \code{\link{qkernmatrix}}}
\examples{
cndkfunc <- rbfcnd(gamma = 1)
cndkfunc

qpar(cndkfunc)

## create two vectors
x <- rnorm(10)
y <- rnorm(10)

## calculate dot product
cndkfunc(x,y)

}
\keyword{symbolmath}

