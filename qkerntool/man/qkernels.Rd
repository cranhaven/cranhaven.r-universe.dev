\name{bases}
\alias{bases}
\alias{rbfbase}
\alias{nonlbase}
\alias{laplbase}
\alias{ratibase}
\alias{multbase}
\alias{invbase}
\alias{wavbase}
\alias{powbase}
\alias{logbase}
\alias{caubase}
\alias{chibase}
\alias{studbase}


\title{qKernel Functions}
\description{
  The kernel generating functions provided in qkerntool. \cr
  The Non Linear Kernel \eqn{k(x,y) = \frac{1}{2(1-q)}(q^{-\alpha||x||^2}+q^{-\alpha||y||^2}-2q^{-\alpha x'y}) }.\cr
  The Gaussian kernel \eqn{k(x,y) =\frac{1}{1-q} (1-q^{(||x-y||^2/\sigma)})}.\cr
  The Laplacian Kernel \eqn{k(x,y) =\frac{1}{1-q} (1-q^{(||x-y||/\sigma)})}.\cr

  The Rational Quadratic Kernel \eqn{k(x,y) =\frac{1}{1-q} (1-q^{\frac{||x-y||^2}{||x-y||^2+c}})}.\cr
  The Multiquadric Kernel \eqn{k(x,y) =\frac{1}{1-q} (q^c-q^{\sqrt{||x-y||^2+c}})}.\cr
  The Inverse Multiquadric Kernel \eqn{k(x,y) =\frac{1}{1-q} (q^{-\frac{1}{c}}-q^{-\frac{1}{\sqrt{||x-y||^2+c}}})}.\cr
  The Wave Kernel \eqn{k(x,y) =\frac{1}{1-q} (q^{-1}-q^{-\frac{\theta}{||x-y||}\sin{\frac{||x-y||}{\theta}}})}.\cr
  The d Kernel \eqn{k(x,y) = \frac{1}{1-q}[1-q^(||x-y||^d)] }.\cr
  The Log Kernel \eqn{k(x,y) =\frac{1}{1-q} [1-q^ln(||x-y||^d+1)]}.\cr
  The Cauchy Kernel \eqn{k(x,y) =\frac{1}{1-q} (q^{-1}-q^{-\frac{1}{1+||x-y||^2/\sigma}})}.\cr
  The Chi-Square Kernel \eqn{k(x,y) =\frac{1}{1-q} (1-q^{\sum{2(x-y)^2/(x+y)} \gamma})}.\cr
  The Generalized T-Student Kernel \eqn{k(x,y) =\frac{1}{1-q} (q^{-1}-q^{-\frac{1}{1+||x-y||^d}})}.\cr
}
\usage{
rbfbase(sigma=1,q=0.8)
nonlbase(alpha = 1,q = 0.8)
laplbase(sigma = 1, q = 0.8)
ratibase(c = 1, q = 0.8)
multbase(c = 1, q = 0.8)
invbase(c = 1, q = 0.8)
wavbase(theta = 1,q = 0.8)
powbase(d = 2, q = 0.8)
logbase(d = 2, q = 0.8)
caubase(sigma = 1, q = 0.8)
chibase(gamma = 1, q = 0.8)
studbase(d = 2, q = 0.8)
}

\arguments{
  \item{q}{for all the qkernel function.}
   \item{sigma}{for the Radial Basis qkernel function "rbfbase" , the Laplacian qkernel function "laplbase" and the Cauchy qkernel function "caubase".}
   \item{alpha}{for the Non Linear qkernel function "nonlbase".}
   \item{c}{for the Rational Quadratic qkernel function "ratibase" , the Multiquadric qkernel
   function "multbase" and the Inverse Multiquadric qkernel function "invbase".}
   \item{theta}{for the Wave qkernel function "wavbase".}
   \item{d}{for the d qkernel function "powbase" , the Log qkernel function "logbase"
   and the Generalized T-Student qkernel function "studbase".}
   \item{gamma}{for the Chi-Square qkernel function "chibase".}
}
\details{
  The kernel generating functions are used to initialize a kernel
  function
  which calculates the kernel function value between two feature vectors in a
  Hilbert Space. These functions can be passed as a \code{qkernel} argument on almost all
  functions in \pkg{qkerntool}(e.g., \code{qkgda}, \code{qkpca}  etc).
}
\value{
 Return an S4 object of class \code{qkernel} which extents the
 \code{function} class. The resulting function implements the given
 kernel calculating the kernel function value between two vectors.
 \item{qpar}{a list containing the kernel parameters (hyperparameters)
   used.}
 The kernel parameters can be accessed by the \code{qpar} function.
 }

\author{Yusen Zhang\cr
  \email{yusenzhang@126.com}}



\seealso{\code{\link{qkernmatrix}}, \code{\link{cndkernmatrix}}}
\examples{
qkfunc <- rbfbase(sigma=1,q=0.8)
qkfunc

qpar(qkfunc)

## create two vectors
x <- rnorm(10)
y <- rnorm(10)

## calculate dot product
qkfunc(x,y)

}
\keyword{symbolmath}

