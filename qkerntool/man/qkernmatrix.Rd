\name{qkernmatrix}
\alias{qkernmatrix}
\alias{qkernmatrix,qkernel-method}
\alias{qkernmatrix,rbfqkernel-method}
\alias{qkernmatrix,nonlqkernel-method}
\alias{qkernmatrix,laplqkernel-method}
\alias{qkernmatrix,ratiqkernel-method}
\alias{qkernmatrix,multqkernel-method}
\alias{qkernmatrix,invqkernel-method}
\alias{qkernmatrix,wavqkernel-method}
\alias{qkernmatrix,powqkernel-method}
\alias{qkernmatrix,logqkernel-method}
\alias{qkernmatrix,cauqkernel-method}
\alias{qkernmatrix,chiqkernel-method}
\alias{qkernmatrix,studqkernel-method}
\alias{qkernmatrix.rbfqkernel}
\alias{qkernmatrix.nonlqkernel}
\alias{qkernmatrix.laplqkernel}
\alias{qkernmatrix.ratiqkernel}
\alias{qkernmatrix.multqkernel}
\alias{qkernmatrix.invqkernel}
\alias{qkernmatrix.wavqkernel}
\alias{qkernmatrix.powqkernel}
\alias{qkernmatrix.logqkernel}
\alias{qkernmatrix.cauqkernel}
\alias{qkernmatrix.chiqkernel}
\alias{qkernmatrix.studqkernel}




\title{qKernel Matrix functions}
\description{
  \code{qkernmatrix} calculates the qkernel matrix \eqn{K_{ij} = k(x_i,x_j)} or \eqn{K_{ij} =
    k(x_i,y_j)}.\cr
 }
\usage{
\S4method{qkernmatrix}{qkernel}(qkernel, x, y = NULL)
}

\arguments{
  \item{qkernel}{the kernel function to be used to calculate the qkernel
    matrix.
  This has to be a function of class \code{qkernel}, i.e. which can be
  generated either one of the build in
  kernel generating functions (e.g., \code{rbfbase} etc.) or a user defined
  function of class \code{qkernel} taking two vector arguments and returning a scalar.}
  \item{x}{a data matrix to be used to calculate the kernel matrix}
  \item{y}{second data matrix to calculate the kernel matrix}
  }
\details{
  Common functions used during kernel based computations.\cr
  The \code{qkernel} parameter can be set to any function, of class
  qkernel, which computes the kernel function value in feature space between two
  vector arguments. \pkg{qkerntool} provides more than 10 qkernel functions
  which can be initialized by using the following
  functions:
    \itemize{
      \item \code{nonlbase} Non Linear qkernel function
      \item \code{rbfbase}  Gaussian qkernel function
      \item \code{laplbase} Laplacian qkernel function
      \item \code{ratibase} Rational Quadratic qkernel function
      \item \code{multbase} Multiquadric qkernel function
      \item \code{invbase}  Inverse Multiquadric qkernel function
      \item \code{wavbase}  Wave  qkernel function
      \item \code{powbase}  d qkernel function
      \item \code{logbase}  Log qkernel function
      \item \code{caubase}  Cauchy qkernel function
      \item \code{chibase}  Chi-Square qkernel function
      \item \code{studbase} Generalized T-Student qkernel function
      }  (see example.)
  }

\value{
  \code{qkernmatrix} returns a conditionally negative definite matrix with a zero diagonal element.\cr
 }
\author{Yusen Zhang \cr
\email{yusenzhang@126.com}}


\seealso{\code{\link{nonlcnd}}, \code{\link{rbfcnd}},\code{\link{polycnd}},\code{\link{laplcnd}},
   \code{\link{anocnd}}, \code{\link{raticnd}}, \code{\link{multcnd}}, \code{\link{invcnd}},
  \code{\link{wavcnd}}, \code{\link{powcnd}}, \code{\link{logcnd}}, \code{\link{caucnd}},
  \code{\link{chicnd}}, \code{\link{studcnd}}}


\examples{

data(iris)
dt <- as.matrix(iris[ ,-5])

## initialize kernel function
rbf <- rbfbase(sigma = 1.4, q=0.8)
rbf

## calculate qkernel matrix
qkernmatrix(rbf, dt)


}

\keyword{algebra}
\keyword{array}
