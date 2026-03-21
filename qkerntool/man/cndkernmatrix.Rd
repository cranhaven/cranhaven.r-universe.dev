\name{cndkernmatrix}
\alias{cndkernmatrix}

\alias{cndkernmatrix,cndkernel-method}
\alias{cndkernmatrix,anokernel-method}
\alias{cndkernmatrix,caukernel-method}
\alias{cndkernmatrix,chikernel-method}
\alias{cndkernmatrix,invkernel-method}
\alias{cndkernmatrix,laplkernel-method}
\alias{cndkernmatrix,logkernel-method}
\alias{cndkernmatrix,multkernel-method}
\alias{cndkernmatrix,nonlkernel-method}
\alias{cndkernmatrix,powkernel-method}
\alias{cndkernmatrix,ratikernel-method}
\alias{cndkernmatrix,studkernel-method}
\alias{cndkernmatrix,wavkernel-method}
\alias{cndkernmatrix,polykernel-method}
\alias{cndkernmatrix,rbfkernel-method}
\alias{cndkernmatrix,norkernel-method}

\alias{cndkernmatrix.anokernel}
\alias{cndkernmatrix.caukernel}
\alias{cndkernmatrix.chikernel}
\alias{cndkernmatrix.invkernel}
\alias{cndkernmatrix.laplkernel}
\alias{cndkernmatrix.logkernel}
\alias{cndkernmatrix.multkernel}
\alias{cndkernmatrix.nonlkernel}
\alias{cndkernmatrix.polykernel}
\alias{cndkernmatrix.powkernel}
\alias{cndkernmatrix.ratikernel}
\alias{cndkernmatrix.rbfkernel}
\alias{cndkernmatrix.studkernel}
\alias{cndkernmatrix.wavkernel}
\alias{cndkernmatrix.norkernel}


\title{CND Kernel Matrix functions}
\description{
  \code{cndkernmatrix} calculates the kernel matrix \eqn{K_{ij} = k(x_i,x_j)} or \eqn{K_{ij} =
    k(x_i,y_j)}.\cr
 }
\usage{
\S4method{cndkernmatrix}{cndkernel}(cndkernel, x, y = NULL)
}

\arguments{
  \item{cndkernel}{the cndkernel function to be used to calculate the CND kernel
    matrix.
  This has to be a function of class \code{cndkernel}, i.e. which can be
  generated either one of the build in
  kernel generating functions (e.g., \code{rbfcnd} \code{nonlcnd} etc.) or a user defined
  function of class \code{cndkernel} taking two vector arguments and returning a scalar.}
  \item{x}{a data matrix to be used to calculate the kernel matrix.}
  \item{y}{second data matrix to calculate the kernel matrix.}
  }
\details{
  Common functions used during kernel based computations.\cr
  The \code{cndkernel} parameter can be set to any function, of class
  cndkernel, which computes the kernel function value in feature space between two
  vector arguments. \pkg{qkerntool} provides more than 10 CND kernel functions
  which can be initialized by using the following
  functions:
    \itemize{
      \item \code{nonlcnd} Non Linear cndkernel function
      \item \code{polycnd} Polynomial  cndkernel function
      \item \code{rbfcnd}  Gaussian cndkernel function
      \item \code{laplcnd} Laplacian cndkernel function
      \item \code{anocnd}  ANOVA cndkernel function
      \item \code{raticnd} Rational Quadratic cndkernel function
      \item \code{multcnd} Multiquadric cndkernel function
      \item \code{invcnd}  Inverse Multiquadric cndkernel function
      \item \code{wavcnd}  Wave  cndkernel function
      \item \code{powcnd}  d cndkernel function
      \item \code{logcnd}  Log cndkernel function
      \item \code{caucnd}  Cauchy cndkernel function
      \item \code{chicnd}  Chi-Square cndkernel function
      \item \code{studcnd} Generalized T-Student cndkernel function
    }  (see example.)
  }

\value{
  \code{cndkernmatrix} returns a conditionally negative definite matrix with a zero diagonal element.\cr
 }
\author{Yusen Zhang \cr
\email{yusenzhang@126.com}}


\seealso{\code{\link{nonlbase}}, \code{\link{rbfbase}},
  \code{\link{laplbase}}, \code{\link{ratibase}}, \code{\link{multbase}}, \code{\link{invbase}},
  \code{\link{wavbase}}, \code{\link{powbase}}, \code{\link{logbase}}, \code{\link{caubase}},
  \code{\link{chibase}}, \code{\link{studbase}}}

\examples{
## use the iris data
data(iris)
dt <- as.matrix(iris[ ,-5])

## initialize cndkernel function
lapl <- laplcnd(gamma = 1)
lapl

## calculate cndkernel matrix
cndkernmatrix(lapl, dt)
}
\keyword{algebra}
\keyword{array}
