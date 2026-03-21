\name{qsammon}
\alias{qsammon}
\alias{qsammon,matrix-method}
\alias{qsammon,cndkernmatrix-method}
\alias{qsammon,qkernmatrix-method}
\title{qKernel Sammon Mapping}
\description{The qkernel Sammon Mapping is an implementation for Sammon mapping, one of the earliest
dimension reduction techniques that aims to find low-dimensional embedding
that preserves pairwise distance structure in high-dimensional data space.
qsammon is a nonlinear form of Sammon Mapping.}
\usage{


\S4method{qsammon}{matrix}(x, kernel = "rbfbase", qpar = list(sigma = 0.5, q = 0.9),
          dims = 2, Initialisation = 'random', MaxHalves = 20,
          MaxIter = 500, TolFun = 1e-7, na.action = na.omit, ...)

\S4method{qsammon}{cndkernmatrix}(cndkernel, x, k, dims = 2, Initialisation = 'random',
          MaxHalves = 20,MaxIter = 500, TolFun = 1e-7, ...)

\S4method{qsammon}{qkernmatrix}(qkernel, x, k, dims = 2, Initialisation = 'random',
          MaxHalves = 20, MaxIter = 500, TolFun = 1e-7, ...)
}

\arguments{
  \item{x}{the data matrix indexed by row or a kernel matrix of \code{cndkernmatrix} or \code{qkernmatrix}.}

	\item{kernel}{the kernel function used in training and predicting.
    This parameter can be set to any function, of class kernel, which computes a kernel function value between two
    vector arguments. qkerntool provides the most popular kernel functions
    which can be used by setting the kernel parameter to the following
    strings:
    \itemize{
      \item \code{rbfbase} Radial Basis qkernel function "Gaussian"
      \item \code{nonlbase} Non Linear qkernel function
      \item \code{laplbase} Laplbase qkernel function
      \item \code{ratibase} Rational Quadratic qkernel function
      \item \code{multbase} Multiquadric qkernel function
      \item \code{invbase} Inverse Multiquadric qkernel function
      \item \code{wavbase} Wave qkernel function
      \item \code{powbase} d qkernel function
      \item \code{logbase} Log qkernel function
      \item \code{caubase} Cauchy qkernel function
      \item \code{chibase} Chi-Square qkernel function
      \item \code{studbase} Generalized T-Student qkernel function
      \item \code{nonlcnd} Non Linear cndkernel function
      \item \code{polycnd} Polynomial cndkernel function
      \item \code{rbfcnd} Radial Basis cndkernel function "Gaussian"
      \item \code{laplcnd} Laplacian cndkernel function
      \item \code{anocnd} ANOVA cndkernel function
      \item \code{raticnd} Rational Quadratic cndkernel function
      \item \code{multcnd} Multiquadric cndkernel function
      \item \code{invcnd} Inverse Multiquadric cndkernel function
      \item \code{wavcnd} Wave cndkernel function
      \item \code{powcnd} d cndkernel function
      \item \code{logcnd} Log cndkernel function
      \item \code{caucnd} Cauchy cndkernel function
      \item \code{chicnd} Chi-Square cndkernel function
      \item \code{studcnd} Generalized T-Student cndkernel function
    }
    The kernel parameter can also be set to a user defined function of
    class kernel by passing the function name as an argument.
  }

  \item{qpar}{the list of hyper-parameters (kernel parameters).
    This is a list which contains the parameters to be used with the
    kernel function. Valid parameters for existing kernels are :
    \itemize{

      \item \code{sigma, q}  for the Radial Basis qkernel function "rbfbase" , the Laplacian qkernel function "laplbase" and the Cauchy qkernel function "caubase".
      \item \code{alpha, q} for the Non Linear qkernel function "nonlbase".
      \item \code{c, q} for the Rational Quadratic qkernel function "ratibase" , the Multiquadric qkernel function "multbase" and the Inverse Multiquadric qkernel function "invbase".
      \item \code{theta, q} for the Wave qkernel function "wavbase".
      \item \code{d, q} for the d qkernel function "powbase" , the Log qkernel function "logbase" and the Generalized T-Student qkernel function "studbase".
      \item \code{alpha} for the Non Linear cndkernel function "nonlcnd".
      \item \code{d, alpha, c} for the Polynomial cndkernel function "polycnd".
      \item \code{gamma} for the Radial Basis cndkernel function "rbfcnd" and the Laplacian cndkernel function "laplcnd" and the Cauchy cndkernel function "caucnd".
      \item \code{d, sigma} for the ANOVA cndkernel function "anocnd".
      \item \code{c} for the Rational Quadratic cndkernel function "raticnd" , the Multiquadric cndkernel function "multcnd" and the Inverse Multiquadric cndkernel function "invcnd".
      \item \code{theta} for the Wave cndkernel function "wavcnd".
      \item \code{d} for the d cndkernel function "powcnd" , the Log cndkernel function "logcnd" and the Generalized T-Student cndkernel function "studcnd".
    }
    Hyper-parameters for user defined kernels can be passed through the
    qpar parameter as well.}

  \item{qkernel}{the kernel function to be used to calculate the qkernel
    matrix.}
  \item{cndkernel}{the cndkernel function to be used to calculate the CND kernel
    matrix.}

  \item{k}{the dimension of the original data.}

  \item{dims}{Number of features to return. (default: 2)}

  \item{Initialisation}{\code{"random"} or \code{"pca"}; the former performs
fast random projection and the latter performs standard PCA  (default : \code{"random") }}
  \item{MaxHalves}{maximum number of step halvings. (default : 20) }
  \item{MaxIter}{the maximum number of iterations allowed. (default : 500)}
  \item{TolFun}{relative tolerance on objective function. (default : 1e-7) }
  \item{na.action}{A function to specify the action to be taken if \code{NA}s are
      found. The default action is \code{na.omit}, which leads to rejection of cases
      with missing values on any required variable. An alternative
  	  is \code{na.fail}, which causes an error if \code{NA} cases
	    are found. (NOTE: If given, this argument must be named.)}
  \item{\dots}{ additional parameters}

}
\details{Using kernel functions one can efficiently compute
  principal components in high-dimensional
  feature spaces, related to input space by some non-linear map.\cr
  The data can be passed to the \code{qsammon} function in a \code{matrix}, in addition \code{qsammon} also supports input in the form of a
kernel matrix of class \code{qkernmatrix} or class \code{cndkernmatrix}.
}
\value{
\item{dimRed}{The matrix whose rows are embedded observations.}
\item{kcall}{The function call contained}
\item{cndkernf}{The kernel function used}
all the slots of the object can be accessed by accessor functions.
}
\references{
  Sammon, J.W. (1969) \emph{A Nonlinear Mapping for Data Structure Analysis}.
IEEE Transactions on Computers, C-18 5:401-409.
}
\author{Yusen Zhang \cr
\email{yusenzhang@126.com}}


\seealso{\code{\link{qkernmatrix}}, \code{\link{cndkernmatrix}}}
\examples{
data(iris)
train <- as.matrix(iris[,1:4])
labeltrain<- as.integer(iris[,5])
## S4 method for signature 'matrix'
kpc2 <- qsammon(train, kernel = "rbfbase", qpar = list(sigma = 2, q = 0.9), dims = 2,
                Initialisation = 'pca', TolFun = 1e-5)
plot(dimRed(kpc2), col = as.integer(labeltrain))
cndkernf(kpc2)


}
\keyword{cluster}

