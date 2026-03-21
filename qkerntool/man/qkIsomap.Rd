\name{qkIsomap}
\alias{qkIsomap}
\alias{qkIsomap,matrix-method}
\alias{qkIsomap,cndkernmatrix-method}
\alias{qkIsomap,qkernmatrix-method}
\title{
	qKernel Isometric Feature Mapping
}
\description{
	Computes the Isomap embedding as introduced in 2000 by Tenenbaum, de Silva and Langford.
}
\usage{
\S4method{qkIsomap}{matrix}(x, kernel = "rbfbase", qpar = list(sigma = 0.1, q = 0.9),
dims = 2, k, mod = FALSE, plotResiduals = FALSE, verbose = TRUE, na.action = na.omit, ...)

\S4method{qkIsomap}{cndkernmatrix}(x, dims = 2, k, mod = FALSE, plotResiduals = FALSE,
verbose = TRUE, na.action = na.omit, ...)

\S4method{qkIsomap}{qkernmatrix}(x, dims = 2, k, mod = FALSE, plotResiduals = FALSE,
verbose = TRUE, na.action = na.omit, ...)
}

\arguments{
  \item{x}{
	N x D matrix (N samples, D features) or a kernel matrix of \code{cndkernmatrix} or \code{qkernmatrix}.}

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
      \item \code{powbase} Power qkernel function
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
      \item \code{powcnd} Power cndkernel function
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
      \item \code{d, q} for the Power qkernel function "powbase" , the Log qkernel function "logbase" and the Generalized T-Student qkernel function "studbase".
      \item \code{alpha} for the Non Linear cndkernel function "nonlcnd".
      \item \code{d, alpha, c} for the Polynomial cndkernel function "polycnd".
      \item \code{gamma} for the Radial Basis cndkernel function "rbfcnd" and the Laplacian cndkernel function "laplcnd" and the Cauchy cndkernel function "caucnd".
      \item \code{d, sigma} for the ANOVA cndkernel function "anocnd".
      \item \code{c} for the Rational Quadratic cndkernel function "raticnd" , the Multiquadric cndkernel function "multcnd" and the Inverse Multiquadric cndkernel function "invcnd".
      \item \code{theta} for the Wave cndkernel function "wavcnd".
      \item \code{d} for the Power cndkernel function "powcnd" , the Log cndkernel function "logcnd" and the Generalized T-Student cndkernel function "studcnd".


    }
    Hyper-parameters for user defined kernels can be passed through the
    qpar parameter as well.}

  \item{dims}{vector containing the target space dimension(s)}
  \item{k}{	number of neighbours}
  \item{mod}{	use modified Isomap algorithm}
  \item{plotResiduals}{
	show a plot with the residuals between the high and the low dimensional data}
  \item{verbose}{show a summary of the embedding procedure at the end}
  \item{na.action}{A function to specify the action to be taken if \code{NA}s are
          found. The default action is \code{na.omit}, which leads to rejection of cases
          with missing values on any required variable. An alternative
	  is \code{na.fail}, which causes an error if \code{NA} cases
	  are found. (NOTE: If given, this argument must be named.)}
  \item{\dots}{ additional parameters}
}
\details{
  The qkIsomap is a nonlinear dimension reduction technique, that preserves
  global properties of the data. That means, that geodesic distances
  between all samples are captured best in the low dimensional
  embedding. \cr
  This R version is based on the Matlab implementation by Tenenbaum and
  uses Floyd's Algorithm to compute the neighbourhood graph of shortest
  distances, when calculating the geodesic distances. \cr
  A modified version of the original Isomap algorithm is included. It
  respects nearest and farthest neighbours. \cr
  To estimate the intrinsic dimension of the data, the function can plot
  the residuals between the high and the low dimensional data for a
  given range of dimensions.
}
\value{

 \code{qkIsomap} gives out an S4 object which is a LIST with components
 \item{prj}{a N x dim matrix (N samples, dim features) with the reduced input data (list of several matrices if more than one dimension was specified). }
 \item{dims}{the dimension of the target space.}
 \item{Residuals}{the residual variances for all dimensions.}
 \item{eVal}{the corresponding eigenvalues.}
 \item{eVec}{the corresponding eigenvectors.}
 \item{cndkernf}{the kernel function used.}
 \item{kcall}{The formula of the function called}

all the slots of the object can be accessed by accessor functions.
}
\references{
	Tenenbaum, J. B. and de Silva, V. and Langford, J. C., "A global geometric framework for nonlinear dimensionality reduction.", 2000;
	Matlab code is available at http://waldron.stanford.edu/~isomap/
}
\author{Yusen Zhang \cr
\email{yusenzhang@126.com}}

\examples{
 # another example using the iris
  data(iris)
  testset <- sample(1:150,20)
  train <- as.matrix(iris[-testset,-5])
  labeltrain<- as.integer(iris[-testset,5])
  test <- as.matrix(iris[testset,-5])
  # ratibase(c=1,q=0.8)
  d_low = qkIsomap(train, kernel = "ratibase", qpar = list(c=1,q=0.8),
                    dims=2,  k=5, plotResiduals = TRUE)
  #plot the data projection on the components
  plot(prj(d_low),col=labeltrain, xlab="1st Principal Component",ylab="2nd  Principal Component")

  prj(d_low)
	dims(d_low)
	Residuals(d_low)
	eVal(d_low)
	eVec(d_low)
	kcall(d_low)
	cndkernf(d_low)
}
