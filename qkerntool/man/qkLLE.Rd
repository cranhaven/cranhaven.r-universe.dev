\name{qkLLE}
\alias{qkLLE}
\alias{qkLLE,matrix-method}
\alias{qkLLE,cndkernmatrix-method}
\alias{qkLLE,qkernmatrix-method}

\title{qKernel Locally Linear Embedding}
\description{
	Computes the qkernel Locally Linear Embedding
}
\usage{
\S4method{qkLLE}{matrix}(x, kernel = "rbfbase", qpar = list(sigma = 0.1, q = 0.9),
                         dims = 2, k, na.action = na.omit, ...)
\S4method{qkLLE}{cndkernmatrix}(x, dims = 2, k, na.action = na.omit, ...)
\S4method{qkLLE}{qkernmatrix}(x, dims = 2, k, na.action = na.omit,...)
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
      \item \code{power, alpha, c} for the Polynomial cndkernel function "polycnd".
      \item \code{gamma} for the Radial Basis cndkernel function "rbfcnd" and the Laplacian cndkernel function "laplcnd" and the Cauchy cndkernel function "caucnd".
      \item \code{power, sigma} for the ANOVA cndkernel function "anocnd".
      \item \code{c} for the Rational Quadratic cndkernel function "raticnd" , the Multiquadric cndkernel function "multcnd" and the Inverse Multiquadric cndkernel function "invcnd".
      \item \code{theta} for the Wave cndkernel function "wavcnd".
      \item \code{power} for the Power cndkernel function "powcnd" , the Log cndkernel function "logcnd" and the Generalized T-Student cndkernel function "studcnd".


    }
    Hyper-parameters for user defined kernels can be passed through the
    qpar parameter as well.}

  \item{dims}{dimension of the target space}

  \item{k}{the number of nearest neighbours.}

  \item{na.action}{A function to specify the action to be taken if \code{NA}s are
          found. The default action is \code{na.omit}, which leads to rejection of cases
          with missing values on any required variable. An alternative
	  is \code{na.fail}, which causes an error if \code{NA} cases
	  are found. (NOTE: If given, this argument must be named.)}

  \item{\dots}{additional parameters}
}
\details{

	The qkernel Locally Linear Embedding (qkLLE) preserves local properties of the data by
	representing each sample in the data by a linear combination of
	its k nearest neighbours with each neighbour weighted
	independently. qkLLE finally chooses the low-dimensional
	representation that best preserves the weights in the target
	space. It is an extension of Locally Linear Embedding (LLE) with qkernel method. \cr

}

\value{
	It returns an S4 object containing the principal component vectors along with the
 corresponding eigenvalues.
  \item{prj}{a matrix with the reduced input data}
  \item{dims}{dimension of the target space}
  \item{eVal}{The corresponding eigenvalues}
  \item{eVec}{The corresponding eigenvectors}
  \item{cndkernf}{the kernel function used}

all the slots of the object can be accessed by accessor functions.
}

\references{
	Roweis, Sam T. and Saul, Lawrence K., "Nonlinear Dimensionality Reduction by Locally Linear Embedding",2000;
}
\author{Yusen Zhang \cr
\email{yusenzhang@126.com}}
\examples{
## S4 method for signature 'matrix'
data(iris)
testset <- sample(1:150,20)
train <- as.matrix(iris[-testset,-5])
labeltrain<- as.integer(iris[-testset,5])
test <- as.matrix(iris[testset,-5])
plot(train ,col=labeltrain, xlab="1st Principal Component",ylab="2nd Principal Component")
# ratibase(c=1,q=0.8)
d_low <- qkLLE(train, kernel = "ratibase", qpar = list(c=1,q=0.8), dims=2, k=5)
#plot the data projection on the components
plot(prj(d_low),col=labeltrain, xlab="1st Principal Component",ylab="2nd Principal Component")

## S4 method for signature 'qkernmatrix'
# ratibase(c=0.1,q=0.8)
qkfunc <- ratibase(c=0.1,q=0.8)
ktrain1 <- qkernmatrix(qkfunc,train)
d_low <- qkLLE(ktrain1, dims = 2, k=5)
#plot the data projection on the components
plot(prj(d_low),col=labeltrain, xlab="1st Principal Component",ylab="2nd Principal Component")
}


