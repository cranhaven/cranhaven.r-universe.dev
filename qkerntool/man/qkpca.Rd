\name{qkpca}
\alias{qkpca}
\alias{qkpca,formula-method}
\alias{qkpca,matrix-method}
\alias{qkpca,cndkernmatrix-method}
\alias{qkpca,qkernmatrix-method}
\alias{predict,qkpca-method}


\title{qKernel Principal Components Analysis}
\description{The qkernel Principal Components Analysis is a nonlinear form of principal
component analysis.}
\usage{
\S4method{qkpca}{formula}(x, data = NULL, na.action, ...)
\S4method{qkpca}{matrix}(x, kernel = "rbfbase", qpar = list(sigma = 0.1, q = 0.9),
                        features = 0, th = 1e-4, na.action = na.omit, ...)
\S4method{qkpca}{cndkernmatrix}(x, features = 0, th = 1e-4, ...)
\S4method{qkpca}{qkernmatrix}(x, features = 0, th = 1e-4, ...)
}

\arguments{
  \item{x}{the data matrix indexed by row, a formula describing the
    model or a kernel matrix of \code{cndkernmatrix} or \code{qkernmatrix}.}
\item{data}{an optional data frame containing the variables in
	  the model (when using a formula).}

	\item{kernel}{the kernel function used in training and predicting.
    This parameter can be set to any function, of class kernel, which computes a  kernel function value between two
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
      \item \code{powcnd} power cndkernel function
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
      \item \code{d} for the power cndkernel function "powcnd" , the Log cndkernel function "logcnd" and the Generalized T-Student cndkernel function "studcnd".
    }
    Hyper-parameters for user defined kernels can be passed through the
    qpar parameter as well.}

  \item{features}{Number of features (principal components) to
    return. (default: 0 , all)}
  \item{th}{the value of the eigenvalue under which principal
      components are ignored (only valid when features =  0). (default : 0.0001) }
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
  The data can be passed to the \code{qkpca} function in a \code{matrix}, in addition \code{qkpca} also supports input in the form of a
kernel matrix of class \code{qkernmatrix} or class \code{cndkernmatrix}.
}
\value{
 An S4 object containing the principal component vectors along with the
 corresponding eigenvalues.
  \item{pcv}{a matrix containing the principal component vectors (column
  wise)}
\item{eVal}{The corresponding eigenvalues}
\item{rotated}{The original data projected (rotated) on the principal components}
\item{cndkernf}{the kernel function used}
\item{xmatrix}{The original data matrix}

all the slots of the object can be accessed by accessor functions.
}
\note{The predict function can be used to embed new data on the new space}
\references{
  Schoelkopf B., A. Smola, K.-R. Mueller :\cr
  \emph{Nonlinear component analysis as a kernel eigenvalue problem}\cr
  Neural Computation 10, 1299-1319\cr
  \url{http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.29.1366}
}
\author{Yusen Zhang \cr
\email{yusenzhang@126.com}}


\seealso{\code{\link{qkernmatrix}}, \code{\link{cndkernmatrix}}}
\examples{
# another example using the iris data
data(iris)
test <- sample(1:150,20)
qkpc <- qkpca(~.,data=iris[-test,-5],kernel="rbfbase",
              qpar=list(sigma=50,q=0.8),features=2)

# print the principal component vectors
pcv(qkpc)
#plot the data projection on the components
plot(rotated(qkpc),col=as.integer(iris[-test,5]),
     xlab="1st Principal Component",ylab="2nd Principal Component")

# embed remaining points
emb <- predict(qkpc,iris[test,-5])
points(emb,col=as.integer(iris[test,5]))
}
\keyword{cluster}

