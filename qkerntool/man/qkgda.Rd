\name{qkgda}
\alias{qkgda}
\alias{qkgda,matrix-method}
\alias{qkgda,cndkernmatrix-method}
\alias{qkgda,qkernmatrix-method}
\alias{predict,qkgda-method}


\title{qKernel Generalized Discriminant Analysis}
\description{
The qkernel Generalized Discriminant Analysis is a method that deals with nonlinear discriminant analysis using kernel function operator.}
\usage{
\S4method{qkgda}{matrix}(x, label, kernel = "rbfbase", qpar = list(sigma = 0.1, q = 0.9),
          features = 0, th = 1e-4, na.action = na.omit, ...)

\S4method{qkgda}{cndkernmatrix}(x, label, features = 0, th = 1e-4, na.action = na.omit, ...)
\S4method{qkgda}{qkernmatrix}(x, label, features = 0, th = 1e-4, ...)
}

\arguments{
  \item{x}{the data matrix indexed by row, or a kernel matrix of \code{cndkernmatrix} or \code{qkernmatrix}.}
  \item{label}{The original labels of the samples.}
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

\details{The qkernel Generalized Discriminant Analysis method provides a mapping of the input vectors into high dimensional feature space, generalizing the classical Linear Discriminant Analysis to non-linear discriminant analysis. \cr
    The data can be passed to the \code{qkgda} function in a \code{matrix}, in addition \code{qkgda} also supports input in the form of a
    kernel matrix of class \code{qkernmatrix} or class \code{cndkernmatrix}.
}
\value{
 An S4 object containing the eigenvectors and their normalized projections, along with the
 corresponding eigenvalues and the original function.
\item{prj}{The normalized projections on eigenvectors)}
\item{eVal}{The corresponding eigenvalues}
\item{eVec}{The corresponding eigenvectors}
\item{kcall}{The formula of the function called}
\item{cndkernf}{The kernel function used}
\item{xmatrix}{The original data matrix}

all the slots of the object can be accessed by accessor functions.
}
\note{The predict function can be used to embed new data on the new space}
\references{
      1.Baudat, G, and F. Anouar:\cr
      \emph{Generalized discriminant analysis using a kernel approach}\cr
      Neural Computation 12.10(2000),2385\cr
      2.Deng Cai, Xiaofei He, and Jiawei Han:\cr
      \emph{Speed Up Kernel Discriminant Analysis}\cr
      The VLDB Journal,January,2011,vol.20, no.1,21-33.
}
\author{Yusen Zhang \cr
\email{yusenzhang@126.com}}


\seealso{\code{\link{qkernmatrix}}, \code{\link{cndkernmatrix}}}
\examples{
Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]), Sp = rep(c("1","2","3"), rep(50,3)))
testset <- sample(1:150,20)
train <- as.matrix(iris[-testset,-5])
test <- as.matrix(iris[testset,-5])
Sp = rep(c("1","2","3"), rep(50,3))
labels <-as.numeric(Sp)
trainlabel <- labels[-testset]
testlabel <- labels[testset]

kgda1 <- qkgda(train, label=trainlabel, kernel = "ratibase", qpar = list(c=1,q=0.9),features = 2)

prj(kgda1)
eVal(kgda1)
eVec(kgda1)
kcall(kgda1)
# xmatrix(kgda1)

#print the principal component vectors
prj(kgda1)
#plot the data projection on the components
plot(kgda1@prj,col=as.integer(train), xlab="1st Principal Component",ylab="2nd Principal Component")

}
\keyword{classif}




