\name{qkdbscan}
\alias{qkdbscan}
\alias{qkdbscan,matrix-method}
\alias{qkdbscan,cndkernmatrix-method}
\alias{qkdbscan,qkernmatrix-method}
\alias{print,qkdbscan-method}
\alias{predict,qkdbscan-method}

\title{qKernel-DBSCAN density reachability and connectivity clustering}
\description{
Similiar to the Density-Based Spatial Clustering of Applications with Noise(or DBSCAN) algorithm,
qKernel-DBSCAN is a density-based clustering algorithm that can be applied under both linear and non-linear situations.}
\usage{
\S4method{qkdbscan}{matrix}(x, kernel = "rbfbase", qpar = list(sigma = 0.1, q = 0.9),
eps = 0.25, MinPts = 5, hybrid = TRUE, seeds = TRUE,  showplot  = FALSE,
countmode = NULL, na.action = na.omit, ...)

\S4method{qkdbscan}{cndkernmatrix}(x, eps = 0.25, MinPts = 5, seeds = TRUE,
showplot  = FALSE, countmode = NULL, ...)

\S4method{qkdbscan}{qkernmatrix}(x, eps = 0.25, MinPts = 5, seeds = TRUE,
showplot  = FALSE, countmode = NULL, ...)

\S4method{predict}{qkdbscan}(object, data, newdata = NULL, predict.max = 1000, ...)

}

\arguments{
  \item{x}{the data matrix indexed by row, or a kernel matrix of \code{cndkernmatrix} or \code{qkernmatrix}.}
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

  \item{eps}{reachability distance, see Ester et al. (1996). (default:0.25)}
  \item{MinPts}{reachability minimum number of points, see Ester et al.(1996).(default : 5) }
  \item{hybrid}{whether the algothrim expects raw data but calculates partial distance matrices, can be TRUE or FALSE}
  \item{seeds}{can be TRUE or FALSE, FALSE to not include the \code{isseed}-vector in the \code{dbscan}-object.}
  \item{showplot}{whether to show the plot or not, can be TRUE or FALSE}
  \item{na.action}{a function to specify the action to be taken if \code{NA}s are
          found. The default action is \code{na.omit}, which leads to rejection of cases
          with missing values on any required variable. An alternative
	        is \code{na.fail}, which causes an error if \code{NA} cases  are found.
          (NOTE: If given, this argument must be named.)}
	\item{countmode}{NULL or vector of point numbers at which to report progress. }
  \item{object}{object of class \code{dbscan}.}
  \item{data}{matrix or data.frame.}
  \item{newdata}{matrix or data.frame with raw data to predict. }
  \item{predict.max}{max. batch size for predictions. }
  \item{\dots}{Further arguments transferred to plot methods.}
}
\details{
  The data can be passed to the \code{qkdbscan} function in a \code{matrix}, in addition \code{qkdbscan} also supports input in the form of a kernel matrix of class \code{qkernmatrix} or class \code{cndkernmatrix}.
}
\value{

\code{predict}(qkdbscan-method) gives out a vector of predicted clusters for the points in \code{newdata}.

\code{qkdbscan} gives out an S4 object which is a LIST with components
 \item{clust}{integer vector coding cluster membership with noise observations (singletons) coded as 0 }
 \item{eps}{parameter eps}
 \item{MinPts}{parameter MinPts}
 \item{kcall}{the function call}
 \item{cndkernf}{the kernel function used}
 \item{xmatrix}{the original data matrix}

all the slots of the object can be accessed by accessor functions.
}
\note{The predict function can be used to embed new data on the new space.}
\references{
  Martin Ester, Hans-Peter Kriegel, Joerg Sander, Xiaowei Xu(1996).\cr
  \emph{A Density-Based Algorithm for Discovering Clusters in Large Spatial Databases with Noise}\cr
  Institute for Computer Science, University of Munich.\cr
  \emph{Proceedings of 2nd International Conference on Knowledge Discovery and Data Mining (KDD-96)}

}
\author{Yusen Zhang \cr
\email{yusenzhang@126.com}}


\seealso{\code{\link{qkernmatrix}}, \code{cndkernmatrix}}
\examples{
# a simple example using the iris
data(iris)
test <- sample(1:150,20)
x<- as.matrix(iris[-test,-5])
ds <- qkdbscan (x,kernel="laplbase",qpar=list(sigma=3.5,q=0.8),eps=0.15,
MinPts=5,hybrid = FALSE)
plot(ds,x)
emb <- predict(ds, x, as.matrix(iris[test,-5]))
points(iris[test,], col= as.integer(1+emb))

}
\keyword{cluster}

