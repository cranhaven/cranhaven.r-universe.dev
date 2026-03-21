\name{qkspecc}
\alias{qkspecc}
\alias{qkspecc,matrix-method}
\alias{qkspecc,qkernmatrix-method}
\alias{qkspecc,cndkernmatrix-method}
\alias{show,qkspecc-method}

\title{qkernel spectral Clustering}
\description{A qkernel spectral clustering algorithm. Clustering is performed by
embedding the data into the subspace of the eigenvectors
of a graph Laplacian matrix.}

\usage{
\S4method{qkspecc}{matrix}(x,kernel = "rbfbase", qpar = list(sigma = 2, q = 0.9),
          Nocent=NA, normalize="symmetric", maxk=20, iterations=200,
          na.action = na.omit, ...)

\S4method{qkspecc}{cndkernmatrix}(x, Nocent=NA, normalize="symmetric",
          maxk=20,iterations=200, ...)

\S4method{qkspecc}{qkernmatrix}(x, Nocent=NA, normalize="symmetric",
          maxk=20,iterations=200, ...)
}

\arguments{
 \item{x}{the matrix of data to be clustered or a kernel Matrix of class
    \code{qkernmatrix} or \code{cndkernmatrix}.}


\item{kernel}{the kernel function used in computing the affinity matrix.
    This parameter can be set to any function, of class kernel, which computes a kernel function value between two
    vector arguments. kernlab provides the most popular kernel functions
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

  \item{qpar}{a character string or the list of hyper-parameters (kernel parameters).
    The default character string \code{list(sigma = 2, q = 0.9)} uses a heuristic to determine a
    suitable value for the width parameter of the RBF kernel.
    The second option \code{"local"} (local scaling) uses a more advanced heuristic
     and sets a width parameter for every point in the data set. This is
    particularly useful when the data incorporates multiple scales.
    A list can also be used containing the parameters to be used with the
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
       where length is the length of the strings considered, lambda the
       decay factor and normalized a logical parameter determining if the
       kernel evaluations should be normalized.
    }

    Hyper-parameters for user defined kernels can be passed through the
    qpar parameter as well.}

  \item{Nocent}{the number of clusters.}
  \item{normalize}{Normalisation of the Laplacian ("none", "symmetric" or "random-walk").}
  \item{maxk}{If k is NA, an upper bound for the automatic estimation. Defaults to 20.}
  \item{iterations}{the maximum number of iterations allowed. }
  \item{na.action}{the action to perform on NA.}
  \item{\dots}{additional parameters.}

}
\details{
  The qkernel spectral clustering  works by embedding the data points of the
partitioning problem into the subspace of the eigenvectors corresponding to the \eqn{k}
smallest eigenvalues of the graph Laplacian matrix. Using a simple clustering method like
\code{kmeans} on the embedded points usually leads to good performance. It can be shown that
qkernel spectral clustering methods boil down to  graph partitioning.\cr
The data can be passed to the \code{qkspecc} function in a \code{matrix},
in addition \code{qkspecc} also supports input in the form of a
kernel matrix of class \code{qkernmatrix} or \code{cndkernmatrix}.
}
\value{
 An S4 object of class \code{qkspecc} which extends the class \code{vector}
 containing integers indicating the cluster to which
 each point is allocated. The following slots contain useful information
  \item{clust}{The cluster assignments}
  \item{eVec}{The corresponding eigenvector}
  \item{eVal}{The corresponding eigenvalues}
  \item{ymatrix}{The eigenvectors corresponding to the \eqn{k} smallest eigenvalues
  of the graph Laplacian matrix.}

}
\references{Andrew Y. Ng, Michael I. Jordan, Yair Weiss\cr
  \emph{On Spectral Clustering: Analysis and an Algorithm}\cr
  Neural Information Processing Symposium 2001\cr
}
\author{Yusen Zhang \cr \email{yusenzhang@126.com}
}


\seealso{\code{\link{qkernmatrix}}, \code{\link{cndkernmatrix}}, \code{\link{qkpca}}}
\examples{
data("iris")
x=as.matrix(iris[,-5])

qspe <- qkspecc(x,kernel = "rbfbase", qpar = list(sigma = 10, q = 0.9),
                Nocent=3, normalize="symmetric", maxk=15, iterations=1200)
plot(x, col = clust(qspe))

qkfunc <- nonlbase(alpha=1/15,q=0.8)
Ktrain <- qkernmatrix(qkfunc, x)
qspe <- qkspecc(Ktrain, Nocent=3, normalize="symmetric", maxk=20)
plot(x, col = clust(qspe))
}
\keyword{cluster}

