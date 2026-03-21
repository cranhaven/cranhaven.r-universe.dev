\name{qkspecc-class}
\docType{class}
\alias{qkspecc-class}

\alias{withinss}
\alias{withinss<-}

\alias{clust,qkspecc-method}
\alias{eVec,qkspecc-method}
\alias{eVal,qkspecc-method}
\alias{withinss,qkspecc-method}
\alias{plot,qkspecc-method}
\alias{clust<-,qkspecc-method}
\alias{eVec<-,qkspecc-method}
\alias{eVal<-,qkspecc-method}
\alias{withinss<-,qkspecc-method}

\title{Class "qkspecc"}
\description{ The qKernel Spectral Clustering Class}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("qkspecc", ...)}.
    or by calling the function \code{qkspecc}.
}
\section{Slots}{
  \describe{
    \item{\code{clust}:}{Object of class \code{"vector"} containing the cluster assignments}
    \item{\code{eVec}:}{Object of class \code{"matrix"} containing
      the corresponding eigenvector in each  cluster}
    \item{\code{eVal}:}{Object of class \code{"vector"} containing
      the corresponding eigenvalue for each cluster}
    \item{\code{withinss}:}{Object of class \code{"vector"} containing
      the within-cluster sum of squares for each cluster}
  }
}
\section{Methods}{
  \describe{
    \item{clust}{\code{signature(object = "qkspecc")}: returns the cluster assignments}
    \item{eVec}{\code{signature(object = "qkspecc")}: returns the corresponding eigenvector in each  cluster}
    \item{eVal}{\code{signature(object = "qkspecc")}: returns the corresponding eigenvalue for each cluster }
    \item{xmatrix}{\code{signature(object = "qkspecc")}: returns the original data matrix or a kernel Matrix }
    \item{ymatrix}{\code{signature(object = "qkspecc")}: returns The eigenvectors corresponding to the \eqn{k} smallest eigenvalues of the graph Laplacian matrix. }
    \item{cndkernf}{\code{signature(object = "qkspecc")}: returns the used kernel function}
    \item{kcall}{\code{signature(object = "qkspecc")}: returns the performed call}
  }
}

\author{Yusen Zhang \cr
   \email{yusenzhang@126.com}}


\seealso{
  \code{\link{qkspecc}}, \code{\link{qkernel-class}},
  \code{\link{cndkernel-class}}
}

\examples{
## Cluster the iris data set.
data("iris")
x=as.matrix(iris[,-5])

qspe <- qkspecc(x,kernel = "rbfbase", qpar = list(sigma = 10, q = 0.9),
                Nocent=3, normalize="symmetric", maxk=15, iterations=1200)
clust(qspe)
eVec(qspe)
eVal(qspe)
xmatrix(qspe)
ymatrix(qspe)
cndkernf(qspe)
}
\keyword{classes}
