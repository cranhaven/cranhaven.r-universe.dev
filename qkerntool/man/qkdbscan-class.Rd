\name{qkdbscan-class}
\docType{class}
\alias{qkdbscan-class}

\alias{clust}
\alias{eps}
\alias{plot}
\alias{MinPts}
\alias{isseed}
\alias{clust<-}
\alias{eps<-}
\alias{MinPts<-}
\alias{isseed<-}




\alias{clust,qkdbscan-method}
\alias{eps,qkdbscan-method}
\alias{plot,qkdbscan-method}
\alias{MinPts,qkdbscan-method}
\alias{isseed,qkdbscan-method}
\alias{clust<-,qkdbscan-method}
\alias{eps<-,qkdbscan-method}
\alias{MinPts<-,qkdbscan-method}
\alias{isseed<-,qkdbscan-method}


\title{Class "qkdbscan"}
\description{ The qkernel-DBSCAN class.}
\section{Objects of class "qkdbscan"}{
  Objects can be created by calls of the form \code{new("qkdbscan", ...)}.
  or by calling the \code{qkdbscan} function.
}
\section{Slots}{
  \describe{
    \item{\code{clust}:}{Object of class \code{"vector"} containing the
      cluster membership of the samples}
    \item{\code{eps}:}{Object of class \code{"numeric"} containing the
      reachability distance}
    \item{\code{MinPts}:}{Object of class \code{"numeric"} containing the
      reachability minimum number of points}
    \item{\code{isseed}:}{Object of class \code{"logical"} containing the logical vector indicating
    whether a point is a seed (not  border, not noise)}
  }
}
\section{Methods}{
  \describe{

    \item{clust}{\code{signature(object = "qkdbscan")}: returns the cluster membership}
    \item{kcall}{\code{signature(object = "qkdbscan")}: returns the performed call}
    \item{cndkernf}{\code{signature(object = "qkdbscan")}: returns the used kernel function}
    \item{eps}{\code{signature(object = "qkdbscan")}: returns the reachability distance}
    \item{MinPts}{\code{signature(object = "qkdbscan")}: returns the reachability minimum number of points}
    \item{predict}{\code{signature(object = "qkdbscan")}: embeds new data }
    \item{xmatrix}{\code{signature(object = "qkdbscan")}: returns the used data matrix }
  }
}

\author{Yusen Zhang\cr
\email{yusenzhang@126.com}}

\seealso{
  \code{\link{qkernel-class}},
  \code{\link{cndkernel-class}}
}
\examples{

# a simple example using the iris data
x<- as.matrix(iris[,-5])
ds <- qkdbscan (x,kernel="laplbase",qpar=list(sigma=3.5,q=0.8),eps=0.15,
MinPts=5,hybrid = FALSE)
# print the results
clust(ds)
eps(ds)
MinPts(ds)
cndkernf(ds)
xmatrix(ds)
kcall(ds)

}
\keyword{classes}
