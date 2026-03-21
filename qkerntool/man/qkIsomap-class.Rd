\name{qkIsomap-class}
\docType{class}
\alias{qkIsomap-class}


\alias{dims}
\alias{connum}
\alias{Residuals}
\alias{dims<-}
\alias{connum<-}
\alias{Residuals<-}


\alias{prj,qkIsomap-method}
\alias{dims,qkIsomap-method}
\alias{connum,qkIsomap-method}
\alias{Residuals,qkIsomap-method}
\alias{eVal,qkIsomap-method}
\alias{eVec,qkIsomap-method}
\alias{prj<-,qkIsomap-method}
\alias{dims<-,qkIsomap-method}
\alias{connum<-,qkIsomap-method}
\alias{Residuals<-,qkIsomap-method}
\alias{eVal<-,qkIsomap-method}
\alias{eVec<-,qkIsomap-method}


\title{qKernel Isomap embedding}
\description{The qKernel Isometric Feature Mapping class}
\section{Objects of class "qkIsomap"}{
  Objects can be created by calls of the form \code{new("qkIsomap", ...)}.
  or by calling the \code{qkIsomap} function.
}
\section{Slots}{
  \describe{
    \item{\code{prj}:}{Object of class \code{"matrix"} containing the Nxdim matrix (N samples, dim features) with the reduced input data (list of several matrices if more than one dimension specified)}
    \item{\code{dims}:}{Object of class \code{"numeric"} containing the
      dimension of the target space (default 2)}
    \item{\code{connum}:}{Object of class \code{"numeric"} containing the number of connected components in graph}
    \item{\code{Residuals}:}{Object of class \code{"vector"} containing the
      residual variances for all dimensions}
    \item{\code{eVal}:}{Object of class \code{"vector"} containing the corresponding eigenvalues}
    \item{\code{eVec}:}{Object of class \code{"vector"} containing the corresponding eigenvectors }
   }
}

\section{Methods}{
  \describe{

    \item{prj}{\code{signature(object = "qkIsomap")}: returns the Nxdim
    matrix (N samples, dim features) }
    \item{dims}{\code{signature(object = "qkIsomap")}: returns the
      dimension }
    \item{Residuals}{\code{signature(object = "qkIsomap")}: returns the
      residual variances }
    \item{eVal}{\code{signature(object = "qkIsomap")}: returns the
      eigenvalues}
    \item{eVec}{\code{signature(object = "qkIsomap")}: returns the
      eigenvectors }
    \item{xmatrix}{\code{signature(object = "qkIsomap")}: returns the used
      data matrix }
    \item{kcall}{\code{signature(object = "qkIsomap")}: returns the
      performed call}
    \item{cndkernf}{\code{signature(object = "qkIsomapa")}: returns the used
      kernel function}
  }
}

\author{Yusen Zhang\cr \email{yusenzhang@126.com}}

\seealso{
  \code{\link{qkernel-class}},
  \code{\link{cndkernel-class}},
  \code{\link{qkIsomap}}
}
\examples{
   # another example using the iris data
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
\keyword{classes}
