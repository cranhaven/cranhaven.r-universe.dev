\name{qkMDS-class}
\docType{class}
\alias{qkMDS-class}


\alias{prj,qkMDS-method}
\alias{dims,qkMDS-method}
\alias{connum,qkMDS-method}
\alias{Residuals,qkMDS-method}
\alias{eVal,qkMDS-method}
\alias{eVec,qkMDS-method}
\alias{prj<-,qkMDS-method}
\alias{dims<-,qkMDS-method}
\alias{connum<-,qkMDS-method}
\alias{Residuals<-,qkMDS-method}
\alias{eVal<-,qkMDS-method}
\alias{eVec<-,qkMDS-method}


\title{qKernel Metric Multi-Dimensional Scaling}
\description{The qkernel Metric Multi-Dimensional Scaling class}
\section{Objects of class "qkMDS"}{
  Objects can be created by calls of the form \code{new("qkMDS", ...)}.
  or by calling the \code{qkMDS} function.
}
\section{Slots}{
  \describe{
    \item{\code{prj}:}{Object of class \code{"matrix"} containing the Nxdim matrix (N samples, dim features) with the reduced input data (list of         several matrices if more than one dimension specified)}
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

    \item{prj}{\code{signature(object = "qkMDS")}: returns the Nxdim
    matrix (N samples, dim features) }
    \item{dims}{\code{signature(object = "qkMDS")}: returns the
      dimension }
    \item{Residuals}{\code{signature(object = "qkMDS")}: returns the
      residual variances }
    \item{eVal}{\code{signature(object = "qkMDS")}: returns the
      eigenvalues}
    \item{eVec}{\code{signature(object = "qkMDS")}: returns the
      eigenvectors }
    \item{xmatrix}{\code{signature(object = "qkMDS")}: returns the used
      data matrix }
    \item{kcall}{\code{signature(object = "qkMDS")}: returns the
      performed call}
    \item{cndkernf}{\code{signature(object = "qkMDS")}: returns the used
      kernel function}
  }
}

\author{Yusen Zhang\cr \email{yusenzhang@126.com}}

\seealso{
  \code{\link{qkernel-class}},
  \code{\link{cndkernel-class}},
  \code{\link{qkMDS}}
}
\examples{
   # another example using the iris
  data(iris)
  testset <- sample(1:150,20)
  train <- as.matrix(iris[-testset,-5])
  labeltrain<- as.integer(iris[-testset,5])
  test <- as.matrix(iris[testset,-5])
  # ratibase(c=1,q=0.8)
  d_low = qkMDS(train, kernel = "ratibase", qpar = list(c=1,q=0.8),
                    dims=2, plotResiduals = TRUE)
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
