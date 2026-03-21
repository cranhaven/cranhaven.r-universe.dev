\name{qkLLE-class}
\docType{class}
\alias{qkLLE-class}
\alias{prj,qkLLE-method}
\alias{dims,qkLLE-method}
\alias{eVal,qkLLE-method}
\alias{eVec,qkLLE-method}
\alias{prj<-,qkLLE-method}
\alias{dims<-,qkLLE-method}
\alias{eVal<-,qkLLE-method}
\alias{eVec<-,qkLLE-method}


\title{Class "qkLLE"}
\description{ The qKernel Locally Linear Embedding class}
\section{Objects of class "qkLLE"}{
  Objects can be created by calls of the form \code{new("qkLLE", ...)}.
  or by calling the \code{qkLLE} function.
}
\section{Slots}{
  \describe{
    \item{\code{prj}:}{Object of class \code{"matrix"} containing the reduced input data}
    \item{\code{dims}:}{Object of class \code{"numeric"} containing the
      dimension of the target space (default 2)}
    \item{\code{eVal}:}{Object of class \code{"vector"} containing the corresponding eigenvalues}
    \item{\code{eVec}:}{Object of class \code{"matrix"} containing the corresponding eigenvectors}
  }
 }

\section{Methods}{
  \describe{
    
    \item{prj}{\code{signature(object = "qkLLE")}: returns the reduced input data}
    \item{dims}{\code{signature(object = "qkLLE")}: returns the 
      dimension }
    \item{eVal}{\code{signature(object = "qkLLE")}: returns the 
      eigenvalues}
    \item{eVec}{\code{signature(object = "qkLLE")}: returns the 
      eigenvectors }
    \item{xmatrix}{\code{signature(object = "qkLLE")}: returns the used
      data matrix }  
    \item{kcall}{\code{signature(object = "qkLLE")}: returns the
      performed call}
    \item{cndkernf}{\code{signature(object = "qkLLE")}: returns the used
      kernel function}
      
  }
}

\author{Yusen Zhang\cr \email{yusenzhang@126.com}}

\seealso{
  \code{\link{qkernel-class}}, 
  \code{\link{cndkernel-class}} 
}
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
plot(prj(d_low),col=labeltrain,xlab="1st Principal Component",ylab="2nd Principal Component")

## S4 method for signature 'qkernmatrix'
# ratibase(c=0.1,q=0.8)
qkfunc <- ratibase(c=0.1,q=0.8)
ktrain1 <- qkernmatrix(qkfunc,train)
d_low <- qkLLE(ktrain1, dims = 2, k=5)
#plot the data projection on the components
plot(prj(d_low),col=labeltrain,xlab="1st Principal Component",ylab="2nd Principal Component")
  }
\keyword{classes}