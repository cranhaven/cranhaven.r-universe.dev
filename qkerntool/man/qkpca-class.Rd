\name{qkpca-class}
\docType{class}
\alias{qkpca-class}
\alias{rotated}
\alias{rotated<-}
\alias{pcv}
\alias{pcv<-}


\alias{eVal,qkpca-method}
\alias{pcv,qkpca-method}
\alias{rotated,qkpca-method}
\alias{eVal<-,qkpca-method}
\alias{pcv<-,qkpca-method}
\alias{rotated<-,qkpca-method}

\title{Class "qkpca"}
\description{ The qkernel Principal Components Analysis class}
\section{Objects of class "qkpca"}{
  Objects can be created by calls of the form \code{new("qkpca", ...)}.
  or by calling the \code{qkpca} function.
}
\section{Slots}{
  \describe{
    \item{\code{pcv}:}{Object of class \code{"matrix"} containing the
      principal component vectors }
    \item{\code{eVal}:}{Object of class \code{"vector"} containing the
      corresponding eigenvalues}
    \item{\code{rotated}:}{Object of class \code{"matrix"} containing the
      projection of the data on the principal components}
   }
}
\section{Methods}{
  \describe{

    \item{eVal}{\code{signature(object = "qkpca")}: returns the eigenvalues }
    \item{pcv}{\code{signature(object = "qkpca")}: returns the principal
      component vectors }
    \item{predict}{\code{signature(object = "qkpca")}: embeds new data }
    \item{rotated}{\code{signature(object = "qkpca")}: returns the
      projected data}
    \item{xmatrix}{\code{signature(object = "qkpca")}: returns the used
      data matrix }
    \item{kcall}{\code{signature(object = "qkpca")}: returns the
      performed call}
    \item{cndkernf}{\code{signature(object = "qkpca")}: returns the used
      kernel function}
  }
}

\author{Yusen Zhang\cr \email{yusenzhang@126.com}}

\seealso{
  \code{\link{qkernel-class}},
  \code{\link{cndkernel-class}}
}
\examples{
  # another example using the iris data
  data(iris)
  test <- sample(1:150,20)
  qkpc <- qkpca(~.,iris[-test,-5], kernel = "rbfbase",
                qpar = list(sigma = 50, q = 0.8), features = 2)

  # print the principal component vectors
  pcv(qkpc)
  rotated(qkpc)
  cndkernf(qkpc)
  eVal(qkpc)
  xmatrix(qkpc)
  names(eVal(qkpc))
}
\keyword{classes}
