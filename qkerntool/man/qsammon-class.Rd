\name{qsammon-class}
\docType{class}
\alias{qsammon-class}
\alias{dimRed}
\alias{dimRed<-}
\alias{fun}

\alias{dimRed,qsammon-method}
\alias{dimRed<-,qsammon-method}



\title{Class "qsammon"}
\description{ The qKernel Sammon Mapping class}
\section{Objects of class "qsammon"}{
  Objects can be created by calls of the form \code{new("qsammon", ...)}.
  or by calling the \code{qsammon} function.
}
\section{Slots}{
  \describe{
    \item{\code{dimRed}:}{Object of class \code{"matrix"} containing the
      matrix whose rows are embedded observations }
    \item{\code{cndkernf}:}{Object of class \code{"function"} containing
      the kernel function used}
    \item{\code{kcall}:}{Object of class \code{"ANY"} containing the
      function call }
  }
}
\section{Methods}{
  \describe{

    \item{dimRed}{\code{signature(object = "qsammon")}: returns the matrix whose rows are embedded observations }
    \item{kcall}{\code{signature(object = "qsammon")}: returns the
      performed call}
    \item{cndkernf}{\code{signature(object = "qsammon")}: returns the used
      kernel function}
  }
}

\author{Yusen Zhang\cr \email{yusenzhang@126.com}}

\seealso{
  \code{\link{qsammon}}
}
\examples{
  data(iris)
  train <- as.matrix(iris[,1:4])
  labeltrain<- as.integer(iris[,5])
  ## S4 method for signature 'matrix'
  qkpc <- qsammon(train, kernel = "rbfbase", qpar = list(sigma = 0.5, q = 0.9),
                   dims = 2, Initialisation = 'pca', MaxHalves = 50)

  cndkernf(qkpc)
  dimRed(qkpc)
  kcall(qkpc)
}
\keyword{classes}
