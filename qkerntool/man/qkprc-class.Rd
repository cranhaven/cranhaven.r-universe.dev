\name{qkprc-class}
\docType{class}
\alias{qkprc-class}

\alias{cndkernf}
\alias{qpar}
\alias{xmatrix}
\alias{ymatrix}
\alias{kcall}
\alias{terms}
\alias{n.action}

\alias{cndkernf<-}
\alias{qpar<-}
\alias{xmatrix<-}
\alias{ymatrix<-}
\alias{kcall<-}
\alias{terms<-}
\alias{n.action<-}


\alias{cndkernf,qkprc-method}
\alias{qpar,qkprc-method}
\alias{xmatrix,qkprc-method}
\alias{ymatrix,qkprc-method}
\alias{kcall,qkprc-method}
\alias{terms,qkprc-method}
\alias{n.action,qkprc-method}
\alias{cndkernf<-,qkprc-method}
\alias{qpar<-,qkprc-method}
\alias{xmatrix<-,qkprc-method}
\alias{ymatrix<-,qkprc-method}
\alias{kcall<-,qkprc-method}
\alias{terms<-,qkprc-method}
\alias{n.action<-,qkprc-method}



\title{Class "qkprc"}
\description{The qKernel Prehead class}

\section{Objects of class "qkprc"}{Objects from the class cannot be created directly but only contained
  in other classes.}

\section{Slots}{
  \describe{
    \item{\code{cndkernf}:}{Object of class \code{"kfunction"} containing
      the kernel function used}
    \item{\code{qpar}:}{Object of class \code{"list"} containing the
      kernel parameters used }
    \item{\code{xmatrix}:}{Object of class \code{"input"} containing
      the data matrix used }
    \item{\code{ymatrix}:}{Object of class \code{"input"} containing
      the data matrix used }
    \item{\code{kcall}:}{Object of class \code{"ANY"} containing the
      function call }
    \item{\code{terms}:}{Object of class \code{"ANY"} containing the
      function terms }
    \item{\code{n.action}:}{Object of class \code{"ANY"} containing the
      action performed on NA }
  }
}
\section{Methods}{
  \describe{
    \item{cndkernf}{\code{signature(object = "qkprc")}: returns the used
      kernel function}
    \item{xmatrix}{\code{signature(object = "qkprc")}: returns the used
      data matrix }
    \item{ymatrix}{\code{signature(object = "qkprc")}: returns the used
      data matrix }
    \item{kcall}{\code{signature(object = "qkprc")}: returns the
      performed call}
  }
}

\author{Yusen Zhang\cr \email{yusenzhang@126.com}}

\seealso{
  \code{\link{qkernel-class}},
  \code{\link{cndkernel-class}}
}
\keyword{classes}
