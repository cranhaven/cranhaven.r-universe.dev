\name{Biclust-class}
\title{The Biclust Class}
\docType{class}
\alias{Biclust}
\alias{Biclust-class}
\alias{BiclustResult}
\alias{show,Biclust-method}
\alias{summary,Biclust-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\description{Biclust is the class structure for results of a bicluster algorithm. It contains all information needed for further processing.
    The \code{show} Method gives the Name of the Algorithm used and the first Bicluster found.
    The \code{summary} Method gives sizes of all bicluster found.
}

\section{Objects from the Class}{
  Objects can be created by performing a bicluster algorithm via the \code{biclust()} function.} 


\section{Slots}{
  Objects of class \code{Biclust} have the following slots:
  \describe{
    \item{\code{Parameters}:}{Saves input Parameters in a list}

    \item{\code{RowxNumber}:}{Logical Matrix which contains 1 in [i,j] if Row i is in Bicluster j}
    
    \item{\code{NumberxCol}:}{Logical Matrix which contains 1 in [i,j] if Col j is in Bicluster i}

    \item{\code{Number}:}{Number of Bicluster}

    \item{\code{info}:}{Additional Outputs from the different bicluster algorithms}
     }
  }
\section{Details}{
\code{RowxNumber} and \code{NumberxCol} are named after the arrangement of the data they contain. The column results are transposed in order to ensure a easy processing.
}
\author{Sebastian Kaiser 
\email{sebastian.kaiser@stat.uni-muenchen.de}
}



\seealso{
  \code{\link{biclust}}, \code{\link{BiclustMethod-class}}
}
\keyword{classes}
%\examples{}
