\name{cenmle-lognormal-class}
\docType{class}
\alias{cenmle-lognormal-class}

\title{Class "cenmle-lognormal"}
\description{
    A "cenmle-lognormal" object is returned from \code{cenmle} when a 
    lognormal distribution is chosen with the `dist' option.  
}
\section{Objects from the Class}{
    Objects can be created by calls of the form 
    \code{cenmle(obs, censored, dist="lognormal")}.
}
\section{Slots}{
  \describe{
    \item{\code{n}:}{Total number of observations associated with the model}
    \item{\code{n.cen}:}{Number of censored observations}
    \item{\code{y}:}{Vector of observations}
    \item{\code{ycen}:}{Censoring indicator}
    \item{\code{conf.int}:}{Confidence interval associated with the model}
    \item{\code{survreg}:}{Object of class \code{"survreg"}  }
  }
}
\section{Extends}{
Class \code{"cenmle"}
}
\section{Methods}{
  \describe{
    \item{mean}{\code{signature(x = "cenmle")}: ... }
    \item{median}{\code{signature(x = "cenmle")}: ... }
    \item{sd}{\code{signature(x = "cenmle")}: ... }
    %\item{predict}{\code{signature(object = "cenmle")}: ... }
    %\item{print}{\code{signature(x = "cenmle")}: ... }
    %\item{quantile}{\code{signature(x = "cenmle")}: ... }
    \item{summary}{\code{signature(object = "cenmle")}: ... }
  }
}
\author{
    R. Lopaka Lee <rclee@usgs.gov>

    Dennis Helsel <dhelsel@practicalstats.com>
} 
\seealso{
  \code{\link{cenmle}}
  \code{\link{survreg}}
}
\keyword{classes}
