\name{cenreg-gaussian-class}
\docType{class}
\alias{cenreg-gaussian-class}

\title{Class "cenreg-gaussian"}
\description{
    A "cenreg-gaussian" object is returned from \code{cenreg} when a 
    gaussian distribution is chosen with the `dist' option.  
}
\section{Objects from the Class}{
    Objects can be created by calls of the form 
    \code{cenreg(obs, censored, dist="gaussian")}.
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
Class \code{"cenreg"}
}
\section{Methods}{
  \describe{
    \item{predict}{\code{signature(object = "cenreg")}: ... }
    \item{print}{\code{signature(x = "cenreg")}: ... }
    \item{summary}{\code{signature(object = "cenreg")}: ... }
  }
}
\author{
    R. Lopaka Lee <rclee@usgs.gov>

    Dennis Helsel <dhelsel@practicalstats.com>
} 
\seealso{
  \code{\link{cenreg}}
  \code{\link{survreg}}
}
\keyword{classes}
