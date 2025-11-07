\name{D_levelplot-methods}
\docType{methods}
\alias{levelplot-methods}
\alias{levelplot,formula,zoo-method}
\alias{levelplot,formula,Sol-method}
\alias{levelplot,formula,Meteo-method}
\alias{levelplot,formula,G0-method}
\title{Methods for function levelplot.}
\description{
  Methods for function \code{levelplot} and \code{zoo} and \code{solaR} objects.
}
\section{Methods}{
  \describe{

    \item{\code{signature(x = "formula", data = "Meteo")}: }{
      The \code{Meteo} object is converted into a \code{data.table} object, and
      the previous method is used.}

    \item{\code{signature(x = "formula", data = "Sol")}: }{idem}

    \item{\code{signature(x = "formula", data = "G0")}: }{idem}

  }
}
\author{Oscar Perpiñán Lamigueiro,
  Francisco Delgado López.}
\keyword{methods}

