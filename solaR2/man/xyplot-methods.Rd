\name{D_xyplot-methods}
\docType{methods}
\alias{xyplot-methods}
\alias{xyplot,formula,Shade-method}
\alias{xyplot,formula,Meteo-method}
\alias{xyplot,formula,Sol-method}
\alias{xyplot,formula,G0-method}
\alias{xyplot,data.table,missing-method}
\alias{xyplot,Meteo,missing-method}
\alias{xyplot,G0,missing-method}
\alias{xyplot,ProdPVPS,missing-method}
\alias{xyplot,ProdGCPV,missing-method}

\title{Methods for function xyplot in Package `solaR'}
\description{Methods for function \code{xyplot} in Package `solaR'}
\section{Methods}{
  \describe{
    \item{\code{signature(x = "data.table", data = "missing")}: }{
      This method creates an XY plot for objects of class
      \code{data.table} without specifying a \code{data} argument. It
      must contain a column named \code{Dates} with the time information.}

    \item{\code{signature(x = "formula", data = "Meteo")}: }{
      The \code{Meteo} object is converted into a \code{data.table} object with
      \code{getData(x)} and displayed with the method for \code{data.table}.
      

    }
    \item{\code{signature(x = "formula", data = "Sol")}: }{
      The \code{Sol} object is converted into a \code{data.table} object
      with \code{as.data.tableI(x, complete = TRUE, day = TRUE)} and
      displayed with the method for \code{data.table}.
    }

    \item{\code{signature(x = "formula", data = "G0")}: }{
      Idem.
    }

    \item{\code{signature(x = "Meteo", data = "missing")}: }{
      The \code{Meteo} object is converted into a \code{data.table}
      object with \code{getData(data)}. This \code{data.table} is the
      \code{x} argument for a call to \code{xyplot}, using the S4
      method for \code{signature(x = "data.table", data = "missing")}.
    }

    \item{\code{signature(x = "G0", data = "missing")}: }{
      The \code{G0} object is converted into a \code{data.table} object
      with \code{indexD(data)}. This \code{data.table} is the \code{x}
      argument for a call to \code{xyplot}, using the S4 method for
      \code{signature(x = 'data.table', data = 'missing')}.
    }

    \item{\code{signature(x = "ProdGCPV", data = "missing")}: }{
      Idem, but the variables are not superposed.}

    \item{\code{signature(x = "ProdPVPS", data = "missing")}: }{
      Idem.}

    \item{\code{signature(x = "formula", data = "Shade")}: }{
      Idem.}

}}
\author{Oscar Perpiñán Lamigueiro,
  Francisco Delgado López.}
\keyword{methods}

