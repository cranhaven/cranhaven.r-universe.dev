\name{D_as.data.tableY-methods}
\docType{methods}
\alias{as.data.tableY}
\alias{as.data.tableY-methods}
\alias{as.data.tableY,G0-method}
\alias{as.data.tableY,Gef-method}
\alias{as.data.tableY,ProdGCPV-method}
\alias{as.data.tableY,ProdPVPS-method}

\title{Methods for Function as.data.tableY}
\description{Convert a \code{G0}, \code{Gef},
  \code{ProdGCPV} or \code{ProdPVPS} object into a
  \code{data.table} object with yearly values.}

\usage{
\S4method{as.data.tableY}{G0}(object, complete=FALSE, day=FALSE)
}

\arguments{

  \item{object}{A \code{G0} object (or extended.)}
  \item{complete}{A logical.}
  \item{day}{A logical.}
  }
\section{Methods}{
\describe{
\item{\code{signature(object = "G0")}}{The result is the \code{G0y}
  slot. If \code{day = TRUE} (default is \code{FALSE}), the result
  includes a column named \code{year}.
}

\item{\code{signature(object = "Gef")}}{If \code{complete=FALSE}
  (default) the result is the slot \code{Gefy}. If
  \code{complete=TRUE} it returns the slot \code{G0y}.
}

\item{\code{signature(object = "ProdGCPV")}}{If \code{complete=FALSE}
  (default) the result is the \code{prody} slot. If
  \code{complete=TRUE} the result includes the slots \code{G0y} and \code{Gefy}.
}

\item{\code{signature(object = "ProdPVPS")}}{If \code{complete=FALSE}
  (default) the result is the \code{prody} slot. If
  \code{complete=TRUE} the result includes the slots \code{G0y} and \code{Gefy}.
}

}}

\examples{
library("data.table")
\dontshow{setDTthreads(2)}

lat <- 37.2
G0dm <- c(2766, 3491, 4494, 5912, 6989, 7742, 7919, 7027, 5369, 3562,
          2814, 2179)
Ta <- c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2,
        17.2, 15.2)
prom <- list(G0dm = G0dm, Ta = Ta)
prodfixed = prodGCPV(lat, dataRad = prom)
prodY = as.data.tableY(prodfixed, complete = TRUE, day = TRUE)
prodY
}

\author{Oscar Perpiñán Lamigueiro,
  Francisco Delgado López.}

\keyword{methods}
