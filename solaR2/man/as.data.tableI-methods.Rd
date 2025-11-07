\name{D_as.data.tableI-methods}
\docType{methods}
\alias{as.data.tableI}
\alias{as.data.tableI-methods}
\alias{as.data.tableI,G0-method}
\alias{as.data.tableI,Gef-method}
\alias{as.data.tableI,ProdGCPV-method}
\alias{as.data.tableI,ProdPVPS-method}
\alias{as.data.tableI,Sol-method}

\title{Methods for Function as.data.tableI}
\description{Convert a \code{Sol}, \code{G0}, \code{Gef},
  \code{ProdGCPV} or \code{ProdPVPS} object into a
  \code{data.table} object with daily values.}

\usage{
\S4method{as.data.tableI}{Sol}(object, complete=FALSE, day=FALSE)
}

\arguments{

  \item{object}{A \code{Sol} object (or extended.)}
  \item{complete}{A logical.}
  \item{day}{A logical.}
  }
\section{Methods}{
\describe{
\item{\code{signature(object = "Sol")}}{If \code{complete=FALSE} and
  \code{day=FALSE} (default) the result includes only the content of the
  \code{solI} slot. If \code{complete=TRUE} the contents of the
  \code{solD} slots are included.
}

\item{\code{signature(object = "G0")}}{If \code{complete=FALSE} and
  \code{day=FALSE} (default) the result includes only   the columns of
  \code{G0}, \code{D0} and \code{B0} of the \code{G0I} slot.
  If  \code{complete=TRUE} it returns the contents of the slots \code{G0I}
  and \code{solI}. If \code{day=TRUE} the daily values (slots \code{G0D}
  and \code{solD}) are also included.)
}

\item{\code{signature(object = "Gef")}}{If \code{complete=FALSE} and
  \code{day=FALSE} (default) the result includes only  the columns of
  \code{Gef}, \code{Def} and \code{Bef} of the \code{GefI} slot. If  \code{complete=TRUE} it
  returns the contents of the slots \code{GefI}, \code{G0I} and
  \code{solI}. If \code{day=TRUE} the daily values (slots \code{GefD}, \code{G0D}
  and \code{solD}) are also included.)
}

\item{\code{signature(object = "ProdGCPV")}}{If \code{complete=FALSE} and
  \code{day=FALSE} (default) the result includes only  the columns of
  \code{Pac} and \code{Pdc} of the \code{prodI} slot. If  \code{complete=TRUE} it
  returns the contents of the slots \code{prodI}, \code{GefI}, \code{G0I} and
  \code{solI}. If \code{day=TRUE} the daily values (slots \code{prodD}, \code{GefD}, \code{G0D}
  and \code{solD}) are also included.)
}

\item{\code{signature(object = "ProdPVPS")}}{If \code{complete=FALSE} and
  \code{day=FALSE} (default) the result includes only  the columns of
  \code{Pac} and \code{Q} of the \code{prodI} slot. If  \code{complete=TRUE} it
  returns the contents of the slots \code{prodI}, \code{GefI}, \code{G0I} and
  \code{solI}. If \code{day=TRUE} the daily values (slots \code{prodD}, \code{GefD}, \code{G0D}
  and \code{solD}) are also included.)
}

}}

\examples{
library("data.table")
\dontshow{setDTthreads(2)}

lat = 37.2
BTd = fBTd(mode = 'prom')[1]
sol = calcSol(lat, BTd, keep.night = FALSE)
solI = as.data.tableI(sol)
solI

solI2 = as.data.tableI(sol, day = TRUE)
solI2 

G0dm <- c(2766, 3491, 4494, 5912, 6989, 7742, 7919, 7027, 5369, 3562,
          2814, 2179)
Ta <- c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2,
        17.2, 15.2)
prom <- list(G0dm = G0dm, Ta = Ta)
prodfixed = prodGCPV(lat, dataRad = prom)
prodI = as.data.tableI(prodfixed, complete = TRUE, day = TRUE)
prodI
}

\author{Oscar Perpiñán Lamigueiro,
  Francisco Delgado López.}

\keyword{methods}
