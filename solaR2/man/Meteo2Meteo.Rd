\name{A8_Meteo2Meteo}
\alias{Meteoi2Meteod}
\alias{Meteod2Meteom}

\title{Transformation of intradaily meteorological data into daily and
  daily into monthly data.}

\description{Functions for the class \code{Meteo} that transforms an
  intradaily \code{Meteo} object into a daily and a daily into a
  monthly.}

\usage{
Meteoi2Meteod(G0i)

Meteod2Meteom(G0d)
}

\arguments{
  \item{G0i}{A \code{Meteo} object with intradaily data}

  \item{G0d}{A \code{Meteo} object with daily data}
}

\value{A \code{Meteo} object}

\author{Oscar Perpiñán Lamigueiro,
  Francisco Delgado López.}

\seealso{
  \code{\link{readBDd}},
  \code{\link{readG0dm}},
  \code{\link{readSIAR}}
}

\examples{
library("data.table")
\dontshow{setDTthreads(2)}

G0dm <- c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,
         7.027,5.369,3.562,2.814,2.179) * 1000;
Ta <- c(10, 14.1, 15.6, 17.2, 19.3, 21.2,
       28.4, 29.9, 24.3, 18.2, 17.2, 15.2)
prom <- list(G0dm = G0dm, Ta = Ta)

g0 <- calcG0(lat = 37.2, dataRad = prom, modeRad = 'aguiar')
G0i <- as.data.tableI(g0)
G0i <- dt2Meteo(G0i, lat = 37.2)
G0i

G0d <- Meteoi2Meteod(G0i)
G0d

G0m <- Meteod2Meteom(G0d)
G0m

}

