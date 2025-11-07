\name{E_SIAR}
\alias{est_SIAR}
\docType{data}
\title{Data on the stations that make up the SIAR network}
\description{Information about the location and operational status of
  the stations that make up the SIAR network}

\usage{data(SIAR)}
\format{
  \code{est_SIAR} is a \code{data.table} with 625 estations containing the following information:
  \describe{
    \item{\code{Estacion}}{character, name of the station.}
    \item{\code{Codigo}}{character, code of the station.}
    \item{\code{Longitud}}{numeric, longitude of the station in degrees
      (negative is for locations in the west).}
    \item{\code{Latitud}}{numeric, latitud of the station in degrees.}
    \item{\code{Altitud}}{integer, altitude of the station in meters.}
    \item{\code{Fecha_Instalacion}}{Date, day the station was installed,
      and therefore, the start of its records.}
    \item{\code{Fecha_Baja}}{Date, day the station was decommisioned,
      and therefore, the end of its records (if its value is \code{NA},
      it means it is still operational).}
  }
}

\source{https://servicio.mapa.gob.es/websiar/}


\keyword{datasets}
