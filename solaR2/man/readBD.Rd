\name{A8_readBD}
\Rdversion{1.1}
\encoding{UTF-8}
\alias{readBDd}
\alias{readBDi}
\alias{dt2Meteo}
\alias{zoo2Meteo}


\title{Daily or intradaily values of global horizontal irradiation and ambient
  temperature from a local file or a data.frame.}
\description{Constructor for the class \code{Meteo} with values of
  \emph{daily} or \emph{intradaily} values of global horizontal irradiation and ambient temperature
  from a local file or a data.frame.}
\usage{
readBDd(file,  lat,
        format = '\%d/\%m/\%Y',
        header = TRUE, fill = TRUE, dec = '.', sep = ';',
        dates.col = 'Dates', ta.col = 'Ta',
        g0.col = 'G0', keep.cols = FALSE, ...)

readBDi(file,  lat,
        format = '\%d/\%m/\%Y \%H:\%M:\%S',
        header = TRUE, fill = TRUE, dec = '.',
        sep = ';', dates.col = 'Dates', times.col,
        ta.col = 'Ta', g0.col = 'G0', keep.cols = FALSE, ...)

dt2Meteo(file,  lat, source = '', type)

zoo2Meteo(file, lat, source = '')
}
\arguments{

  \item{file}{The name of the file (\code{readBDd} and \code{readBDi}),
    \code{data.frame} (or \code{data.table}) (\code{dt2Meteo}) or \code{zoo}
    (\code{zoo2Meteo}) which the data are to be read from.
    It should contain a column \code{G0d} with \emph{daily}
    (\code{readBDd}) or \code{G0} with \emph{intradaily}
    (\code{readBDi}) values of global horizontal irradiation (Wh/m²). It
    should also include a column named \code{Ta} with values of ambient
    temperature. However, if the object is only a vector with
    irradiation values, it will converted to a \code{data.table} with
    two columns named \code{G0} and \code{Ta} (filled with constant
    values)
    
    If the \code{Meteo} object is to be used with \code{\link{calcG0}}
    (or \code{fCompD}, \code{fCompI}) and the option
    \code{corr = 'none'}, the file/data.frame \bold{must} include three
    columns named \code{G0}, \code{B0} and \code{D0} with values of
    global, direct and diffuse irradiation on the horizontal plane.

    Only for daily data: if the ambient temperature is not available,
    the file should include two columns named \code{TempMax} and
    \code{TempMin} with daily values of maximum and minimum ambient
    temperature, respectively (see \code{\link{fTemp}} for details).}

  \item{header, fill, dec, sep}{See \code{\link[data.table]{fread}}}
  
  \item{format}{character string with the format of the dates or time
    index.
    (Default for daily time bases:\code{\%d/\%m/\%Y}).
    (Default for intradaily time bases: \code{\%d/\%m/\%Y  \%H:\%M:\%S})}

  \item{lat}{numeric, latitude (degrees) of the location.}

  \item{dates.col}{character string with the name of the column wich
    contains the dates of the time series.}

  \item{times.col}{character string with the name of the column wich
    contains the time index of the series in case is in a different
    column than the dates.}

  \item{source}{character string with information about the source of
    the values. (Default: the name of the file).}
  
  \item{ta.col, g0.col}{character, the name of the columns with the
    information of ambient temperature and radiation in the provided
    file}
  
  \item{keep.cols}{If \code{keep.cols=FALSE}(default value), the
    \code{Meteo} object does not include the columns that are not
    important for the rest of operations}
  
  \item{...}{Arguments for \code{\link[data.table]{fread}}}

  \item{type}{character, type of the data in \code{dt2Meteo}. To choose between
    \code{'prom'}, \code{'bd'} and \code{'bdI'}. If it is not provided,
    the function \code{dt2Meteo} calculate the type.}
}

\value{A \code{Meteo} object.}


\author{
  Oscar Perpiñán Lamigueiro,
  Francisco Delgado López.
}
\seealso{
  \code{\link[data.table]{fread}},
  \code{\link{readG0dm}}.
}
\examples{
library("data.table")
\dontshow{setDTthreads(2)}

data(helios)
names(helios) = c('Dates', 'G0d', 'TempMax', 'TempMin')

bd = dt2Meteo(helios, lat = 41, source = 'helios-IES', type = 'bd')

getData(bd)

xyplot(bd)
}

\keyword{constructors}
