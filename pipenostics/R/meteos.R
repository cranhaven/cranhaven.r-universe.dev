#' @title
#'  Get list of weather stations (meteos)
#'
#' @family utils
#'
#' @description
#'  Get a list of weather stations located primarily in the
#'  central and northern parts of Eurasia. For each weather station, the 
#'  following information is provided: an integer station ID, geographic 
#'  coordinates, altitude, and the mean annual ground temperature averaged over
#'  depth.
#'
#' @return
#'  list of weather stations (meteos) with the next fields:
#'  \describe{
#'    \item{\code{station_id}}{Weather station unique identifier. Type: \code{\link{assert_integer}}.}
#'    \item{\code{name}}{Human-readable name of weather station. Type: \code{\link{assert_character}}.}
#'    \item{\code{lat}}{Geographical position of wether station. Latitude, [\emph{DD}]. Type: \code{\link{assert_double}}.}
#'    \item{\code{lon}}{Geographical position of wether station. Longitude, [\emph{DD}]. Type: \code{\link{assert_double}}.}
#'    \item{\code{alt}}{Altitude - position of weather station above sea level, [m]. Type: \code{\link{assert_double}}.}
#'    \item{\code{avg}}{Mean annual ground temperature averaged over depth, [\emph{°C}]. Type: \code{\link{assert_double}}.}
#'  }
#'  Type: \code{\link{assert_data_frame}}.
#'
#' @seealso
#'  \code{\link{mgtdhid}} to get hourly ground temperature values at different 
#'  depths measured at the listed weather stations.
#'
#' @references
#'  \href{http://meteo.ru/structure/lipik/}{Climate Change Investigation Laboratory}.
#'  Description of the array of daily data on soil temperature at depths up to 320 centimeters
#'  by meteorological stations of the \emph{Russian Federation}.
#'
#' @examples
#'  library(pipenostics)
#'  head(meteos())
#'
#' @export 
meteos <- function(){
  meteosd[,
    c(
       "station_id"
      ,"name"
      ,"lat"
      ,"lon"
      ,"alt"
      ,"avg"
    )
  ]
}
