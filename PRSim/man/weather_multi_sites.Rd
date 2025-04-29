\name{weather_multi_sites}
\alias{weather_multi_sites}
\alias{weather multi sites}
\docType{data}
\title{
Sample temperature and precipitation of four catchments derived from the ERA5-Land gridded dataset
}
\description{
Reanalysis data of four grid cells from ERA5-Land.
}
\usage{data("weather_multi_sites")}
\format{
  Contains two lists data_p and data_t containing precipitation and temperature
  data, respectively.
  Each list consists of four data frames (one list per station/grid cell) of the following 4 variables.
  \describe{
    \item{\code{YYYY}}{a numeric vector, year}
    \item{\code{MM}}{a numeric vector, month}
    \item{\code{DD}}{a numeric vector, day}
    \item{\code{Precip/Temp}}{a numeric vector, observed precipitation/temperature}
  }
}
\details{
The data contains data for four grid cells in the Pacific Northwest.
}
\source{
The preciptation data were downloaded from ERA5-Land
\url{https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=overview}.
}
\references{
Brunner, M. I., and E. Gilleland (2021). Spatial compound hot-dry events in the United States: assessment using a multi-site multi-variable weather generator, in preparation.
}
\examples{
data(weather_multi_sites)
weather_multi_sites[[1]][[1]]$timestamp <- paste(weather_multi_sites[[1]][[1]]$YYYY, 
weather_multi_sites[[1]][[1]]$MM, weather_multi_sites[[1]][[1]]$DD, sep=" ")
weather_multi_sites[[1]][[1]]$timestamp <- 
as.POSIXct(strptime(weather_multi_sites[[1]][[1]]$timestamp,
format="\%Y \%m \%d", tz="GMT"))
plot(weather_multi_sites[[1]][[1]]$timestamp[1:1000],
weather_multi_sites[[1]][[1]]$Qobs[1:1000], type="l", 
xlab="Time [d]", ylab=expression(paste("Temperature [degrees]")))
}
\keyword{datasets}
