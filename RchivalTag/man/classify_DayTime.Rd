

\name{classify_DayTime}
\alias{classify_DayTime}
\title{
Classifying the time period of the day
}

\description{
Classifying the time period of the day based on the timing of sunrise, sunset (and twilight events) or alternatively, geolocation estimates, as specified in \link{get_DayTimeLimits}, that allow their internal estimation during the function call.
}

\usage{
classify_DayTime(pos, twilight.set="ast")
}
 
\arguments{
\item{pos}{
A \link{data.frame} \code{pos} with the columns \code{sunrise}, \code{sunset}, \code{dawn.ast},/\code{dawn.naut} and \code{dawn.ast}/\code{dawn.naut} in \code{POSIXct}-format. Note that the expected twilight vector (suffix \code{"ast"} for astronomical dawn and dusks; vs suffix \code{"naut"} for nautical twilight events) is defined by the function's second argument \code{twilight.set} (see description below).
}


\item{twilight.set}{
character string, indicating the type of twilight used for the long daytime classifcation: \code{"ast"} (default) for astronomical and \code{"naut"} for nautical twilight events with sun angles of 18 vs 12 below the horizon, respectvely. Corresponding (expected) dawn and dusk vector names are \code{dawn.ast} & \code{dusk.ast} vs \code{dawn.naut} & \code{dusk.naut}).
}
}

\value{
The input \link{data.frame} \code{pos} extended by the time vectors \code{daytime} and \code{daytime.long}. In the former case, "Day" and "Night" periods are distinguished. In the latter case, "Day", "Night", "Dawn" and "Dusk".
}




\references{
Meeus, J. (1991) Astronomical Algorithms. Willmann-Bell, Inc.
}


\seealso{\link{sunriset}, \link{crepuscule}, \link{get_DayTimeLimits}
}


\author{
Robert K. Bauer
}

\examples{

#### example 1) estimate current times of dawn, sunrise, dusk and sunset in Mainz, Germany:
pos <- data.frame(Lat=8.2667, Lon=50)
pos$datetime <- strptime(Sys.Date(),"\%Y-\%m-\%d")
get_DayTimeLimits(pos)

#### example 1b) classify current ime of the day in Mainz, Germany:
classify_DayTime(get_DayTimeLimits(pos))

## convert 1c) back-to-back histogram showing day vs night TAD frequencies:
### load sample depth and temperature time series data from miniPAT:
ts_file <- system.file("example_files/104659-Series.csv",package="RchivalTag")
ts_df <- read.table(ts_file, header = TRUE, sep = ",")
tad_breaks <- c(0, 2, 5, 10, 20, 50, 100, 200, 300, 400, 600, 2000)

ts_df$Lat <- 4; ts_df$Lon=42.5 ## required geolocations to estimate daytime
ts_df$datetime <- strptime(paste(ts_df$Day,ts_df$Time),"\%d-\%B-\%Y \%H:\%M:\%S")
head(ts_df)
ts_df2 <- classify_DayTime(get_DayTimeLimits(ts_df)) # estimate daytime
head(ts_df2)

ts2histos(ts_df2, tad_breaks = tad_breaks,split_by = "daytime")
hist_tad(ts_df2, bin_breaks = tad_breaks,split_by = "daytime", do_mid.ticks = FALSE)


}

