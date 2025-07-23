

\name{dy_DepthTS}
\alias{dy_DepthTS}
\alias{dy_TS}


\title{
plot time series data via the dygraphs interactive time series plotting interface.
}
\description{
plot time series data (e.g. depth or temperature time series data from archival tags) via the dygraphs interactive time series plotting interface.
}

\usage{
dy_TS(ts_df, y="Depth", xlim, ylim, 
           ylab=y, xlab, main,
           ID, ID_label="Serial", 
           plot_DayTimePeriods=TRUE, twilight.set="ast", 
           color="darkblue",
           doRangeSelector=TRUE, drawPoints=FALSE, pointSize=2, tz="UTC",...)


dy_DepthTS(ts_df, y="Depth", xlim, ylim, 
           ylab=y, xlab, main,
           ID, ID_label="Serial", 
           plot_DayTimePeriods=TRUE, twilight.set="ast", 
           color="darkblue",
           doRangeSelector=TRUE, drawPoints=FALSE, pointSize=2, tz="UTC", ...)
}
 

\arguments{
\item{ts_df}{\link{data.frame} holding the time series data to be plotted, including the x-vector 'datetime' (in \code{POSIXct}-format  and UTC), and the numeric y-vector whose label is defined by \code{y}.
}

\item{y}{character label of time series vector to be plotted (by default 'Depth').
}

\item{xlim}{the x limits (x1, x2) of the plot (by default range(ts_df$datetime), but needs to be specified in \code{empty.plot_TS}).
}

\item{ylim}{the y limits of the plot (by default range(ts_df[[y]]), but needs to be specified in \code{empty.plot_TS}).
}

\item{ylab, xlab}{
the y- and x-axis labels. 
}

\item{main}{
main title (by default "Tag ID") for the plot).
}


\item{ID, ID_label}{
Tag ID and its label (column name; by default "Serial") to be selected (e.g. if input data frame holds tagging data from several tags).
}


\item{plot_DayTimePeriods, twilight.set}{
whether day-time periods ('Night', 'Dawn', 'Day', 'Dusk') should be plotted as shaded areas. In case that \code{plot_DayTimePeriods} is set \code{TRUE}), the limits of each time period are required (columns \code{sunrise}, \code{sunset}, \code{dawn.ast},/\code{dawn.naut} and \code{dawn.ast}/\code{dawn.naut} in \code{POSIXct}-format. In case of the twilight events, the additional argument \code{twilight.set} defines the suffix of the twlight-set to be selected ( \code{"ast"} for astronomical dawn and dusks vs \code{"naut"} for nautical twilight events).
If any of the day-time columns, described above, is missing, it/they will be calculated based on geolocation estimates (required columns \code{Lon} and \code{Lat}) through an internal call of function \code{get_DayTimeLimits}.\cr
}

\item{color}{
color of the line to be plotted (by default "darkblue")
}

\item{doRangeSelector}{
whether to add dygraph interactive range selection and zooming bar below the figure (by default \code{TRUE})

}


\item{drawPoints, pointSize}{
Whether to indicate add points to the figure at the sampling time steps as well their size.
}

\item{tz}{
The time zone in which the data should be illustrated (By default "UTC"). ATTENTION: The required date format of the input data is "UTC" (across all RchivalTag-functions). Run \code{OlsonNames(tzdir = NULL)} for valid time zone definitions.
}


\item{\dots}{
additional arguments to be passed to \link{dygraph}.Further arguments can be passed after the function call, e.g. via \link{dyOptions}.
}
}

\seealso{\link{plot_TS}, \link{plot_DepthTempTS}} 

\value{
An interactive dygraph plot object that can be altered further using for exemple the \link{dyOptions}.
}

\author{
Robert K. Bauer
}


\examples{
### load sample depth and temperature time series data from miniPAT:
# ts_file <- system.file("example_files/104659-Series.csv",package="RchivalTag")
# ts_df <- read_TS(ts_file)
# ts_df$Serial <- ts_df$DeployID
# head(ts_df)

## plot depth-time series data
# dy_DepthTS(ts_df)

## add missing Lon, Lat information for night-time and twilight shadings
# ts_df$Lon <- 5; ts_df$Lat <- 43

# dy_DepthTS(ts_df)

## same figure with plot_DepthTS:
# plot_DepthTS(ts_df, plot_DayTimePeriods = TRUE)

## some further arguments:
# dy_DepthTS(ts_df, xlim = unique(ts_df$date)[2:3], plot_DayTimePeriods = FALSE)

## add further options via dyOptions-call:
# dg <- dy_DepthTS(ts_df, xlim = unique(ts_df$date)[2:3], 
#                  plot_DayTimePeriods = FALSE, drawPoints = TRUE)
# dg <- dyOptions(dg,drawGrid=FALSE)
# dg

}
