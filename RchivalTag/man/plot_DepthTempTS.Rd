

\name{plot_DepthTempTS}
\alias{plot_DepthTempTS}
\alias{plot_DepthTempTS_resampled}
\alias{plot_DepthTempTS_resampled_PDT}

\title{
plot Depth Temperature time series data
}
\description{
line plot for xyz-time series data with colorized z-variable (e.g. depth-temperature time series data from archival tags).
}

\usage{
plot_DepthTempTS(ts_df, y="Depth", z="Temperature", 
                 xlim, ylim, zlim, show.colorbar=TRUE, 
                 pal="jet", cb.xlab, cb.xlab.line=0, 
                 pt.lwd, do_interp=TRUE, Return=FALSE, mars, tz="UTC", ...)
                 
plot_DepthTempTS_resampled(ts_df, y="Depth",z="Temperature", bin_res=10, 
                          xlim, ylim, zlim, show.colorbar=TRUE, 
                           pal="jet", cb.xlab, cb.xlab.line=0,
                           pt.lwd, do_interp=TRUE, Return=FALSE, mars, tz="UTC", ...)
                           
plot_DepthTempTS_resampled_PDT(ts_df, PDT, y="Depth", z="Temperature", 
                               xlim, ylim, zlim, show.colorbar=TRUE, 
                               pal="jet", cb.xlab, cb.xlab.line=0,  
                               pt.lwd, do_interp=TRUE, Return=FALSE, mars, tz="UTC", ...)
}
 

\arguments{
\item{ts_df,PDT}{\link{data.frame}s holding the time series data to be plotted, including the x-vector 'datetime' (in \code{POSIXct}-format  and UTC), and the numeric y-vector whose label is defined by \code{y}. In case of \code{plot_DepthTempTS_resampled} the depth temperature time series data will be interpolated on a daily basis and then resampled for temperature data by the depth records of the original time series data.
\code{plot_DepthTempTS_resampled_PDT} does the same but uses PDT data for resampling.
}



\item{y}{character label of time series vector to be plotted (by default 'Depth').
}

\item{z}{character label of time series vector to be plotted (by default 'Temperature').
}

\item{bin_res}{specific argument for \code{plot_DepthTempTS_resampled}: the depth interval at which temperature records should be binned. (by default 10).
}

\item{xlim}{the x limits (x1, x2) of the plot (by default range(ts_df$datetime)).
}

\item{ylim}{the y limits of the plot (by default range(ts_df[[y]])).
}

\item{zlim}{the y limits of the plot (by default range(ts_df[[z]])).
}

\item{show.colorbar}{weather a colorbar should be plotted for image plots (default is \code{TRUE}).
}

\item{pal}{
  color map to be plotted (default is the 'jet'-colormap of the \link{oceanmap}-package. See \link{cmap} for available color maps.
}

\item{cb.xlab, cb.xlab.line}{
character string indicating the label of the colorbar (default is Temperature in degrees) and \code{cb.xlab.line} its placement line (default is 0).
}

\item{pt.lwd}{size of points and lines.\cr
}

\item{do_interp}{
whether z-values shall be interpolated over the covered range of the time series data. The default \code{TRUE} value will produce a line plot. If set to FALSE only the available data points of the z-variable will be plotted.
}

\item{Return}{
whether edited time series data set should be returned (by default \code{FALSE}).
}

\item{mars}{
A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot. The default is c(5,4,4,10).
}

\item{tz}{
The time zone in which the data should be illustrated (By default "UTC"). ATTENTION: The required date format of the input data is "UTC" (across all RchivalTag-functions).
}

\item{\dots}{
additional arguments to be passed to \link{plot_TS}.
}
}

\seealso{\link{plot_DepthTS}, \link{plot_TS}} 


\author{
Robert K. Bauer
}


\examples{
### load sample depth and temperature time series data from miniPAT:
ts_file <- system.file("example_files/104659-Series.csv",package="RchivalTag")
ts_df <- read_TS(ts_file)
head(ts_df)
ts_df$Serial <- ts_df$DeployID
# plot_DepthTempTS(ts_df, do_interp = FALSE)
# plot_DepthTempTS(ts_df, do_interp = TRUE)
# plot_DepthTempTS_resampled(ts_df, do_interp = TRUE) # more accurate

# ts_df$Lon <- 5; ts_df$Lat <- 43
# plot_DepthTempTS(ts_df, plot_DayTimePeriods = TRUE, xlim = unique(ts_df$date)[2:3])
# plot_DepthTempTS(ts_df, plot_DayTimePeriods = TRUE, xlim = unique(ts_df$date)[2:3])
# plot_DepthTempTS_resampled(ts_df, plot_DayTimePeriods = TRUE, xlim = unique(ts_df$date)[2:3])
# plot_DepthTempTS_resampled_PDT(ts_df, PDT, plot_DayTimePeriods = TRUE)


}
