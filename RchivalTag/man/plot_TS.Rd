

\name{plot_TS}
\alias{plot_TS}
\alias{plot_DepthTS}
\alias{empty.plot_TS}


\title{
plot time series data
}
\description{
plotting functions for time series data (e.g. depth or temperature time series data from archival tags) with user specified xtick intervals.
}

\usage{
plot_DepthTS(ts_df, y="Depth", xlim, ylim, xticks_interval,
             ylab=y, xlab, main, main.line=1, plot_info=TRUE, 
             ID, ID_label="Serial",
             plot_DayTimePeriods=FALSE, twilight.set="ast", 
             cex=1, cex.main=1.2*cex, cex.lab=1*cex, 
             cex.axis=.9*cex, cex.axis2=1*cex, 
             type="l", las=1, xaxs="i", yaxs="i", 
             plot_box=TRUE, bty="l", Return=FALSE, tz="UTC",...)
             
           
plot_TS(ts_df, y="Depth", xlim, ylim, xticks_interval,
        ylab=y, xlab, main, main.line=1, plot_info=TRUE, 
        ID, ID_label="Serial",
        plot_DayTimePeriods=FALSE, twilight.set="ast", 
        cex=1, cex.main=1.2*cex, cex.lab=1*cex, 
        cex.axis=.9*cex, cex.axis2=1*cex, 
        type="l", las=1, xaxs="i", yaxs="i", 
        plot_box=TRUE, bty="l", Return=FALSE, tz="UTC", ...)
       
empty.plot_TS(xlim, ylim, xticks_interval, ylab="", xlab, main="",
              cex=1, cex.main=1.2*cex, cex.lab=1*cex, 
              cex.axis=.9*cex, cex.axis2=1*cex,               
              las=1, xaxs="i", yaxs="i", do_xaxis=TRUE, do_yaxis = TRUE,
              plot_box=TRUE, bty="l", tz="UTC", ...)
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

\item{xticks_interval}{time step of the x-axis ticklabels in (full) hours. By default 3 hours for xlim differences <= 1 day, and 6 hours for differences > 1 day. 
}

\item{ylab, xlab}{
the y- and x-axis labels. By default xlab="Time (UTC)" and ylab = "Depth (m)"
}

\item{main, main.line}{
main title (by default "Tag ID") for the plot and its line (see \link{mtext} for reference).
}

\item{plot_info}{
whether the plot title and axes labels should be shown (by default \code{TRUE}).
}


\item{ID, ID_label}{
Tag ID and its label (column name; by default "Serial") to be selected (e.g. if input data frame holds tagging data from several tags).
}

\item{type}{
what type of plot should be drawn. Possible types are:

\itemize{
  \item{\code{"p"} for points,}
  \item{\code{"l"} for lines (default),}
  \item{\code{"b"} for both,}
  \item{\code{"c"} for the lines part alone of "b",}
  \item{\code{"o"} for both 'overlapped',}
  \item{\code{"n"} for nothing (similar to \code{empty.plot_TS}-function call)}
}}

\item{las}{numeric in \{0,1,2,3\}; the style of axis labels
\describe{
  \item{0:}{always parallel to the axis [\emph{default}],}
  \item{1:}{always horizontal,}
  \item{2:}{always perpendicular to the axis,}
  \item{3:}{always vertical.}
  }
}

\item{xaxs, yaxs}{
The style of axis interval calculation to be used for the x-and y-axes. Possible values are "r" and "i" (default). The styles are generally controlled by the range of data or xlim, if given.\cr\cr
Style "r" (regular) first extends the data range by 4 percent at each end and then finds an axis with pretty labels that fits within the extended range.\cr\cr
Style "i" (internal) just finds an axis with pretty labels that fits within the original data range.
}

\item{cex,cex.main, cex.lab, cex.axis, cex.axis2}{The standard font size of title, axis labels and tick labels.\cr
}

\item{plot_DayTimePeriods, twilight.set}{
whether day-time periods ('Night', 'Dawn', 'Day', 'Dusk') should be plotted as shaded areas. In case that plot_DayTimePeriods is set \code{TRUE}, the limits of each time period are required (columns \code{sunrise}, \code{sunset}, \code{dawn.ast},/\code{dawn.naut} and \code{dawn.ast}/\code{dawn.naut} in \code{POSIXct}-format. In case of the twilight events, the additional argument \code{twilight.set} defines the suffix of the twlight-set to be selected ( \code{"ast"} for astronomical dawn and dusks vs \code{"naut"} for nautical twilight events).
If any of the day-time columns, described above, is missing, it/they will be calculated based on geolocation estimates (required columns \code{Lon} and \code{Lat}) through an internal call of function \code{get_DayTimeLimits}.\cr
}

\item{do_xaxis,do_yaxis}{Optional arguments in empty.plot_TS to define whether a x and and y-axis shall be plotted (by default \code{TRUE}).
}

\item{plot_box, bty}{
whether a box of box-type \code{bty} should be plotted (by default \code{TRUE}. \code{bty} is one of "o" (the default), "l", "7", "c", "u", or "]" the resulting box resembles the corresponding upper case letter.

}

\item{Return}{
whether edited time series data set should be returned (by default \code{FALSE}).
}

\item{tz}{
The time zone in which the data should be illustrated (By default "UTC"). ATTENTION: The required date format of the input data is "UTC" (across all RchivalTag-functions). Run \code{OlsonNames(tzdir = NULL)} for valid time zone definitions.
}

\item{\dots}{
additional arguments to be passed to \link{plot}.
}
}

\seealso{\link{ggboxplot_DepthTS_by_hour}, \link{dy_DepthTS}, \link{plot_DepthTempTS}} 


\author{
Robert K. Bauer
}


\examples{
### load sample depth and temperature time series data from miniPAT:
ts_file <- system.file("example_files/104659-Series.csv",package="RchivalTag")
ts_df <- read_TS(ts_file)
ts_df$Serial <- ts_df$DeployID
head(ts_df)

## load same data in LOTEK format
ts_file <- system.file("example_files/104659_PSAT_Dive_Log.csv",package="RchivalTag")
ts_df <- read_TS(ts_file,date_format="\%m/\%d/\%Y \%H:\%M:\%S")
head(ts_df) ## attention no identifier (Ptt, Serial, DeployID) included!
ts_df$DeployID <- ts_df$Ptt <- "104659"
ts_df$Serial <- "Tag1"

### select subsets (dates to plot)
# plot_DepthTS(ts_df, plot_DayTimePeriods = FALSE, xlim = unique(ts_df$date)[2:3])
# xlim <- c("2016-08-10 6:10:00", "2016-08-11 17:40:00")
# plot_DepthTS(ts_df, plot_DayTimePeriods = FALSE, xlim = xlim)

### check xtick time step:
# plot_DepthTS(ts_df, plot_DayTimePeriods = FALSE, xlim = "2016-08-10")
# plot_DepthTS(ts_df, plot_DayTimePeriods = FALSE, xlim = "2016-08-10", xticks_interval = 2)


### add daytime periods during plot-function call and return extended data set
# ts_df$Lon <- 5; ts_df$Lat <- 43
# plot_DepthTS(ts_df, plot_DayTimePeriods = TRUE, xlim = unique(ts_df$date)[2:3])
# ts_df2 <- plot_DepthTS(ts_df, plot_DayTimePeriods = TRUE, Return = TRUE) 
# names(ts_df)
# names(ts_df2)

### add daytime periods before function call
# ts_df_extended <- get_DayTimeLimits(ts_df)
# plot_DepthTS(ts_df_extended, plot_DayTimePeriods = TRUE)
# plot_DepthTS(ts_df_extended, plot_DayTimePeriods = TRUE, twilight.set = "naut")

### introduce data transmission gaps that are then filled internally
### as well as daytime periods based on interpolated Lon & Lat positions
# ts_df_cutted <- ts_df[-c(200:400, 1800:2200), ]
# plot_DepthTS(ts_df_cutted, plot_DayTimePeriods = FALSE)
# plot_DepthTS(ts_df_cutted, plot_DayTimePeriods = TRUE) 

### example for empty.plotTS and adding time series data as line:
# empty.plot_TS(xlim="2016-08-10",ylim=c(100,0))
# lines(ts_df$datetime, ts_df$Depth)

### alternative:
# plot_DepthTS(ts_df, xlim=c("2016-08-10","2016-08-12"), plot_DayTimePeriods = TRUE, type='n') 
# lines(ts_df$datetime, ts_df$Depth)

}
