

\name{resample_TS}
\alias{resample_TS}
\alias{empty.resample_TS}


\title{
resample time series data at a lower resolution
}
\description{
resample time series data at a lower resolution
}

\usage{
resample_TS(df, tstep, nsims)
}
 

\arguments{
\item{df}{\link{data.frame} holding the time series data to be resampled, including a 'datetime'-vector (in \code{POSIXct}-format  and UTC).
}

\item{tstep}{numeric vector indicating the resampling resolution (in seconds).
}

\item{nsims}{number of simulated datasets to be generated. If missing, the maximum number of datasets will be returned, based on a moving window of the temporal resolution of the input dataset.
}

}

\seealso{\link{plot_TS}} 


\author{
Robert K. Bauer
}


\examples{
### load sample depth and temperature time series data from miniPAT:
ts_file <- system.file("example_files/104659-Series.csv",package="RchivalTag")
ts_df <- read.table(ts_file, header = TRUE, sep = ",")
head(ts_df)
ts_df$datetime <- as.POSIXct(strptime(paste(ts_df$Day, ts_df$Time), 
                              "\%d-\%b-\%Y \%H:\%M:\%S",tz = "UTC"))

tsims <- resample_TS(ts_df,600)
length(tsims)

}
