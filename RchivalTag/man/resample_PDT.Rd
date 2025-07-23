

\name{resample_PDT}
\alias{resample_PDT}
\alias{resample_DepthTempTS}

\title{
resample temperature at depth data from interpolated daily temperature at depth profiles or time series data
}
\description{
interpolates depth-temperature data from a provided source (depth-temperature time series data or PDT data) and resamples the interpolated data by the depth time series data provided, to faciliate \link{plot_DepthTempTS}-plots even for tags with no temperature time series data or to improve interpolation results of the \link{plot_DepthTempTS}-plots from low-resolution depth-temperature time series data.
}

\usage{
resample_PDT(ts_df, PDT, ...)
resample_DepthTempTS(ts_df, ...)
}
 

\arguments{
\item{ts_df}{
\code{ts_df} is a \link{data.frame} with depth-temperature time series data data or only depth time series data. Required columns are \code{Depth} for the depth data and a column containing temperature data, whose name is defined by \code{Temp_field}, by default Temperature.
}

\item{PDT}{
an optional data.frame containing PDT-data from \link{read_PDT}.
}

\item{\dots}{
additional arguments to be passed to \link{interpolate_TempDepthProfiles}, or \link{interpolate_PDTs}.
}
}



\value{a \link{data.frame} with depth-temperature time series data.
  
}

\references{
Bauer, R., F. Forget and JM. Fromentin (2015) Optimizing PAT data transmission: assessing the accuracy of temperature summary data to estimate environmental conditions. Fisheries Oceanography, 24(6): 533-539, 
\doi{10.1111/fog.12127}
}

\seealso{\link{read_PDT}, \link{interpolate_TempDepthProfiles}, \link{get_thermalstrat}, \link{image_TempDepthProfiles}} 


\author{
Robert K. Bauer
}

\examples{

## read in depth temperature time series data (sampling rate 5min)
ts_file <- system.file("example_files/104659-Series.csv",package="RchivalTag")
ts_df <- read_TS(ts_file)
head(ts_df)

## run daily interpolation of depth temperature time series data
m <- interpolate_TempDepthProfiles(ts_df)
image_TempDepthProfiles(m$station.1)
ts_df2 <- resample_DepthTempTS(ts_df) ## reassign temperature at depth values

## read PDT data from same tag 
## (= low resolution depth temperature data (8 Depth and Temperature records per day))
path <- system.file("example_files",package="RchivalTag")
PDT <- read_PDT("104659-PDTs.csv",folder=path)
head(PDT)

m <- interpolate_PDTs(PDT) ## interpolate PDTs
image_TempDepthProfiles(m$station.1)
ts_df3 <- resample_PDT(ts_df, PDT) ## reassign temperature at depth values



#### plot results:
## 1) dot plots:

## dot plot of RECORDED depth temperature time series data
## plot_DepthTempTS(ts_df, do_interp = FALSE) 

## dot plot of RESAMPLED depth temperature time series data 
## from previously daily interpolated depth temperature time series data
# plot_DepthTempTS(ts_df2, do_interp = FALSE) 

## dot plot of RESAMPLED depth temperature time series data 
## from daily interpolated PDT data (external resampling)
# plot_DepthTempTS(ts_df3, do_interp = FALSE) 

## dot plot of RESAMPLED depth temperature time series data 
## from daily interpolated PDT data (internal resampling)
# plot_DepthTempTS_resampled_PDT(ts_df, PDT, do_interp = FALSE) 


## 2) line plots:

## line plot of depth temperature time series data
## (internal interpolation between neighboring temperature records)
## not recommended for low resolution time series data
# plot_DepthTempTS(ts_df, do_interp = TRUE) 

## line plot of depth temperature time series data 
## (based on internal daily interpolated depth temperature time series data)
# plot_DepthTempTS_resampled(ts_df, do_interp = TRUE) 

## line plot of depth temperature time series data 
## from daily interpolated PDT data (external resampling)
# plot_DepthTempTS(ts_df3, do_interp = TRUE) 

## line plot of depth temperature time series data
## from daily interpolated PDT data (internal resampling)
# plot_DepthTempTS_resampled_PDT(ts_df, PDT, do_interp = TRUE)

}