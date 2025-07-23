

\name{bin_TempTS}
\alias{bin_TempTS}
\title{
bin depth-temperature time series data
}

\description{
bins depth-temperature time series data to a user-defined resolution, returning the minimum, maxmium and average temperature recorded at each depth interval (bin) per sampling day. The output is comparable to that of \link{read_PDT}.
\cr\cr\strong{Why binning temperature data?}\cr
In case of archival tagging data, depth-temperature time series data at a given day may consist of multiple tempertaure profiles of different signatures, depending on the animal's behaviour. Slight differences in temperature profiles could impede further analyses (e.g. the estimation of the thermocline depth), if just the average profile is applied. To avoid such problmes, it is useful to calculate the average temperature at given depth intervals (bins) and thus smooth temperature profiles of a given period.

In addition, temperature at depth profiles can be interpolated and then visualized using functions \link{interpolate_TempDepthProfiles} and \link{image_TempDepthProfiles}, respectively. This faciliates the analysis of temporal changes of temperature profiles, for instance, in relation to animal behaviour (e.g. diving behaviour).
}

\usage{
bin_TempTS(ts, res=8, verbose=FALSE)
}
 

\arguments{
\item{ts}{a \link{data.frame} with columns \code{date}, \code{Depth} and \code{Temperature}
}

\item{res}{the depth interval at which temperatures should be binned.
}

\item{verbose}{
whether the sampling dates should be indicated during the binning process (by default \code{FALSE}).
}

}


\value{
A \link{data.frame} with the columns \code{date}, \code{MeanTemp}, \code{MinTemp}, \code{MaxTemp}, \code{bin} and \code{MeanPDT} (the latter being the average of the min and maximum water temperatures). Additional columns, used to distinguish tags, may include \code{Serial}, \code{DeployID} and \code{Ptt}, depending on their availability in the original ts-data.fralme.

}

\references{
Bauer, R., F. Forget and JM. Fromentin (2015) Optimizing PAT data transmission: assessing the accuracy of temperature summary data to estimate environmental conditions. Fisheries Oceanography, 24(6): 533-539, 
\doi{10.1111/fog.12127}
}


\seealso{\link{read_PDT}, \link{interpolate_TempDepthProfiles}, \link{image_TempDepthProfiles}}


\author{
Robert K. Bauer
}

\examples{

# #### example 1) run on time series data:
## step I) read sample time series data file:
DepthTempTS <- read.table(system.file("example_files/104659-Series.csv",
                                       package="RchivalTag"),header = TRUE,sep=',')
DepthTempTS$date <- as.Date(DepthTempTS$Day,"\%d-\%b-\%Y")
head(DepthTempTS)
# 
# 
# ## step Ib) bin temperature data on 10m depth bins 
# ##          to increase later estimate accuracy (see Bauer et al. 2015):
# # DepthTempTS_binned <- bin_TempTS(DepthTempTS,res=10)
# 
# ## step II) interpolate average temperature fields (MeanTemp) from binned data:
# m <- interpolate_TempDepthProfiles(DepthTempTS)
# # m <- interpolate_PDTs(DepthTempTS_binned)
# str(m)
# m$sm
# 
# ## step III) calculate thermal stratifcation indicators per day (and tag):
# get_thermalstrat(m, all_info = TRUE)
# get_thermalstrat(m, all_info = FALSE)
# 
# ## step IV) plot interpolated profiles:
# image_TempDepthProfiles(m$station.1)

}

