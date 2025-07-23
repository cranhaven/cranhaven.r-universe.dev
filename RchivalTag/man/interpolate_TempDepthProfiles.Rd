

\name{interpolate_TempDepthProfiles}
\alias{interpolate_TempDepthProfiles}
\alias{interpolate_PDTs}

\title{
interpolate daily temperature at depth profiles
}
\description{
interpolates depth-temperature data and returns daily average temperature at depth profiles on a user-specified resolution (\code{Depth_res}). \cr Results are returned as a list containing the interpolated Temperature-matrix, and the corresponding date and depth values. Thus interpolated temperature at depth profiles can be visualized using function \link{image_TempDepthProfiles} and faciliates the analysis of temporal changes of temperature profiles, for instance, in relation to animal behaviour (e.g. diving behaviour).
}

\usage{
interpolate_TempDepthProfiles(ts, Temp_field="Temperature", ID_key="Serial", %return_as_matrix=FALSE, 
                              Depth_res=.5, verbose=TRUE, Data_Source='station')
                              
interpolate_PDTs(ts, Temp_field="MeanPDT", ID_key="Serial", #return_as_matrix=FALSE, 
                             Depth_res=.5, verbose=TRUE, Data_Source='station')
}
 

\arguments{
\item{ts, Temp_field, ID_key}{
\code{ts} is a \link{data.frame} with temperature at depth data. Required columns are \code{Depth} for the depth data and a column containing temperature data, whose name is defined by \code{Temp_field}. \code{ID_key} specifies the name of an optional column on which sampling stations or tags can be distinguished (by default \code{Serial}).
}

\item{Depth_res}{
numeric value, defining the depth resolution at which the temperature data should be interpolated.
}

\item{verbose}{
whether the sampling dates and ids of stations or tags, as defined by the columns \code{date} and \code{ID_key},  
should be indicated during the interpolation process.
}

%\item{return_as_matrix}{
%whether the interpolated profiles should be returned as matrix. If FALSE (default), interpolated profiles are listed per sampling day.
%}

\item{Data_Source}{
a character string, defining the data source (by default \code{station}).
}



}



\value{A list containing the interpolated temperature at depth profiles and their corresponding date and interpolated depth values as well as a summary table with the original depth values and their number per day:

 $ Data_Source.ID_key:List of 4\cr
  ..$ Temperature_matrix: num\cr
  ..$ Depth           : num\cr 
  ..$ Date            :Date\cr
  ..$ sm            :data.frame\cr

  
Please see the examples for further understaning.
  
}

%\value{A list of dataframes or matrices, containing the interpolated temperature at depth profiles.
%\cr In case of \code{return_as_matrix} is FALSE:\cr
%$ Data_Source.ID_key : List\cr
%  ..$ Date_????????  : data.frame':  ? obs. of  2 variables:\cr
%  .. ..$ Depth: num\cr 
%  .. ..$ Temperature: num \cr

%\cr In case of \code{return_as_matrix} is TRUE:\cr
% $ Data_Source.ID_key:List of 3\cr
%  ..$ Temperature_matrix: num\cr
%  ..$ Depth           : num\cr 
%  ..$ Date            :Date\cr
  
%Please see the examples for further understaning.
%  
%}
\references{
Bauer, R., F. Forget and JM. Fromentin (2015) Optimizing PAT data transmission: assessing the accuracy of temperature summary data to estimate environmental conditions. Fisheries Oceanography, 24(6): 533-539, 
\doi{10.1111/fog.12127}
}

\seealso{\link{read_PDT}, \link{bin_TempTS}, \link{get_thermalstrat}, \link{image_TempDepthProfiles}} 


\author{
Robert K. Bauer
}

\examples{
#### example 1) run on PDT file:
## step I) read sample PDT data file:
path <- system.file("example_files",package="RchivalTag")
PDT <- read_PDT("104659-PDTs.csv",folder=path)
head(PDT)
# 
# ## step II) interpolate average temperature fields (MeanPDT) from PDT file:
# m <- interpolate_PDTs(PDT)
# str(m)
# m$sm
# 
# ## step III) calculate thermal stratifcation indicators per day (and tag):
# get_thermalstrat(m, all_info = TRUE)
# get_thermalstrat(m, all_info = FALSE)
# 
# ## step IV) plot interpolated profiles:
# image_TempDepthProfiles(m$station.1)
# 
# 
# #### example 2) run on time series data:
# ## step I) read sample time series data file:
# ts_file <- system.file("example_files/104659-Series.csv",package="RchivalTag")
# DepthTempTS <- read_TS(ts_file)
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

