##
##    Metric to calculate the number of outliers for a Stream of seismic data
##
##    Copyright (C) 2012  Mazama Science, Inc.
##    by Jonathan Callahan, jonathan@mazamascience.com
##
##    This program is free software; you can redistribute it and/or modify
##    it under the terms of the GNU General Public License as published by
##    the Free Software Foundation; either version 2 of the License, or
##    (at your option) any later version.
##
##    This program is distributed in the hope that it will be useful,
##    but WITHOUT ANY WARRANTY; without even the implied warranty of
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##    GNU General Public License for more details.
##
##    You should have received a copy of the GNU General Public License
##    along with this program; if not, write to the Free Software
##    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA


################################################################################
# The IRIS DMC MUSTANG project keeps track of
#
#   - num_spikes
#
# This metric applies the roll_hampel filter from the 'seismicRoll' package.
# See:  "Median absolute deviation (MAD) outlier in Time Series"

spikesMetric <- function(st, windowSize=41, thresholdMin=10, selectivity=NA, fixedThreshold=TRUE) {
  
  starttime <- st@requestedStarttime
  endtime <- st@requestedEndtime

  num_traces <- length(st@traces)
  x <- sapply(1:length(st@traces),function(x) { c(st@traces[[x]]@data) })
  x <- c(x, recursive=TRUE)
    
  if (length(x) < windowSize) {
     stop(paste("spikesMetric: skipping ", st@traces[[1]]@id, "trace length", length(x), "is less than windowSize", windowSize))
  }

  # Calculate the total number of outliers from all traces
  count <- 0  
  result <- try( outlierIndices <- seismicRoll::findOutliers(x, n=windowSize, thresholdMin=thresholdMin ,selectivity=selectivity, fixedThreshold=fixedThreshold),
                   silent=TRUE )
    
  if (class(result)[1] != "try-error" ) {    
      # NOTE:  Ignore adjacent outliers when determining the count of spikes. 
      # NOTE:  But be sure there is at least one spike if there is at least one outlier.
      if (length(outlierIndices) != 0 ) {
          count <- length(which(diff(outlierIndices) > 1)) + 1
      }
  } else { 
     stop(paste("spikesMetric: skipping ", st@traces[[1]]@id, geterrmessage())) 
  } 
 
  # Create and return a list of Metric objects
  snclq <- st@traces[[1]]@id
  m1 <- new("GeneralValueMetric", snclq=snclq, starttime=starttime, endtime=endtime, metricName="num_spikes", elementNames=c("value"), elementValues=count)

  return(c(m1))
  
}
