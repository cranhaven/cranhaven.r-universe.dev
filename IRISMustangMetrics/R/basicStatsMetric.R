##
##    Metric to calculate signal and mass min/mean/median/max for a Stream of seismic data
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
#

basicStatsMetric <- function(st) {
  
  starttime <- st@requestedStarttime
  endtime <- st@requestedEndtime
  
  # Make sure we're working with a single snclq
  unique_ids <- uniqueIds(st)
  if (length(unique_ids) > 1) {
    stop(paste("basicStatsMetric: Stream has",length(unique_ids),"unique identifiers"))
  }
  snclq <- unique_ids[1]
  
  # Calculate the range metrics
  min <- min(st, na.rm=TRUE)
  median <- median(st, na.rm=TRUE)
  mean <- mean(st, na.rm=TRUE)
  max <- max(st, na.rm=TRUE)
  rmsVariance <- rmsVariance(st,na.rm=TRUE)

  countUnique <- function(x){
     data <- unlist(lapply(x@traces,slot,"data"))
     return(length(unique(stats::na.omit(data))))
  }
  uniqueSample <- countUnique(st)

  # Create and return a list of Metric objects
  m1 <- new("GeneralValueMetric", snclq=snclq, starttime=starttime, endtime=endtime, metricName="sample_min", elementNames=c("value"), elementValues=min)
  m2 <- new("GeneralValueMetric", snclq=snclq, starttime=starttime, endtime=endtime, metricName="sample_median", elementNames=c("value"), elementValues=median)
  m3 <- new("GeneralValueMetric", snclq=snclq, starttime=starttime, endtime=endtime, metricName="sample_mean", elementNames=c("value"), elementValues=mean)
  m4 <- new("GeneralValueMetric", snclq=snclq, starttime=starttime, endtime=endtime, metricName="sample_max", elementNames=c("value"), elementValues=max)
  m5 <- new("GeneralValueMetric", snclq=snclq, starttime=starttime, endtime=endtime, metricName="sample_rms", elementNames=c("value"), elementValues=rmsVariance)
  m6 <- new("GeneralValueMetric", snclq=snclq, starttime=starttime, endtime=endtime, metricName="sample_unique", elementNames=c("value"), elementValues=uniqueSample)
  
  return(c(m1,m2,m3,m4,m5,m6))
  
}
