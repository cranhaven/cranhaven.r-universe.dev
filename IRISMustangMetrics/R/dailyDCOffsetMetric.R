##
##    Metric to process a dataframe with daily means and return a vector of daily
##    likelihoods that a DC shift occurred. -- modified to return the daily likelihood
##	  for just the last day represented.
##
##    Copyright (C) 2013  Mazama Science, Inc.
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
# REC - Jan 2014 - making modifications around the roll_sd call in order to address
# seg faults that are stopping production.
#
dailyDCOffsetMetric <- function(df, 
                                offsetDays=5,
                                outlierWindow=7,
                                outlierThreshold=3.0,
                                outputType=1) {               # outputType is a flag (0,1) that determines whether the function returns values for one day or multiple days
                                                          # default outputType=1 returns one day, outputType=0 returns multiple days
  

  # Sanity checks
  if (!inherits(df,"data.frame")) {
    stop(paste("dailyDCOffsetMetric: Argument 'df' is of class",class(df),". A dataframe is required."))
  }
  if (nrow(df) < 1) {
    stop("dailyDCOffsetMetric: No data found in dataframe.")
  }
  if (all(!c("sample_mean","starttime","endtime") %in% names(df))) {
    stop("dailyDCOffsetMetric: Dataframe does not contain one or more of variables 'sample_mean', 'starttime','endtime'.")    
  }
  
  if ( !(outputType %in% c(0,1))  ) {
    stop(paste("dailyDCOffsetMetric: variable 'outputType'=", outputType, "is invalid, must be 1 or 0"))
  }
  
  # Metric algorithm:
  #
  #   data0 = download length(df$sample_mean) of daily means
  #   data1 = remove outliers using MAD outlier detection with a (outlierWindow)-day window
  #   data2 = replace outliers with rolling median values using a (outlierWindow)-day window
  #   weights = calculate absolute lagged differences with 1-N day lags (big jumps have large values)
  #   metric0 = multiply the lagged differences together and take the N'th root
  #   stddev0 = calculate the rolling standard deviation of data2 with a N-day window
  #   METRIC = divide metric0 by the median value of stddev0

  # Replace outliers with rolling median values
  outliers <- seismicRoll::findOutliers(df$sample_mean,n=outlierWindow,thresholdMin=outlierThreshold)
  cleanMean <- df$sample_mean
  cleanMean[outliers] <- seismicRoll::roll_median(df$sample_mean,n=7)[outliers]
  cleanMean <- cleanMean[1:(length(cleanMean)-floor(outlierWindow/2))]        #last floor(outlierWindow/2) number of values are not checked for outlier status by the findOutlier algorithm, do not use further
  
  # Vanilla metric
  metric <- rep(1.0,length(cleanMean))
  
  # Have a minimum value to prevent occasional zeros associated with different lags from completely wiping out large values
  diffMin <- rep(1e-3,length(cleanMean))

  # Create vectors of daily differences with N=offsetDays increasing lags, multiplying them together and then taking the N'th root
  for (i in seq(offsetDays)) {
    # Create a daily metric from the lagged data with NA's at the beginning.  Each date has the difference
    # between that date and the value 'i' days earlier.
    dailyDiff <- c(rep(NA,i),abs(diff(cleanMean,lag=i))) 

    # Multiplying them together weights those shifts over the previous N days. 
    metric <- metric * pmax(diffMin, dailyDiff) 
  }
  metric <- metric^(1/offsetDays)

  # Scale the metric by the median of the rolling sd with a window size of offsetDays
  scaling <- stats::quantile(seismicRoll::roll_sd(cleanMean,offsetDays),0.5,na.rm=TRUE)
  metric <- metric / scaling

  # We have a vector of metric values and now convert these into a list of GeneralValueMetric objects
  metricList <- list() 

  if (outputType==1) {               # either take the last value in the metric vector or entire metric vector, depending on outputType flag
      j <- length(metric)
      if (is.na(metric[j])) {
           stop("dailyDCOffsetMetric: NA value returned")
      } else {
           metricList[[1]] <- list( new("GeneralValueMetric",
                                 snclq=df[j,"snclq"], starttime=df[j,"starttime"], endtime=df[j,"endtime"],
                                 metricName="dc_offset", elementNames=c("value"), elementValues=metric[j])   )
      }
  } else {
      index <- 1
      for (j in seq(length(metric))) {
          if (is.na(metric[j])) {
              next
          } else {
              metricList[[index]] <- list( new("GeneralValueMetric",
                                 snclq=df[j,"snclq"], starttime=df[j,"starttime"], endtime=df[j,"endtime"],
                                 metricName="dc_offset", elementNames=c("value"), elementValues=metric[j])   )
          }
          index <- index+1
      }
  }
  if (length(metricList) == 0) {
     stop("dailyDCOffsetMetric: NA values returned")
  }
  
  return(metricList)
}

