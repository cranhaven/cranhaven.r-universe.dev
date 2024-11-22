##
##    Boolean metrics that compares miniSEED sample rate with metadata 
##    channel sample rate. An error is flagged (1=TRUE) if sample rates disagree.
##

################################################################################
# Metrics calculated by this script include
#
#   - sample_rate_channel
#
sampleRateChannelMetric <- function(st, channel_pct=1, chan_rate=NULL) {
  
  starttime <- st@requestedStarttime
  endtime <- st@requestedEndtime

  mseed_rate <- st@traces[[1]]@stats@sampling_rate
  network <- st@traces[[1]]@stats@network
  station <- st@traces[[1]]@stats@station
  location <- st@traces[[1]]@stats@location
  if (location == "") {
    location <- "--"
  }
  channel <- st@traces[[1]]@stats@channel
  quality <- st@traces[[1]]@stats@quality

  if (is.null(chan_rate)) {
    if (!inherits(iris,"IrisClient")) {
       iris <- new("IrisClient") # defaults to fdsnws 
    }
    if (is.null(chan_rate)) { # if metadata sample rate or normalized frequency not provided, go to IRIS web services
       av <- getChannel(iris,network,station,location,channel,st@traces[[1]]@stats@starttime, st@traces[[1]]@stats@endtime)
    }
  }

  # Calculate sample_rate_channel metric

  if (is.null(chan_rate)) { # if metadata sample rate not provided, go to IRIS web services
    chan_rate <- av$samplerate   
  }
  
  # Boolean: 0 (FALSE) if miniSEED and metadata channel sample rates agree within +/- channel_pct %
  if (abs(mseed_rate - chan_rate) < (mseed_rate*(channel_pct/100))) {
    sample_rate_channel <- 0  # Agreement
  } else {
    sample_rate_channel <- 1  # Disagreement
  }
  
  snclq <- st@traces[[1]]@id
  m1 <- new("GeneralValueMetric", snclq=snclq, starttime=starttime, endtime=endtime, metricName="sample_rate_channel", elementNames=c("value"), elementValues=sample_rate_channel)
  
  return(c(m1))
}
