##
##    Boolean metrics that compare miniSEED sample rate with  
##    the high-frequency amplitude response corner 
##    for consistency.  An error is flagged (1=TRUE) if sample rates disagree.
##

################################################################################
# Metrics calculated by this script include
#
#   - sample_rate_resp
#
################################################################################

sampleRateRespMetric <- function(st, resp_pct=15, norm_freq=NULL, evalresp=NULL) {
  
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

  if (is.null(norm_freq) || is.null(evalresp)) {
    if (!inherits(iris,"IrisClient")) {
       iris <- new("IrisClient") # defaults to fdsnws 
    }
    if (is.null(norm_freq)) { # if metadata sample rate or normalized frequency not provided, go to IRIS web services
       av <- getChannel(iris,network,station,location,channel,st@traces[[1]]@stats@starttime, st@traces[[1]]@stats@endtime)
       norm_freq <- av$scalefreq
    }
  }

  # Calculate sample_rate_resp metric
 
  if (is.null(norm_freq)) {
    stop("sampleRateRespMetrics: normalization frequency is null")
  }

  if (norm_freq == 0 ) {
    stop("sampleRateRespMetrics: normalization frequency is 0")
  }
  
  # Retrieve 100 frequency samples per decade for the response
  minf <- norm_freq / 10
  maxf <- 10*mseed_rate
  nf <- ceiling(log10(maxf/minf)*100)
  units <- 'def'

  if (minf >= maxf) {
    stop("sampleRateRespMetrics: minimum frequency calculated from the normalization frequency is greater than or equal to the maximum frequency calculated from the miniSEED sample rate")
  }
  
  colNames <- c("frequency","amp", "dAdf")
  colClasses <- c("character","character","character")

  # query evalresp
  #   If ISPAQ calls this function but lacks access to evalresp from IRIS web services, it will 
  #   generate equivalent response values by running evalresp on a local RESP file and pass this 
  #   information using the "evalresp" argument to the sampleRateRespMetric function.  When this is 
  #   not the case, this argument will be NULL.

  if ( is.null(evalresp) ) {
    DF <- getEvalresp(iris,network,station,location,channel,st@traces[[1]]@stats@starttime,minf,maxf,nf,units,output="fap")
  } else { 
    DF <- evalresp
  } 
  colnames(DF) <- colNames

  if ( !("amp" %in% colnames(DF) & "frequency" %in% colnames(DF)) ) {
    stop(paste("sampleRateRespMetrics:","error response dataframe does not have columns named 'amp' and 'frequency'"))
  }

  if (nrow(DF)==0) {
    stop(paste("sampleRateRespMetrics:","getEvalresp returned no content"))
  }

  if (length(which(DF$amp == 0)) == nrow(DF)) {
    stop(paste("sampleRateRespMetrics:","getEvalresp returned all zero values"))
  }

  if(all(is.nan(DF$amp))) {
    stop(paste("sampleRateRespMetrics:","getEvalresp returned all null values"))
  }

  if (nrow(DF) > 0) {
    dAmp <- with(DF, ( ( as.numeric(DF$amp[-1]) - as.numeric(DF$amp[-length(DF$amp)]) ) / as.numeric(DF$amp[-1]) ) )
    df <- with(DF, ( abs( as.numeric(DF$frequency[-1]) - as.numeric(DF$frequency[-length(DF$frequency)]) ) / as.numeric(DF$frequency[-1]) ) )
    dAdf <- c(NA,dAmp/df)
    DF$dAdf <- as.character(dAdf)
    
    # Find Nyquist and sample rate from response curve
    foundRollOff <- FALSE
    rollOffDF <- data.frame(frequency=character(),amp=character(),dAdf=character(),stringsAsFactors=FALSE)
    cornerFreq <- 0

    # Find the rolloff and find the frequency associated with the maximum negative slope
    # Empirically, a slope of -50 appears to protend FIR rolloffs, but not sensor poles
    for (j in 1:length(DF$dAdf)) {
      if (is.na(DF$dAdf[j])) { next }
      if (as.numeric(DF$dAdf[j]) > -50 && foundRollOff == FALSE) { next }
      if (as.numeric(DF$dAdf[j]) <= -50) {
        foundRollOff <- TRUE
        rollOffDF <- rbind(rollOffDF,DF[j,])
      }
      if (as.numeric(DF$dAdf[j]) > -50 && foundRollOff == TRUE) { 
        cornerFreq <- as.numeric(rollOffDF$frequency[which.min(as.numeric(rollOffDF$dAdf))])
        break 
      }
    }

    #if (length(rollOffDF$dAdf) == 0) {
    #  cornerFreq <- 0
    #}

    resp_rate <- 2*cornerFreq
    
    # Boolean: 0 (FALSE) if respSampRate is within +/- resp_pct % of the miniSEED samplerate
    if (abs(resp_rate - mseed_rate) < (mseed_rate*(resp_pct/100))) {
      sample_rate_resp <- 0   # Agreement
    } else {
      sample_rate_resp <- 1   # Disagreement
    }
    
  }
  
  # Create and return a list of Metric objects
  snclq <- st@traces[[1]]@id
  m1 <- new("GeneralValueMetric", snclq=snclq, starttime=starttime, endtime=endtime, metricName="sample_rate_resp", elementNames=c("value"), elementValues=sample_rate_resp)
  
  return(c(m1))
}
