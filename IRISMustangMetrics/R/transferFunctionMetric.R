##
##    Metric to calculate the correlation between two streams of seismic data
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
# Notes from Mary Templeton email:
#
#2) For each channel pair
#     Calculate complex transfer function:
#       request 1 hr of data (noise is ideal, so any consistent time is fine; only need this monthly or weekly)
#       if channel sample rates differ, decimate to make them identical
#         detrend
#         demean
#         compute complex cross-spectrum Pyx of traces x and y (same proceedure as PSD)
#         retrieve stored PSD Pxx(f)
#         calculate transfer function values:
#           TFxy(f) = Pyx(f) / Pxx(f)
#           save average amplitude and phase values for T=5-7s
#     Calculate the corresponding response amplitude ratio and phase difference for x and y:
#       request responses for x and y for that 1 hour
#       calculate RESPgain = gainRESPy(f) / gainRESPx(f)
#       calculate RESPphase = phaseRESPy(f) - phaseRESPx(f)
#         save average gain & phase values for T=5-7s
# 
#     Calculate (data/resp) gain ratio from the two saved gain averages
#     Calculate (data - resp) phase difference from the two saved phase averages
# 
#     Calculate the magnitude squared coherence of x and y:
#       retrieve stored PSDs for Pxx and Pyy
#       MScoherence = |Pxy(f)|^2 / (Pxx*Pyy)
#       save the average MScoherence for T=5-7s
# 
#     Report:
#       averaged (data/resp) gain ratio
#       (data - resp) phase difference
#       averaged MScoherence
#       start & end time of the noise window


transferFunctionMetric <- function(st1, st2, evalresp1, evalresp2) {

  # TransferFunctionMetric was updated to accept two seismic traces plus two 
  # evalresp fap spectra.  Spectra had previously been calculated inside this
  # function, but since we are now rotating horizontal channels for the secondary 
  # instrument to match those of the primary, the spectra must also be rotated
  # before being passed.  This functionality is now in 
  # IRISMustangUtils::generateMetrics_transferFunction 
  # (the IRISMustangUtils package is not available on CRAN at this time)
  #
  # The evalresp input expected by the transferFunctionMetric can be generated using
  # the IRISSeismic::transferFunctionSpectra function (IRISSeismic 1.6.0+). 
  # However, this will not perform rotation if any is needed.
  #
  # I have also modified phase difference reporting so that its sign is preserved. 
  #
  # Also, channel names use the convention SS:PPP where SS is the first two letters 
  # of the secondary channel code and PPP is all three channels of the primary 
  # channel code. This bookkeeping convention clarifies cases where channel names 
  # being compared have different letters.  Only the first two characters of the 
  # secondary instrument are used since this instrument may or may not be rotated.
  # met (2016/04/13)

  #-----------------------------------------------------------------------------
  # Configurable element
  #-----------------------------------------------------------------------------
  
  # NOTE:  When spans=NULL, R's spec.pgram() returns a vector of 1.0 for the coherence.
  # NOTE:  The minimal Daniell smoothing for a vlid return is spans=c(3). We choose 
  # NOTE:  c(3,5) here which is still very minimal for any sampling rate > 1 Hz.
  
  # Choose Daniell smoothing spans for crossSpectrum()
  spans <- c(3,5)
  
  #-----------------------------------------------------------------------------
  # Sanity checks and data resampling
  #-----------------------------------------------------------------------------
  
  # Sanity check number of traces
  if (length(st1@traces) > 1) {
    stop(paste("transferFunctionMetric:",st1@traces[[1]]@id,"has more than one trace."))
  }
  if (length(st2@traces) > 1) {
    stop(paste("transferFunctionMetric:",st2@traces[[1]]@id,"has more than one trace."))
  }
  
  tr1 <- st1@traces[[1]]
  tr2 <- st2@traces[[1]]   
  
  # Get the start- and endtimes
  starttime <- tr1@stats@starttime
  endtime <- tr1@stats@endtime
  
  # Sanity check temporal extent
  if (abs(difftime(tr2@stats@starttime,starttime,units='sec')) > 1) {
    stop(paste("transferFunctionMetric: starttimes don't match:",tr2@stats@starttime,",",starttime))
  }
  if (abs(difftime(tr2@stats@endtime,endtime,units='sec')) > 1) {
    stop(paste("transferFunctionMetric: endtimes don't match:",tr2@stats@endtime,",",endtime))
  }
  
  # Sanity check sampling rates
  if (round(tr1@stats@sampling_rate,digits=5) < 1) {
    stop(paste("transferFunctionMetric:",tr1@id,"has a sampling_rate < 1."))
  }
  if (round(tr2@stats@sampling_rate,digits=5) < 1) {
    stop(paste("transferFunctionMetric:",tr2@id,"has a sampling_rate < 1."))
  } 
  
  # Deal with potentially different sampling rates
  sr1 <- as.integer(round(tr1@stats@sampling_rate))
  sr2 <- as.integer(round(tr2@stats@sampling_rate))
  sampling_rate <- min(sr1,sr2)
  
  if (sr1 > sampling_rate) {
    if (sr1 %% sampling_rate > 0) {
      stop(paste("transferFunctionMetric: sampling rates are not multiples of each other,", tr1@id,",", tr2@id))
    }
    increment <- round(sr1/sampling_rate)
    if (increment > 10) {
      stop(paste("transferFunctionMetric: sampling rates differ by more than a factor of 10,", tr1@id,",", tr2@id))
    }
    d1 <- signal::decimate(tr1@data,increment,7)      
  } else {
    d1 <- signal::decimate(tr1@data,1,7)
  }
  
  if (sr2 > sampling_rate) {
    if (sr2 %% sampling_rate > 0 ) {
      stop(paste("transferFunctionMetric: sampling rates are not multiples of each other,", tr1@id,",", tr2@id))
    }
    increment <- round(sr2/sampling_rate)
    if (increment > 10) {
      stop(paste("transferFunctionMetric: sampling rates differ by more than a factor of 10,", tr1@id,",", tr2@id))
    }
    d2 <- signal::decimate(tr2@data,increment,7)      
  } else {
    d2 <- signal::decimate(tr2@data,1,7)
  }
  
  # Sanity check that we have valid data everywhere
  if ( any(is.na(d1)) ) {
    stop(paste("transferFunctionMetric: NA values generated during resampling,", tr1@id))
  }

  if ( any(is.na(d2)) ) {
    stop(paste("transferFunctionMetric: NA values generated during resampling,", tr2@id))
  }
  
  #-----------------------------------------------------------------------------
  # Whew! Sanity checks are done. Now on to the spectral analysis part.
  #-----------------------------------------------------------------------------
  
  
  # Choose McNamara frequencies based on the chanel 'band code'(=sampling rate)
  # See:  http://www.iris.edu/manuals/SEED_appA.htm
  # NOTE:  This choice was recommended by Mary Templeton
  channel <- tr1@stats@channel
  if (stringr::str_detect(channel,"^L")) {
    loFreq <- 0.001
    hiFreq <- 0.5 * tr1@stats@sampling_rate
  } else if (stringr::str_detect(channel,"^M")) {
    loFreq <- 0.0025
    hiFreq <- 0.5 * tr1@stats@sampling_rate
  } else {
    loFreq <- 0.005
    hiFreq <- 0.5 * tr1@stats@sampling_rate
  }
  
  # Set an alignment frequency from which octaves will be generated
  alignFreq <- 0.1
  
  
  # Truncate segment to nearest power of 2 samples
  pow2 <- floor(log(length(d1),2))
  truncatedLength <- 2^pow2
  
  dfSum <- c()

  # Divide the truncated trace data into 13 segments with 75% overlap
  for (i in 0:12) {
    
    first <- i * truncatedLength/16 + 1
    last <- (i+4) * truncatedLength/16
    
    # Create 'timseries' objects
    ts1 <- stats::ts(d1[first:last],frequency=sampling_rate)
    ts2 <- stats::ts(d2[first:last],frequency=sampling_rate)

    # Sanity check that we have valid data everywhere
    if ( any(is.na(ts1)) ) {
      stop(paste("transferFunctionMetric: NA values generated during smoothing,", tr1@id))
    }
    if (any(is.na(ts2))) {
      stop(paste("transferFunctionMetric: NA values generated during smoothing,", tr2@id))
    }

    # Cross-spectrum
    df <- crossSpectrum(stats::ts.union(ts1,ts2),
                        spans=spans,
                        taper=0.1,
                        demean=FALSE,
                        detrend=TRUE)
    
    # NOTE:  crossSpectrum() returns 'two-sided' spectra and needs to be multiplied
    # NOTE:  by 2.0 when the time series being evaluated is Real rather than Complex.
    # NOTE:  Please see these two excellent sources, especially the 'psd' vignette:
    # NOTE:  
    # NOTE:    http://www.stanford.edu/class/ee262/software/signal_tb.pdf
    # NOTE:    http://cran.r-project.org/web/packages/psd/vignettes/normalization.pdf
    df$spec1 <- 2 * df$spec1
    df$spec2 <- 2 * df$spec2
    
    # Create a summary dataframe if it doesn't exist.
    # Otherwise add each column of 'df' to it's similarly named column in 'dfSum'
    if(is.null(dfSum)) {
      dfSum <- df
    } else {
      dfSum <- dfSum + df
    }
    
  } # END OF McNamara loop
  
  # Average values for all columns
  df <- dfSum / 13
  
  # Binned versions of all columns
  DF <- McNamaraBins(df, loFreq, hiFreq, alignFreq)
  
  #-----------------------------------------------------------------------------
  # Spectral analysis done. Now on to calculation of the metrics.
  #-----------------------------------------------------------------------------
  
  
  # Transfer function average amplitude and phase over periods of 5-7 seconds.
  # NOTE:  Need to convert phase from radians to positive degrees for comparison with evalresp.
  dataAmp <- Mod(DF$Pxy/DF$Pxx)
  dataPhase <- DF$phase * 180/pi
  
  # Calculate average values for periods in the 5-7 second range
  indices <- which(1/DF$freq >= 5 & 1/DF$freq <= 7)
  avgDataAmp <- mean( dataAmp[indices] )
  avgDataPhase <- mean( dataPhase[indices] )
  avgCoherence <- mean( DF$coh[indices] )
  
  # Calculate the corresponding response amplitude ratio and phase difference for x and y:
  
  # NOTE:  Order here is important!
  # NOTE:  To compare against Mod(Pxy/Pxx) we need to use evalresp2$amp / evalresp1$amp
  # NOTE:  (see seismic/test/transferFunctionValidation.R)
  
  respAmp <- evalresp2$amp / evalresp1$amp
  respPhase <- evalresp1$phase - evalresp2$phase
  
  # Calculate average values for periods in the 5-7 second range
  indices <- which(1/evalresp1$freq >= 5 & 1/evalresp1$freq <= 7)  
  avgRespAmp <- mean( respAmp[indices] )
  avgRespPhase <- mean( respPhase[indices] )
  
  dataRespGainRatio <- avgDataAmp / avgRespAmp
  dataRespPhaseDiff <- avgDataPhase - avgRespPhase
  
  dataRespPhaseDiffMagnitude <- ifelse( abs(dataRespPhaseDiff)>180, 360-dataRespPhaseDiff, dataRespPhaseDiff )
  
  # NOTE:  Order here is important!
  # NOTE:  To match Mary Templeton's expectations from her MATLAB script we need specify "2:1"
  
  # Create two-channel ids if needed
  locations <- tr2@stats@location
  if (tr2@stats@location != tr1@stats@location) {
    locations <- paste(locations, ":", tr1@stats@location, sep="")
  }
  channels <- tr2@stats@channel
  channels <- gsub(".$",":", channels)
  channels <- paste(channels, tr1@stats@channel, sep="")

  snclq <- paste(tr1@stats@network, tr1@stats@station, locations, channels, tr1@stats@quality, sep=".")
  
  elementNames <- c("gain_ratio", "phase_diff", "ms_coherence")
  elementValues <- c(dataRespGainRatio, dataRespPhaseDiffMagnitude, avgCoherence)
  valueStrings <- c(format(dataRespGainRatio,digits=7,nsmall=4), format(dataRespPhaseDiffMagnitude,digits=7,nsmall=4), format(avgCoherence,digits=7,nsmall=4))

  m1 <- new("GeneralValueMetric", snclq=snclq, starttime=starttime, endtime=endtime,
             metricName="transfer_function", elementNames=elementNames, elementValues=elementValues)

  return(c(m1))

}
