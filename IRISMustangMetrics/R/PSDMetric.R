##
##    Metric to calculate the Power Spectral Density (PSD) for a Stream of 
##    seismic data.
##
##    Copyright (C) 2014  Mazama Science, Inc.
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
# This metric returns a list of average power spectra associated with a stream's
# data by breaking the Stream into chunks and calculating the spectrum for each.
#
# The 2004 McNamara paper "Ambient Noise Levels in the Continental United States"
# is available here:
# http://geohazards.cr.usgs.gov/staffweb/mcnamara/PDFweb/Noise_PDFs.html
#
#
# PSD algorithm from Mary Templeton on 2013-06-05
#   https://seiscode.iris.washington.edu/issues/46
#
# Target channels would include 
# {LMBSEH}{HNL}?
# {LB}G?
# 
# ----
# If channel is named L??
#   Z = 3 hours
# Else If channel is named M??
#   Z = 2 hours  (my recommendation, PQLX doesn't have a rule for M channels)
# Else
#   Z = 1 hour
# 
# Divide trace into Z-hour segments with 50% overlap
# Foreach Z-hour segment
#     Truncate segment to nearest power of 2 samples
#     Generate an averaged/smoothed PSD as follows
#         Divide each truncated Z-hour segment into 13 segments with 75% overlap (Z/4 seconds length each)
#         Foreach of 13 segments
#             Demean
#             Detrend
#             Apply 10% sine taper
#             FFT 
#             PSD 
#             Normalize the power at each PSD frequency, multiplying it by (2*dt/Nseg) where Nseg is the number of samples in the segment
#         Average the 13 resulting PSDs to get averaged PSD
#         Multiply averaged PSD by sine taper scale factor (= 1.142857)
#         Frequency smooth the averaged PSD over 1-octave intervals at 1/8-octave increments, 
#             (reducing # frequency samples by factor of 169; include 0.1 Hz as one of the geometric mean f values to sync f sampling)
#         Store the smoothed, averaged Z-hour PSD
# 

utils::globalVariables(c('freq','hits','power'))

PSDMetric <- function(st,
                      linLoPeriod=4/(st@traces[[1]]@stats@sampling_rate),
                      linHiPeriod=100,
                      evalresp=NULL,
                      noCorrection=FALSE) {
  
  # NOTE:  All the details about choosing window sizes, etc. are done in the psdList function
 
  if (length(st)==1) { 
      stop(c("PSDMetric: stopping PSD calculation because st length is one sample"))
  }
  stm <- mergeTraces(st,fillMethod="fillZero")@traces[[1]]
  if (isDC(stm)) {
      stop(c("PSDMetric: stopping PSD calculation because st is flatlined"))
  }
  rm(stm)
  
  sampling_rate <- st@traces[[1]]@stats@sampling_rate   # remember the sample rate
  
  # Use the psdList() function to apply the McNamara algorithm
  psdList <- psdList(st)

  if (!length(psdList)) {
     stop(paste("PSDMetric: No PSDs returned for",st@traces[[1]]@id))
  }

  # There is a remote possibility that different traces will have different quality identifiers
  snclqs <- sapply(psdList, getElement, "snclq")
  if (length(unique(snclqs)) > 1) {
    ids <- paste(unique(snclqs),collapse=",")
    stop(paste("PSDMetric: More than one SNCLQ in trace:",ids))
  } else {
    snclq <- unique(snclqs)[1]
  }

  
  # Use PSDs to create SpectrumMetric objects, psds before instrument correction
  spectrumMetricList <- list()
  index <- 1
  for (psd in psdList) {
    spectrumMetricList[[index]] <- new("SpectrumMetric", snclq=snclq, 
                                       starttime=psd$starttime, endtime=psd$endtime, metricName="psd",
                                       freqs=psd$freq, amps=psd$spec, phases=psd$freq*0)
    index <- index + 1
  }
  
  # Calculate SingleValueMetric to be stored in a separate list
  # This also contains the pdfMatrix, NLNM, NHNM, and associated noise stats
  # See spectralUtils.R for details

  svMetricList <- list()
  cPsdDF <- NULL
  pdfDF <- NULL

  if (noCorrection) {
    listOfLists <- list(svMetricList=svMetricList,  # empty list
                      spectrumMetricList=spectrumMetricList)   # uncorrected PSDs
    return(listOfLists)
  } else {
  
     #if evalresp NULL and trace starttime > PSD starttime, calculate evalresp here using trace starttime+1

     if (is.null(evalresp) && st@traces[[1]]@stats@starttime > psdList[[1]]$starttime) {
         if (inherits(iris,"data.frame")) {
           iris <- new("IrisClient")
         }
         minfreq <- min(psdList[[1]]$freq)
         maxfreq <- max(psdList[[1]]$freq)
         nfreq <- length(psdList[[1]]$freq)
         units <- "acc"
         evalresp <- getEvalresp(iris,st@traces[[1]]@stats@network,st@traces[[1]]@stats@station,
                              st@traces[[1]]@stats@location,st@traces[[1]]@stats@channel,time=st@traces[[1]]@stats@starttime+1, 
                              minfreq=minfreq,maxfreq=maxfreq,nfreq=nfreq,units=units)

         if (!("amp" %in% colnames(evalresp) & "freq" %in% colnames(evalresp))) {
             stop(paste("PSDMetrics:","error evalresp dataframe does not have columns named 'amp' and 'freq'"))
         }

         if (nrow(evalresp)==0) {
             stop(paste("PSDMetrics:","getEvalresp returned no content"))
         }
     }

    result <- try(psdStats <- psdStatistics(psdList, evalresp), silent=TRUE) 

    if (inherits(result,"try-error")) {
        err_msg <- geterrmessage()
        if ( !stringr::str_detect(err_msg, "psdList2NoiseMatrix: length")) {
             stop(paste("PSDMetrics:", stringr::str_replace_all(err_msg,"[\r\n]",""), snclq))
        } else {
             stop(paste("PSDMetrics:",stringr::str_replace_all(err_msg,"[\r\n]",""),snclq)) 
        }
    } else {
        # pct_above and pct_below are functions of frequency returned by psdStats
        # NOTE: We only use frequencies less than nyquist/1.5 when calculating avg_pct_above/below because of instrument response effects on PSD power as you approach the nyquist (= sample_rate/2). 
        # NOTE: This is a reasonable rule-of-thumb for most modern seismic instrumentation, but may not apply to older dataloggers that relied on anti-alias filters.

        nyquist <- sampling_rate/2
        avg_pct_above <- mean(psdStats$pct_above[psdStats$freq < nyquist/1.5], na.rm=TRUE)
        avg_pct_below <- mean(psdStats$pct_below[psdStats$freq < nyquist/1.5], na.rm=TRUE)
      
        # NOTE:
        # NOTE:  Another metric fitting the PSD mean line as a linear function of log10(period)
        # NOTE:  over a specific band has been added. Some types of sensor malfunction result in a flat spectra that
        # NOTE:  is different from the curve produced by digitizer noise. This metric attempts to identify 
        # NOTE:  these instances. Be aware that some normal noise patterns will also score low on this metric.
        # NOTE:
        # NOTE:  The dead_channel_lin metrics are not valid for LH? or VH? channels.
        # NOTE:
        # NOTE:  Yet another metric (dead_channel_gsn) has been added to the dead_channel family where we look at a
        # NOTE:  narrow band of the PDF and determine if the deviation below the NLNM is sufficient
        # NOTE:  to label it dead, versus not dead.  This is a boolean metric using numeric indicators.
      
        # Convert frequency to period
        period <- 1/psdStats$freq
      
        # Linear fit metric
        #   1) determine index range (inverted because of freq->period conversion)
        #   3) Fit PSD mean vs. log10(period) to a line
        #   4) Calculate the standard deviation of residuals of the fit

        first <- max(which(period >= linHiPeriod)) + 1
        last <- min(which(period <= linLoPeriod)) - 1
        psdMean <- psdStats$mean[first:last]
        linFit <- stats::lm(psdMean ~ log10(period[first:last]))
        dead_channel_lin <- stats::sd(linFit$residuals)
      
        # GSN Dead Channel metric
        #   1) only accept 1 SPS and greater sample rate
        #   2) get the PDF matrix and slice the 4 to 8 second period
        #   3) calculate the median value at each period step, writing result to a vector
        #   4) collect differences in the median vector from the equivalent NLNM vector [ NLNM(x) - Y(x) ]
        #   5) average the result of the differences
        #   6) if average > 5.0, mark dead_channel_gsn value = 1 (TRUE), else mark value = 0 (FALSE)

        dead_channel_gsn <- "" # for scope
        if (sampling_rate > 0.999) {
            first <- max(which(period >= 4)) + 1
            last <- min(which(period <= 8)) - 1
            psdSlice <- psdStats$noiseMatrix[,first:last]  
            if (is.vector(psdSlice)) {
                psdMedian_v <- psdSlice
            } else {
                psdMedian_v <- apply(psdSlice,2,FUN=function(x) median(x))
            }
            # verify that we have an average low dB value in this case and treat this as a dead channel condition
            nlnmSlice <- psdStats$nlnm[first:last]  # get the NLNM slice to match
	    diffToNM_v <- nlnmSlice[!(sapply(psdMedian_v,is.null))] - unlist(psdMedian_v)  # [ NLNM(x) - Y(x) ]
	    averageDiff <- mean(diffToNM_v)                     # get average of the differences (i.e. deviation)
	    dead_channel_gsn <- ifelse(averageDiff > 5.0,1,0)  # compare average to 5 dB, save result
        }
      
        #  Create metricList
        starttime <- st@traces[[1]]@stats@starttime
        endtime <- st@traces[[length(st@traces)]]@stats@endtime

        m1 <- new("GeneralValueMetric", snclq=snclq, starttime=starttime, endtime=endtime, metricName="pct_above_nhnm", elementNames=c("value"), elementValues=avg_pct_above)
        svMetricList <- append(svMetricList,list(m1))

        m2 <- new("GeneralValueMetric", snclq=snclq, starttime=starttime, endtime=endtime, metricName="pct_below_nlnm", elementNames=c("value"), elementValues=avg_pct_below)
        svMetricList <- append(svMetricList,list(m2))

        if(is.numeric(dead_channel_lin) && stringr::str_detect(st@traces[[1]]@stats@channel,"BH|HH|CH|DH|FH|BX|HX")) {
              m4 <- new("GeneralValueMetric", snclq=snclq, starttime=starttime, endtime=endtime, metricName="dead_channel_lin", elementNames=c("value"), elementValues=dead_channel_lin)
	      svMetricList <- append(svMetricList,list(m4))
        }
        if(is.numeric(dead_channel_gsn) && stringr::str_detect(st@traces[[1]]@stats@channel,"BH|HH|CH|DH|FH|LH|MH|BX|HX")) {
              m5 <- new("GeneralValueMetric", snclq=snclq, starttime=starttime, endtime=endtime, metricName="dead_channel_gsn", elementNames=c("value"), elementValues=dead_channel_gsn)
	      svMetricList <- append(svMetricList,list(m5))
        }

        # create a corrected PSD data frame similar to that returned by http://services.iris.edu/mustang/noise-psd/1/
        startlist <- do.call("c",lapply(psdList,`[[`,"starttime"))
        endlist <- do.call("c",lapply(psdList,`[[`,"endtime"))

        freqV <- rep(psdStats$freq,nrow(psdStats$noiseMatrix))
        powerV <- as.vector(t(psdStats$noiseMatrix))
        startV <- rep(startlist, rep(ncol(psdStats$noiseMatrix),length(startlist)))
        endV <- rep(endlist, rep(ncol(psdStats$noiseMatrix),length(endlist)))
        cPsdDF <- data.frame(startV, endV, freqV, powerV)
        colnames(cPsdDF) <- c("starttime","endtime","freq","power")

        hitsV <- as.vector(psdStats$pdfMatrix)
        binV <- rep(psdStats$pdfBins,ncol(psdStats$pdfMatrix))
        freq2V <- rep(psdStats$freq,rep(nrow(psdStats$pdfMatrix),length(psdStats$freq)))
        pdfDF <- data.frame(freq2V, binV,hitsV)
        colnames(pdfDF) <- c("freq","power","hits")
        pdfDF <- dplyr::filter(pdfDF, hits>0)
    }

    # NOTE:  Instead of returning a metricList, we return a list of lists.  
    listOfLists <- list(svMetricList=svMetricList,
                      spectrumMetricList=spectrumMetricList, #uncorrected PSDs
                      correctedPsdDF=cPsdDF,   # corrected PSDs
                      pdfDF=pdfDF)
  
  
    return(listOfLists)
  }
  
}

