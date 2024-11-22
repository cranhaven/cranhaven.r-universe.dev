\docType{package}

\name{IRISMustangMetrics-package}
\alias{IRISMustangMetrics-package}
\alias{IRISMustangMetrics}

\title{Utilities for calculating seismic metrics from EarthScope (formerly IRIS DMC) data}

\description{
This package provides S4 classes and functions for calculating metrics from seismological 
data available from EarthScope (\url{http://ds.iris.edu/ds/nodes/dmc/}).
This package is part of the MUSTANG project.
}

\section{Introduction}{
  The \pkg{IRISMustangMetrics} package depends upon the \pkg{IRISSeismic} package which  defines
  new S4 classes and methods for manipulating seismic data.  Please see the "seismic-intro" 
  vignette for introductory examples on using \pkg{IRISSeismic}.
}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

\references{
IRIS DMC web services: \url{http://service.iris.edu/}
}

\section{History}{

  version 2.4.6
  \itemize{
     \item(updated email addresses)
     \item(ISPQAUtils, add psd_corrected metric name to allow the ISPAQ tool to return uncorrected psds)
  }

  version 2.4.5
  \itemize{
     \item(typo fix for email address and updated link in documentation)
     \item(updated error handling code to meet CRAN requirements)
  }

  version 2.4.4
  \itemize{
     \item(additional error handling for sample_rate_resp)
  }

  version 2.4.3
  \itemize{
    \item(improved error handling for functions that call IRIS web services)
    \item(additional error handling for sample_rate_resp)
  }
  version 2.4.2
  \itemize{
    \item{minor modification to ISPAQUtils.R for sample_rate_resp, sample_rate_channel}
  }
  version 2.4.1
  \itemize{
    \item{fix for max_range where trace length not evenly divided by window length}
    \item{max_range now rounds window*samplerate and increment*samplerate to integer values}
    \item{corrects typo error in ISPAQUtils.R}
  }
  version 2.4.0
  \itemize{
    \item{adds new metrics sample_rate_resp, sample_rate_channel, max_range}
  }

  version 2.3.0
  \itemize{
    \item{modifies PSD metrics for edge case involving metadata and trace start times}
    \item{adds noCorrection option to PSDMetric, if noCorrection=TRUE it only returns uncorrected 
          PSDs and not corrected PSD or PSD-derived metrics; default noCorrection=FALSE}
  }

  version 2.2.0
  \itemize{
    \item{removes dead_channel_exp, metric has been retired}
    \item{renames pdf_aggregator in ISPAQUtils.R to pdf}
  }

  version 2.1.3
  \itemize{
    \item{fixed bug related to getGeneralValueMetrics,getMustangMetrics error handling}
    \item{added pdf_aggregator to ISPAQUtils.R, for multi-day pdf plotting within ISPAQ}
  }

  version 2.1.2
  \itemize{
   \item{version 2.1.1 unintentionally removed the pct_above_nhnm metric. This version restores it.}
   \item{made sample rate sanity rate check consistent between correlationMetric and crossCorrelationMetric}
  }

  version 2.1.1
  \itemize{
   \item{fixed bug in PSDMetrics that affected dead_channel_gsn results}
  }

  version 2.1.0
  \itemize{
   \item{transfer_function requires sample rates to be within a factor of 10 to avoid decimation effects on amplitude}
   \item{transfer_function uses 7 order Chebyshev filter in the decimate function, to correct 1\% error occurring with default 8 order Chebyshev}
   \item{fixed bug in transfer_function trace start and end time comparisons}
   \item{transfer_function when determining if sample rate < 1, round to 5 digits first}
   \item{getGeneralValueMetrics added metric_error, ts_channel_continuity, ts_channel_up_time, ts_gap_length, ts_gap_length_total, ts_max_gap, 
         ts_max_gap_total, ts_num_gaps, ts_num_gaps_total, ts_percent_availability, ts_percent_availability metrics}
   \item{aliased the getGeneralValueMetrics function to getMustangMetrics}
   \item{dailyDCOffsetMetric now returns error when result is NaN or NA}
  }

  version 2.0.9
  \itemize{
   \item{removed dependency on pracma package}
   \item{removed channel restrictions for pct_above_nhnm,pct_below_nlm}
   \item{cross correlation sampling rates of < 1 will round to 2 digits}
   \item{getGeneralValueMetrics better handles case of no targets found}
   \item{improved error handling in spikesMetric.R}
  }

  version 2.0.8
  \itemize{
   \item{minor bug fix to ISPAQUtils.R, spikes=numSpikes}
  }

  version 2.0.7
  \itemize{
   \item{fixed bug in getGeneralValueMetrics that didn't return measurements if there was more than one for any day}
   \item{crossCorrelationMetric filter now defaults to a butterworth 2 pole 0.1Hz (10 second) low pass filter}
  }

  version 2.0.6
  \itemize{
   \item{fixed bug related to NA -> NULL replacement in Class-Metric}
  }

  version 2.0.5
  \itemize{
   \item{fixed dplyr version dependencies}
  }

  version 2.0.4
  \itemize{
   \item{adds additional sanity check to \code{getGeneralValueMetrics()}}
   \item{createBssUrl() adds "&nodata=404" to url}
  }

  version 2.0.2
  \itemize{
   \item{updates to ISPAQUtils.R}
  }
  version 2.0.1
  \itemize{
    \item{removed dependency on tidyr package}
  } 
  version 2.0.0 -- GeneralValueMetrics
  \itemize{
    \item{\code{GeneralValueMetric} class introduced, \code{SingleValueMetric} class deprecated. All metrics that previously returned
          \code{SingleValueMetric} now return \code{GeneralValueMetric}}
    \item{\code{getGeneralValueMetrics()} function added. Retreives metrics measurements from BSS database}
    \item{\code{crossCorrelationMetric()} does not return timing_drift. The metric proved unreliable}
    \item{users can now supply instrument response information in the form of frequency, amplitude, phase
          to the function \code{PSDMetric}, in place of the getEvalresp webservice call}
  }
    
  version 1.3.1 -- PSDs
  \itemize{
    \item{\code{getPsdMetrics} reworked}
  }

  version 1.3.0 -- latency
  \itemize{
    \item{\code{getLatencyValuesXML()} removed from package.}
    \item{documentation improvements.}
    \item{additional error checking for \code{getSingleValueMetrics()}.}
  }

  version 1.2.7 -- PSDs
  \itemize{
    \item{\code{PSDMetrics()} metrics \code{percent_above_nhnm} and \code{percent_below_nlnm} limited to frequencies less than nyquist/1.5.}
  }

  version 1.2.6 -- PSDs
  \itemize{
    \item{Depends on \pkg{IRISSeismic} (>= 1.3.0).}
    \item{\code{dead_channel_exp} and \code{dead_channel_lin} metrics will only return values for station channel codes matching "BH|HH".}
  }

  version 1.2.5 -- ISPAQUtils
  \itemize{
    \item{ISPAQUtils.R contains functions for use with the ISPAQ standalone metrics system.}
  }

  version 1.2.4 -- package version dependencies
  \itemize{
    \item{Depends on \pkg{IRISSeismic} (>= 1.2.3). Imports \pkg{seismicRoll} (>=1.1.2).}
  }

  version 1.2.2 -- correlationMetric tweak
  \itemize{
    \item{\code{correlationMetric()} allows trace sample lengths to differ by 2 samples without stopping.}
  }

  version 1.2.1 -- PSDs
  \itemize{
    \item{Better fix to very low powers issue in \code{PSDMetrics()} \code{dead_channel_gsn} metric.}
    \item{\code{PSDMetrics()} shifts PDF bin centers by 0.5 dB.}
  }

  version 1.2.0 -- PSDs
  \itemize{
    \item{\code{PSDMetric()} returns corrected PSD and PDF dataframes in addition to uncorrected PSDs and PSD derived metrics.}
    \item{Depends on R (>= 3.2.0) and \pkg{IRISSeismic} (>=1.1.7).}
    \item{Imports \pkg{tidyr}, \pkg{dplyr}.}
  }

  version 1.1.3 -- bug fix, import version increased
  \itemize{
    \item{Fixes typo in \code{SNRMetric()} function \code{windowSecs} argument default value.}
    \item{Imports \pkg{seismicRoll} (>=1.1.1)}
  }

  version 1.1.2 -- modifications
  \itemize{
    \item{Improves error handling messages.}
    \item{\code{dailyDCOffsetMetric()} removes unused selectivity argument and adds argument controlling output type.}
    \item{Fixes bug in \code{dailyDCOffsetMetrics()} related to outlier removal and vector length.}
    \item{Fixes bug in \code{PSDMetrics()} \code{dead_channel_gsn} metric related to very low power values.}
    \item{\code{PSDMetrics()} only returns metrics that generate numeric values.}
  }

  version 1.1.1 -- bug fix
  \itemize{
    \item{\code{crossCorrelationMetric()} exits if either input trace is flatlined (all values equal).}
  }

  version 1.1.0 -- updates package dependencies
  \itemize{
    \item{Depends on \pkg{IRISSeismic} (>= 1.1.0).}
  }

  version 1.0.8 -- new metric and bug fix
  \itemize{
    \item{Improves error handling messages.}
    \item{Adds new \code{dead_channel_gsn} metric to \code{PSDMetric()} function output.}
    \item{Fixes bug in \code{STALTAMetric()} involving required trace length.}
  }

  version 1.0.7 -- bug fix
  \itemize{
    \item{Fixes issue with \code{spikesMetric()} passing argument values to \code{findOutliers}.}
  }
 
  version 1.0.6 -- function argument changes
  \itemize{
    \item{Changes \code{spikesMetric()} default argument values \code{thresholdMin=10},\code{selectivity=NA},\code{fixedThreshold=TRUE}.}
    \item{\code{transferFunctionMetric()} now requires input of evalresp fap spectra, new arguments \code{evalresp1} and \code{evalresp2}.}
    \item{Additional sanity checks for \code{transferFunctionMetric()} and \code{PSDMetric()}.}
    \item{Depends on \pkg{IRISSeismic} (>= 1.0.10). Imports \pkg{seismicRoll} (>=1.1.0). Imports \pkg{stats}.}
  }

  version 1.0.5 -- new PSD metric
  \itemize{
    \item{Changes URL syntax for MUSTANG web services to use "format=..." instead of "output=...".}
    \item{Adds new \code{sample_unique} metric to \code{PSDMetric()} output.}
  }

  version 1.0.3 -- new functionality and bug fixes
  \itemize{
    \item{Adds new \code{metricList2DF()} function.}
    \item{Adds new \code{dead_channel_lin} metric to \code{PSDMetric()} output.}
    \item{Fixes typo in \code{Class-Metric.R} value string format.}
  }
  
  version 1.0.0 -- First Public Release
  
}

%% \seealso{ }

\examples{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient", debug=TRUE)

# Get the seismic data
starttime <- as.POSIXct("2010-02-27 06:45:00",tz="GMT")
endtime <- as.POSIXct("2010-02-27 07:45:00",tz="GMT")
result <- try(st <- getDataselect(iris,"IU","ANMO","00","BHZ",starttime,endtime))
if (class(result) == "try-error" ) {
   print(geterrmessage())
} else {
   # Apply a metric and show the results
   metricList <- basicStatsMetric(st)
   dummy <- lapply(metricList, show)
}
}
%% Optional keywords
