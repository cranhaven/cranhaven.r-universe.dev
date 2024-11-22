\docType{package}
\name{IRISSeismic-package}
\alias{IRISSeismic-package}
\alias{IRISSeismic}

\title{Classes and methods for seismic data analysis}

\description{
This package provides S4 classes for downloading and processing seismological 
data available from the IRIS Data Management Center (DMC) (\url{http://ds.iris.edu/ds/nodes/dmc/}).
Core classes \code{Trace}, \code{Stream} and \code{IrisClient} and their associated methods
are inspired by the functionality available in the python ObsPy package (\url{http://obspy.org/}).
}

\section{Introduction}{
  The "IRISSeismic-intro" vignette gives introductory examples on using the package.
}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

\references{
ObsPy: \url{http://obspy.org/}

IRIS DMC web services: \url{https://service.iris.edu/}
}

\section{History}{

  version 1.6.6
  \itemize{
    \item{ for web service calls that support the nodata=<204|404> option, use nodata=204 }
    \item{ corrected the crossSpectrum documentation, Pxy is the cross-periodogram for ts1 and ts2 }
  }

  version 1.6.5
  \itemize{
    \item{ examples and vignette updated for better error handling when accessing internet resources}
  }
 
  version 1.6.4
  \itemize{
    \item{ updated to (modified version of) libmseed-2.19.8 }
  }

  version 1.6.3
  \itemize{
    \item{ Stream object @ timing_quality now averages the values of the miniSEED blockette 1001 timing quality values, instead of summing the blockette 1001 values and dividing by the number of records}
  }

  version 1.6.2
  \itemize{
    \item{ getDataselect, modified default time out values }
    \item{ fixed url in documentation }
  }

  version 1.6.0
  \itemize{
    \item{ irisNetrc definition moved inside getDataselect function}
    \item{ restored getTimeseries function }
    \item{ default values for class Trace InstrumentSensitivity and SensitivityFrequency changed to NA from 1.0 }
    \item{ added transferFunctionSpectra function }
  }

  version 1.5.2
  \itemize{
    \item{ updated getDataAvailablity to use new fdsnws availability web service specification
           http://service.iris.edu/fdsnws/availability/1/ }
    \item{ fixed bug in getDataAvailability when mergequality=FALSE }
    \item{ fixed bug in getDataAvailability affecting start/end times, introduced in version 1.5.1 }
    \item{fixed bug in mergeTraces when trace has gap at end, introduced in version 1.5.1}
  }

  version 1.5.1
  \itemize{
    \item{ changed a subset of time format OS to OS6}
    \item{ fixed bug in mergeTraces when fdsnws/dataselect implementation cuts
           on records instead of sample }
  }

  version 1.5.0
  \itemize{
    \item{ added spacing as an option to getEvalresp }
    \item{ modified getEvalresp to use IrisClient service_type }
    \item{ new getDataAvailability() to return dataframe of
           miniseed data extents in the IRIS archive using 
           IRIS web service http://service.iris.edu/irisws/availability/1/ }
    \item{ minor change to src code to pass CRAN checks } 
  }

  version 1.4.9
  \itemize{
    \item{ additional error handling }
    \item{ minor updates to the plot.Trace and plot.Stream functions }
    \item{ updated src/libmseed to version 2.19.6 }
  }

  version 1.4.8
  \itemize{
    \item{ updated src/libmseed to version 2.19.5 }
    \item{ fix bug related to leap seconds }
    \item{ functions that call web services now follow redirects }
    \item{ some error outputs have changed slightly }
    \item{ rmsVariance function, na.rm=TRUE calculates data length minus NA values }
    \item{ rmsVariance.Stream now honors na.rm=TRUE }
    \item{ getGaps() error handling now checks for negative sample rates }
    \item{ getEvent, getEvalresp now truncates start and end input times to seconds 
          (time format OS0 instead of OS) to fix error when user set options(digits.secs=) > 3 } 
  }

  version 1.4.7
  \itemize{
    \item{ additional error handling for getDistaz }
    \item{ added input service_type to IrisClient, defaults to fdsnws}
    \item{ plot.Trace x-axis labels are "MM dd" instead of days of week for traces > 1 day and < 1 week}
    \item{ getDataselect will retry once if it encounters http code 401}
    \item{ additional error handling for spectralUtils }
  }

  version 1.4.6
  \itemize{
    \item{bug fix for IRISSeismic::slice}
  }

  version 1.4.5
  \itemize{
    \item{fixed bug in noiseModels for low noise model results at periods > 10000 seconds}
    \item{retry if getEvent returns a service unavailable message}
  }

  version 1.4.4
  \itemize{
    \item{modified error messages for getEvalresp() and getDistaz()}
  }
  version 1.4.3
  \itemize{
    \item{changed getEvent default url from http://earthquake.usgs.gov/fdsnws/event/1/ to https://earthquake.usgs.gov/fdsnws/event/1/}
  }
  version 1.4.2
  \itemize{
    \item{updated libmseed version to 2.19}
  }
  version 1.4.1
  \itemize{
    \item{updated libmseed version to 2.18}
    \item{fix for reading miniseed with out of order records}
  }
  version 1.4.0
  \itemize{
    \item{addition of repository argument to getDataselect and getSNCL, to match change in fdsnws-dataselect web service}
  }
  version 1.3.9
  \itemize{
    \item{fixes compile warning generated by clang}
    \item{removes followlocation=TRUE from getDataselect RCurl options}
  }
  version 1.3.8
  \itemize{
    \item{\code{getDataselect} does not add a quality indicator to url by default. IRIS webservices itself defaults to quality="M"}
    \item{\code{getStation} and \code{getChannel} do not add includerestricted indicator to url by default. IRIS webservices itself defaults to \code{TRUE}}
    \item{better handling of textConnections}
  }
  version 1.3.7
  \itemize{
    \item{users can now supply instrument response information in the form of frequency, amplitude, phase
          to the functions \code{psdStatistics}, \code{psdList2NoiseMatrix}, \code{psdPlot}, 
          in place of the getEvalresp webservice call. Function argument order for \code{psdPlot} is changed.}
    \item{added \code{showMedian} option to \code{psdPlot}}
  }

  version 1.3.5 
  \itemize{
    \item{added ignoreEpoch option to getDataselect}
  }

  version 1.3.4 -- webservices and plotting
  \itemize{
    \item{\code{getEvent} forwards http://service.iris.edu/fdsnws/event/1/ calls to http://earthquake.usgs.gov/fdsnws/event/1/}
    \item{\code{getDistaz} changes output dataframe column name ellipsoid..attrs to ellipsoid.name}
    \item{\code{plot.Trace} allows for user supplied ylab and xlab input}
  }

  version 1.3.3 -- documentation
  \itemize{
    \item{Updated documentation and corrected outdated links}
  }

  version 1.3.2 -- bug fix
  \itemize{
    \item{noiseModels(), minor correction to the New High Noise Model}
  }

  version 1.3.1 -- bug fixes
  \itemize{
    \item{psdStatistics() correctly handles NA values when calculating high and low PDF bin limits
          and returns pct_above and pct_below vectors of correct length}
  }

  version 1.3.0 -- compatibility with IRIS webservices
  \itemize{
    \item{getDistaz() returns new variables from output of http://services.iris.edu/irisws/distaz/1/}
  }

  version 1.2.2 -- PDF bug fix
  \itemize{
    \item{psdList2NoiseMatrix() adds 1 second to start time in getEvalresp call to work around a quirk in http://services.iris.edu/irisws/evalresp/1/ 
          webservice that will not return a response if the start time is exactly on a metadata epoch boundary.}
  }
  
  version 1.2.1 -- PDF
  \itemize{
    \item{psdPlot() now compatible with changes to psdStatistics() in previous version. Adds ylo, yhi arguments to customize y-axis limits in plot.}
  }

  version 1.2.0 -- PDF
  \itemize{
    \item{psdStatistics() changes method of setting PDF bins from fixed values to bins based on the high and low PSD values 
          and shifts bin centers by 0.5 dB. The result now matches output from http://services.iris.edu/mustang/noise-pdf.}
  }

  verison 1.1.7 -- improved error handling
  \itemize{
    \item{getDataselect(), getNetwork(),  getStation(), getChannel(), getAvailability(), getEvalresp(), getTraveltime() error handling 
          now report unexpected http status codes.}
  }

  version 1.1.6 -- bug fixes
  \itemize{
    \item{getGaps() fixes issues with multiple sample rates and setting minimum gap length.}
    \item{mergeTraces.Stream() relaxes criteria for acceptable sample rate jitter.}
  }

  version 1.1.5 -- trace rotation
  \itemize{
    \item{rotate2D() changes orthogonality test tolerance from 5 degrees to 3 degrees.}
  }

  version 1.1.4 -- trace rotation
  \itemize{
    \item{rotate2D() exits if traces are not orthogonal.}
  }

  version 1.1.3 -- bug fix
  \itemize{
    \item{psdStatistics() fixes bug in calculation of pct_above and pct_below.}
  }

  version 1.1.1 -- bug fixes
  \itemize{
    \item{getGaps() minor bug fix.}
    \item{mergeTraces.Stream() minor bug fix.}
  }

  version 1.0.10 -- new data request argument and bug fixes
  \itemize{
    \item{Imports seismicRoll (>= 1.1.0).}
    \item{getGaps() fixes bugs in calculation of initial and final gap of Trace.}
    \item{getDataselect(), getSNCL() adds "inclusiveEnd" argument, a logical that determines whether a data point that falls exactly on the requested endtime 
          is included in the Trace.}
    \item{libmseed change, when multiple sample rates exist in miniseed records use the mode of all sample rates instead of using the sample rate in the first record.}
    \item{psdList() added rule for octave generation for channel codes that start with "V".}
  }

  version 1.0.9 -- Trace class expansion and bug fixes
  \itemize{
    \item{Improved error handling for getAvailability(), getChannel(), getDataselect(), getEvalresp(), miniseed2Stream().}
    \item{parseMiniSEED.c, unpackdata.c  updated. Fixes protection stack overflow issue.}
    \item{getGaps() includes a 0.5/sampling_rate tolerance factor.}
    \item{miniseed2Stream() uses endtime from parseMiniSEED instead of calculating from the sample rate.}
    \item{Trace class now contains slots for optional metadata "latitude", "longitude", "elevation", "depth", "azimuth", "dip", "SensitivityFrequency".}
    \item{rotate2D() uses Trace class "azimuth" slot information to identify channel orientation before rotation instead of assuming lead and lag channel 
          from trace input order.}
  }

  version 1.0.8 -- fixes required by ISPAQ
  \itemize{
    \item{Removed 'maps' and 'mapdata' from Suggested: packages.}
    \item{Changed URL syntax for FDSN web services to use "format=..." instead of "output=...".}
    \item{Fixed bug in getSNCL() so that it works when the "quality" argument is missing.}
  }

  version 1.0.6 -- CRAN updates required
  \itemize{
    \item{Removed "mode" argument form Trace.as.vector() signature.}
  }

  version 1.0.4 -- name change to IRISSeismic
  \itemize{
    \item{Name change required because 'seismic' was recently taken.}
    \item{Using explicit references for 'utils' and 'stats' package functions as this is now required for CRAN.}
  }

  version 1.0.3 -- cleanup for submission to CRAN
  \itemize{
    \item{Updated libmseed to version 2.16}
  }

  version 0.2.8.0 -- minor tweaks to 0.2.7
  \itemize{
    \item{Updated links to IRIS web services in the documentation.}
    \item{McNamaraBins() ignores bin #0 (~= DC)}
    \item{McNamaraPSD() conversion to dB occurs \strong{after} binning, not before}
  }

  version 0.2.7.0 -- hilbert transform
  \itemize{
    \item{New hilbertFFT() function.}
    \item{New hilbert() trace method.}
  }
  
  version 0.2.6.0 -- cross correlation
  \itemize{
    \item{Added surfaceDistance() function.}
    \item{Added rotate2D() function.}
  }
  
  version 0.2.5.0 -- channel orientation
  \itemize{
    \item{Jumping to version 0.2.5 to match project milestone names.}
    \item{Added getSNCL() convenience wrapper for getDataselect() method.}
    \item{Added getDistaz() method of IrisClient.}
    \item{Added miniseed2Stream() and readMiniseedFile() functions.}
    \item{Added getRotation() method of IrisClient.}
  }
  
  version 0.2.3.0 -- cross spectrum
  \itemize{
    \item{Moved McNamaraPSD() from trace method to spectral utility function.}
    \item{Added spectral utility functions:
    \itemize{
      \item{crossSpectrum()}
      \item{McNamaraBins()}
    }
    }
    \item{All get~ methods that return dataframes now guarantee a default ordering of rows.}
  }
  
  version 0.2.2.0 -- PSD and friends
  \itemize{
    \item{Add dependency on pracma package.}
    \item{Use pracma::detrend() function in DDT.Trace().}
    \item{Added "increment" parameter to STALTA.Trace().}
    \item{Removed STALTA.Trace() algorithm "classic_LR2".}
    \item{Fixed URL generation for getEvalresp() when location="".}
    \item{Added NamaraPSD.Trace() method.}
    \item{Added PSD/PDF utility functions:
      \itemize{
        \item{noiseMatrix2PdfMatrix()}
        \item{noiseModels()}
        \item{psdDF2NoiseMatrix()}
        \item{psdList()}
        \item{psdList2NoiseMatrix()}
        \item{psdStatistics()}
        \item{psdPlot()}
      }
    }
  }
    
  version 0.2.1.1 -- Bug fix release
  \itemize{
    \item{Removed dependcy on signal, XML packages.}
  }
    
  version 0.2.1.0 -- FDSN web services
  \itemize{
    \item{Conversion to FDSN web services including the following new/rewritten methods:
    \code{getNetwork}, \code{getStation}, \code{getChannel}, \code{getAvailability}, \code{getUnavailability}}
    \item{Updated version of \code{getEvent} to return a dataframe with columns named "latitude" and "longitude"
    for consistency with all other web services}
    \item{Updated documentation and Rscripts to match the API changes in the conversion to FDSN web services.}
    \item{Removal of all \code{StationXML} classes in favor of storing that information in slots of the \code{Trace} class.}
    \item{Updates to \code{Trace} object slots \code{@Sensor}, \code{@InstrumentSensitivity} and \code{@InputUnits}
    to store information as \code{character}, \code{numeric} and \code{character} instead of \code{StationXML} classes.}
    \item{The \code{TraceHeader@quality} slot now reflects the data quality returned in the miniSEED record
    rather than the quality that was requested by getDataselect. (Requests with \code{quality=B} for "Best" typically return
    \code{quality=M}.)}
    \item{Improved \code{STALTA.Trace()} method removes experimental algorithms and now uses C++ code from package 
    \code{rollSeismic} to calculate rolling means.}
    \item{Updated \code{IrisClient} now uses web services from \url{http://service.iris.edu} for the following methods:
    \code{getDataselect}, \code{getEvalresp}, \code{getEvent}}
  }
  
  version 0.2.0.0
  \itemize{
    \item{Removed PSD methods of \code{Stream} and \code{Trace}. PSD algorithms are now part of the PSD metric.}
    \item{Improved \code{mergeTraces.Stream()} method now accepts \code{fillMethod="fillZero"}.}
  }
  
  version 0.1.9.0 
  \itemize{
    \item{New \pkg{rollSeismic} package for fast rolling algorithms implemented in C++/Rcpp.}
    \item{New \code{num_spikes} metric based on \code{seismicRoll::roll_hampel} outlier detection.}
    \item{New \code{correlation} metric.}
    \item{New scripts \code{glitchMetrics.Rscript}, \code{correlationMetric.Rscript}, \code{pressureCorrelation.Rscript}}
    \item{New \code{trace@stats@processing} slot for data processing information.}
    \item{New \code{Stream} methods: \code{mergeTraces}, \code{plot}}
    \item{Improved \code{getGaps.Stream()} method properly handles initial and final gaps.}
    \item{Improved MCR error messing.}
  }
  
  version 0.1.8.0 -- IrisClient methods getEvent and getTraveltime, improved SNR metric
  
  version 0.1.7.0 -- PSD

  version 0.1.6.0 -- improved errors, miniSEED parser
  
  version 0.1.5.0 -- code cleanup, improved errors, package vignette
  
  version 0.1.4.0 -- STA/LTA, upDownTimes, basic plotting
  
  version 0.1.3.0 -- SNR, memory profiling
  
  version 0.1.2.0 -- ...
  
  version 0.1.1.0 -- ...
}

\seealso{
% Optional links to other man pages, e.g. jonc
\code{\link{IrisClient-class}}, 
\code{\link{Trace-class}}, 
\code{\link{Stream-class}}, 
}

\examples{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient", debug=TRUE)

starttime <- as.POSIXct("2010-02-27 06:45:00", tz="GMT")
endtime <- as.POSIXct("2010-02-27 07:45:00", tz="GMT")

# Get the seismic data
result <- try(st <- getDataselect(iris,"IU","ANMO","00","BHZ",starttime,endtime))
if (inherits(result,"try-error")) {
  message(geterrmessage())
} else {

# Extract the first trace, display the metadata and plot it
  tr1 <- st@traces[[1]]
  show(tr1@stats)
  plot(tr1)
}
}

%% Optional keywords
