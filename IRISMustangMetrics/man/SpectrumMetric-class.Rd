\name{SpectrumMetric-class}
\Rdversion{1.1}
\docType{class}

\alias{SpectrumMetric-class}
\alias{SpectrumMetric}

%%\alias{show}

%% NOTE:  the following lines suppress warning messages from R CMD check but clutter up the index

\alias{show,SpectrumMetric-method}


\title{ Class \code{"SpectrumMetric"} }

\description{
A container for metrics consisting of discrete spectra.  This information is used to create XML that is
then submitted to the MUSTANG Backend Storage System (BSS).
}
\section{Objects from the Class}{
Objects can be created by calls of the form:

\code{new("SpectrumMetric", snclq, starttime, endtime, metricName, freqs, amps, phases)}
}

\section{Slots}{
  \describe{
    \item{\code{snclq}:}{Object of class \code{"character"}: SNCLQ identifier.}
    \item{\code{metricName}:}{Object of class \code{"character"}: Name of the metric.}
    \item{\code{elementName}:}{Object of class \code{"character"}: Name of the datetime element (default=\code{"t"}).}
    \item{\code{starttime}:}{Object of class \code{"POSIXct"}: Start time.}
    \item{\code{endtime}:}{Object of class \code{"POSIXct"}: End time.}
    \item{\code{freqs}:}{Object of class \code{"numeric"}: Frequency values.}
    \item{\code{freqStrings}:}{Object of class \code{"character"}: String representations of the frequency values.}
    \item{\code{amps}:}{Object of class \code{"numeric"}: Amplitude values.}
    \item{\code{ampStrings}:}{Object of class \code{"character"}: String representations of the amplitude values.}
    \item{\code{phases}:}{Object of class \code{"numeric"}: Phase values.}
    \item{\code{phaseStrings}:}{Object of class \code{"character"}: String representations of the phase values.}
    \item{\code{quality_flag}:}{Object of class \code{"numeric"}: Quality flag.}
    \item{\code{quality_flagString}:}{Object of class \code{"character"}: String representation of quality flag.}
  }
}

\section{Methods}{
  \describe{
    %%\item{initialize}{\code{signature(.Object = "SpectrumMetric")}: \strong{NOT YET WRITTEN} }
    \item{show}{\code{signature(object = "SpectrumMetric")}: Prettyprints the information in the \code{SpectrumMetric} }
	 }
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

\note{
The \code{quality_flag} is an optional value available for storing information related to the
processing of a particular metric.  Its meaning will vary from metric to metric.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{ }


\keyword{classes}
