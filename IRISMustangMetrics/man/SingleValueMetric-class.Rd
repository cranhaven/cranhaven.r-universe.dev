\name{SingleValueMetric-class}
\Rdversion{1.1}
\docType{class}

\alias{SingleValueMetric-class}
\alias{SingleValueMetric}

%%\alias{show}

%% NOTE:  the following lines suppress warning messages from R CMD check but clutter up the index

\alias{show,SingleValueMetric-method}


\title{Class \code{"SingleValueMetric"}}

\description{
A container for metrics results and associated metadata.  This information is used to create XML that is
then submitted to the MUSTANG Backend Storage System (BSS). This has been superceded by \code{GeneralValueMetric} 
and is no longer in use.
}
\section{Objects from the Class}{
Objects can be created by calls of the form:

\code{new("SingleValueMetric", snclq, starttime, endtime, metricName, value)}

Lists of \code{SingleValueMetric} objects are returned by various metrics functions in this package.
}

\section{Slots}{
  \describe{
    \item{\code{snclq}:}{Object of class \code{"character"}: SNCLQ identifier.}
    \item{\code{metricName}:}{Object of class \code{"character"}: Name of the metric.}
    \item{\code{starttime}:}{Object of class \code{"POSIXct"}: Start time.}
    \item{\code{endtime}:}{Object of class \code{"POSIXct"}: End time.}
    \item{\code{valueName}:}{Object of class \code{"character"}: Name of the XML value identifier (default=\code{"value"}).}
    \item{\code{value}:}{Object of class \code{"numeric"}: Metric value.}
    \item{\code{valueString}:}{Object of class \code{"character"}: String representation of the metric value.}
    \item{\code{quality_flag}:}{Object of class \code{"numeric"}: Quality flag.}
    \item{\code{quality_flagString}:}{Object of class \code{"character"}: String representation of quality flag.}
    \item{\code{attributeName}:}{Object of class \code{"character"}: Name of one or more optional attributes.}
    \item{\code{attributeValueString}:}{Object of class \code{"character"}: String representation of one or more attribute values.}
  }
}

\section{Methods}{
  \describe{
    %%\item{initialize}{\code{signature(.Object = "SingleValueMetric")}: \strong{NOT YET WRITTEN} }
    \item{show}{\code{signature(object = "SingleValueMetric")}: Prettyprints the information in the \code{SingleValueMetric} }
	 }
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

\note{
The \code{starttime} and \code{endtime} slots are typically associated with the \emph{user requested} times 
which may not match up with the
\code{starttime} associated with the first \code{Trace} and the \code{endtime} associated with last \code{Trace}
in the \code{Stream} object being analyzed.  This ensures that
metrics results for a single time period but covering many stations or channels will have the same date range and
improves performance of the BSS which expects XML of the following form:

\preformatted{
<measurements>
  <date start='2012-02-10T00:00:00.000' end='2012-02-10T09:20:00.000'>
    <target snclq='N.S.L.C1.Q'>
      <example value='1.0'/>
    </target>
    <target snclq='N.S.L.C2.Q'>
      <example value='2.0'/>
    </target>
    <target snclq='N.S.L.C3.Q'>
      <example value='3.0'/>
    </target>
  </date>
  </date>
</measurements>
}

The \code{quality_flag} is an optional value available for storing information related to the
processing of a particular metric.  Its meaning will vary from metric to metric.

For an IRIS/DMC specific example, the \code{station_completeness} metric obtains a list of available channels
for a station from the availability web service and compares this list with the list of \code{percent_availability} metrics for this station stored in the MUSTANG BSS.  In the case of the \code{station_completeness} metric, the 
\code{quality_flag} is set to the number of channels that should be available but for whom no \code{percent_availability} measure is obtained form the BSS.

The \code{attributeName} and \code{attributeValueString} slots can be used to store additional attributes associated with a metric values. For example, the \code{max_stalta} value for a seismic trace can be calculated and a metric can be created that contains this value and another attribute with a string representation of the time at which this maximum occurred.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{ }

\examples{
  \dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

# Get the waveform
starttime <- as.POSIXct("2012-01-24", tz="GMT")
endtime <- as.POSIXct("2012-01-25", tz="GMT")
st <- getDataselect(iris,"AK","PIN","","BHZ",starttime,endtime)

# Apply a metric and show the results
metricList <- basicStatsMetric(st)
show(metricList[[1]])
  }
}

\keyword{classes}
