\name{MultipleTimeValueMetric-class}
\Rdversion{1.1}
\docType{class}

\alias{MultipleTimeValueMetric-class}

%%\alias{show}

%% NOTE:  the following lines suppress warning messages from R CMD check but clutter up the index

\alias{show,MultipleTimeValueMetric-method}


\title{Class \code{"MultipleTimeValueMetric"}}

\description{
A container for metrics consisting of a vector of \code{POSIXct} datetimes.  This information is used to create XML that is
then submitted to the MUSTANG Backend Storage System (BSS).
}
\section{Objects from the Class}{
Objects can be created by calls of the form:

\code{new("MultipleTimeValueMetric", snclq, starttime, endtime, metricName, values)}

Lists of \code{MultipleTimeValueMetric} objects are returned by various metrics functions in this package.
}

\section{Slots}{
  \describe{
    \item{\code{snclq}:}{Object of class \code{"character"}: SNCLQ identifier.}
    \item{\code{metricName}:}{Object of class \code{"character"}: Name of the metric.}
    \item{\code{elementName}:}{Object of class \code{"character"}: Name of the datetime element (default=\code{"t"}).}
    \item{\code{starttime}:}{Object of class \code{"POSIXct"}: Start time.}
    \item{\code{endtime}:}{Object of class \code{"POSIXct"}: End time.}
    \item{\code{values}:}{Object of class \code{"POSIXct"}: Datetime values.}
    \item{\code{valueStrings}:}{Object of class \code{"character"}: String representations of the datetime values.}
    \item{\code{quality_flag}:}{Object of class \code{"numeric"}: Quality flag.}
    \item{\code{quality_flagString}:}{Object of class \code{"character"}: String representation of quality flag.}
  }
}

\section{Methods}{
  \describe{
    %%\item{initialize}{\code{signature(.Object = "MultipleTimeValueMetric")}: \strong{NOT YET WRITTEN} }
    \item{show}{\code{signature(object = "MultipleTimeValueMetric")}: Prettyprints the information in the \code{MultipleTimeValueMetric} }
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
      <up_down_times>
        <t value="2012-02-10T00:00:00.000"/>
        <t value="2012-02-10T00:01:00.000"/>
        <t value="2012-02-10T00:02:00.000"/>
        <t value="2012-02-10T00:03:00.000"/>
      </up_down_times>
    </target>
  </date>
</measurements>
}

The \code{quality_flag} is an optional value available for storing information related to the
processing of a particular metric.  Its meaning will vary from metric to metric.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
\code{\link{upDownTimesMetric}}
}

\examples{
  \dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

# Get the waveform
starttime <- as.POSIXct("2012-01-24", tz="GMT")
endtime <- as.POSIXct("2012-01-25", tz="GMT")
st <- getDataselect(iris,"AK","PIN","","BHZ",starttime,endtime)

# Make sure we're working with a single snclq
unique_ids <- uniqueIds(st)
if (length(unique_ids) > 1) {
  stop(paste("meanMetric: Stream has",unique_ids,"unique identifiers"))
}
snclq <- unique_ids[1]

# get the upDownTimes with a minimum signal length and minimum gap (secs)
upDownTimes <- getUpDownTimes(st, min_signal=30, min_gap=60)
  
# Create and return a MultipleTimeValue metric from the upDownTimes
m <- new("MultipleTimeValueMetric", snclq=snclq,
         starttime=starttime, endtime=endtime,
         metricName="up_down_times", values=upDownTimes)

# Show the results
show(m)
  }
}

\keyword{classes}
