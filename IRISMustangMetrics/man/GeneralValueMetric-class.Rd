\name{GeneralValueMetric-class}
\Rdversion{1.1}
\docType{class}

\alias{GeneralValueMetric-class}

%%\alias{show}

%% NOTE:  the following lines suppress warning messages from R CMD check but clutter up the index

\alias{show,GeneralValueMetric-method}


\title{Class \code{"GeneralValueMetric"}}

\description{
A container for metrics consisting of a vector of numeric values.  This information is used to create XML that is
then submitted to the MUSTANG Backend Storage System (BSS).
}
\section{Objects from the Class}{
Objects can be created by calls of the form:

\code{new("GeneralValueMetric", snclq, starttime, endtime, metricName, elementNames,  elementValues, valueStrings, quality_flag, quality_flagString)}

Lists of \code{GeneralValueMetric} objects are returned by various metrics functions in this package.
}

\section{Slots}{
  \describe{
    \item{\code{snclq}:}{Object of class \code{"character"}: SNCLQ identifier.}
    \item{\code{starttime}:}{Object of class \code{"POSIXct"}: Start time.}
    \item{\code{endtime}:}{Object of class \code{"POSIXct"}: End time.}
    \item{\code{metricName}:}{Object of class \code{"character"}: Name of the metric.}
    \item{\code{elementNames}:}{Object of class \code{"character"}: Names of the elements storing the metric values (default=\code{"x"}).}
    \item{\code{elementValues}:}{Object of class \code{"numeric"}: Numeric values.}
    \item{\code{valueStrings}:}{Object of class \code{"character"}: String representations of the numeric values.}
    \item{\code{quality_flag}:}{Object of class \code{"numeric"}: Quality flag.}
    \item{\code{quality_flagString}:}{Object of class \code{"character"}: String representation of quality flag.}
  }
}

\section{Methods}{
  \describe{
    %%\item{initialize}{\code{signature(.Object = "GeneralValueMetric")}: \strong{NOT YET WRITTEN} }
    \item{show}{\code{signature(object = "GeneralValueMetric")}: Prettyprints the information in the \code{GeneralValueMetric} }
	 }
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan.s.callahan@gmail.com}
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
      <EXAMPLE>
        <x value="1"/>
        <x value="2"/>
        <x value="3"/>
        <x value="4"/>
      </EXAMPLE>
    </target>
  </date>
</measurements>
}

The \code{quality_flag} is an optional value available for storing information related to the
processing of a particular metric.  Its meaning will vary from metric to metric.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{ }


\keyword{classes}
