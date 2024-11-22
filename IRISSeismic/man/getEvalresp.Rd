\name{getEvalresp}
\docType{methods}
\alias{getEvalresp}
\alias{getEvalresp,IrisClient,character,character,character,character,POSIXct-method}

\title{Retrieve instrument response information from IRIS DMC}

\description{
The \code{getEvalresp} method obtains instrument response data from
the IRIS DMC \code{evalresp} webservice.
}

\usage{
getEvalresp(obj, network, station, location, channel,
            time, minfreq, maxfreq, nfreq, units, output, spacing)
}

\arguments{
  \item{obj}{ an \code{IrisClient} object }
  \item{network}{ character string with the two letter seismic network code }
  \item{station}{ character string with the station code }
  \item{location}{ character string with the location code }
  \item{channel}{ character string with the three letter channel code }
  \item{time}{ POSIXct class specifying the time at which response is evaluated (GMT) }
  \item{minfreq}{ optional minimum frequency at which response will be evaluated }
  \item{maxfreq}{ optional maximum frequency at which response will be evaluated }
  \item{nfreq}{ optional  number of frequencies at which response will be evaluated }
  \item{units}{ optional code specifying unit conversion }
  \item{output}{ optional code specifying output type (default="fap")}
  \item{spacing}{ optional code specifying spacing, accepted values are "lin" or "linear",
                  "log" or "logarithmic" (default="log")}
}

\details{
The \code{evalresp} webservice responds to requests with data that can be used to
remove instrument response from a seismic signal.

%Each of \code{network}, \code{station}, \code{location} or \code{channel} may contain
%a valid code or a wildcard expression, e.g. "BH?" or "*". The ascii string that is used for
%these values is simply passed through to \code{evalresp}.

Each of \code{network}, \code{station} or \code{channel} should contain
a valid code without wildcards. The ascii string that is used for
these values is simply passed through to \code{evalresp}.

If the \code{location} argument contains an empty string to specify a 'blank' location code, a location
code of \code{"--"} will be used in the dataselect request URL.
(See \href{https://service.iris.edu/fdsnws/dataselect/1/}{dataselect documentation}.)

The response from \code{evalresp} is converted into a dataframe with rows in order of increasing frequency.
}


\value{
For \code{output="fap"}, a dataframe with columns named:

\preformatted{freq, amp, phase}

For \code{output="cs"}, a dataframe with columns named:

\preformatted{freq, real, imag}
}

\references{
The IRIS DMC evalresp webservice:

  \url{https://service.iris.edu/irisws/evalresp/1/}
}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

\seealso{
\code{\link{IrisClient-class}},
}


\keyword{webservices}
