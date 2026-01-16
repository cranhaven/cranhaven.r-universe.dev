
\newcommand{\opt}{\ifelse{latex}{\code{"#1"}}{\verb{"#1"}}}
\newcommand{\nl}{\ifelse{latex}{ }{\ifelse{html}{ }{ \cr}}}

\name{Diflat}
\alias{Diflat}
\alias{print.Diflat}
\alias{summary.Diflat}
\alias{mark.Diflat}
\title{
Class methods for Diflat Objects
}
\description{
Common functions to handle \code{"Diflat"} class results, which store
information about local flats and their probability.
}

\usage{
\method{print}{Diflat}(x, \dots)
\method{summary}{Diflat}(object, \dots)
\method{mark}{Diflat}(x, hist=NULL, opt=Diopt(), \dots)
}

\arguments{
\item{x}{ an object of class \code{"Diflat"} }
\item{object}{ an object of class \code{"Diflat"} }
\item{hist}{ the histogram being marked or NULL for a low-pass or interval
  spacing graph }
\item{opt}{ local version of options for color scheme }
\item{\dots}{ extra arguments, ignored for all methods }
}

\details{
A \opt{Diflat} object is a data frame that describes local flats,
including their position or span in the low-pass or interval spacing and
the equivalent raw values, their length, and any tests that have run to
judge the significance of the feature.  The basic information comes from
the \code{find.flats} detector.  The analysis in \code{Dimodal} moves the
positions to the original data and adds the test results.  The class
inherits from \opt{data.frame}.

The print method shows the endpoints (incl.) of the flat and the raw value
at each end, the statistics (test values) and separately the probability from
each.  The acceptance level of the tests are also given underneath.  These
come from the options \opt{alpha.len}, \opt{alpha.ftexcur.lp}, and
\opt{alpha.ftexcur.diw}.  If the option \opt{mark.alpha} is TRUE then
probabilities at or below the acceptance level are underlined.  Set the
option FALSE if your terminal does not support ANSI escape codes, or see the
\pkg{crayon} package for the extensive tests needed to determine this
automatically.  These options are set at the global level and cannot be
overridden for a single call to print; Diopt is called internally.  The pass
column after the overall flat probability will contain the number of
accepted/passing tests, if any.  Raw values and statistics are printed with
option \opt{digits} significant digits.  If the option is set to zero then
the default \code{options()$digits} is used.

The summary method gives the endpoints, the overall probability or best result
from all tests, and a list of the tests that pass per the \code{Diopt}
acceptance levels.

Flats are marked in a spacing graph according to the \opt{mark.flat} option,
either as a box around the spacing or as a bar above or below it.  The border
or line is thicker if the flat passes any test at the option acceptance
levels.  When annotating a histogram the flats are always drawn as bars at
the top of the graph.  The marks are colored per the option \opt{colID.flat}
index of the current palette.  Marking the histogram assumes that Dimodal
has added the \code{xst} and \code{xend} columns.
}

\value{
The basic data frame returned from \code{find.flats} contains the columns
 \item{\code{src}}{ the data point that triggered the flat, whose value is
   nearly centered in the height.  This is a unique identifier for the
   feature. }
 \item{\code{stID}}{ first data point in the flat }
 \item{\code{endID}}{ last data point (incl.) in the flat }
 \item{\code{len}}{ the length of the flat, or endID - stID + 1 }
 \item{\code{srcval}}{ the data value at src }
 \item{\code{ht}}{ the height of the flat }
 \item{\code{htsd}}{ the flat height standardized by the data's standard
   deviation, or ht / sd(x)
   \nl }
There is one row per flat, and the result is an empty data frame with zero
rows if there are no flats.  The indices of stID and endID are located in
the data passed to the detector, which excludes the filter's NA columns.

\code{Dimodal} adds several columns.  It shifts all indices to the original
data, adjusting for the filter or interval size.  Mapping indices back to
the raw data, it adds
 \item{\code{xst}}{ the location of the start point in the original data }
 \item{\code{xend}}{ the location of the end point (incl.) in the
   original data
   \nl }
This can be done by calling the utility function \code{shiftID.place} on the
\code{find.flats} result.  It also adds an attribute \opt{source} with value
"LP" or "Diw" depending on the filter used.  As a summary of the results it
adds \item{\code{pflat}}{ the best probability of all tests run, NA if no
test results are available }
 \item{\code{naccept}}{ number of tests passing their acceptance level,
   0 if none or pflat is NA
   \nl }

For low-pass spacing the test results may include
 \item{\code{plen}}{ the probability of the feature per the length model }
 \item{\code{hexcur}}{ the flat height used for the excursion test }
 \item{\code{pexcur}}{ fraction of bootstrap samples from the low-pass spacing
   generating smaller heights than hexcur
   \nl }
depending on what has been run.
   
The interval spacing omits the length model test and may perform the excursion
test on the difference of the interval spacing.
 \item{\code{hexcur}}{ the feature height used for the excursion test }
 \item{\code{pexcur}}{ fraction of bootstrap samples from the interval
   spacing that generate smaller heights than hexcur
   \nl }

The methods return the object, invisibly.
}

\seealso{
  \code{\link{find.flats}},
  \code{\link{Dimodal}},
  \code{\link{Diopt}},
  \code{\link{shiftID.place}},
  \code{\link{data.frame}}
}

\examples{
## We override the analysis option to avoid changepoints,
## which may not be available.
m <- Dimodal(faithful$eruptions, Diopt.local(analysis=c('diw','lp'), diw.window=16))
m$lp.flats
summary(m$diw.flats)
}

\keyword{Diflat}
\keyword{Dimodal}

