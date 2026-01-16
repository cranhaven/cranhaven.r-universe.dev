
\newcommand{\opt}{\ifelse{latex}{\code{"#1"}}{\verb{"#1"}}}
\newcommand{\nl}{\ifelse{latex}{ }{\ifelse{html}{ }{ \cr}}}

\name{Dicpt}
\alias{Dicpt}
\alias{print.Dicpt}
\alias{summary.Dicpt}
\alias{mark.Dicpt}
\alias{plot.Dicpt}
\title{
Class methods for Dicpt Objects
}
\description{
Common functions to handle \code{"Dicpt"} class results which store
information about changepoints, both the individual library results and the
majority vote for classifier fusion.
}

\usage{
\method{print}{Dicpt}(x, \dots)
\method{summary}{Dicpt}(object, \dots)
\method{mark}{Dicpt}(x, hist=NULL, opt=Diopt(), \dots)
\method{plot}{Dicpt}(x, opt=Diopt(), \dots)
}

\arguments{
\item{x}{ an object of class \opt{Dicpt} }
\item{object}{ an object of class \opt{Dicpt} }
\item{hist}{ the histogram being marked or NULL for a low-pass or interval
  spacing graph }
\item{opt}{ local version of options for color scheme }
\item{\dots}{ extra arguments, ignored for all methods }
}

\details{
The \opt{Dicpt} class stores several different items in a list.  The
analysis in \code{find.cpt} takes two steps, first running the spacing
without any filtering through all changepoint detectors available on the
system and then combining the results into a master list by majority vote
over all libraries.

The plot needs a tall window.  It shows the changepoints from each variant
within a library as dots, labelled on the right side, and the roll-up for
each detector as crosses, labelled on the left.  The final changepoint
list is in the first row at top, and vertical bands show the region within
which library points match the final.  The plot uses the \opt{palette}
option to set its colors.

Changepoints are marked by ticks along the upper edge of the current graph,
using option color \opt{colID.cpt} in the palette.  Marking the histogram
assumes that \code{Dimodal} has added the \opt{x} column to the \code{cpt}
data frame.

The print method gives a table with the changepoints, the spacing at that
index and corresponding raw data value, and the number of votes.  It also
lists the libraries that take part in the voting.  The summary gives just
the position of the changepoint and the library list.  These methods use
option \opt{digits} to control the number of significant digits in the
table.  If the option is set to zero then the usual \code{options()$digits}
is used.
}

\value{
The \code{Dicpt} list contains seven elements.  Two describe the individual
algorithms that have run and their results.
 \item{\code{test}}{ is a data frame with one row per library and detector
   combination.  It contains four columns, \opt{library} with the full
   changepoint library name, \opt{detector} the function name and/or
   method and/or test statistic, \opt{abbrev} an abbreviation of the
   detector used for plot labels, and \opt{npt} the number of changepoints
   found.  The number may be zero. }
 \item{\code{rawpts}}{ is a list whose length is the same as the number of
   rows of \code{test}.  Its elements are each a vector of points whose length
   matches the \opt{npt} column, or NULL if the detector returned no points
   or if there were too many per the \opt{maxfncpt} option or if the
   detector timed out or had some other error.
   \nl }
\code{test} will have 0 rows if no detectors were available, and \code{rawpts}
will be an empty list.  Summarizing the arguments for this first step, the
Dicpt list element
 \item{\code{rawparam}}{ is a list with elements \opt{cptlibs} the
   specification of which libraries to use, \opt{maxfncpt} the maximum
   fraction of the data that can be identified as changepoints, \opt{ncpt}
   the conversion of maxfncpt to points, \opt{timeout} the
   run time in seconds allowed for each detector, and \opt{nx} the number
   of data points.  cptlibs, maxfncpt, and timeout come from \code{Diopt}
   options.
   \nl }

To describe the fusion of the detectors the \code{Dicpt} list adds four
elements.
 \item{\code{libpts}}{ is a list of the changepoints found by combining
   the results of each detector within a library.  It has a length equal
   to the number of libraries that were run, the unique entries of the
   first column of the \code{test} data frame. }
 \item{\code{voting}}{ is a boolean vector with the same length as
   \code{libpts} and TRUE if that library's points were counted in the voting
   or FALSE if not. }
 \item{\code{cpt}}{ is a data frame with three columns that gives the
   final changepoint list.  It has one row per changepoint.  Column \opt{pos}
   is the index of the changepoint in the original data.  \opt{val} is the
   data at the changepoint.  \opt{nvote} gives the number of votes the
   point received. }
 \item{\code{cptparam}}{ is a list with elements \opt{qvote} the pair
   of quantile bounds on the library point count used to discard libraries
   with unusually few or many changepoints, \opt{qpts} the pair
   translated into an absolute count, \opt{sep} the minimum
   absolute separation in data points between changepoints, \opt{fsep}
   the minimum separation as a fraction of the data length, \opt{libsep}
   the separation while merging library variants, \opt{near} the final
   minimum separation in data points, and \opt{minvote} the minimum number
   of votes a point must receive to appear in the final result.
   \nl }

\code{Dimodal} adds one column to the \code{cpt} data frame.  It shifts the
 changepoints to the original data grid, without the loss of the initial
 index.
 \item{\code{x}}{ the location of the peak in the original data, using the
   mid-distribution function
   \nl }
This can be done by calling the utility function \code{shiftID.place} on the
find.cpt result.  Dimodal also adds an attribute \opt{source} with value
\opt{Di} to show the changepoints lie in the raw spacing.

The methods return the Dicpt object invisibly.
}

\seealso{
 \code{\link{find.cpt}},
 \code{\link{Dimodal}},
 \code{\link{Diopt}},
 \code{\link{shiftID.place}}
}

\examples{
 ## Not run because detectors may not be available.
 \dontrun{ m <- Dimodal(faithful$eruptions, Diopt.local(analysis='cpt')) }
 \dontrun{ m$cpt }
 \dontrun{ summary(m$cpt) }
 ## You want a tall plot window for this.
 \dontrun{ dev.new(width=4, height=12) ; plot(m$cpt) }
}

\keyword{Dimodal}
\keyword{Dicpt}
\keyword{spacing}
\keyword{changepoints}

