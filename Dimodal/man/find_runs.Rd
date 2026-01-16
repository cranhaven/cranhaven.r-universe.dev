
\newcommand{\opt}{\ifelse{latex}{\code{"#1"}}{\verb{"#1"}}}
\newcommand{\nl}{\ifelse{latex}{ }{\ifelse{html}{ }{ \cr}}}

\name{find.runs}
\alias{find.runs}
\title{
Fuzzy run detector
}
\description{
A runs length detector that handles nearly equal real values and skips
invalid values.
}

\usage{
find.runs(x, feps)
}

\arguments{
\item{x}{
  a vector
}
\item{feps}{
  fractional (0 - 1 excl.) relative difference to treat values as same
}
}

\details{
This runs finder looks for sequences of real values that almost match,
considering them as a run of same values.  Two points match if the
difference between their values as a fraction of their average is less than
the threshold: |x[i] - x[j]| / ((|x[i]| + |x[j]|)/2) < feps.  For each
point in \code{x} the detector scans forward until a point fails to be close,
with the run taken over the matching interval.  The base of the comparison is
always the first point in the run, and not a chain of adjacent values.  The
detector treats infinities as equal to each other but not to finite values
and skips over any NA or NaN values while counting them.  Values within
double precision tolerance of each other always match, irrespective of the
relative threshold.

Note that this relative difference depends on the data not averaging close
to zero, especially at zero-crossing points, and is sensitive to a constant
shift.  The first is not a problem for spacing, which is always positive,
but may require shifting other types of data.  That would also help with the
second issue.

For integer, logical, and character data, values must match exactly and feps
is not used.

Other data types are not supported.

Although not exported from the Dimodal package, this detector may be useful
outside the spacing analysis for any signal.  Within Dimodal it is called
by the run count and length tests and internally within the C code.

The detector returns two vectors, each with the same length as the original
data.  \code{runs} stores the length of the run starting at the data point
and \code{nskip} the number of skipped elements within the run.  To process
the runs, start at index \code{i=runs[1L]}, or if that is zero
\code{i=skip[1L]}.  The next run starts at \code{i + runs[i] + nskip[i]},
until i goes beyond the length of the data.

The detector is O(\code{n}) in both time and memory, albeit with several
passes through the data.
}

\value{
find.runs returns a list with elements:
\item{\code{runs}}{
  an integer vector of length x with the length of
  the run starting at each point or 0 if the point does not start a run
}
\item{\code{nskip}}{
  an integer vector of length x with the number of
  skipped data points within the run
}
\item{\code{stats}}{
  a vector of three integer values, \code{nrun} the total number of runs,
  \code{maxrun} the longest run, and \code{nx} the length of the original
  data and of \code{runs} and \code{stats}
}
}

\note{
Use the utility function \code{runs.as.rle} to convert the returned value to
a \opt{rle} encoding.
}

\seealso{
 \code{\link{rle}},
 \code{\link{runs.as.rle}}
}

\keyword{Dimodal}
\keyword{runs}

