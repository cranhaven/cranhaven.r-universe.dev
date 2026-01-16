
\newcommand{\opt}{\ifelse{latex}{\code{"#1"}}{\verb{"#1"}}}
\newcommand{\nl}{\ifelse{latex}{ }{\ifelse{html}{ }{ \cr}}}

\name{Dimodal Utility Functions}
\alias{midquantile}
\alias{runs.as.rle}
\alias{select.peaks}
\alias{center.diw}
\alias{match.features}
\alias{shiftID.place}
\title{
Dimodal Utility Functions
}
\description{
Miscellaneous functions for working with Dimodal results.
}

\usage{
midquantile(x, q=((1:length(x))-1)/(length(x)-1), type=0L, feps=0.0)
runs.as.rle(runs, x)
select.peaks(pk)
center.diw(m)
match.features(m, near=10, foverlap=0.70, nomatch=NA_integer_, quiet=FALSE)
shiftID.place(feat, offset, xmid, midoff)
}

\arguments{
\item{m}{
  a \opt{Dimodal} object returned from \code{Dimodal}
}
\item{x}{
  the original data, with the same length as the members of \code{runs}
}
\item{runs}{
  the list returned from \code{find.runs}
}
\item{pk}{
  a \opt{Dipeak} object
}
\item{near}{
  maximum distance in points between matching peaks, or as a fraction of the
  length of the original data
}
\item{foverlap}{
  minimum fraction of the length of either flat that the common segment must
  cover
}
\item{nomatch}{
  value to use when a feature has no match in the other spacing, treated as
  integer internally
}
\item{quiet}{
  a boolean, TRUE to only determine the matching, FALSE to also print the
  aligned features
}
\item{q}{
  quantile(s) for mid-quantile approximation, by default at the data indices
}
\item{type}{
  algorithm determining segments approximating x, an integer from 0 to 4 as
  described in Details
}
\item{feps}{
  tolerance for matching values, per \code{find.runs}
}
\item{feat}{
  a \opt{Dipeak}, \opt{Diflat}, or \opt{Dicpt} data frame
}
\item{offset}{
  an integer, the amount to shift position of peaks or points or endpoints
  of flats
}
\item{xmid}{
  a vector of interpolated quantiles to convert indices back to raw data,
  as stored in the \opt{Didata} 
}
\item{midoff}{
  an integer, the amount to shift positions in addition to offset
}
}

\details{
The \code{midquantile} function approximates the quantile function by
replacing the steps of the \code{ecdf} distribution with piecewise linear
segments; see Ma, Genton, and Parzen (2011).  This creates a ramp over tied
or discrete values, giving a better estimate of the position of features,
especially when there are large gaps between modes and few or no data points
within them.  The function determines the segment endpoints and by default
evaluates them on the original data grid, scaling the vector indices to run
from 0 through 1.  It first converts the data to runs using \code{find.runs},
with \code{feps} defining ties.  Segmentation type 1 is the mid-distribution
function of Ma, with the data value at the ends of runs shifted to the middle
of the change.  Segmentation type 2 instead shifts the quantiles by half an
index, extending the step in the \code{ecdf}.  These two approaches can
create an envelope around the quantile function, with the type 1 offset from
the data at q = 0 and the type 2 at q = 1.  Segmentation type 3 combines both
shifts, interpolating on a half grid for both x and q.  It follows the
quantile function better, but does round off the curve at single data points.
In practice types 1 and 3 are close. Type 4 runs segments between the middle
of runs, or through the data points when there are none.  This reduces its
estimation error, but the strategy does assume that the step in data to
either side of the run is about the same.  If not, the other approximations
would move away from the center of the run.

Type 4 is best when the data has very few ties.  Use types 3 or 1 when there
are.  Type 0 will automatically select the strategy, using 3 when there are
ties and 4 when not.  It uses a simple check, whether the number of unique
values is a tenth of the data, to decide if there are enough ties.

Internally the function makes two calls C-side.
\code{.Call("C_midq", x, type, feps, PACKAGE="Dimodal")} returns a vector
with the piecewise linear segments, with \code{$x} the endpoints along the
data and \code{$q} along the quantiles.
\code{.Call("C_eval_midq", pts$x, pts$q, q, PACKAGE="Dimodal")} uses these
segments as the first two arguments and new quantiles as the third to
interpolate data values.

The \code{find.runs} returned value has two vectors with the length of the
data.  One has non-zero values at the start of runs, the other counts skipped
invalid points.  The \opt{rle} class is more compact, storing only the runs
and the data values at the start.  The \code{runs.as.rle} function does this
compaction.

\code{find.peaks} returns a data frame with not only maxima but also the
minima between them.  It includes maxima even if they are at the first or last
point, with minima to only one side.  \code{select.peaks} selects only
those peaks surrounded by minima.  It may return a \opt{Dipeak} object with
no rows.  \code{pk} need not include the modifications from \code{Dimodal};
select.peaks keeps all columns of its argument.

Indexing in interval spacing is at the end of the interval but the low-pass
filter is centered.  \code{center.diw} shifts the interval spacing features
to align with the data, including peak positions, flat source identifiers,
and flat start and end points.  Note that the raw value is already shifted
when set by Dimodal and will not change.

\code{match.features} aligns peaks and flats between the low-pass and
interval spacing.  It compares only valid maxima, as per \code{find.peaks},
and shifts interval spacing positions with \code{center.diw} before matching
them.  Peaks must lie within \code{near} points to match.  Flats must overlap,
and the common segment must be at least \code{foverlap} of the length of
either flat.  The function prints the position, raw value, and the number of
tests that have passed their acceptance level, unless \code{quiet} is TRUE.
The \code{nomatch} value is cast internally to an integer and cannot be
between 1 and the number of features in either spacing, to prevent conflicts.
NA, 0, and negative values are acceptable.

The \code{shiftID.place} function is used in Dimodal to modify the
placement of features, and is provided separately if the detectors
\code{find.peaks}, \code{find.flats}, or \code{find.cpt} are called
directly.  It adds \code{offset} to the columns \opt{pos}, \opt{stID},
\opt{endID}, \opt{lminID}, \opt{rminID}, \opt{lsuppID}, and \opt{rsuppID}
if they exist in the features data frame to account for values skipped
during filtering.  Use the \opt{lp.stID} attribute for low-pass features,
\opt{diw.stID} for interval spacing, and 2 for changepoints.  If pos, stID,
or endID are in the data frame the function also adds columns \opt{x},
\opt{xst}, and \opt{xend} respectively with the original data value for
the index by using the midquantile result \code{xmid}.  Here the index is
further modified by \code{midoff}; use 0 for low-pass features and
changepoints, and half the interval width, stored as attribute \opt{wdiw}.
}

\value{
\code{midquantile} returns a vector the same length as the data.  Quantiles
outside the range [0,1] return the first or last data point, even if this is
discontinuous with the values at 0 or 1.  In other words, the function does
not follow the piecewise linear segment outside the valid range, but clips
it.  NA or NaN quantiles propagate.

\code{runs.as.rle} returns a list of class \opt{rle} with members
\opt{lengths} and \opt{values}, as per the \code{rle} command.  It also
adds a member \opt{nskip} with the number of non-finite values in the data
within the run.

\code{select.peaks} returns a subset of the argument, possibly with zero
rows.  If the argument is not a \opt{Dipeak} object, it returns a dummy
empty object.

\code{center.diw} returns its argument with modified \code{diw.peaks} and
\code{diw.flats}, if they exist.

\code{match.features} returns a list with four elements.  \opt{peak.lp2diw}
is a vector with one element per row in lp.peaks whose value is the matching
row number in diw.peaks, or \code{nomatch} if there is no match or the
lp.peaks row is not a valid peak.  \opt{peak.diw2lp} is a similar map
from \code{diw.peaks} to \code{lp.peaks}.  \opt{flat.lp2diw} and
\opt{flat.diw2lp} are the equivalent maps for flats.

\code{shiftID.place} returns the modified feat data frame.
}

\references{
Y. Ma, M. Genton, E. Parzen (2011),
Asymptotic properties of sample quantiles of discrete distributions.
\emph{Ann Inst Stat Math} 63, pp. 227--243.
}

\seealso{
 \code{\link{Dimodal}},
 \code{\link{Didata}},
 \code{\link{find.runs}},
 \code{\link{rle}},
 \code{\link{find.peaks}},
 \code{\link{Dipeak}},
 \code{\link{find.flats}},
 \code{\link{Diflat}},
 \code{\link{find.cpt}},
 \code{\link{Dicpt}},
 \code{\link{ecdf}}
}

\examples{

m <- Dimodal(faithful$eruptions, Diopt.local(analysis=c('lp','diw')))
# How many peaks were found?  Use print.data.frame to see the full structure.
nrow(select.peaks(m$lp.peaks))
nrow(select.peaks(m$diw.peaks))
# Compare to m$diw.peaks.
m$diw.peaks
center.diw(m)$diw.peaks
# Flats do not match because the Diw feature only covers 50% of the LP.
match.features(m)

plot(sort(iris$Petal.Length))
lines(midquantile(iris$Petal.Length, type=1L), col='red')
lines(midquantile(iris$Petal.Length, type=2L), col='blue')
lines(midquantile(iris$Petal.Length, type=3L), col='green')
lines(midquantile(iris$Petal.Length, type=4L), col='orange')

# See the Dimodal.R source code for the use of shiftID.place.

# To simplify the runs in the signed difference of the interval spacing
# runs.as.rle(Dimodal:::find.runs(m$data['signed',], 0.01), m$data['signed',]) 
}

\keyword{Dimodal}
\keyword{runs}
\keyword{peaks}
\keyword{flats}
\keyword{interval spacing}

