
\newcommand{\opt}{\ifelse{latex}{\code{"#1"}}{\verb{"#1"}}}
\newcommand{\nl}{\ifelse{latex}{ }{\ifelse{html}{ }{ \cr}}}

\name{find.peaks}
\alias{find.peaks}
\title{
Local minima and maxima detector.
}
\description{
Locate local minima and maxima within data, removing small peaks or shallow
minima.
}

\usage{
find.peaks(x, fht, frelht, fhtie, fhsupp)
}

\arguments{
\item{x}{
  a vector of real or integer values
}
\item{fht}{
  minimum absolute peak height as fraction (0 - 1 excl.) of data range
}
\item{frelht}{
  minimum relative peak height as fraction (0 - 1 excl.) of average value
}
\item{fhtie}{
  relative fraction (0 - 1 excl.) of values to consider as same
}
\item{fhsupp}{
  fraction (0 - 1 incl.) of max-min heights for support
}
}

\details{
The detector begins by using \code{find.runs} to identify sequences of nearly
equal values, in case there are flat tops at peaks or in valleys at minima.
It treats runs as a single point when marking all triples as local minima
(x[i] > x[j] < x[k]) or maxima (x[i] < x[j] > x[k]).  It determines the
smallest difference between a maximum and its adjacent minima, both as an
absolute value and as a fraction of their average,
\code{|x[j] - x[i or k]| / ((|x[j]| + |x[i or k]|)/2)}, for \code{i} and
\code{k} the minima to the left and right of the maximum \code{j}.  Working
from the smallest absolute difference to the largest, the detector checks if
either the absolute height is below the fraction \code{fht} of the data's
range or if the relative height is below \code{frelht}.  If so, the peak is
merged into its neighbor by deleting it and the minimum and re-calculating
the height of the neighbor.

The first and last data point are always extrema, a maximum if they are larger
than their neighbors.

We may want to limit the extent of a peak between two minima, for example
to avoid one that lies in the middle of a long flat and that makes the peak
appear much wider than it is.  Let the support of a peak be the points whose
value drops less from the peak than the fraction \code{fhsupp} of the
feature's height.  If 1.0, the support extends to the minima.  If 0 the
support is only the local maximum.  The Full Width at Half Maximum (FWHM)
measure would use set fhsupp=0.5.  a larger value, for example 0.9, will back
away from a minimum in a flat without distorting the feature for testing.

Although the order of checking and merging peaks can affect the final set,
and the process is deterministic, we do not document what this order is.
If there are multiple local extrema with the same value, we do not guarantee
which will be selected, although the result will be stable over repeated calls.

The arguments correspond to \code{Diopt} options \opt{peak.fht},
\opt{peak.frelht}, \opt{peak.fhtie}, and \opt{peak.fhsupp}.  The caller
provides them; this function does not access \code{Diopt} directly.

This function is not exported from the Dimodal package, but may be useful on
its own for any signal.  In Dimodal it is followed by a call to
\code{shiftID.place} to move indices to the original data grid.

The detector is O(\code{n log n}) in time and O(\code{n}) in memory.
}

\value{
find.peaks returns a \code{"Dipeak"} object.  If the data is bad
(constant or has two or fewer points or is all NA) then the result will be
empty and have zero rows.
}

\seealso{
 \code{\link{find.runs}},
 \code{\link{Dipeak}},
 \code{\link{Diopt}},
 \code{\link{shiftID.place}}
}

\keyword{Dimodal}
\keyword{peak detector}

