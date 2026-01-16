
\newcommand{\opt}{\ifelse{latex}{\code{"#1"}}{\verb{"#1"}}}
\newcommand{\nl}{\ifelse{latex}{ }{\ifelse{html}{ }{ \cr}}}

\name{find.flats}
\alias{find.flats}
\title{
Local flat detector.
}
\description{
Locate flat sections, specified by the ripple parameter, within data.
}

\usage{
find.flats(x, fripple, minlen, fminlen, noutlier)
}

\arguments{
\item{x}{
  a vector of real or integer values
}
\item{fripple}{
  height of flat as fraction (0 -- 1 excl.) of data range
}
\item{minlen}{
  minimum length of flat, in data points (absolute)
}
\item{fminlen}{
  minimum length of flat, as fraction (0 -- 1 excl.) of data length (relative)
}
\item{noutlier}{
  number of points outside the ripple range allowed to either side of source
}
}

\details{
The detector scans every point in \code{x} to find the contiguous range of
indices that lie within the ripple centered at the point's value.  The
algorithm allows noutlier points to each side of the source point (so the
total can be twice this), but not as the first or last points.  In other
words, the endpoints of the flat will be within the ripple specification.
Flats must have a length of the larger of \code{minlen} or the relative
\code{fminlen} of \code{length(x)}, counting that portion not covered by a
longer flat.  Flats may therefore overlap, but only if the extension meets
the length requirement.  Flats are reported for their original extent and
not the exposed portion.

The algorithm will ignore non-finite values in \code{x}, but the endpoints
of the features apply to the original, unscreened data.

The arguments correspond to options \opt{flat.fripple}, \opt{flat.minlen},
\opt{flat.fminlen}, and \opt{flat.noutlier}.  These are provided when
called within Dimodal; this function does not access \code{Diopt} directly.

This function is not exported from the Dimodal package, but may be useful on
its own for any signal.  Within Dmodal it is followed by a call to
\code{shiftID.place} to move indices to the original data grid.

The detector switches its scan algorithms based on the data size.  The
base approach, which scans outward from each source point, is
O(\code{n Lflat}) in time and O(\code{n}) in memory, where Lflat is the
average length of all ranges.  This is usually a substantial fraction of the
data, a third to half, so the algorithm is essentially O(\code{n^2}).  For
larger data sets it switches to a segtree-based search that has a high
overhead.  The time complexity is not known, and the memory consumption is
still O(\code{n}).  The second approach runs faster with more than
10 thousand data points, and is an order of magnitude quicker for
100 thousand points.
}

\value{
find.flats returns a \code{"Diflat"} object.  If there are no flats in the
data then the result will be empty and have zero rows.
}

\seealso{
 \code{\link{Diflat}},
 \code{\link{Diopt}},
 \code{\link{shiftID.place}}
}

\keyword{Dimodal}
\keyword{flat detector}

