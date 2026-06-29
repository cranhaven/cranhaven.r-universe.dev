\name{profileMark}
\alias{profileMark}
\title{
Profile individual marker and interval statistics for an R/qtl cross object
}
\description{
Graphically profile individual marker and interval statistics for an R/qtl cross object
}
\usage{
profileMark(cross, chr, stat.type = "marker", use.dist = TRUE,
          map.function = "kosambi", crit.val = NULL,
          display.markers = FALSE, mark.line = FALSE, ...)
}
\arguments{
  \item{cross}{
An R/qtl \code{cross} object with class structure \code{"bc"},
  \code{"dh"}, \code{"riself"}, \code{"bcsft"}. (see \code{?mstmap.cross} for more details.)
}
\item{chr}{
Character vector of linkage group names used for subsetting the
linkage map.
}
\item{stat.type}{
Character string of either \code{"marker"} or \code{"interval"} or
both. Also this can be a set of character strings relating to individual
marker or interval statistics that want to be viewed simultaneously (see Details).
}
\item{use.dist}{
Logical value determining whether the actual map distances should be use
to represent marker positions. If \code{FALSE} then markers are placed
equidistant from each other.
}
\item{map.function}{
Character string of either \code{"koasmbi"}, \code{"haldane"},
\code{"morgan"} or \code{"cf"} defining the map
function to be used for interval related statistics.
}
\item{crit.val}{
The critical value to be used in displaying marker or intervals above a
certain threshold (see Details).
}
\item{display.markers}{
A logical value determining whether marker names should be displayed on
the bottom axis.
}
\item{mark.line}{
A logical value determining whether vertical lines should be drawn at
marker positions. This may be useful to line up marker positions across
several plots.
}
\item{\ldots}{
Other arguments to be passed to the high level lattice plot.
}
}
\details{
This graphical function calls the function \code{statMark} to retrieve
marker and interval statistics. If \code{"marker"} is given as the
\code{stat.type} then the complete set of marker statistics is plotted
simultaneously. If \code{"interval"} is given as the
\code{stat.type} then the function simultaneously plots the complete set
of interval statistics. Both can also be chosen.

This function also allows users to choose any combination of marker or interval
statistics they would like to view. The set of available marker
statistics that can be profiled are given below
\itemize{
  \item \code{"seg.dist"}: Profile the -log10 p-value.
  results from a test of segregation distortion for each marker.
  \item \code{"miss"}: Profile the proportion of missing values
 for each marker.
  \item \code{"prop"}: Profile the allele proportions for each
  marker.
  \item \code{"dxo"}: Profile the number of double crossovers
  occurring at each marker.
}

The set of available interval statistics that can be profiled are given below
\itemize{
  \item \code{"erf"}: Profile the recombination fractions for the
  intervals.
  \item \code{"lod"}: Profile the LOD score for the test of no linkage
  between markers in an interval.
  \item \code{"dist"}: Profile the interval map distance taken
  from the map component of each linkage group.
  \item \code{"mrf"}: Profile the map recombination fraction for
  the intervals.
  \item \code{"recomb"}: Profile the actual number of recombinations
  within each of the intervals.
}

If \code{crit.val="bonf"} and marker statistics are plotted then any
markers that have p-value for the test of segregation distortion less
than the family wise error rate based on a bonferroni adjustment of the
usual 0.05 alpha level, are annotated on each of the marker plots. If any interval statistics
are being plotted then any intervals that have a p-value for the test of
no linkage that is less than a bonferroni adjustment of the usual 0.05
alpha level are annotated on each of the interval statistics plots.
}
\value{
A lattice panel plot is displayed with panels described by the
\code{stat.type} given in the call and the complete marker/interval statistics
are returned invisibly. If \code{crit.val} is not NULL then both
the marker/interval statistics are returned with an extra logical column called
\code{"crit.val"} from testing markers for segregation distortion and
intervals for weak linkage (see Details).
}
\references{
Taylor, J., Butler, D. (2017) R Package ASMap: Efficient Genetic
Linkage Map Construction and Diagnosis. Journal of Statistical Software,
\bold{79}(6), 1--29.
}
\author{
Julian Taylor
}
\seealso{
\code{\link{profileMark}}
}
\examples{

data(mapDH, package = "ASMap")

## profile chosen statistics

profileMark(mapDH, stat.type = c("seg.dist","prop","erf"), layout =
      c(1,4), type = "l")

}
\keyword{misc}
