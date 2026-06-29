\name{statMark}
\alias{statMark}
\title{
Individual marker and interval statistics for an R/qtl cross object
}
\description{
Individual marker and interval statistics for an R/qtl cross object
}
\usage{
statMark(cross, chr, stat.type = c("marker","interval"),
         map.function = "kosambi")
}
\arguments{
  \item{cross}{
An \pkg{qtl} \code{cross} object with class structure \code{"bc"},
  \code{"dh"}, \code{"riself"}, \code{"bcsft"}. (see \code{?mstmap.cross} for more details.)
}
\item{chr}{
Character vector of linkage group names used for subsetting the
linkage map.
}
\item{stat.type}{
Character string of either \code{"marker"} or \code{"interval"} or both. \code{"marker"}
produces individual marker related statistics and \code{"interval"} produces
interval related statistics for the current map order (see Details).
}
\item{map.function}{
Character string of either \code{"koasmbi"}, \code{"haldane"},
\code{"morgan"} or \code{"cf"} defining the map
function to be used for interval related statistics.
}
}
\details{
If \code{"marker"} is chosen then a call to \code{geno.table} from
\pkg{qtl} is used to return individual marker statistics for segregation distortion,
as well as allele and missing value proportions. For the current map
order the number of double crossovers at each marker are also returned.

If \code{"interval"} is chosen then interval statistics are returned for
the current map order. These include the estimated recombination
fraction and LOD score between adjacent markers, calculated from
\code{est.rf} in \pkg{qtl}. Also returned are the map interval distances and
converted map recombination fractions extracted from the \code{"map"}
component of each linkage group as well as the actual number of
recombinations between markers.

This function is used in \code{profileMark} to plot any combination of
returned linkage map statistics on a single graphical display.
}
\value{
A list named by the \code{stat.type} used in the call. Each element is a
data frame of statistics with columns named by the statistic.
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

## produce all statistics

sm <- statMark(mapDH, stat.type = c("marker","interval"))

}
\keyword{misc}
