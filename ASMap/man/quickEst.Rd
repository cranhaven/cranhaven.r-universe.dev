\name{quickEst}
\alias{quickEst}
\title{
Very quick estimation of genetic map distances.
}
\description{
Very quick estimation of genetic map distances for a constructed R/qtl object
}
\usage{
quickEst(object, chr, map.function = "kosambi", ...)
}
\arguments{
  \item{object}{
An R/qtl \code{object} object with any class structure.
}
\item{chr}{
A character string of linkage group names that require (re)estimation of
their genetic map distances.
}
\item{map.function}{
Character string of either \code{"koasmbi"}, \code{"haldane"},
\code{"morgan"} or \code{"cf"} defining the mapping
function to be used.
}
\item{\ldots}{
Other arguments passed to \code{argmax.geno}.
}
}
\details{
For linkage groups with large numbers of markers, the Hidden Markov algorithm in \code{est.map}
can be extremely slow. The computational burden for this algorithm
increases as the number of missing values and genotyping errors
increase. \code{quickEst} circumvents this by using the Viterbi
algorithm computationally implemented in \code{argmax.geno} of the
\pkg{qtl} package. Initial conservative estimates of the map distances
are calculated from inverting recombination fractions outputted from
\code{est.rf}. These are then passed to \code{argmax.geno} and
imputation of missing allele scores is performed along with
re-estimation of map distances.
}
\value{
The cross object is returned with identical class structure as the
inputted cross object.
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
\code{\link[qtl]{est.map}}
}
\examples{

data(mapDH, package = "ASMap")

mapDH1 <- quickEst(mapDH, map.function = "kosambi")

}
\keyword{misc}
