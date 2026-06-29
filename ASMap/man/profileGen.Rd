\name{profileGen}
\alias{profileGen}
\title{
Profile individual genotype statistics for an R/qtl cross object
}
\description{
Profile individual genotype statistics for the current linkage map
order of and R/qtl cross object
}
\usage{
profileGen(cross, chr, bychr = TRUE, stat.type = c("xo", "dxo",
           "miss"), id = "Genotype", xo.lambda = NULL, ...)
}
\arguments{
  \item{cross}{
An \pkg{qtl} \code{cross} object with class structure \code{"bc",
  "dh", "riself", "bcsft"}. (see \code{?mstmap.cross} for more details.)
}
\item{chr}{
Character vector of linkage group names used for subsetting the
linkage map.
}
\item{bychr}{
Logical vector determining whether statistics should be plotted by
chromosome (see Details).
}
\item{stat.type}{
Character string of any combination of \code{"xo"} or \code{"dxo"} or
\code{"miss"}. \code{"xo"} calculates the number of crossovers,
\code{"dxo"} calculates the number of double crossover and \code{"miss"}
calculates the number of missing values.
}
\item{id}{
Character string determining the column of \code{cross$pheno} that
contains the genotype names.
}
\item{xo.lambda}{
A numerical value for the expected rate of recombination. (see Details).
}
\item{\ldots}{
Other arguments to be passed to the high level lattice plot.
}
}
\details{
This function uses \code{statGen} to profile statistics for the
genotypes for the current order of the linkage map. Any combination of
\code{"xo"} or \code{"dxo"} or \code{"miss"} may be given to
simultaneous plot. If \code{bychr = TRUE} then the plots will be further partitioned by
linkage groups given by \code{chr}.

If a numerical value is given for \code{xo.lambda} then the
recombination count for each genotype is tested against the expected
recombination rate \code{xo.lambda} using a simple one-tailed test of a
Poisson mean. Any lines that have a p-value less than than a family wise
error rate based on bonferroni adjustment of the usual alpha level of 0.05 are
annotated on the profiles being plotted.
}
\value{
A lattice panel plot with panels described by the \code{stat.type} given
in the call and genotype statistics are returned invisibly. If
\code{xo.lambda} is not NULL then these statistics also include a
logical vector named \code{"xo.lambda"} that is returned from testing
the individuals for inflated recombination rates (see Details).
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
\code{\link{statGen}}
}
\examples{

data(mapDH, package = "ASMap")

## profile all genotype crossover and double crossover statistics

profileGen(mapDH, bychr = FALSE, stat.type = c("xo","dxo"),
     xo.lambda = 25, layout = c(1,3))

}
\keyword{misc}
