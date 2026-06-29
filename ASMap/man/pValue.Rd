\name{pValue}
\alias{pValue}
\title{
P-value graph
}
\description{
P-value graph to determine threshold for marker clustering
}
\usage{
pValue(dist = seq(25,40, by = 5), pop.size = 100:500,
       map.function = "kosambi", LOD = FALSE)
}
\arguments{
\item{dist}{
Numeric range of genetic distances in cM.
}
\item{pop.size}{
Numeric range of population sizes.
}
\item{map.function}{
Character string of either \code{"koasmbi"}, \code{"haldane"},
\code{"morgan"} or \code{"cf"} defining the mapping function to be used.
}
\item{LOD}{
If \code{LOD = TRUE} the LOD score of linkage is calculated
or if \code{LOD = FALSE} then the minus log10 p-value used to threshold the
hoeffding inequality is calculated (defaults to \code{FALSE}).
}
}
\details{
This function provides the ability to create a user specified p-value
plot similar to Figure 1.1 in the vignette for the package.
}
\value{
A plot is displayed showing minus log10 pvalue (or LOD score) of linkage vs the
range of specified population sizes for different specified genetic
distances.
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
\code{\link{mstmap.cross}} and \code{\link{mstmap.data.frame}}
}
\examples{

pValue(dist = seq(25, 40, by = 2))
}
\keyword{misc}
