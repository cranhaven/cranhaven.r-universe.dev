\name{pp.init}
\alias{pp.init}
\title{
Parameter initialization function
}
\description{
Parameter initialization function for \code{pushCross} and \code{pullCross}
}
\usage{
pp.init(seg.thresh = 0.05, seg.ratio = NULL, miss.thresh = 0.1,
        max.rf = 0.25, min.lod = 3)
}
\arguments{
 \item{seg.thresh}{
Numerical value between zero and one determining the p-value threshold
for the test of marker segregation distortion.
}
\item{seg.ratio}{
A character string of the form "AA:BB" or "AA:AB:BB" describing the
ratio of the alleles.
}
\item{miss.thresh}{
Numerical value between zero and one determining the proportion of
missing values.
}
\item{max.rf}{
The maximum recombination fraction to consider when attempting to
cluster pushed markers back into linkage groups.
}
\item{min.lod}{
The minimum LOD score to consider when attempting to cluster pushed
markers back into linkage groups.
}
}
\details{
This parameter initialization function is used by the function \code{pullCross} to
pull markers from a linkage map and \code{pushCross} to push markers
back into a linkage map. How the arguments \code{seg.thresh},
\code{seg.ratio} and \code{miss.thresh} are used depends on which
function is called. See \code{pushCross} and \code{pullCross} for
more details.
}
\value{
Return user defined parameter values for each of the parameters.
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
\code{\link{pushCross}}; \code{\link{pullCross}}
}
\examples{

data(mapDH, package = "ASMap")

## pull markers from a linkage map with a segregation distortion

pars <- pp.init(seg.thresh = 0.05)
mapDH.s <- pullCross(mapDH, type = "seg.distortion", pars = pars)
mapDH.s$seg.distortion$table
}
\keyword{misc}
