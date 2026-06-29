\name{mstmap.cross}
\alias{mstmap.cross}
\alias{mstmap}
\title{
Extremely fast linkage map construction for \pkg{qtl} objects using MSTmap.
}
\description{
Extremely fast linkage map construction for \pkg{qtl} objects using the
source code for MSTmap (see Wu et al., 2008). The construction includes
linkage group clustering, marker ordering and genetic distance calculations.
}
\usage{
\method{mstmap}{cross}(object, chr, id = "Genotype", bychr = TRUE,
       suffix = "numeric", anchor = FALSE, dist.fun = "kosambi",
       objective.fun = "COUNT", p.value = 1e-06, noMap.dist = 15,
       noMap.size = 0, miss.thresh = 1, mvest.bc = FALSE,
       detectBadData = FALSE, return.imputed = FALSE,
       trace = FALSE, \ldots)
}
\arguments{
  \item{object}{
A \code{"cross"} object generated from the \pkg{qtl} package. Specifically
the object needs to inherit from one of the following classes
\code{"bc"}, \code{"dh"}, \code{"riself"}, \code{"bcsft"} (see Details).
}
  \item{chr}{
A character string of linkage group names that require re-construction
and/or optimal ordering of the markers they contain.
(see Details).
}
  \item{id}{
The name of the column in \code{object$pheno} that uniquely identifies the
genotype names. Default is \code{"Genotype"}.
}
\item{bychr}{
Logical value. For a given set of linkage groups defined by \code{chr},
if \code{TRUE} then split linkage groups (only if required, see
\code{p.value}) and order markers within linkage groups. If \code{FALSE} then combine
linkage groups and reconstruct. Default is \code{TRUE}.
}
\item{suffix}{
Character string either \code{"numeric"} or \code{"alpha"} determining
whether numeric or alphabetic ascending values are post-fixed to linkage
group names when splitting linkage groups.
}
 \item{anchor}{
Logical value. The MSTmap algorithm does not respect the inputted marker
order of the linkage map required for construction. For a given set of
linkage groups defined by \code{chr}, if \code{TRUE} the order of the
inputted markers is respected regardless of the choices of \code{chr}
and \code{bychr}. Default is \code{FALSE}.
}
\item{dist.fun}{
Character string defining the distance function used for calculation of
genetic distances. Options are "kosambi" and "haldane". Default is "kosambi".
}
  \item{objective.fun}{
Character string defining the objective function to be used when
constructing the map. Options are \code{"COUNT"} for minimising the sum of
recombination events between markers and \code{"ML"} for maximising the
likelihood objective function. Default is \code{"COUNT"}.
}
  \item{p.value}{
Numerical value to specify the threshold to use when clustering
markers. Defaults to \code{1e-06}. If a value greater than one
is given this feature is turned off inputted marker data are assumed to
belong to the same linkage group (see Details).
}
  \item{noMap.dist}{
Numerical value to specify the smallest genetic distance a set of
isolated markers can appear distinct from other linked markers. Isolated
markers will appear in their own linkage groups and will be of size
specified by \code{noMap.size}.
}
 \item{noMap.size}{
Numerical value to specify the maximum size of isolated marker linkage
groups that have been identified using \code{noMap.dist}. This feature
can be turned off by setting it to 0. Default is 0.
}
\item{miss.thresh}{
Numerical value to specify the threshold proportion of missing marker
scores allowable in each of the markers. Markers above this threshold
will not be included in the linkage map. Default is 1.
}
\item{mvest.bc}{
Logical value. If \code{TRUE} missing markers will be imputed
before clustering the markers into linkage groups. This is restricted
to \code{"bc","dh","riself"} populations only (see Details). Default is \code{FALSE}.
}
\item{detectBadData}{
Logical value. If \code{TRUE} possible genotyping errors are detected,
set to missing and then imputed as part of the
marker ordering algorithm. Genotyping errors will also be printed in the
file specified by \code{trace}.  This is restricted
to \code{"bc","dh","riself"} populations only. (see Details). Default is \code{FALSE}.
}
\item{return.imputed}{
Logical value. If \code{TRUE} then the imputed marker probability matrix is
returned for the linkage groups that are constructed (see
Details). Default is \code{FALSE}.
}
  \item{trace}{
An automatic tracing facility. If \code{trace = FALSE} then
minimal \code{MSTmap} output is piped to the screen during the algorithm.
If \code{trace = TRUE}, then detailed output from MSTmap is
piped to \code{"MSToutput.txt"}. This file is equivalent to the output that
would be obtained from running the MSTmap executable from the command line.
}
\item{\ldots}{
Currently ignored.
}
}
\details{
The \pkg{qtl} cross object needs to inherit one of the allowable classes
\code{"bc","dh","riself", "bcsft"}. This provides a safeguard against
attempts to construct a map for more complex populations that can
exist in \pkg{qtl}. Users should be aware when doubled haploid
populations are read in using \code{read.cross()} from the \pkg{qtl}
package they inherit the class \code{"bc"}. Users can apply the class
\code{"dh"} by simply changing the class of the object. For the purpose
of linkage map construction the classes \code{"bc"} and \code{"dh"} will
provide equivalent results.

MSTmap supports \code{"RILn"} populations, where n is the number of generations
of selfing. Markers in these populations are required to be fully
informative i.e. contain 3 distinct allele types such as AA, BB for
parental homozygotes and AB for phase unknown heterozygotes.
If \code{read.cross} is used to import the \code{"RILn"} population the resultant
object will initially be given a class \code{"f2"}. The level of selfing
would then have to be encoded into the object by applying one of the two conversion
functions available in the \pkg{qtl} package. For a
population that has been generated by selfing n times the conversion
function \code{convert2bcsft} can be used by setting the arguments
\code{F.gen = n} and \code{BC.gen = 0}. Populations that are genuine
advanced RILs can be converted using the \code{convert2riself} function.

This method function is designed to be an "all-in-one" function that
will allow you to construct linkage maps extremely fast in multiple
different ways from the supplied cross \code{object}. Initially, the map
can be kept complete or a subset of selected linkage groups can be chosen
using the \code{chr} argument. Setting \code{bychr = FALSE} will
bulk the marker information for the selected linkage groups and, if
necessary, form new linkage groups and optimise the marker order within
each. Setting \code{bychr = TRUE} will ensure that markers
are optimally ordered within each linkage group. This will also break
linkage groups depending on the p-value given in the call (see
below for details of the use of \code{p.value}). If the
linkage map was initially subsetted, the linkage groups not involved in
the subset are returned to ensure the map is complete.

The algorithm allows an adjustment of the \code{p.value} threshold for
clustering of markers to distinct linkage groups (see Wu et al.,
2008) and is highly dependent on the number of individuals in
the population. As the number of individuals increases the
\code{p.value} threshold should be decreased accordingly. This may
require some trial and error to achieve desired results.
When \code{bychr = TRUE}, established linkage groups may also split
depending on the \code{p.value} given. To prevent this the \code{p.value} threshold
may be increased to a desired value or the splitting may be prevented
altogether by supplying a value greater than one to this argument.

If \code{mvest.bc = TRUE} and the population type is \code{"bc","dh","riself"}
then missing values are imputed before markers are clustered into
linkage groups. This is only a simple imputation that places a 0.5
probability of the missing observation being one allele or the other and
is used to assist the clustering algorithm when there is known to be high numbers of
missing observations between pairs of markers.

It should be highlighted that for population types
\code{"bc","dh","riself"}, imputation of missing values occurs
regardless of the value of \code{mvest.bc}. This is achieved using an EM algorithm that is
tightly coupled with marker ordering (see Wu et al., 2008). Initially
a marker order is obtained omitting missing marker scores and then
imputation is performed based on the underlying recombinant probabilities
of the flanking markers with the markers containing the missing
value. The recombinant probabilities are then recomputed and an update of
the pairwise distances are calculated. The ordering algorithm is then
run again and the complete process is repeated until
convergence. Note, the imputed probability matrix for the linkage map
being constructed is returned if \code{return.imputed = TRUE}.

For populations \code{"bc","dh","riself"}, if \code{detectBadData =
  TRUE} the marker ordering algorithm also
includes the detection of genotyping errors. For any individual
genotype, the detection method is based on a weighted Euclidean metric
(see Wu et al., 2008) that is a function of the
recombination probabilities of all the markers with the marker containing
the suspicious observation. Any genotyping errors detected are set to
missing and the missing values are then imputed as part of the marker
ordering algorithm. Note, the detection of these errors and their
amendment can be returned in the imputed probability matrix if
\code{return.imputed = TRUE}.

If \code{return.imputed = TRUE} and the object has class
\code{"bc","dh","riself"} then the marker probability matrix is
returned for the linkage groups that have been constructed using the
algorithm. Each linkage group is named identically to the linkage groups
of the map and contains an ordered \code{"map"} element and a \code{"data"}
element consisting of marker probabilities of the A allele being
present (i.e. P(A) = 1, P(B) = 0). Both elements contain a
possibly reduced version of the marker set that includes all
non-colocating markers as well as the first marker of any set of
co-locating markers.
}
\value{The function returns a cross object with an identical class
  structure to the cross \code{object} inputted. The object is a list
  with usual components \code{"pheno"} and \code{"geno"}. If markers were
  omitted for any reason during the construction, the object will have an
  \code{"omit"} component with all omitted markers in a collated
  matrix. If \code{return.imputed = TRUE} then the object will also
  contain an \code{"imputed.geno"} element.
}
\references{
Wu, Y., Bhat, P., Close, T.J, Lonardi, S. (2008) Efficient and Accurate
Construction of Genetic Linkage Maps from Minimum Spanning Tree of a
Graph. Plos Genetics, \bold{4}, Issue 10.

Taylor, J., Butler, D. (2017) R Package ASMap: Efficient Genetic
Linkage Map Construction and Diagnosis. Journal of Statistical Software,
\bold{79}(6), 1--29.
}
\author{
Julian Taylor, Dave Butler, Timothy Close, Yonghui Wu, Stefano Lonardi
}
\seealso{
\code{\link{mstmap.data.frame}} and \code{\link{breakCross}}
}
\examples{

data(mapDH, package = "ASMap")

## bulking linkage groups and reconstructing entire linkage map

test1 <- mstmap(mapDH, bychr = FALSE, dist.fun = "kosambi",
                trace = FALSE)
pull.map(test1)

## one linkage group at a time (possibly break established linkage
## groups)

test2 <- mstmap(mapDH, bychr = TRUE, dist.fun = "kosambi", trace = FALSE)
pull.map(test2)

## one linkage group at a time (do not break established linkage groups)

test3 <- mstmap(mapDH, bychr = TRUE, dist.fun = "kosambi", p.value = 2,
                trace = FALSE)
pull.map(test3)

## impute before clustering and detect genotyping errors, pipe output to
## file

test4 <- mstmap(mapDH, bychr = FALSE, dist.fun = "kosambi",
                trace = TRUE, mvest.bc = TRUE, detectBadData = TRUE)
pull.map(test4)
unlink("MSToutput.txt")

}
\keyword{misc}
