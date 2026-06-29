\name{mstmap.data.frame}
\alias{mstmap.data.frame}
\title{
Extremely fast linkage map construction for data frame objects using MSTmap.
}
\description{
Extremely fast linkage map construction for data frame objects utilizing the
source code for MSTmap (see Wu et al., 2008). The construction includes
linkage group clustering, marker ordering and genetic distance calculations.
}
\usage{
\method{mstmap}{data.frame}(object, pop.type = "DH", dist.fun = "kosambi",
      objective.fun = "COUNT", p.value = 1e-06, noMap.dist = 15,
      noMap.size = 0, miss.thresh = 1, mvest.bc = FALSE,
      detectBadData = FALSE, as.cross = TRUE, return.imputed = FALSE,
      trace = FALSE, \ldots)
}
\arguments{
  \item{object}{
A \code{"data.frame"} object containing marker information. The
data.frame must explicitly be arranged with markers in rows and
genotypes in columns. Marker names are obtained from the \code{rownames} of the
\code{object} and genotype names are obtained from the \code{names}
component of the \code{object} (see Details).
}
\item{pop.type}{
Character string specifying the population type of the data frame
\code{object}. Accepted values are \code{"DH"} (doubled haploid),
\code{"BC"} (backcross), \code{"RILn"} (non-advanced RIL population with
n generations of selfing) and \code{"ARIL"} (advanced RIL) (see
Details). Default is \code{"DH"}.
}
  \item{dist.fun}{
Character string defining the distance function used for calculation of
genetic distances. Options are \code{"kosambi"} and \code{"haldane"}.
Default is \code{"kosambi"}.
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
markers will appear in their own linkage groups ad will be of size
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
before clustering the markers into linkage groups.  This is restricted
to \code{"BC","DH","ARIL"} populations only (see Details).
}
\item{detectBadData}{
Logical value. If \code{TRUE} possible genotyping errors are detected,
set to missing and then imputed as part of the
marker ordering algorithm. Genotyping errors will also be printed in the
file specified by \code{trace}.  This is restricted
to \code{"BC","DH","ARIL"} populations only. (see Details). Default is \code{FALSE}.
}
\item{as.cross}{
Logical value. If \code{TRUE} the constructed linkage map is returned as
a \pkg{qtl} cross object (see Details). If \code{FALSE} then the constructed
linkage map is returned as a \code{data.frame} with extra columns
indicating the linkage group, marker name/position and genetic distance.
Default is \code{TRUE}.
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
piped to "\code{MSToutput.txt}". This file is equivalent to the output that
would be obtained from running the MSTmap executable from the command line.
}
\item{\ldots}{
Currently ignored.
}
}
\details{
The data frame \code{object} must have an explicit format with markers
in rows and genotypes in columns. The marker names are required to be in
the \code{rownames} component and the genotype names are
required to be in the \code{names} component of the \code{object}. In
each set of names there must be no spaces. If spaces are detected they
are exchanged for a "-". Each of the columns of the data frame must be of class
\code{"character"} (not factors). If converting from a matrix, this can
easily be achieved by using the \code{stringAsFactors = FALSE} argument
for any \code{data.frame} method.

It is important to know what population type the data frame
\code{object} is and to correctly input this into \code{pop.type}. If
\code{pop.type = "ARIL"} then it is assumed that the minimal number of heterozygotes have been
set to missing before proceeding. The advanced RIL population is then
treated like a backcross population for the purpose of linkage map
construction. Genetic distances are adjusted post construction.
For non-advanced RIL populations \code{pop.type =
  "RILn"}, the number of generations of selfing is limited to 20 to
ensure sensible input.

The content of the markers in \code{object} can either be all numeric
(see below) or all character. If markers are of type character then
the following allelic content must be explicitly adhered to. For \code{pop.type} \code{"BC"},
\code{"DH"} or \code{"ARIL"} the two allele types should
be represented as (\code{"A"} or \code{"a"}) and (\code{"B"} or
\code{"b"}). For non-advanced RIL populations (\code{pop.type = "RILn"})
phase unknown heterozygotes should be represented as
\code{"X"}. For all populations, missing marker scores should be represented
as (\code{"U"} or \code{"-"}).

This function also extends the functionality of the MSTmap
algorithm by allowing users to input a complete numeric data frame of
marker probabilities for \code{pop.type} \code{"BC"}, \code{"DH"} or
\code{"ARIL"}. The values must be inclusively between 1 (A) and 0 (B) and be
representative of the probability that the A allele is present. No
missing values are allowed.

The algorithm allows an adjustment of the \code{p.value} threshold for
clustering of markers to distinct linkage groups (see Wu et al.,
2008) and is highly dependent on the number of individuals in
the population. As the number of individuals increases the
\code{p.value} threshold should be decreased accordingly. This may
require some trial and error to achieve desired results.

If \code{mvest.bc = TRUE} and the population type is \code{"BC","DH","ARIL"}
then missing values are imputed before markers are clustered into
linkage groups. This is only a simple imputation that places a 0.5
probability of the missing observation being one allele or the other and
is used to assist the clustering algorithm when there is known to be high numbers of
missing observations between pairs of markers.

It should be highlighted that for population types
\code{"BC","DH","ARIL"}, imputation of missing values occurs
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

For populations \code{"BC","DH","ARIL"}, if \code{detectBadData = TRUE},
the marker ordering algorithm also
includes the detection of genotyping errors. For any individual
genotype, the detection method is based on a weighted Euclidean metric
(see Wu et al., 2008) that is a function of the
recombination probabilities of all the markers with the marker containing
the suspicious observation. Any genotyping errors detected are set to
missing and the missing values are then imputed if \code{mv.est =
  TRUE}.  Note, the detection of these errors and their
amendment is returned in the imputed probability matrix if
\code{return.imputed = TRUE}

If \code{as.cross = TRUE} then the constructed object is returned as a
\pkg{qtl} cross object with the appropriate class structure. For \code{"RILn"}
populations the constructed object is given the class \code{"bcsft"} by
using the \pkg{qtl} package conversion function \code{convert2bcsft}
with arguments \code{F.gen = n} and \code{BC.gen =
  0}. For \code{"ARIL"} populations the constructed object is given the
class \code{"riself"}.

If \code{return.imputed = TRUE} and \code{pop.type} is one of
\code{"BC","DH","ARIL"}, then the marker probability matrix is
returned for the linkage groups that have been constructed using the
algorithm. Each linkage group is named identically to the linkage groups
of the map and, if \code{as.cross = TRUE}, contains an ordered
\code{"map"} element and a \code{"data"}
element consisting of marker probabilities of the A allele being present
(i.e. P(A) = 1, P(B) = 0). Both elements contain a
possibly reduced version of the marker set that includes all
non-colocating markers as well as the first marker of any set of
co-locating markers. If \code{as.cross = FALSE} then an ordered data frame of matrix
probabilities is returned.
}
\value{
  If \code{as.cross = TRUE} the function returns an R/qtl cross object with the appropriate
  class structure. The object is a list with usual components
  \code{"pheno"} and \code{"geno"}. If \code{as.cross = FALSE} the
  function returns an ordered data frame object
  with additional columns that indicate the linkage group, the position
  and marker names and genetic distance of the markers within in each
  linkage group. If markers were omitted for any reason during the
  construction, the object will have an \code{"omit"} component with
  all omitted markers in a collated matrix.  If \code{return.imputed =
    TRUE} then the object will also contain an \code{"imputed.geno"} element.
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
  \code{\link{mstmap.cross}}
}
\examples{

data(mapDH, package = "ASMap")

## forming data frame object from R/qtl object

dfg <- t(do.call("cbind", lapply(mapDH$geno, function(el) el$data)))
dimnames(dfg)[[2]] <- as.character(mapDH$pheno[["Genotype"]])
dfg <- dfg[sample(1:nrow(dfg), nrow(dfg), replace = FALSE),]
dfg[dfg == 1] <- "A"
dfg[dfg == 2] <- "B"
dfg[is.na(dfg)] <- "U"
dfg <- cbind.data.frame(dfg, stringsAsFactors = FALSE)

## construct map

testd <- mstmap(dfg, dist.fun = "kosambi", trace = FALSE)
pull.map(testd)

## let's get a timing on that ...

system.time(testd <- mstmap(dfg, dist.fun = "kosambi", trace = FALSE))

}
\keyword{misc}
