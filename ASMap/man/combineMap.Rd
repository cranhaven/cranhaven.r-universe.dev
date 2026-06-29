\name{combineMap}
\alias{combineMap}
\title{
Combine linkage maps from multiple \pkg{qtl} cross objects
}
\description{
Combine map information, marker data and phenotype data from multiple \pkg{qtl} cross objects
}
\usage{
combineMap(..., id = "Genotype", keep.all = TRUE,
           merge.by = "genotype")
}
\arguments{
  \item{\ldots}{
An unlimited set of arguments with each argument defining an \pkg{qtl} cross
object. All \pkg{qtl} objects can have any class structure but it must be
identical across objects. (see Details for more information.)
}
\item{id}{
The name of the common column in the \code{pheno} element of each cross
object representing the genotype names. Default is \code{"Genotype"}.
}
\item{keep.all}{
A logical value determining whether all genotypes should be kept in the
final linkage map regardless of their absence in some linkage maps (see
Details). Default is \code{TRUE}.
}
\item{merge.by}{
A character string. If "genotype" then combining of maps occurs by
common genotypes and if "marker" combining of maps occurs by common
markers. Default is "genotype". (see Details for more information.)
}
}
\details{
This function combines linkage maps from multiple \pkg{qtl} cross
objects by merging marker data and map information as well as phenotypic
data if present. The function contains some initial checks before
proceeding with the combining. Firstly, all \pkg{qtl} cross objects must
have the same class structure and have a column in the \code{pheno}
element of the object named by the argument \code{id}. The symbol ";"
should be avoided in markers as this is reserved for string manipulation
within the function.

If \code{merge.by = "genotype"} then the combining occurs sequentially
across linkage maps based on common genotype names. If
\code{keep.all=TRUE} then the marker set and phenotypic data are
"padded out" when genotype names are not shared between maps. If
\code{keep.all=FALSE} then the marker set and phenotype data are shrunk
to only include genotypes that are shared among all linkage
maps. Marker names must be unique across the set of linkage
maps. Non-matching genotype names between linkage maps will expand the
final marker data and phenotypic data so it is prudent to check genotype
names are correct in each of the linkage maps before combining.

If \code{merge.by = "marker"} then the combining occurs
sequentially across linkage maps based on common markers. If
\code{keep.all=TRUE} then the marker set is
"padded out" when marker names are not shared between maps. If
\code{keep.all=FALSE} then the marker set is shrunk
to only include markers that are shared among all linkage
maps. Genotypes must be unique across the set of linkage maps. It should
be noted, this function does not use a consensus map
algorithm to determine chromosome identification and genetic distances
of common markers. These are both calculated using the first instance of
the markers appearance across the sequential maps. This makes it ideal
for potentially pushing additional genotypes into an established map.

For both \code{merge.by} types, if a linkage group name is shared across
linkage maps then the marker data from the shared linkage group in each
of the maps will be merged. If maps share the same
linkage group names and do not require merging the duplicate linkage
group names in one of the linkage maps will need to be altered before
combining. As a final process, markers are ordered within linkage groups
according to distances supplied in each of the linkage maps.

It should also be noted that this function does not re-construct the
final linkage map after combining the set of linkage maps. For efficient
linkage map reconstruction of a combined \pkg{qtl} object see
\code{mstmap.cross()}.
}
\value{
A single R/qtl cross object is returned with identical class structure as the
inputted cross objects.
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
\code{\link{breakCross}} and \code{\link{mergeCross}}
}
\examples{

data(mapDH, package = "ASMap")

## create copy of mapDH with some different linkage groups
## and change marker names so they are unique

mapDH1 <- mapDH
names(mapDH1$geno)[5:14] <- paste("L",1:10, sep = "")
mapDH1$geno <- lapply(mapDH1$geno, function(el){
    nam <- paste(names(el$map), "A", sep = "")
    names(el$map) <- dimnames(el$data)[[2]] <- nam
    el})

mapDHc <- combineMap(mapDH, mapDH1)
nmar(mapDHc)
}
\keyword{misc}