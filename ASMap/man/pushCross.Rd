\name{pushCross}
\alias{pushCross}
\title{
Push markers into an established R/qtl linkage map.
}
\description{
Push unlinked markers or markers that were originally placed aside by
\code{pullCross} back into linkage groups of an established R/qtl linkage map.
}
\usage{
pushCross(object, type = c("co.located","seg.distortion",
          "missing","unlinked"), unlinked.chr = NULL,
          pars = NULL, ...)
}
\arguments{
\item{object}{
An R/qtl \code{cross} object with class structure \code{"bc"},
  \code{"dh"}, \code{"riself"}, \code{"bcsft"}. (see \code{?mstmap.cross} for more details.)
}
\item{type}{
A character string determining the type of markers to be pushed into
the linkage map (see Details).
}
\item{unlinked.chr}{
A character string of linkage group names containing markers that
require pushing into the remaining linkage groups of the object. This is only useful when
\code{type="unlinked"}. Default is \code{NULL}.
}
\item{pars}{
A list of parameters that are used by \code{pushCross} to push markers
a certain type back into the linkage group. The default NULL calls the parameter
initialization function \code{pp.init} with defaults (see Details and
Examples).
}
\item{\ldots}{
Currently ignored.
}
}
\details{
This function was written explicitly to complement \code{pullCross} by
"pushing" markers of certain types back into linkage groups of an
established linkage map.

Currently supported marker types are:
\itemize{
\item \code{type = "co.located"}. Users can push co-located markers back
into the linkage map that have been set aside in the cross object element
\code{co.located}. To ensure this can be used at any stage of the linkage map
construction process the function disregards the linkage group information
provided in the table formed by using \code{pullCross}. Instead it uses the
current positions of the markers in the reduced linkage map to determine
where to push the co-located markers back to.

\item \code{type = "seg.distortion"}. Users can push markers from the
\code{"seg.distortion"} element of the object back into a linkage map using the thresholding
mechanisms \code{seg.thresh} and \code{seg.ratio} called using
\code{pars}. If \code{seg.thresh} is given then the markers are pushed
back that have p-values that are GREATER than \code{seg.thresh}. If
\code{pars} contains an element \code{seg.ratio} then markers are pushed
back based on the ratio provided. The ratio must be in character format and of the type
"AA:BB" for two allele populations and "AA:AB:BB" for three allele
populations (see Examples for more details). Markers are pushed back if their
allele proportions are LESS than the largest proportional ratio or GREATER
than the smallest proportional ratio given in \code{seg.thresh}. If neither
thresholding mechanisms are given then the default is to use \code{seg.thresh = 0.05}.

\item \code{type = "missing"}. Users can push markers from the object
element \code{"missing"} back into the linkage map using the
thresholding parameter \code{miss.thresh} called using
\code{pars}. Markers will be pushed back that have a
proportion of missing values LESS than \code{miss.thresh}. If no value
is given for this parameter it defaults to 0.1 or 10\% missing values.

\item \code{type = "unlinked"}. Users can push unlinked markers that
reside in linkage groups of the established linkage map. If this type is
chosen \code{unlinked.chr} must be a character string of linkage group
names in the object.
}
For types \code{"seg.distortion"}, \code{"missing"} and
\code{"unlinked"} a fast clustering method is used to allocate markers
to established linkage groups. This is done very
efficiently by reducing the constructed linkage map to a skeleton set
of markers before checking linkages. How these linkages are formed can
be tweaked by setting \code{max.rf} and \code{min.lod} when calling
\code{pars}. These currently default to \code{max.rf = 0.25} and
\code{min.lod = 3}.

Users should explicitly avoid the use of "UL" as part of a linkage group name as
this is used internally to name unlinked groups of markers if required.
It should also be noted that this function does not re-construct the
object after allocating markers to linkage groups. For efficient linkage map
reconstruction of an R/qtl object see \code{mstmap.cross()}.
}
\value{
The cross object is returned with an identical class structure as the
inputted cross object with additional markers from the marker types
pushed into linkage groups of the established linkage map. If all
markers of an element type are pushed back then the element type is
removed from the object.
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
\code{\link{pullCross}}
}
\examples{

data(mapDH, package = "ASMap")

## pull co-located markers from map

mapDH.c <- pullCross(mapDH, type = "co.located")
mapDH.c$co.located$table

## push co-located markers back into linkage map

mapDH.z <- pushCross(mapDH.c, type = "co.located")
pull.map(mapDH.z)

}
\keyword{misc}
