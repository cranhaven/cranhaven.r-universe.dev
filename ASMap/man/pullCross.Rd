\name{pullCross}
\alias{pullCross}
\title{
Pull markers from a linkage map.
}
\description{
Pull markers of a certain type from a linkage map and place them aside in the
R/qtl object and, if appropriate, keeping their connections with the reduced linkage map.
}
\usage{
pullCross(object, chr, type = c("co.located","seg.distortion",
          "missing"), pars = NULL, replace = FALSE, ...)
}
\arguments{
  \item{object}{
An \pkg{qtl} \code{cross} object with class structure \code{"bc"}
  \code{"dh"}, \code{"riself"}, \code{"bcsft"}. (see \code{?mstmap.cross} for more details.)
}
\item{chr}{
A character vector of linkage group names with which to subset the linkage
map before pulling any markers.
}
\item{type}{
A character string determining the type of markers to be pulled from the
map (see Details).
}
\item{pars}{
A list of parameters that are used by \code{pullCross} to pull markers
of certain type. The default NULL calls the parameter
initialization function \code{pp.init} with defaults (see Details and
Examples).
}
\item{replace}{
A logical value determining whether the markers and summary of marker information that is
pulled from the map replaces information that is already residing in the
\code{type} element of the object.
}
\item{\ldots}{
Currently ignored.
}
}
\details{
This function gives users the ability to "pull" markers of several
different types from the linkage map and place them in appropriately named
elements of the cross object. These elements can be examined by the
user and can even be "pushed" back using the complementary command
\code{pushCross}.

Currently supported types are:
\itemize{
\item \code{type = "co.located"}. This type gives the user the ability to
reduce a linkage map to a unique set of markers for the purpose of
efficient map construction. Co-located markers are pulled from the
linkage map using the technology of \code{findDupMarkers} from the
\pkg{qtl} package and places them aside
in a separate list element called \code{"co.located"}. This element
contains the removed marker data as well as a table
that displays the connections between the co-located markers with
markers that remain in the linkage map. If required, this table is used
by \code{pushCross} to "push" the co-located markers back into the
linkage map.

\item \code{type = "seg.distortion"}. Users can pull markers with
segregation distortion from a linkage map with two different
thresholding mechanisms called using \code{pars}. If the list argument
\code{pars} is used with an element called \code{seg.thresh}
then markers are pulled from the map if the p-value from the test for segregation distortion
is LESS than \code{seg.thresh}. Values of \code{seg.thresh} must be
between 0 and 1. If \code{pars} contains an element
\code{seg.ratio} then markers are pulled from the map based on the
ratio provided. The ratio must be in character format and of the type
"AA:BB" for two allele populations and "AA:AB:BB" for three allele
populations (see Examples for more details). Markers are pulled if their
allele proportions are GREATER than the largest proportional ratio or LESS
than the smallest proportional ratio given in \code{seg.thresh}. If neither
thresholding mechanisms are given then the default is
to use \code{seg.thresh = 0.05}. If markers are found matching the above
criteria they are pulled from the linkage map and placed aside in an
element called \code{"seg.distortion"}. This element contains the
removed distorted marker data as well as a table summarizing each of the
markers. See examples below for more detail.

\item \code{type = "missing"}. Users can pull markers with a
proportional amount of missing allele scores. If \code{pars} contains an
element \code{miss.thresh} then markers are pulled from the linkage map
that have a proportion of missing values GREATER than
\code{miss.thresh}. If no value is given for \code{miss.thresh} then it defaults
to 0.1 or 10\% missing values. If markers are found matching the above
criteria they are pulled from the map and are placed aside in an separate list element
called \code{"missing"}. This element contains the
removed marker data as well as a table summarizing each of the
markers. See examples below for more detail.
}
}
\value{
The cross object is returned with identical class structure as the
inputted cross object and an additional elements corresponding to the
marker types being pulled from the map.
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
\code{\link{pushCross}}
}
\examples{

data(mapDH, package = "ASMap")

## pull co-located markers from linkage map

mapDH.c <- pullCross(mapDH, type = "co.located")
mapDH.c$co.located$table

## pull distorted markers from linkage map using seg.thresh

mapDH.s <- pullCross(mapDH, type = "seg.distortion", pars =
           list(seg.thresh = 0.05))
mapDH.s$seg.distortion$table

## pull distorted markers from linkage map using seg.ratio

mapDH.s <- pullCross(mapDH, type = "seg.distortion", pars =
            list(seg.ratio = "56:44"))
mapDH.s$seg.distortion$table

}
\keyword{misc}
