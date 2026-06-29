\name{subsetCross}
\alias{subsetCross}
\title{
Subset an R/qtl object
}
\description{
Subset an R/qtl object by chromosome or by individuals for populations used
within the R/ASMap package.
}
\usage{
subsetCross(cross, chr, ind, ...)
}
\arguments{
 \item{cross}{
A \code{"cross"} object generated from the R/qtl package. Specifically
the object needs to inherit from one of the following classes
\code{"bc"}, \code{"dh"}, \code{"riself"}, \code{"bcsft"} (see Details).
}
\item{chr}{
Optional vector specifying which chromosomes to keep or
discard. This may be a logical, numeric, or character string
vector. See \code{?subset.cross}.
}
\item{ind}{
Optional vector specifying which individuals to keep or discard.
This may be a logical or numeric vector (see Details).
}
\item{...}{
Kept for compatability with \code{subset.cross} and is ignored at this point.
}
}
\details{
This function is a replacement version of \code{subset.cross} that should be
used if the \code{cross} object contains any or all of the components \code{"co.located"},
\code{"seg.distortion"} and \code{"missing"} created by a
\code{pullCross} call. For a given \code{ind}, the function calls
\code{subset.cross} to ensure that all elements created from calls to
native R/qtl functions are subsetted appropriately. In addition,
the \code{"co.located"}, \code{"seg.distortion"} and \code{"missing"}
elements are also subsetted and if components \code{"seg.distortion"}
and \code{"missing"} exist, statistics in their respective tables are
recalculated.

It provides identical functionality to \code{subset.cross}
with the exception that \code{ind} can only be a logical or numeric
vector.
}
\value{
The cross object is returned with the appropriate subsetting.
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
\code{\link[qtl]{subset.cross}} and \code{pullCross}
}
\examples{

data(mapDH, package = "ASMap")

mapDH.s <- pullCross(mapDH, type = "seg.distortion")
mapDH.s <- subsetCross(mapDH.s, ind = 3:218)
dim(mapDH.s$seg.distortion$data)

}
\keyword{misc}
