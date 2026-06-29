\name{alignCross}
\alias{alignCross}
\title{
Graphical linkage group identity and alignment.
}
\description{
A graphical tool for identity and alignment of linkage groups in \pkg{qtl}
cross objects using reference maps.
}
\usage{
alignCross(object, chr, maps, ...)
}
\arguments{
  \item{object}{
A \pkg{qtl} cross \code{object} with any class structure.
}
\item{chr}{
A character string of linkage group names or a logical vector equal to
the length of the number of linkage groups (see \code{\link[qtl]{subset.cross}}).
}
\item{maps}{
A named list of \pkg{qtl} cross objects or \code{data.frame} objects
containing markers that are present in \code{object}. The matching
markers are used to identify and orient the \code{object} linkage groups (see Details).
}
\item{\ldots}{
Other arguments to be passed to the high level lattice plot.
}
}
\details{
If any list elements of \code{map} are \pkg{qtl} \code{"cross"} objects
then marker names, linkage group identity and genetic distance
information are extracted. List elements of \code{map} that are \code{data.frame}
objects must explicitly contain named columns
\code{"marker"}, \code{"ref.chr"}, \code{"ref.dist"} otherwise an error will be produced.

For each linkage group determined by \code{chr}, the contents of the
listed \code{maps} are checked for matching markers in
\code{object}. For each \code{chr} and reference map combination, a
scatter plot of the \code{object} genetic distances against the
reference distances is displayed with reference
linkage group names as the plotting character. If a linkage group is in
correct orientation the overall slope of the scatter plot should be
positive. If a linkage group requires inverting then the overall slope
should be negative.
}
\value{
A lattice panel plot is displayed with panels labelled by a combination
of \code{chr} and the \code{maps} used as a reference. A data frame of
these results is also invisibly returned.
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

chrl <- sample(c(TRUE,FALSE), 23, replace = TRUE)
mapDH1 <- subset(mapDH, chr = chrl)
alignCross(mapDH, maps = list(DH = mapDH1), layout = c(3,5), col = 1:7)

}
\keyword{misc}
