\name{Plot.phylocl}
\alias{Plot.phylocl}
\title{Plot phylogenetic tree with clades collapsed}
\description{Plot phylogenetic tree with clades collapsed into triangles or rectangles}

\usage{Plot.phylocl(tree, cl, strict=TRUE, keep.mono=FALSE, what="triangles",
 col.ed="black", col.td="black", col.etr="transparent", col.ttr="transparent",
 col.pfl="lightgrey", col.pbr="black", lty.p=1, lwd.p=1, col.ct="black",
 ct.off=0, ct.fnt=1, cex=par("cex"), longer="0\%", ...)}

\arguments{
\item{tree}{phylo object}
\item{cl}{two columns classification table}
\item{strict}{default TRUE: do not join all descendants}
\item{keep.mono}{default FALSE: do not keep monotypic clades}
\item{what}{default "triangles", also possible to use "rectangles"}
\item{col.ed}{default "black", default edge color}
\item{col.td}{default black", default tips color}
\item{col.etr}{default "transparent", color to suppress original edges}
\item{col.ttr}{default "transparent", color to suppress original tips}
\item{col.pfl}{default "lightgrey", fill color for polygons}
\item{col.pbr}{default "black", border color of polygons}
\item{lty.p}{default 1, line type of polygon borders}
\item{lwd.p}{default 1, line width}
\item{col.ct}{default "black", color of clade labels}
\item{ct.off}{default 0, text offset of clade labels}
\item{ct.fnt}{default 1, text font of clade labels}
\item{cex}{default par("cex"), text font size of all labels}
\item{longer}{default "0\%", percent to increase xlim to fit longer clade labels}
\item{...}{options to ape::plot.phylo()}
}

\details{

Plot.phylocl() plots phylogenetic tree with clades collapsed into
triangles or rectangles.

Alternative is phytools::plot.backbonePhylo() which however requires more
manual work.

Some tricks used (null plotting and transparent elements), the last one
is actually useful in other ways.

Intersections and other deviated cases not controlled. However, they are
really easy to spot.

All parameters of polygons should be either "scalars" or vectors of the
same length as clade list (minus monotypic clades), clades are in
alphabetical order. To help, list of clade names is invisibly returned in
the end.

If keep.mono=TRUE, then monotypic clades must have names in the clade
list, otherwise this option is useless.

}

\value{Returns list of clade names.}

% \references{}

\author{Alexey Shipunov}

\seealso{phytools::plot.backbonePhylo()}

\examples{
aa.d <- hclust(dist(t(atmospheres)))
tree <- ape::unroot(ape::as.phylo(aa.d))

cl <- data.frame(
planet=c(
 "Mercury",
 "Venus",
 "Earth",
 "Mars",
 "Jupiter",
 "Saturn",
 "Uranus",
 "Neptune"),
clade=c(
 "Mercury",
 "Mars group",
 "Earth",
 "Mars group",
 "Close giants",
 "Close giants",
 "Distant giants",
 "Distant giants"),
stringsAsFactors=FALSE)

Plot.phylocl(tree, cl, longer="5\%", ct.off=0.1)
}

\keyword{hplot}
