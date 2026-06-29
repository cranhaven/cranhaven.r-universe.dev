\name{heatMap}
\alias{heatMap}
\title{
Heat map of the estimated pairwise recombination fractions and LOD
linkage between markers.
}
\description{
Heat map of the estimated pairwise recombination fractions and LOD
linkage between markers that provides extended functionality of Bromans
\pkg{qtl} package \code{plotRF} function.
}
\usage{
heatMap(x, chr, mark, what = c("both", "lod", "rf"), lmax = 12,
   rmin = 0, markDiagonal = FALSE, color =
rev(colorRampPalette(brewer.pal(11,"Spectral"))(256)),  \ldots)
}
\arguments{
\item{x}{
A \code{"cross"} object generated from the \pkg{qtl} package.
}
\item{chr}{
A character string of linkage group names to subset the cross object.
}
\item{mark}{
An argument to subset linkage groups further into marker subsets. This
can be a single numerical vector of markers positions which will
subset all linkage groups in the same manner. Or it may be a list of
numerical vectors named by the linkage group names with which to subset
the linkage groups separately.
}
\item{what}{
A character string of either \code{"lod"}, \code{"rf"} or
\code{"both"}. If \code{"lod"} only pairwise LOD scores between markers
are plotted. If \code{"rf"} then only pairwise recombination fractions
between markers are plotted. If \code{"both"} then both are plotted
with LOD on the lower triangle and recombination in the upper
triangle. This is the default (see Details).
}
\item{lmax}{
The threshold LOD score to implemented. Scores above this threshold will be
plotted at the same colour.
}
\item{rmin}{
The threshold recombination fraction to be implemented. Recombination
fractions below this threshold will be plotted at the same colour.
}
\item{markDiagonal}{
Logical value. If \code{TRUE} then borders are added around the diagonal
elements of the heat map.
}
\item{color}{
The colour spectrum used to display the heat map. The default is a
\code{"Spectral"} diverging palette from the \pkg{RColorBrewer} package (see Details).
}
\item{\ldots}{
There are additional features available through this argument that
can be used to customize the heatmap (see Details).
}
}
\details{
This function is a rewrite of Bromans \pkg{qtl} package function
\code{plot.rf} that provides extended functionality. When \code{what =
  "lod"} is chosen the pairwise LOD linkage
between markers is displayed on the heat map with
a legend on the right hand side spanning zero to \code{lmax} across the
\code{color} spectrum. If \code{what = "rf"} the pairwise estimated recombination
fractions are displayed on the heat map with a legend on the right hand side spanning
\code{rmin} to 0.5 across the \code{color} spectrum. The legend also
extends past 0.5 to display estimated recombination fractions between
0.5 and one through a colour spectrum of the maximum \code{color} value
to white. This functionality now gives users the ability to detect
markers that may be problematic or possibly out of phase. For \code{what
  = "both"} the pairwise LOD linkage is displayed on the lower triangle of the
heat map and the pairwise estimated recombination fractions are
displayed on the upper triangle. If this option is chosen, legends are displayed for both
components of the heat map.

The default colour spectrum is the diverging palette \code{"Spectral"}
from the \pkg{RcolorBrewer} package. This diverging palette
provides an aesthetically pleasing colour spectrum for the
diagnosis of pairwise linkage between markers. Specifically, the palette
displays weak linkage and/or low recombination between markers as blue
or "cool" areas and strong linkage and/or recombination between markers are
shown as red or "hot" areas.

Much of the extra functionality of this function comes from the use of
\code{image.plot} in the \pkg{fields} package. This function allows the
partitioning of the plotting region into a \code{bigplot} region for the
heat map and a \code{smallplot} region for the legend. This is called
twice when \code{what = "both"}. The size of the regions can be
manipulated by passing the \code{bigplot} or \code{smallplot} arguments
to the function but it is advised to use the defaults. Further
manipulation of the heat map can achieved by passing other arguments of
the function \code{image.plot}. Users should consult the help file for
\code{image.plot} for more details. It should be noted that the
argument \code{legend.args} needs to be avoided as it used in this
function.
}
\value{A heat map is displayed on the current plotting device.
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
\code{\link[qtl]{plotRF}}
}
\examples{

data(mapDH, package = "ASMap")

## bulking linkage groups and reconstructing entire linkage map

test1 <- mstmap(mapDH, bychr = FALSE, dist.fun = "kosambi",
                trace = FALSE)

## plot heat map of result

heatMap(test1, lmax = 30)

}
\keyword{graphics}
