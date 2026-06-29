\name{ASMap-package}
\alias{ASMap-package}
\docType{package}
\title{
Additional functions for linkage map construction and manipulation of R/qtl
objects.
}
\description{
Additional functions for linkage map construction and manipulation of R/qtl
objects. This includes extremely fast linkage map clustering and marker
ordering using MSTmap (see Wu et al., 2008).
}
\details{
\tabular{ll}{
Package: \tab ASMap\cr
Type: \tab Package\cr
Version: \tab 1.0-4\cr
Date: \tab 2018-10-24\cr
License: \tab GPL 2\cr
}
Welcome to the ASMap package!

One of the fundamental reasons why this package exists was to utilize
and implement the source code for the the Minimum Spanning Tree
algorithm derived in Wu et al. (2008) (reference
below) for linkage map construction. The algorithm is lightning quick at
linkage group clustering and optimal marker ordering and can handle large numbers of
markers.

The package contains two very efficient functions, \code{mstmap.data.frame}
and \code{mstmap.cross}, that provide users with a highly flexible set
linkage map construction methods using the MSTmap
algorithm. \code{mstmap.data.frame} constructs a linkage map from
a data frame of genetic marker data and will use the entire
contents of the object to form linkage groups and optimally order
markers within each linkage group. \code{mstmap.cross} is a
linkage map construction function for \pkg{qtl} package objects and can
be used to construct linkage maps in a flexible number of ways.
See \code{?mstmap.cross} for complete details.

To complement the computationally efficient linkage map construction
functions, the package also contains functions \code{pullCross} and
\code{pushCross} that allow the pulling/pushing markers of different
types to and from the linkage map. This system gives users the ability
to initially pull markers aside that are not needed for immediate
construction and push them back later if required. There are also
functions for fast numerical and graphical diagnosis of unconstructed
and constructed linkage maps. Specifically, there is an improved
\code{heatMap} that graphically displays pairwise recombination
fractions and LOD scores with separate legends for
each. \code{profileGen} can be used to simultaneously profile multiple
statistics such as recombination counts and double recombination
counts for individual lines across the constructed linkage
map. \code{profileMark} allows simultaneous graphical visualization of
marker or interval statistics profiles across the genome or subsetted
for a predefined set of linkage groups. Graphical identification and
orientation of linkage groups using reference linkage maps can be conducted using
\code{alignCross}. All of these graphical functions utilize the power of
the advanced graphics package \pkg{lattice} to provide seamless multiple
displays.

Other miscellaneous utilities for \pkg{qtl} objects include
\itemize{
  \item \code{mergeCross}: Merging of linkage groups
  \item \code{breakCross}: Breaking of linkage groups
  \item \code{combineMap}: Combining linkage maps
  \item \code{quickEst}: Very quick estimation of genetic map distances
  \item \code{genClones}: Reporting genotype clones
  \item \code{fixClones}: Consensus genotypes for clonal groups
}

A comprehensive vignette showcasing the package is now available! It
contains detailed explanations of the functions in the package and how
they can be used to perform efficient map construction. There is a fully
worked example that involves pre-construction diagnostics, linkage map
construction and post construction diagnostics. This example also shows
how functions of the package can be used for post linkage map
construction techniques such as fine mapping and combining linkage maps.
The vignette has be succinctly summarised in the Journal of
Statistical Software publication Taylor and Butler (2017) referenced
below.
}
\author{
Julian Taylor, Dave Butler, Timothy Close, Yonghui Wu, Stefano Lonardi
Maintainer: Julian Taylor <julian.taylor@adelaide.edu.au>
}
\references{
Wu, Y., Bhat, P., Close, T.J, Lonardi, S. (2008) Efficient and Accurate
Construction of Genetic Linkage Maps from Minimum Spanning Tree of a
Graph. Plos Genetics, \bold{4}, Issue 10.

Taylor, J., Butler, D. (2017) R Package ASMap: Efficient Genetic
Linkage Map Construction and Diagnosis. Journal of Statistical Software,
\bold{79}(6), 1--29.
}
\keyword{package}
\seealso{
\code{\link[qtl]{qtl-package}}
}

