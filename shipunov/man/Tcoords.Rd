\name{Tcoords}
\alias{Tcoords}
\title{Calculates coordinates of tips from 'hclust' plot}
\description{Takes the 'hclust' plot and calculates coordinates of all tips}

\usage{
  Tcoords(hcl, hang=0.1, add=0, horiz=FALSE)
}

\arguments{

  \item{hcl}{\code{hclust} object}

  \item{hang}{The fraction of the plot height by which labels should hang
  below the rest of the plot; better to make it equal to the 'hang' from
  \code{hclust} (default is 0.1).}

  \item{add}{Add to 'hang' to make labels look better; the reliable value
  is about 0.03}

  \item{horiz}{Plot values for a horizontal tree?}

}

\details{

  This function calculates coordinates for each tip of the plotted
  'hclust' object. It is useful together with plot.hclust(...,
  labels=FALSE).

  There are numerous applications of Tcoords(). Typical situation is when
  user wants to change default labels on plot.hclust() but it is possible
  only after conversion into the dendrogram. However, this conversion
  might alter the graphical representation, and, what is worse, is not
  suitable for advanced forms of plot.hclust(), for example, in 'pvclust'
  package or those from Bclust() or Jclust()-related commands.

  Tcoords() allows to plot labels (or points, or any low lewel
  structures) separately. Therefore, it is possible to rotate them,
  colorize them, abbreviate them, change their font and so on. By
  default, labels will be plotted horizontally, not vertically as it is
  in plot.hclust().

  Tcoords() treats labels in order of their appearance on the dendrogram
  and not in their initial order, so do not forget to apply the 'order'
  component of 'hclust' object (see below for examples).

  Together with Hcoords(), Tcoords() in principle allows to plot
  dendrogram in the alternative way (for example, with aid of segments()
  and text()). That will allow, for example, to plot 'hclust'
  horizontally without conversion into dendrogram. This possibility,
  however, reqiures a further research.

  Please also see Fence() and Tctext() which are convenient functions
  based on Tcoords() for adding segments and text labels, respectively.

}

\seealso{

  \code{\link{hclust}}, \code{\link{Ploth}}, \code{\link{Fence}},
  \code{\link{Tctext}}

}

\examples{

hcl <- hclust(UScitiesD, "ward.D2")
newlabels <- abbreviate(hcl$labels, 3)
newlabels <- newlabels[hcl$order] # do not forget to reorder labels!
plot(hcl, labels=FALSE)
text(Tcoords(hcl, add=0.03), newlabels, col=ifelse(grepl("W.D", newlabels), 2, 1))

## points instead of text labels
plot(hcl, labels=FALSE)
points(Tcoords(hcl), pch=19)

## how to colorize tips, useful if dendrogram is dense
iris.h <- hclust(dist(iris[, -5]))
plot(iris.h, labels=FALSE)
points(Tcoords(iris.h, add=0.01), col=iris$Species[iris.h$order]) # reorder lalels!

## can be used with Ploth(), i.e., with dendrogram
Ploth(hcl, labels=NA, horiz=TRUE) # hang=-1 is default
tc <- Tcoords(hcl, hang=-1, horiz=TRUE)
text(tc, newlabels, pos=4, xpd=TRUE, cex=1.1) # Ploth() cannot change text size

}

\keyword{cluster}
