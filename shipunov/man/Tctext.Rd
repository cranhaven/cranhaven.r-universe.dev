\name{Tctext}
\alias{Tctext}
\title{Easy way to add text labels to 'hclust' plot}
\description{Uses text() and Tcoords() to add text labels to 'hclust' plot}

\usage{

  Tctext(hcl, labels=hcl[["labels"]], hang=0.1, add=0, horiz=FALSE, xpd=TRUE, ...)
}

\arguments{

  \item{hcl}{\code{hclust} object}

  \item{labels}{Character vector with the size of 'labels' component of
  'hcl'; by default, these exact 'labels'}

  \item{hang}{The fraction of the plot height by which labels should hang
  below the rest of the plot; by default, it is equal to the default
  'hang' from \code{hclust} which is 0.1}

  \item{add}{Add to 'hang' to make labels look better; the reliable value
  is about 0.03}

  \item{horiz}{Plot on a horizontal tree?}

  \item{xpd}{Plot text if it goes outside of the plotting region?}

  \item{...}{Further arguments to text()}
}

\details{

  Please note that labels (similarly to Ploth()) are treated in their
  _initial_, pre-clustered order because Tctext() reorders everything
  internally. This is not similar to Tcoords() which treats them in order
  of their appearance on the dendrogram and therefore requires manual
  re-ordering.

  Please feel free to use the simple enough code of this function to
  produce other convenient 'hclust'-labeling routines, for example, one
  can make 'Tcpoints' based on Tcoords() and points().

}

\seealso{

  \code{\link{Tcoords}}, \code{\link{hclust}}, \code{\link{Ploth}}

}

\examples{

hcl <- hclust(UScitiesD, "ward.D2")

## how to imitate the default plot.hclust() with Tctext()
old.par <- par(mfrow=c(1, 2))
plot(hcl, labels=gsub("[A-z.]", "  ", hcl$labels))
Tctext(hcl, srt=90, add=0.04, adj=c(1, 0.5))
plot(hcl)
par(old.par)

## how to use different properties of text()
plot(hcl, labels=FALSE)
Tctext(hcl, srt=45, add=0.02, adj=c(0.8, 1), font=2:1, col=1:5, cex=0.8)

## how to use Tctext() with Ploth()
old.par <- par(mar=c(3, 1, 0, 7))
Ploth(hcl, horiz=TRUE, labels=NA, col=c(1:5, 1:5), col.edges=TRUE)
Tctext(hcl, horiz=TRUE, hang=-1, col=1:5, pos=4, cex=1.1, font=3)
par(old.par)

}

\keyword{cluster}
