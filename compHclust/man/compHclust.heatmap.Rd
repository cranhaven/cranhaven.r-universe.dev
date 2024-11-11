\name{compHclust.heatmap}

\alias{compHclust.heatmap}

\title{Heat Map for Complementary Hierarchical Clustering}

\description{Displays a heat map of X, a dendrogram of the clustering of
  the columns of X and a bar plot of the relative gene importances.}

\usage{
compHclust.heatmap(x, xhc, gi, d.title = "Cluster Dendrogram",
                   hm.lab = TRUE, hm.lab.cex = 1, d.ht = 0.25,
                   gi.width = 0.5, d.mar = c(0, 4, 4, 2),
                   hm.mar = c(5, 4, 2, 2))
}

\arguments{
  \item{x}{A numeric matrix X, where interest lies in clustering its
  columns.}
  \item{xhc}{An object of class \code{hclust}, specifically, a hierarchical
  clustering of the columns of X.}
  \item{gi}{A vector of the relative gene importances, as returned by
  \code{\link{compHclust}}.}
  \item{d.title}{The title for the dendrogram.}
  \item{hm.lab}{Logical.  If true, the columns of the heat map are
  labeled with column numbers.}
  \item{hm.lab.cex}{The magnification to be used for the column labels
  relative to the current setting of \code{cex}.  See \code{\link{axis}} and
  \code{\link{par}}.}
  \item{d.ht}{The relative height of the plotting region for the
  dendrogram.  Note that the relative height of the plotting region for
  the heat map is set to 1.  See \code{\link{layout}}.}
  \item{gi.width}{The relative width of the plotting region for the
  relative gene importance plot.  Note that the relative width of the
  plotting region for the heat map is set to 1.  See \code{\link{layout}}.}
  \item{d.mar}{The margins of the plotting region for the dendrogram.
  See \code{\link{par}}.}
  \item{hm.mar}{The margins of the plotting region for the heat map.
  See \code{\link{par}}.}
}

\details{Given a numeric matrix X, a hierarchical clustering of the
  columns of X and a vector of the relative gene importances as returned
  by \code{\link{compHclust}}, this function displays a heat map of X
  with a dendrogram above and a bar plot of the relative gene importances
  to the right.  The columns of X are reordered to correspond with the
  leaves of the dendrogram.

  This function can be fragile - depending on the dimensions of X,
  some of the arguments such as the margins, heights and widths of the
  plotting regions may need to be tweaked in order for the figure to
  look nice.  However, it provides a quick and easy way of displaying
  the output of \code{\link{compHclust}} and seeing which genes (rows)
  may be most influential in the clustering of the samples (columns).

  For examples of its usage, see the help file for
  \code{\link{compHclust}}.
}

\references{Nowak, G. and Tibshirani, R.  (2008)  Complementary
  hierarchical clustering.  \emph{Biostatistics}, \bold{9}(3),
  467--483.}

\author{Gen Nowak \email{gen.nowak@gmail.com} and Robert Tibshirani}

\seealso{\code{\link{compHclust}}}

\keyword{cluster}
\keyword{graphs}
