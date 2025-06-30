\name{factorMap}
\alias{factorMap}
\alias{fmap}
\alias{limage}
\title{
Visualising nominal data with various factors.
}
\description{
A factor map ("fmap") is a counterpart of the base function \code{\link{image}}. In contrast to an image, a factor map can be used for nominal data with various levels (instead of continuous numerical data). A matrix (or a dataframe coerced as matrix) is visualised by showing the most frequent contents of the cells by colouring. There are various methods for ordering of rows and columns provided, alike to a \code{\link{heatmap}}).
}
\usage{
factorMap(x, order = NULL, col = rainbow(4), show.remaining = FALSE,
  col.remaining = "grey", pch.na = 20, col.na = "lightgrey", legend = length(col),
  labels.x = rownames(x), labels.y = colnames(x), cex.axis = 1, cex.legend = 1,
  cex.remaining = 1, font = "", asp = nrow(x)/ncol(x), method = "hamming",
  control = NULL, plot = TRUE)
}
\arguments{
  \item{x}{
A matrix or dataframe with the data to be displayed. Rows are shown on the x-axis, columns on the y-axis, showing the row- and column-names in the display. All data in the whole matrix is interpreted as one large factor with different levels.
}
  \item{order}{
How should rows and columns be ordered? By default the order of the data matrix \code{x} is used. Many possible algorithmic orderings are available, see Details. custom orderings should simply be applied to the \code{x} matrix beforehand, then without invoking this \code{order} argument.
}
  \item{col}{
Colors to be used for the display. By default, the colours specified here are used in order of frequency of the phenomena in the data (i.e. \code{order(table(x), decreasing = TRUE}). A named vector of colors (or a named list) are applied to the levels as named. All other levels are shown as 'others'. Optionally use \code{show.remaining} to show labels for these others in the visualisation.
}
  \item{show.remaining}{
Logical: should all levels without color be shown inside the boxes as text?
}
  \item{col.remaining}{
Which color should the text of the uncolored levels have?
  }
  \item{pch.na}{
Symbol to be used for NA elements. Use \code{NULL} for no symbol, but complete coloring of the boxes.
  }
  \item{col.na}{
Color to be used for NA elements.
  }
  \item{legend}{
How many levels should be shown in the legend (in the order of frequency? Alternatively, provide a vector with names of the levels to be shown. Use \code{NULL} to suppress the legend.
  }
  \item{labels.x}{
Labels to be used on the x-axis. Defaults to rownames of the data. Use \code{NULL} to suppress labels.
  }
    \item{labels.y}{
Labels to be used on the y-axis. Defaults to colnames of the data. Use \code{NULL} to suppress labels.
  }
  \item{cex.axis}{
Size of the row and columns names of \code{x}, shown as axis labels.
}
  \item{cex.legend}{
Size of the legend text.
}
  \item{cex.remaining}{
Size of the text in the boxes. Only shown when \code{show.remaining = TRUE}.
}
  \item{font}{
Font to be used in the plotting, can be necessary for unusual unicode symbols. Passed internally to \code{par(family)}.
}
  \item{asp}{
Aspect-ratio of the plotting of the boxes. By default the complete plot will be approximately square. Use the value 1 for all square boxes. Manually resizing the boxes by changing the plotting window can be achieved by setting \code{asp = NA}.
  }
  \item{method}{
Method used to determine similarity, passed to \link[qlcMatrix]{sim.obs}, which is used internally to determine the order of rows and columns, using the method chosen in \code{order}.
  }
  \item{control}{
List of options passed to \link[seriation]{seriate}.
  }
  \item{plot}{
By default, a plot is returned. When \code{FALSE}, nothing is plotted, but the re-ordering is returned.
  }
}
\details{
There are many different orderings implemented: \code{"pca"} and \code{"varimax"} use the second dimension of \link{prcomp} and \link{varimax} respectively. \code{"eig"} will use the first eigenvector as computed by \link[RSpectra]{eigs}. This is really quick for large datasets. \code{"mds"} will use the first dimension of \link{cmdscale}.

Further, all methods as provided in the function \link[seriation]{seriate} can be called. Specifcally, \code{"R2E"} and \code{"MDS_angle"} seem worthwhile to try out. Any parameters for these methods can be passed using the option \code{control}.
}
\value{
A plot is returned by default. When \code{plot = FALSE}, a list is returned with the reordering of the rows and the columns.
}
\note{
Note that it is slightly confusing that the resulting image is a transposed version of the data matrix (rows of the matrix are shown as horizontal lines in the graphic, and they are shown from bottom to top). This is standard practice though, also used in \link{image} and \link{heatmap}, so it is continued here.
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\seealso{
\code{\link{image}} in base and \link[seriation]{pimage} in the package \code{seriation}.
}
\examples{
# a simple data matrix
x <- matrix(letters[1:5],3,5)
x[2,3] <- x[1,4] <- NA
rownames(x) <- c("one", "two", "three")
colnames(x) <- 1:5
x

# some basic factor maps
factorMap(x, asp = 1)
factorMap(x, col = heat.colors(5), asp = NA)
factorMap(x, col = list(b = "red", e = "blue"), show.remaining = TRUE)

\dontrun{
# more interesting example, different "f" sounds in german dialects
# note that fonts might be problematic on some platforms
# plotting window should be made really large as well
data(dialects)
factorMap(dialects$data, col = rainbow(8), order = "R2E"
    , cex.axis = 0.3, cex.legend = 0.7
    , show.remaining = TRUE, cex.remaining = 0.2)

# get reordering of rows
# to identify the group of words with "p-f" correspondences
factorMap(dialects$data, order = "R2E", plot = FALSE)
}
}
