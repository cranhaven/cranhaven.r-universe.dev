\name{heeringa}
\alias{heeringa}
\title{
Heeringa-style colours
}
\description{
Proposed in Heeringa (2004) to colour a (dis)similarity by decomposing it into three dimensions (using \code{cmdscale} here) and then mapping these dimensions to RGB to make colours. Highly useful to visualize pairwise similarities between geographic regions.
}
\usage{
heeringa(dist, power = 0.5, mapping = c(1, 2, 3), method = "eigs", center = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dist}{
\code{\link{dist}} object specifying distances between points.
}
  \item{power}{
Factor used to influence the results of the multidimensional scaling. Values closer to one will lead to clearer separated colours, while higher values will lead to more gradual colours.
  }
  \item{mapping}{
Optional vector to change the mapping of the dimensions to the colours. Should be of length 3, specifying to which color each of the three dimensions is mapped. A 1 means 'red', a 2 means 'green' and a 3 means 'blue'. Adding a minus reverses the mapping.
}
  \item{method}{
Method used to determine the colour dimensions. Either \code{mds} (nicer colourbalance) or \code{eigs} (much faster).
  }
  \item{center}{
Optionally, specify an index of one of the points to be put in the center of the coloring scheme, i.e. this point will become grey and all other points will be colored relative to this point.
  }
}
\details{
This proposal goes back to Heeringa (2004). The idea is to visualize distances by mapping the first three dimensions of a multidimensional scaling to the the red-green-blue scales. The \code{mapping} vector can be used to change the mapping to the colours.
}
\value{
A vector of colours of the same length as the size of the dist object.
}
\references{
Heeringa, Wilbert. "Measuring Dialect Pronunciation Differences Using Levenshtein Distance." Ph.D. Thesis, Rijksuniversiteit Groningen, 2004.
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\examples{
data(hessen)
tess <- weightedMap(hessen$villages, window = hessen$boundary, crs = 2397)
d <- dist(hessen$data, method = "canberra")

# different mappings of the colors
c1 <- heeringa(d)
plot(tess$weightedVoronoi, col = c1, border = NA)

c2 <- heeringa(d, power = 1, mapping = c(3, -2, 1))
plot(tess$weightedVoronoi, col = c2, border = NA)
}
