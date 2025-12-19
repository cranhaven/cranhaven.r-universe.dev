\name{heatmapBC}
\alias{heatmapBC}

\title{Overlapping Heatmap}

\description{Other than \code{\link{drawHeatmap}} this function plots all or a chosen number of bicluster in one plot even if they were overlapping.}

\usage{
heatmapBC(x, bicResult, number = 0, local = TRUE, order = FALSE, 
          outside = FALSE, ...)
}

\arguments{
  \item{x}{The data matrix where the bicluster is to be drawn.}
  \item{bicResult}{BiclustResult object with a bicluster result set.}
  \item{number}{ Number of bicluster to be drawn from the result set
    'bicResult'. If the default 0 is chosen all bicluster of the
    bicResult are drawn.}
  \item{local}{If \code{TRUE}, only rows and columns of the bicluster are drawn. This argument is only used if number is not set to 0.}
  \item{order}{If \code{TRUE}, rows and
          columns are ordered by their values.}
  \item{outside}{If \code{TRUE}, Boxes are drawn for overlapping}
  \item{...}{Additional plot options}
  }
\details{Overlap plotting only works for two neighbor bicluster defined
by the order in the number slot.}
%\value{}
%\references{}

\author{
  Sebastian Kaiser
  }

\seealso{
\code{\link{drawHeatmap}},\code{\link{parallelCoordinates}}
  }
\examples{
  set.seed(1234)
  data(BicatYeast)
  resplaid <- biclust(BicatYeast, BCPlaid(), verbose = FALSE)
  heatmapBC(x = BicatYeast, bicResult = resplaid)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{cluster}
