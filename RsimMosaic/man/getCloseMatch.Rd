\name{getCloseMatch}
\alias{getCloseMatch}

\title{
Get a tile which is a close match for a pixel in the parameter space
}
\description{
This function will return the filename of a tile that is a close match to a pixel in the parameter space. The \code{nneig} matches are selected using a nearest neighbour search (\code{\link{nn2}}) in the tile library (\code{libraryDataFrame}). After the candidates selected, one of the is randomly choosen and its filename is returned by the function.
}
\usage{
getCloseMatch(pixelArray, libraryDataFrame, nneig = 20)
}
\arguments{
  \item{pixelArray}{
  The parameters of the pixel to get a similar image from the library in the parameter space.
}
  \item{libraryDataFrame}{
  The tile library containing the data of the tiles in the parameter space.
}
  \item{nneig}{
  Number of neighbours to retrieve in the intermediate test. Only one of the neighbours will be returned to the user.
}
}
\value{
The filename of a tile that is a close match to a pixel in the parameter space.
}
\author{
Alberto Krone-Martins
}

\seealso{
\code{\link{nn2}}
}
\examples{
# Creates the tile library data frame from the example tiles
my2MassTiles <- createLibraryIndexDataFrame(system.file("extdata/2Massier", package="RsimMosaic"))

# Get a close match for the point with parameters (0.2, 0.3, 0.2)
getCloseMatch(c(0.2, 0.3, 0.2), my2MassTiles)

# Get another close match for the point with parameters (0.2, 0.3, 0.2)
getCloseMatch(c(0.2, 0.3, 0.2), my2MassTiles)
}

\keyword{ utilities }
\keyword{ misc }
