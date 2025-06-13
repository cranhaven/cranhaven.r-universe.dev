\name{removeTile}
\alias{removeTile}

\title{
Remove a tile from the tile library
}
\description{
This is a simple function to remove a tile (with the filename \code{tileFilename}) from the tile library (passed as the argument \code{libForMosaic}).
}
\usage{
removeTile(tileFilename, libForMosaic)
}
\arguments{
  \item{tileFilename}{
  The filename of the tile to remove from the tile library.
}
  \item{libForMosaic}{
  The library containing the data of the tiles in the parameter space from which the tile should be removed.
}
}
\value{
It returns the tile library libForMosaic, with the requested tile removed.
}
\author{
Alberto Krone-Martins
}

\seealso{
\code{\link{addBackTile}}
}
\examples{
# Creates the tile library data frame from the example tiles
my2MassTiles <- createLibraryIndexDataFrame(system.file("extdata/2Massier", package="RsimMosaic"))

# Get a random filename of one of the tiles
idx <- round(runif(1, 1, length(my2MassTiles[,1])))
tileToRemove <- as.character(my2MassTiles[idx,1])

# Remove it from the library
my2MassTiles <- removeTile(tileToRemove, my2MassTiles)
}

\keyword{ utilities }
\keyword{ misc }
