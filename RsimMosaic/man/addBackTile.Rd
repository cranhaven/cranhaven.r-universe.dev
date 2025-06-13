\name{addBackTile}
\alias{addBackTile}

\title{
Add a tile back to the tile library
}
\description{
This is a simple function to add a tile (with the filename \code{tileFilename}) back to the tile library (passed as the argument \code{libForMosaic}). The tile data in the parameter space is copied from the original tile library (passed as the argument \code{libForMosaicFull}).
}
\usage{
addBackTile(tileFilename, libForMosaic, libForMosaicFull)
}

\arguments{
  \item{tileFilename}{
  The filename of the tile to add back in the tile library.
}
  \item{libForMosaic}{
  The tile library where the tile should be added back. 
}
  \item{libForMosaicFull}{
  The original tile library containing the data of the all the tiles in the parameter space.
}
}

\value{
It returns the tile library libForMosaic, with the requested tile added.
}
\author{
Alberto Krone-Martins
}

\seealso{
\code{\link{removeTile}}
}
\examples{
# Creates the tile library data frame from the example tiles
my2MassTilesFull <- createLibraryIndexDataFrame(system.file("extdata/2Massier/",
 package="RsimMosaic"))
my2MassTiles <- my2MassTilesFull

# Get a random filename of one of the tiles
idx <- round(runif(1, 1, length(my2MassTiles[,1])))
tileFilename <- as.character(my2MassTiles[idx,1])

# Remove it from the library
my2MassTiles <- removeTile(tileFilename, my2MassTiles)

# Now, put it back
my2MassTiles <- addBackTile(tileFilename, my2MassTiles, my2MassTilesFull)
}

\keyword{ utilities }
\keyword{ misc }

