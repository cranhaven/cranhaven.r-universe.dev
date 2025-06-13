\name{createTiles}
\alias{createTiles}

\title{
Tile creation from a folder of images
}
\description{
A very simple function to create tiles from a folder containing JPEG images. It uses bilinear interpolation (via the \code{\link{bilinearInterpolator}} function), thus note that the quality of the tiles will be sub-optimal. Thus, for high quality purposes please use another external tool providing better interpolation schemes to create your tiles (bicubic splines, sincz, ...).
}
\usage{
createTiles(inPath, outPath, tileHeight = 40, verbose = TRUE)
}

\arguments{
  \item{inPath}{
  A path with the folder where the images are contained.
}
  \item{outPath}{
  A path with the folder where the tiles will be created (if the folder does not exists, it will be created).
}
  \item{tileHeight}{
  The tile height in pixels.
}
  \item{verbose}{
  A boolean flag to indicate if the user wants to have screen output or not.
}
}

\author{
Alberto Krone-Martins
}

\seealso{
\code{\link{bilinearInterpolator}} 
}
\examples{
# Set the folder where the original images are located
pathToOriginalImages <- system.file("extdata/2Massier/", package="RsimMosaic")

# Set the folder where the tiles will be stored. It will be created if it does not exists.
pathToTileImages <- paste(tempdir(), "/myTiles/", sep="")

# Create the tiles (10 pix are used just to make this example run fast)
createTiles(pathToOriginalImages, pathToTileImages, tileHeight=10)

}

\keyword{ utilities }
\keyword{ misc }

