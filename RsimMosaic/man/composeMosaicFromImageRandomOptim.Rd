\name{composeMosaicFromImageRandomOptim}
\alias{composeMosaicFromImageRandomOptim}

\title{
Randomly transform an image into a mosaic optimizing the library usage
}
\description{
A function to compose the mosaic of an image based on regular tiles. This function will compute the mosaic by randomly replacing the pixels of the original image with tiles from a tile library, and it will try to use as many tiles from the library as possible.
}
\usage{
composeMosaicFromImageRandomOptim(originalImageFileName, outputImageFileName, 
  imagesToUseInMosaic, useGradients = FALSE, removeTiles = TRUE, 
  fracLibSizeThreshold = 0.7, repFracSize = 0.25, verbose = TRUE, neig=20)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{originalImageFileName}{
  The original image you want to use to create the mosaic from (note that for the sake of your computer's memory, this image should be small, about 128 or 256 pixels wide, or so...).
}
  \item{outputImageFileName}{
  The filename (with the path) where you want the image to be stored. This image can be quite large, depending on the number of pixels in the original image and on the tiles.
}
  \item{imagesToUseInMosaic}{
  A path with the folder where the tiles are contained (note that the tiles should be square and for the sake of your computer's memory, small, e.g. 40x40, 120x120 pixels or so...). You can use the function \code{\link{createTiles}} to create a folder with smaller tiles from a folder with your original images.
}
  \item{useGradients}{
  A flag to indicate if approximate gradients should be taken into account when selecting the tiles.
}
  \item{removeTiles}{
  A flag to indicate if the user wants to temporarily remove tiles from the library after using them, so tile repetition would be discoraged by the software. If the tile library is small, the tiles will be added back to the library after its size reaches a certain threshold (defined by \code{fracLibSizeThreshold}).
}
  \item{fracLibSizeThreshold}{
  The fraction of the size of the original tile library when the tiles must be (randomly) put back into the library.
}
  \item{repFracSize}{
  The fraction of the size of the original tile library to replace when filling back the array (it should be smaller than, or equal to, 1-fracLibSizeThreshold).
}
  \item{verbose}{
  A flag indicating if the user wants to have messages during the mosaic creation.
}
  \item{neig}{
  An integer indicating the number of neighbors to retrieve from the tile library.
}
}

\author{
Alberto Krone-Martins
}

\seealso{
\code{\link{composeMosaicFromImageRandom}}
}
\examples{
# Set the filename of the original image
#origImgFileN <- system.file("extdata", "verySmallMoon.jpg", package="RsimMosaic") 
origImgFileN <- system.file("extdata", "reallyVerySmallMoon.jpg", package="RsimMosaic") 

# Set the folder where the tiles library is located
pathToTileLib <- system.file("extdata/2Massier", package="RsimMosaic")

# Set the filename of the output image (the mosaic!)
outImgFileN <- file.path(tempdir(), "verySmallMoon-2MASS-Mosaic.jpg")

# Create the mosaic
composeMosaicFromImageRandomOptim(origImgFileN, outImgFileN, pathToTileLib, removeTiles=FALSE)

}

\keyword{ methods }

