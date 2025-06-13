\name{RsimMosaic-package}
\alias{RsimMosaic-package}
\alias{RsimMosaic}
\docType{package}
\title{
R Simple IMage Mosaic creation library
}
\description{
RsimMosaic is a package for transforming an image into a mosaic composed from a set of smaller images (tiles). This library also contains a simple function for creating the tiles from a folder of images directly through R, without the need of any external code. At this moment only the JPEG format is supported, either as input (image and tiles) or output (mosaic transformed image).
}
\details{
\tabular{ll}{
Package: \tab RsimMosaic\cr
Type: \tab Package\cr
Version: \tab 1.0.2\cr
Date: \tab 2014-08-23\cr
License: \tab GPL (>= 2)\cr
}

The RsimMosaic, or the R Simple IMage Mosaic creation library, is a package for the production of mosaics. From a set of images that are called tiles, the implemented methods are able to compose a mosaic that mimics a user selected image. One example is represented in the figure below:

\if{html}{\figure{RsimMosaic-ExampleImage.jpg}{RsimMosaicExample}}
\if{latex}{\figure{RsimMosaic-ExampleImage.pdf}{options: width=5.3in}}

Note that the larger and the more varied is the adopted tile library, the more faithful will be the resulting mosaic. This package comes with a very small library, containing only 100 stamps of astronomical sources (called Messier objects) from the \href{http://www.ipac.caltech.edu/2mass/}{2MASS catalogue}, for example and test purposes.
}
\author{
Author: Alberto Krone-Martins

Maintainer: Alberto Krone-Martins <algol@sim.ul.pt>
}

\keyword{ package }
\seealso{
\code{\link{composeMosaicFromImageRandom}}, \code{\link{composeMosaicFromImageRegular}}, \code{\link{createTiles}}
}
\examples{
# This example will transform an image of the Moon into a mosaic
# composed by objects from the Messier catalogue. The tiles were
# created from the images of the 2MASS catalogue.
# 
# Set the filename of the original image
#origImgFileN <- system.file("extdata", "verySmallMoon.jpg", package="RsimMosaic") 
origImgFileN <- system.file("extdata", "reallyVerySmallMoon.jpg", package="RsimMosaic") 

# Set the folder where the tiles library is located
pathToTileLib <- system.file("extdata/2Massier", package="RsimMosaic")

# Set the filename of the output image (the mosaic!)
outImgFileN <- file.path(tempdir(), "verySmallMoon-2MASS-Mosaic.jpg")

# Create the mosaic
composeMosaicFromImageRandom(origImgFileN, outImgFileN, pathToTileLib, removeTiles=FALSE)
}
