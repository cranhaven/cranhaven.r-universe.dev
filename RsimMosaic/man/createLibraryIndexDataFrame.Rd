\name{createLibraryIndexDataFrame}
\alias{createLibraryIndexDataFrame}

\title{
A function to create the tile library data frame
}
\description{
A function to create the tile library data frame containing the data of the tiles in the parameter space, as well as the filename of the tiles.
}
\usage{
createLibraryIndexDataFrame(path, saveLibraryIndex = FALSE, 
	 libraryFilename, useGradients = FALSE)
}

\arguments{
  \item{path}{
  A path indicating where the images that will compose the library are stored.
}
  \item{saveLibraryIndex}{
  A flag to indicate if the library should be saved in a file.
}
  \item{libraryFilename}{
  The filename to use if the user wants to store the library to a file.
}
  \item{useGradients}{
  A flag indicating if the values of the data in the parameter space in the corners of the tile images should be calculated.
}
}
\details{
The tile image data in the parameter space is calculated by \code{\link{computeStatisticalQuantitiesTile}}. 
}
\value{
It returns a data frame containing the filename of each tile found at the \code{path} and the median RGB values of each tile. Optionally it also includes the median RGB values of the corners of the image.
}

\author{
Alberto Krone-Martins
}

\seealso{
\code{\link{computeStatisticalQuantitiesTile}}
}
\examples{
# Creates the tile library data frame from the example tiles
my2MassTiles <- createLibraryIndexDataFrame(system.file("extdata/2Massier/", 
	package="RsimMosaic"))
}

\keyword{ utilities }
\keyword{ misc }
