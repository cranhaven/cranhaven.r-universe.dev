\name{rasclassRaster-class}
\docType{class}

\alias{rasclassRaster}
\alias{rasclassRaster-class}
\alias{image,rasclassRaster-method}

\title{Class "rasclassRaster"}

\description{
The rasclass representation of ESRI Ascii grids.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new('rasclassRaster')}.
}

\section{Slots}{
  \describe{
    \item{\code{ncols}:}{The number of columns of the raster grid.}
    \item{\code{nrows}:}{The number of rows of the raster grid.}
    \item{\code{xllcorner}:}{Coordinates of the X coordinate of the lower left corner.}
    \item{\code{yllcorner}:}{Coordinates of the Y coordinate of the lower left corner.}
    \item{\code{cellsize}:}{The cell size of the grid.}
    \item{\code{NAvalue}:}{The value in the raster that represents NA values.}
    \item{\code{grid}:}{A numeric vector containing all grid values. The rows of the grid are sequentially appended to this vector.}
  }
}

\section{Methods}{
  \describe{
      \item{\link{readRaster}}{Reads rasters from the ESRI asciifile format.}

    \item{\link{writeRaster}}{Writes rasters in the ESRI asciifile format.}

    \item{\link{image}}{Plots the rasclass-raster.}
	
  }
}

\references{
ESRI ASCII raster format definition.

\url{http://help.arcgis.com/en/arcgisdesktop/10.0/help/index.html#/ESRI_ASCII_raster_format/009t0000000z000000/}
}

\seealso{
\code{\link{rasclass-package}},
\code{\link{rasclass-class}},

\code{\link{readRaster}},
\code{\link{writeRaster}},

\code{\link{readRasterFolder}},
\code{\link{setRasclassData}},

\code{\link{buildFormula}},
\code{\link{checkRasclass}},

\code{\link{rasclassMlc}},
\code{\link{classifyRasclass}}
}

\keyword{classes}
\keyword{raster classification}
\keyword{landcover}
\keyword{multinom}
