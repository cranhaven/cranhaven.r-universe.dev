\name{writeRaster}
\alias{writeRaster}
\alias{writeRaster,rasclassRaster-method}
\title{Load ESRI asciigrid Data for Classification}

\description{
This function exports the information in a \code{\link{rasclassRaster}} object into an external ESRI asciigrid file.
}


\usage{writeRaster(object, path = "predictedGrid.asc")}

\arguments{
	\item{object}{
	A \code{\link{rasclassRaster}} object.
	}
	\item{path}{
	Optional path to write ascii file, the default writes a file "predictedGrid.asc" into the workspace folder.
	}
}

\references{
ESRI ASCII raster format definition.

\url{http://help.arcgis.com/en/arcgisdesktop/10.0/help/index.html#/ESRI_ASCII_raster_format/009t0000000z000000/}
}

\seealso{
\code{\link{rasclass-package}},
\code{\link{rasclass-class}},
\code{\link{rasclassRaster-class}},

\code{\link{readRaster}},

\code{\link{readRasterFolder}},
\code{\link{setRasclassData}},

\code{\link{buildFormula}},
\code{\link{checkRasclass}},

\code{\link{rasclassMlc}},
\code{\link{classifyRasclass}}
}

\examples{
\dontrun{
writeRaster(myRaster, path = "mypath")
}
}

\keyword{raster classification}
\keyword{landcover}
\keyword{multinom}
