\name{readRaster}
\alias{readRaster}
\alias{readRaster,character-method}
\title{Read ESRI asciigrid files}

\description{
This function reads ESRI asciigrid files and stores the information in a \code{\link{rasclassRaster}} object.
}

\usage{readRaster(path, asInteger = FALSE)}

\arguments{
	\item{path}{
	A local path to the input ascii raster file.
	}
	\item{asInteger}{
	An optional logical argument. If set TRUE, the input data values will be rounded and stored as integers.
	}
}

\details{
The information from the header of the ESRI asciigrid raster (\emph{.asc} file extention) is stored in specific slots of the rasclassRaster object. The gridcell values are stored in a numeric vector. The rows of the raster grid are sequencially appended to that vector.

The optional argument allows to load data as integer to reduce memory requirements of the rasclassRaster object.
}

\value{
A \code{\link{rasclassRaster}} object.
}

\seealso{
\code{\link{rasclass-package}},
\code{\link{rasclass-class}},
\code{\link{rasclassRaster-class}},

\code{\link{writeRaster}},

\code{\link{readRasterFolder}},
\code{\link{setRasclassData}},

\code{\link{buildFormula}},
\code{\link{checkRasclass}},

\code{\link{rasclassMlc}},
\code{\link{classifyRasclass}}
}

\examples{
\dontrun{
myraster <- readRaster(path = "mypath", asInteger = FALSE)
image(myraster)}}

\references{
ESRI ASCII raster format definition.

\url{http://help.arcgis.com/en/arcgisdesktop/10.0/help/index.html#/ESRI_ASCII_raster_format/009t0000000z000000/}
}

\keyword{raster classification}
\keyword{ESRI ascii format}
\keyword{landcover}
\keyword{multinom}
