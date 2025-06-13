\name{readRasterFolder}
\alias{readRasterFolder}
\alias{readRasterFolder,character-method}
\title{Load ESRI asciigrid Data for Classification}

\description{
This function automatically loads all ESRI asciigrid files from a specified folder into a \code{\link{rasclass-class}} object.
}

\usage{readRasterFolder(path, samplename = "sample", filenames = NULL,
	object = new("rasclass"), asInteger = FALSE)}

\arguments{
	\item{path}{
	A path to a folder that contains input raster files (\emph{.asc} extention).
	}
	\item{samplename}{
	An optional \code{character} string containing the name of the sample file, the default name is "sample" or equivalently "sample.asc".
	}
	\item{filenames}{
	An optional \code{character} vector containing the names of the files used as explanatory (dependent) variables in the classifictaion.
	}
	\item{object}{
	An optional \code{\link{rasclass-class}} object to store the data in.
	}
	\item{asInteger}{
	An optional logical variable, whether the data should be loaded as integer values to reduce the memory requirements.
	}
}

\value{
A \code{\link{rasclass-class}} object containing the loaded data as a dataframe in the \code{data} slot.
}

\details{
This function loads ESRI asciigrid files (\emph{.asc} file extention) that are found in the specified folder. All files in the folder will be loaded if not specified differently using the \emph{filenames} argument. The data is stored into the \code{data} slot of a \code{\link{rasclass-class}} object, wich can be used for classification using one of the rasclass algorithms. The names of the files provided are stored in the column names of the stored dataframe.

It is required that all the input raster files in the specified folder have the same extent and gridsize (i.e. have the same header) and are in the same projection. The rasclass classifier methods assume that all rasters are aligned and have he same grid size. An identical header and projection system of all the files assures this comparability of all input layers in the subsequent classification.

The rasclass classifiers are supervised classification algorithms and therefore a sample file has to be provided. The sample file contains the training cells for the models. The default sample file name is "sample", if the sample file has another name it can be specified using the optional argument \emph{samplename}. The ".asc" extension is not required in the filenames, they are added and stripped off, depending on the use of the names.
}

\section{Memory Issues}{
The \code{readRasterFolder} function only keeps track of data raster cells that have an observed value in every input layer provided, except for the sample layer. Therefore if any of the layers representing the independent variables in the classification has a NA value in a cell, none of the cell values will be stored. The resulting grid structure of the minimum area with complete inforamtion of each cell is stored as a dummy variable vector in the gridSkeleton slot.

If there are memory constraints when loading large raster files, the order of reading the input raster files can be specified using the filenames option to optimize memory usage. In that way, by specifying the input raster with the most \code{NA} values as first file to read, the number of cells kept from all the other raster files read subsequently is reduced.
}

\seealso{
\code{\link{rasclass-package}},
\code{\link{rasclass-class}},
\code{\link{rasclassRaster-class}},

\code{\link{readRaster}},
\code{\link{writeRaster}},

\code{\link{setRasclassData}},

\code{\link{buildFormula}},
\code{\link{checkRasclass}},

\code{\link{rasclassMlc}},
\code{\link{classifyRasclass}}
}


\examples{
\dontrun{
object <- readRasterFolder(path = "mypath", samplename = "mysample",
	filenames = c('myvar1.asc', 'myvar2.asc'))
object <- classifyRasclass(object)}
}

\keyword{raster classification}
\keyword{landcover}
\keyword{multinom}
