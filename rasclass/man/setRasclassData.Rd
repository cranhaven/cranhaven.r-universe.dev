\name{setRasclassData}
\alias{setRasclassData}
\alias{setRasclassData,data.frame-method}
\title{Add data from dataframe to rasclass object}

\description{
This function adds data from a dataframe to an existing \code{\link{rasclass-class}} object.
}

\usage{setRasclassData(newdata, object = new('rasclass'), ncols = NA, nrows = NA,
	xllcorner = NA, yllcorner = NA, cellsize = NA, NAvalue = NA,
	samplename = 'sample')}

\arguments{
	\item{newdata}{
	A data frame containing raster cell values in its columns.
	}
	\item{object}{
	An optional rasclass object where the data should be updated to, a new object is created if not specified.
	}
	\item{ncols}{
	The number of columns of the raster grid. Optional if the values are already specified in the rasclass object.
	}
	\item{nrows}{
	The number of rows of the raster grid. Optional if the values are already specified in the rasclass object.
	}
	\item{xllcorner}{
	Coordinates of the X coordinate of the lower left corner. Optional if the values are already specified in the rasclass object.
	}
	\item{yllcorner}{
	Coordinates of the Y coordinate of the lower left corner. Optional if the values are already specified in the rasclass object.
	}
	\item{cellsize}{
	The cell size of the grid. Optional if the values are already specified in the rasclass object.
	}
	\item{NAvalue}{
	The value in the raster that represents NA values. Optional if the values are already specified in the rasclass object.
	}
	\item{samplename}{
	Optional name of the column in the input data frame that will be used as sample data. The default is 'sample'.
	}
}

\value{
A \code{\link{rasclass-class}} object containing the loaded data as a dataframe in the \code{data} slot.
}

\details{
This function adds data from a dataframe to a rasclass object for subsequent classification using the classifiers from the rasclass package. Similar to the \code{\link{readRasterFolder}}, the name of the column containing the sample data has to be specified. The default sample column name is "sample".

The data in the input dataframe is assumed to be from raster files with the same projection, grid size and location. For ascii files this is equivalent to having an identical header in all the layers. If the common header has not been specified in the \code{gridSkeleton} slot of the input object before, the values have to be given as arguments to the function.
}

\section{Memory Issues}{
The \code{setRasclassData} function only keeps track of data raster cells that have an observed value (non-\code{NA}) in every column of the input dataframe provided.  The resulting grid structure will be retained in the gridSkeleton slot.}

\seealso{
\code{\link{rasclass-package}},
\code{\link{rasclass-class}},
\code{\link{rasclassRaster-class}},

\code{\link{readRaster}},
\code{\link{writeRaster}},

\code{\link{readRasterFolder}},

\code{\link{buildFormula}},
\code{\link{checkRasclass}},

\code{\link{rasclassMlc}},
\code{\link{classifyRasclass}}
}


\examples{
# For this example, create artificial data
mysample <- c(rep(rep(c(1,2), each = 25), 25), rep(rep(c(3,4), each = 25), 25))
mysample <- mysample + sample(c(0, NA), 2500, replace = TRUE, prob = c(1, 10))
myvar1 <- rep(1:50, each = 50) + rnorm(2500, 0, 5)
myvar2 <- rep(rep(1:50), 50) + rnorm(2500, 0, 5)
newdata <- data.frame(mysample, myvar1, myvar2)

# Prepare a rasclass object using the dataframe and specifying raster properties
object <- new('rasclass')
object <- setRasclassData(newdata, ncols = 50, nrows = 50,
	xllcorner = 0, yllcorner = 0, cellsize = 1, NAvalue = -9999,
	samplename = 'mysample')

# Summarize the rasclass object
summary(object)
}

\keyword{rasclass}
\keyword{data}
