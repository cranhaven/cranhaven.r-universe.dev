\name{checkRasclass}
\alias{checkRasclass}
\alias{checkRasclass,rasclass-method}
\title{Check rasclass object for internal consistency}

\description{
This function checks wether a rasclass-class object is internally consistent.
}

\usage{checkRasclass(object)}

\arguments{
	\item{object}{
	A \code{\link{rasclass}} object.
	}
}

\seealso{
\code{\link{rasclass-package}},
\code{\link{rasclass-class}},
\code{\link{rasclassRaster-class}},

\code{\link{readRaster}},
\code{\link{writeRaster}},

\code{\link{readRasterFolder}},
\code{\link{setRasclassData}},

\code{\link{buildFormula}},

\code{\link{rasclassMlc}},
\code{\link{classifyRasclass}}
}

\examples{
# Instanciate rasclass object
object <- new('rasclass')

# Create artificial data
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

# The object passes the test at this point
checkRasclass(object)

# Manually change some values number of rows to an inconsitent value
object@gridSkeleton@nrows <- 12345
object@samplename <- 'wrongName'

# The rasclass object now fails the test
checkRasclass(object)
}