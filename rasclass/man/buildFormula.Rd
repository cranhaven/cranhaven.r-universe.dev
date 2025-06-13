\name{buildFormula}
\alias{buildFormula}
\alias{buildFormula,rasclass-method}
\title{Build a formula for Raster Classification}

\description{
This function builds a formula from the dataframe from the \code{data} slot in the specified rasclass object.
}

\usage{buildFormula(object, varlist = NULL)}

\arguments{
  \item{object}{A \code{\link{rasclass-class}} object with non-empty \code{data} and \code{samplename} slots.}
  
  \item{varlist}{An optional \code{character} vector containing the names of the variables that shall be used in the classification algorithms.}
}

\details{
A formula is built automatically using all the columns in the dataframe from the \code{data} slot of the specified object. The formula is stored in the \code{call} slot of the \code{\link{rasclass-class}} object. The dependent variable in the formula will be the name specified in the \code{samplename} slot of the given input object.

If not all columns from the \code{data} slot should be used for classifiaction, the list of variables to include can be specified using the optional argument \emph{varlist}. The samplename is specified previously when adding data to the rasclass-class object with the corresponding functions and should not be included in the \code{varlist} argument.
}

\value{
A \code{\link{rasclass-class}} object with the newly built formula in the \code{formula} slot.
}	

\seealso{
\code{\link{rasclass-package}},
\code{\link{rasclass-class}},
\code{\link{rasclassRaster-class}},

\code{\link{readRaster}},
\code{\link{writeRaster}},

\code{\link{readRasterFolder}},
\code{\link{setRasclassData}},

\code{\link{checkRasclass}},

\code{\link{rasclassMlc}},
\code{\link{classifyRasclass}}
}

\examples{
\dontrun{
# Load data from external folder
object <- readRasterFolder(path = "mypath", samplename = "mysample",
	filenames = c('myvar1.asc', 'myvar2.asc'))
}

# For this example, create artificial data
mysample <- c(rep(rep(c(1,2), each = 25), 25), rep(rep(c(3,4), each = 25), 25))
mysample <- mysample + sample(c(0, NA), 2500, replace = TRUE, prob = c(1, 10))
myvar1 <- rep(1:50, each = 50) + rnorm(2500, 0, 5)
myvar2 <- rep(rep(1:50), 50) + rnorm(2500, 0, 5)
myvar3 <- sample(1:2500)
newdata <- data.frame(mysample, myvar1, myvar2, myvar3)

# Prepare a rasclass object using the dataframe and specifying raster properties
object <- new('rasclass')
object <- setRasclassData(newdata, ncols = 50, nrows = 50,
	xllcorner = 0, yllcorner = 0, cellsize = 1, NAvalue = -9999,
	samplename = 'mysample')

# Classify and show results using all columns
object <- classifyRasclass(object)
summary(object)

# Change formula to exclude one variable
object <- buildFormula(object, varlist = c('myvar1', 'myvar3'))

# Classify and show results
object <- classifyRasclass(object)
summary(object)
}

\keyword{formula}
\keyword{rasclass}
