\name{rasclass-package}
\alias{rasclass}
\alias{rasclass-package}
\docType{package}
\title{Supervised Raster Image Classification}

\description{
This package is built to perform supervised, per-pixel based raster image classification.
}

\details{
The raster image classification is carried out by calling a sequence of functions that load data, calculate the classification grid and produce an accuracy assessment of the classification.

The package contains the \link{readRasterFolder} function to load a set of external data layers from a folder containing raster images in the ESRI asciigrid format (\emph{.asc} file extension). All files in the specified folder are parsed and stored in a \link{rasclass-class} object. The requirements for reading the input raster files is that they are all in the same projection, are aligned and have the same grid-size (i.e. all raster files should have the same header). Furthermore one raster file has to be specified as sample data. Alternatively, data can also converted into the rasclass format from a dataframe using the \link{setRasclassData} function.

For the classification, the package contains five supervised, per-pixel classification methods: Maximum Likelihood Classification, Multinomial Logistic Regression, Neural Networks, Random Forests and Support Vector Machines. There is only one classification function \link{classifyRasclass} and the algorithm can be specified with as an argument. The output of the classifications is the classified raster grid and standard accuracy assessment indicators, including user and producer accuracies, the overall accuracy, the confusion matrix and the kappa coefficient.
}

\author{Daniel Wiesmann \email{daniel.wiesmann@ist.utl.pt} and David Quinn \email{djq@mit.edu}}

\seealso{
\code{\link{rasclass-class}},
\code{\link{rasclassRaster-class}},

\code{\link{readRaster}},
\code{\link{writeRaster}},

\code{\link{readRasterFolder}},
\code{\link{setRasclassData}},

\code{\link{buildFormula}},
\code{\link{checkRasclass}},

\code{\link{rasclassMlc}},
\code{\link{classifyRasclass}}
}

\keyword{package}
\keyword{classification}
\keyword{raster}
\keyword{ascii}
\keyword{landcover}
\keyword{multinom}
\keyword{MLC}

\examples{
\dontrun{
# If available, load data from external folder
object <- readRasterFolder(path = "mypath", samplename = "mysample",
	filenames = c('myvar1.asc', 'myvar2.asc'))
}

# For this example, create artificial data
mysample <- c(rep(rep(c(1,2), each = 25), 25), rep(rep(c(3,4), each = 25), 25))
mysample <- mysample + sample(c(0, NA), 2500, replace = TRUE, prob = c(1, 50))
myvar1 <- rep(1:50, each = 50) + rnorm(2500, 0, 5)
myvar2 <- rep(rep(1:50), 50) + rnorm(2500, 0, 5)
newdata <- data.frame(mysample, myvar1, myvar2)

# Prepare a rasclass object using the dataframe and specifying raster properties
object <- new('rasclass')
object <- setRasclassData(newdata, ncols = 50, nrows = 50,
	xllcorner = 0, yllcorner = 0, cellsize = 1, NAvalue = -9999,
	samplename = 'mysample')

# Classify using each algorithm once
outlist <- list()
outlist[['maximumLikelihood']] <- classifyRasclass(object, method = 'maximumLikelihood')
summary(outlist[['maximumLikelihood']])

outlist[['logit']] <- classifyRasclass(object, method = 'logit')
summary(outlist[['logit']])

outlist[['neuralNetwork']] <- classifyRasclass(object, method = 'neuralNetwork')
summary(outlist[['neuralNetwork']])

outlist[['randomForest']] <- classifyRasclass(object, method = 'randomForest')
summary(outlist[['randomForest']])

outlist[['supportVector']] <- classifyRasclass(object, method = 'supportVector')
summary(outlist[['supportVector']])

# Store sample data as a rasclassRaster for display purposes
mysample.ras <- new('rasclassRaster')
mysample.ras@grid <- mysample
mysample.ras@nrows <- 50
mysample.ras@ncols <- 50
mysample.ras@xllcorner <- 0
mysample.ras@yllcorner <- 0
mysample.ras@cellsize <- 1
mysample.ras@NAvalue <- -9999

# Plot results of each classifier
opar <- par(mfrow = c(2, 3))
image(mysample.ras)
title('Sample data')
for(i in 1:length(outlist)) {
	image(outlist[[i]]@predictedGrid)
	title(names(outlist)[[i]])
}
par(opar)
}