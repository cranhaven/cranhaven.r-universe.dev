\name{classifyRasclass}
\alias{classifyRasclass}
\alias{classifyRasclass,rasclass-method}
\title{Classsifier function for rasclass objects}

\description{
Classifies data stored rasclass objects with one of five different classification algorithms. Also predicts the resulting output grid and calculates accuracy measures (accuray matrix, overall accuracy and kappa coefficient).
}

\usage{classifyRasclass(rasclassObj, splitfraction = 1, method = 'logit', \dots)}

\arguments{
  \item{rasclassObj}{A \code{\link{rasclass-class}} object containing the data for classification.}
  
  \item{method}{An optional argument to choose the classification algorithm. The default is \sQuote{logit}, other options are \sQuote{maximumLikelihood}, \sQuote{neuralNetwork}, \sQuote{randomForest} or \sQuote{supportVector}.}
  
  \item{splitfraction}{An optional \code{numeric} argument specifying a fraction for in-sample verifictaion.}
  
  \item{\dots}{Optional additional arguments passed on to the classification functions. This can be used to run the classification algorithms with settings that are different from the default.}
}

\details{
	With this function, data is classified using one out of the following five classification algorithms: Maximum Likelihood Classification, Multinomial Logistic Regression, Neural Networks, Random Forests or Support Vector Machines (see section \sQuote{Classification methods} for details of each algorithm). The algorithm can be specified using the \code{method} argument, with \sQuote{logit} as default. Most classification algoritms are imported from other packages. To choose specific settings for the chosen algorithm, additional arguments specified in \sQuote{\dots} will be passed on to the classification function.
	
	The optional argument \sQuote{splitfraction} can be used for \emph{in-sample verfication}. If \sQuote{splitfraction} differs from zero, the data will be randomly split into two parts with the fraction specified. The classification model will then be trained on one fraction and the other fraction will be used for prediction. The accuracy measures are then calculated as an in-sample verification, only comparing accuracy in data that was not used for training the model. The \sQuote{training} slot of the rasclass object stores the splitting information as a logical vector.
	
	Details of each classification algortithm are described below.
}

\value{A \code{\link{rasclass-class}} object, containing the classified raster, the classification object itself and standard accuracy measures: accuracy matrix, user and producer accuracies, overall accuracy and the kappa coefficient. All the outputs are stored in their corresponding slots of the output object.
}

\section{Classification methods}{
	The classification methods used here are mostly imported from other packages. The description of the details of the classifiers can be found in the documentation files of the imported functions. An algorithm can be chosen using the \code{method} argument. To pass arguments to the chosen classifier, additional arguments can be given that are passed on to the classifier functions.

	\emph{Gaussian Maximum Likelihood Classifier}\cr
	The maximum likelihood classifier is implemented directly in this package. It is a parametric classifier and assumes normal probability distributions in each class. Detailed descriptions of the classifier can be found in the paper by Paola (2005) or in standard textbooks. Specify 'maximumLikelihood' in the methods argument to use this algorithm.
	
	\emph{Multinomial Logistic Regression}\cr
	The logit algorithm is imported from the \code{\link[nnet]{multinom}} function in the \code{\link[nnet]{nnet}} package. Details can be found in the documentation of the functions. Specify 'logit' in the methods argument to use this algorithm.
	
	\emph{Neural Networks}\cr
	Neural networks are implemented in the R Stuttgart Neural Network Simulator package. The function used for classification is the \code{\link[RSNNS]{mlp}} function from the \code{\link[RSNNS]{RSNNS}} package. Specify 'neuralNetwork' in the methods argument to use this algorithm.
	
	\emph{Random Forests}\cr
	The Random Forest classifier is imported from the \code{\link[randomForest]{randomForest}} package using the
	\code{\link[randomForest]{randomForest}} function. Specify 'randomForest' in the methods argument to use this algorithm.
	
	\emph{Support Vector Machines}\cr
	Support Vector Machines are implemented in R in the \code{e1071} package, which is an interface for the external C library \code{libsvm}. The function used for claassification is \code{\link[e1071]{svm}}. Specify 'supportVector' in the methods argument to use this algorithm.
}

\references{
Paola, J. D., Schowengerdt, R. A. (1995). A detailed comparison of backpropagation neural network and maximum-likelihood classifiers for urban land use classification. \emph{IEEE Transactions on Geoscience and Remote Sensing},  33(4), 981-996.
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
\code{\link{checkRasclass}},

\code{\link{rasclassMlc}}
}

\keyword{raster classification}
\keyword{landcover}

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