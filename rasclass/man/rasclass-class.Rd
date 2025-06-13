\name{rasclass-class}
\docType{class}
\title{Class "rasclass"}

\alias{rasclass-class}
\alias{image,rasclass-method}
\alias{summary,rasclass-method}
\alias{View,rasclass-method}

\description{
This class object is a container for all the variables used in the classification algorithms of the \code{\link{rasclass}} package. The methods provided in the package sequentially fill the slots with data and results.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("rasclass")}.
}

\section{Slots}{
  \describe{
    \item{\code{path}:}{Object of class \code{\link{character}}. The path from which the data is loaded.}
    
    \item{\code{data}:}{Object of class \code{\link{data.frame}}. The dataframe that contains the data from the loaded ascii files.}
    
    \item{\code{samplename}:}{Object of class \code{\link{character}}. The name of a column in the \code{data} slot that will be used as sample in the supervised classification.}
    
    \item{\code{formula}:}{Object of class \code{\link{formula}}, storing the formula passed on to the classification algorithm.}
    
    \item{\code{call}:}{Object of class \code{\link{call}}, storing the last classification call applied to the object.}
    
    \item{\code{gridSkeleton}:}{Object of class \code{\link{rasclassRaster}}, containing the skeleton of the output grid. In the structure of rasclass-class objects, only values that are different from null in every input layer (except the sample) are considered in the analysis.}

    \item{\code{training}:}{Object of class \code{\link{logical}}. Is used if the the argument 'splitfraction' of the 'classifyRasclass' function has been used to split data for an in-sample verification.  the vector used to split the data is stored in this slot.}

    \item{\code{maximumLikelihood}:}{Object of class \code{\link{list}}. This list is created when the Maximum Likelihood Classifiaction is preformed on the rasclass-class object. It containins the mean vectors and inverse covariance matrices for each class.}
    
    \item{\code{randomForest}:}{Object of class \code{\link{randomForest}}. This is created when using the Random Forest classification.}
    
    \item{\code{logit}:}{Object of class \code{\link{multinom}}. Created when using the Random Forest classification.}
    
    \item{\code{neuralNetwork}:}{Object of class, \code{\link{mlp}}. Created when using the Neural Network classification.}
    
    \item{\code{supportVector}:}{Object of class \code{\link{svm}}. Created when using the Support Vector Machines classification.}
    
    \item{\code{predictedGrid}:}{Object of class \code{\link{rasclassRaster}}. The predicted grid resulting from the classification.}
        
    \item{\code{overallAccuracy}:}{Object of class \code{\link{numeric}}. This slot is used to store the overall accuracy of the classification.}
    
    \item{\code{accuracyMatrix}:}{Object of class \code{\link{matrix}}. The accuracy matrix of the classification, including user and producer accuracies.}
    
    \item{\code{kappa}:}{Object of class \code{\link{numeric}}. The kappa accuracy coefficient of the classification, a measure for the accuracy of the classification.}
  }
}

\section{Methods}{
  \describe{
	
	\item{\link{buildFormula}}{Builds a formula for classification, based on column names of the data in rasclass object.}

	\item{\link{checkRasclass}}{Checks internal consistency of rasclass object.}
	
    \item{\link{classifyRasclass}}{Classification using one of five algorithms.}

    \item{\link{readRasterFolder}}{Loads data from several asciigrid files in a folder.}
    
	\item{\link{setRasclassData}}{Sets data in rasclass object based on a dataframe.}

    \item{\link{summary}}{Prints a summary of the rasclass-object.}

    \item{\link{image}}{Plots the classified raster from the rasclass-object.}

    \item{\link{View}}{Shows the data frame in the rasclass-object.}    
  }
}

\seealso{
\code{\link{rasclass-package}},
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


\keyword{classes}
\keyword{raster classification}
\keyword{landcover}
\keyword{multinom}
