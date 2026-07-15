\name{reliabilityPlot}
\alias{reliabilityPlot}

\title{Plots reliability plot of probabilities}
\description{
  Given probability scores \code{probScore} and  true probabilities \code{trueProb} the methods plots one against the other using a selected boxing method 
  which groups scores and probabilities to show calibration of probabilities in given probability bands.
  }
\usage{
reliabilityPlot(probScore, trueProb, titleText="", boxing="equipotent", 
                noBins=10, classValue = 1, printWeight=FALSE) 
}
\arguments{
  \item{probScore}{ A vector of predicted probabilities for a given class \code{classValue}. }
  \item{trueProb}{ A vector of true probabilities for a given \code{classValue}, should be of the same length as \code{probScore}. }
  \item{titleText}{The text of the graph title.}
  \item{boxing}{ One of \code{"unique"}, \code{"equidistant"} or \code{"equipotent"}, determines the grouping of probabilities. See details below.}
 \item{noBins}{The value of parameter depends on the parameter \code{boxing} and specifies the number of bins. See details below.}
  \item{classValue}{A class value (factor) or an index of the class value (integer) for which reliability plot is made.}
   \item{printWeight}{ A boolean specifying if box weights are to be printed.}
 }
\details{
   Depending on the specified \code{boxing} the probability scores are grouped in one of three possible ways
   \itemize{
\item \code{"unique"} each unique probability score forms its own box.
\item \code{"equidistant"} forms \code{noBins} equally wide boxes.
\item \code{"equipotent"} forms \code{noBins} boxes with equal number of scores in each box.
}  
The parameter \code{trueProb} can represent either probabilities (in [0, 1] range, in most cases these will be 0s or 1s), 
or the true class values from which the method will form 0 and 1 values corresponding to probabilities for class value \code{classValue}.
}
\value{
  A function returns a graph containing reliability plot on a current graphical device.
  }
  

\author{ Marko Robnik-Sikonja}

\seealso{
\code{\link{CORElearn}},
\code{\link{calibrate}}.
}
\examples{
# generate data consisting from 3 parts:
#  one part for training, one part for calibration, one part for testing
train <-classDataGen(noInst=200)
cal <-classDataGen(noInst=200)
test <- classDataGen(noInst=200)

# build random forests model with default parameters
modelRF <- CoreModel(class~., train, model="rf")
# prediction of calibration and test set
predCal <- predict(modelRF, cal, rfPredictClass=FALSE)
predTest <- predict(modelRF, test, rfPredictClass=FALSE)
destroyModels(modelRF) # no longer needed, clean up

# show reliability plot of uncalibrated test set
class1<-1
par(mfrow=c(1,2))
reliabilityPlot(predTest$prob[,class1], test$class, 
                titleText="Uncalibrated probabilities", classValue=class1) 

# calibrate for a chosen class1 and method using calibration set
calibration <- calibrate(cal$class, predCal$prob[,class1], class1=1, 
                         method="isoReg", assumeProbabilities=TRUE)
calTestProbs <- applyCalibration(predTest$prob[,class1], calibration)
# display calibrated probabilities
reliabilityPlot(calTestProbs, test$class, 
                titleText="Calibrated probabilities", classValue=class1) 

}
\keyword{ models }
\keyword{ classif }
