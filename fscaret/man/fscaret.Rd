\name{fscaret}
\alias{fscaret}
\title{
feature selection caret
}
\description{
Main function for fast feature selection. It utilizes other functions as regPredImp or impCalc to obtain results in a list of data frames.
}
\usage{
fscaret(trainDF, testDF, installReqPckg = FALSE, preprocessData = FALSE,
	with.labels = TRUE, classPred = FALSE, regPred = TRUE, skel_outfile = NULL,
	impCalcMet = "RMSE&MSE", myTimeLimit = 24 * 60 * 60, Used.funcRegPred = NULL,
	Used.funcClassPred = NULL, no.cores = NULL, method = "boot", returnResamp = "all",
	missData=NULL, supress.output=FALSE, saveModel=FALSE, lvlScale=FALSE, ...)
}
\arguments{
  \item{trainDF}{
Data frame of training data set, MISO (multiple input single output) type
}
  \item{testDF}{
Data frame of testing data set, MISO (multiple input single output) type
}
  \item{installReqPckg}{
If TRUE prior to calculations it installs all required packages, please be advised to be logged as root (admin) user
}
  \item{preprocessData}{
If TRUE data preprocessing is performed prior to modeling
}
  \item{with.labels}{
If TRUE header of the input files are read
}
  \item{classPred}{
If TRUE classification models are applied. Please be advised that importance is scaled according to F-measure regardless impCalcMet settings.
}
  \item{regPred}{
If TRUE regression models are applied
}
  \item{skel_outfile}{
Skeleton output file, e.g. skel_outfile=c("_myoutput_") 
}
  \item{impCalcMet}{
Variable importance calculation scaling according to RMSE and MSE, for both please enter impCalcMet="RMSE&MSE"
}
  \item{myTimeLimit}{
Time limit in seconds for single model development 
}
  \item{Used.funcRegPred}{
Vector of regression models to be used, for all available models please enter Used.funcRegPred="all"
}
  \item{Used.funcClassPred}{
Vector of classification models to be used, for all available models please enter Used.funcClassPred="all"
}
  \item{no.cores}{
Number of cores to be used for modeling, if NULL all available cores are used, should be numeric type or NULL
}
  \item{method}{
Method passed to fitControl of caret package
}
  \item{returnResamp}{
Returned resampling method passed to fitControl of caret package
}
  \item{missData}{
Handling of missing data values. Possible values: "delRow" - delete observations with missing values, "delCol" - delete attributes with missing values, "meanCol" - replace missing values with column mean.
}
\item{supress.output}{
If TRUE output of modeling phase by caret functions are supressed. Only info which model is currently calculated and resulting variable importance.
}
\item{saveModel}{
Logical value [TRUE/FALSE] if trained model should be embedded in final model.
}
\item{lvlScale}{
Logical value [TRUE/FALSE] if additional scaling should be applied. For more information plase refer to impCalc().
}
  \item{\dots}{
Additional arguments, preferably passed to fitControl of caret package
}
}
\value{
\item{$ModelPred}{List of outputs from caret model fitting}
\item{$VarImp}{Data frames of variable importance and corresponding trained models}
\item{$PPlabels}{Data frame of resulting preprocessed data set with original input numbers and names}
\item{$PPTrainDF}{Training data set after preprocessing}
\item{$PPTestDF}{Testing data set after preprocessing}
\item{$VarImp$model}{Trained models}
}
\references{
Kuhn M. (2008) Building Predictive Models in R Using the caret Package \emph{Journal of Statistical Software} \bold{28(5)} \url{http://www.jstatsoft.org/}.
}
\note{
Be advised when using fscaret function as it requires hard disk operations for saving fitted models and data frames. Files are written in R temp session folder, for more details see tempdir(), getwd() and setwd()
}
\author{
Jakub Szlek and Aleksander Mendyk
}

\examples{

if((Sys.info()['sysname'])!="SunOS"){

library(fscaret)

# Load data sets
data(dataset.train)
data(dataset.test)

requiredPackages <- c("R.utils", "gsubfn", "ipred", "caret", "parallel", "MASS")

if(.Platform$OS.type=="windows"){

myFirstRES <- fscaret(dataset.train, dataset.test, installReqPckg=FALSE,
                  preprocessData=FALSE, with.labels=TRUE, classPred=FALSE,
                  regPred=TRUE, skel_outfile=NULL,
                  impCalcMet="RMSE&MSE", myTimeLimit=4,
                  Used.funcRegPred=c("lm"), Used.funcClassPred=NULL,
                  no.cores=1, method="boot", returnResamp="all",
                  supress.output=TRUE,saveModel=FALSE)

} else {

myCores <- 2

myFirstRES <- fscaret(dataset.train, dataset.test, installReqPckg=FALSE,
                  preprocessData=FALSE, with.labels=TRUE, classPred=FALSE,
                  regPred=TRUE, skel_outfile=NULL,
                  impCalcMet="RMSE&MSE", myTimeLimit=4,
                  Used.funcRegPred=c("lm","ppr"), Used.funcClassPred=NULL,
                  no.cores=myCores, method="boot", returnResamp="all",
                  supress.output=TRUE,saveModel=FALSE)

}



# Results
myFirstRES

}

}

\keyword{methods}
\keyword{iteration}
\keyword{optimize}
\keyword{array}
