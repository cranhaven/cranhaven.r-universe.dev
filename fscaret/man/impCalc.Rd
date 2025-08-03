\name{impCalc}
\alias{impCalc}
\title{
impCalc
}
\description{
impCalc function is designed to scale variable importance according to MSE and RMSE calculations.
It also stores the raw MSE, RMSE, F-measure and developed models if saveModel=TRUE. impCalc is low-level function, it shouldn't be used alone unless
user has trained models from caret package stored in RData files.
}
\usage{
impCalc(skel_outfile, xTest, yTest, lk_col, 
          labelsFrame,with.labels,regPred,classPred,saveModel,lvlScale)
}
\arguments{
  \item{skel_outfile}{
Skeleton name of output file
}
  \item{xTest}{
Input vector of testing data set
}
  \item{yTest}{
Output vector of testing data set
}
  \item{lk_col}{
Number of columns of whole data set
}
\item{labelsFrame}{
Labels to sort variable importance
}
\item{with.labels}{
Pass with.labels argument. It is advised to ALWAYS use labels as in some cases VarImp returns importance in descending values. If you insist turning with.labels FALSE, then make sure data base contains pure data and you read it (read.csv) to data.frame with option header=FALSE. 
}
\item{regPred}{
Indicating if regression predictions are computed. Logical value [TRUE/FALSE]. If regPred is set TRUE, then classPred should be set FALSE. 
}
\item{classPred}{
Indicating if classification predictions are computed. Possible values TRUE/FALSE. If classPred is set TRUE, then regPred should be set FALSE. Please be advised that importance is scaled according to F-measure. 
}
\item{saveModel}{
Logical value [TRUE/FALSE] if trained model should be embedded in final model.
}
\item{lvlScale}{
Indicating if use additional scaling. The option is especially usefull when large number of features are getting NA's or are not included in feature ranking. It levels the scores of the features taking the overall number of features. Default value is FALSE. Logical value [TRUE/FALSE].
}
}
\details{
impCalc function lists RData files in working directory assuming there are only models derived by caret. In a loop function loads models and tries to get the variable importance.
}
\author{
Jakub Szlek and Aleksander Mendyk
}
\examples{

\dontrun{
# 
# Hashed to comply with new CRAN check
# 
library(fscaret)

# Load dataset
data(dataset.train)
data(dataset.test)

# Make objects
trainDF <- dataset.train
testDF <- dataset.test
model <- c("lm","Cubist")
fitControl <- trainControl(method = "boot", returnResamp = "all") 
myTimeLimit <- 5
no.cores <- 2
supress.output <- TRUE
skel_outfile <- paste("_default_",sep="")
mySystem <- .Platform$OS.type
with.labels <- TRUE
redPred <- TRUE
classPred <- FALSE
saveModel <- FALSE
lvlScale <- FALSE

if(mySystem=="windows"){
no.cores <- 1
}

# Scan dimensions of trainDF [lk_row x lk_col]
lk_col = ncol(trainDF)
lk_row = nrow(trainDF)

# Read labels of trainDF
labelsFrame <- as.data.frame(colnames(trainDF))
labelsFrame <-cbind(c(1:ncol(trainDF)),labelsFrame)
# Create a train data set matrix
trainMatryca_nr <- matrix(data=NA,nrow=lk_row,ncol=lk_col)

row=0
col=0

for(col in 1:(lk_col)) {
   for(row in 1:(lk_row)) {
     trainMatryca_nr[row,col] <- (as.numeric(trainDF[row,col]))
    }
}

# Pointing standard data set train
xTrain <- data.frame(trainMatryca_nr[,-lk_col])
yTrain <- as.vector(trainMatryca_nr[,lk_col])


#--------Scan dimensions of trainDataFrame1 [lk_row x lk_col]
lk_col_test = ncol(testDF)
lk_row_test = nrow(testDF)

testMatryca_nr <- matrix(data=NA,nrow=lk_row_test,ncol=lk_col_test)

row=0
col=0

for(col in 1:(lk_col_test)) {
   for(row in 1:(lk_row_test)) {
     testMatryca_nr[row,col] <- (as.numeric(testDF[row,col]))
    }
}

# Pointing standard data set test
xTest <- data.frame(testMatryca_nr[,-lk_col])
yTest <- as.vector(testMatryca_nr[,lk_col])


# Calling low-level function to create models to calculate on
myVarImp <- regVarImp(model, xTrain, yTrain, xTest,
	    fitControl, myTimeLimit, no.cores, lk_col,
	    supress.output, mySystem)


myImpCalc <- impCalc(skel_outfile, xTest, yTest,
              lk_col,labelsFrame,with.labels,redPred,classPred,saveModel,lvlScale)

}

}

\keyword{ design }
\keyword{ models }
