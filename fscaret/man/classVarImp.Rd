\name{classVarImp}
\alias{classVarImp}
\title{
classVarImp
}
\description{
The function uses the caret package advantage to perform fitting of numerous classification models.
}
\usage{
classVarImp(model, xTrain, yTrain, xTest,
	  fitControl, myTimeLimit, no.cores,
	  lk_col, supress.output)
}
\arguments{
  \item{model}{
Chosed models as called from function fscaret(), argument Used.funcClassPred.
}
  \item{xTrain}{
Training data set, data frame of input vector
}
  \item{yTrain}{
Training data set, vector of observed outputs, must be in binary form 0/1.
}
  \item{xTest}{
Testing data set, data frame of input vector
}
  \item{fitControl}{
Fitting controls passed to caret function
}
  \item{myTimeLimit}{
Time limit in seconds for single model fitting
}
  \item{no.cores}{
Number of used cores for calculations
}
  \item{lk_col}{
Number of columns for whole data set (inputs + output)
}
  \item{supress.output}{
If TRUE output of models are supressed.
}
}
\references{
Kuhn M. (2008) Building Predictive Models in R Using the caret Package \emph{Journal of Statistical Software} \bold{28(5)} \url{http://www.jstatsoft.org/}.
}
\author{
Jakub Szlek and Aleksander Mendyk
}
\keyword{ models }
\keyword{ robust }
