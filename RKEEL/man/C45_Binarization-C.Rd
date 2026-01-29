\docType{class}
\name{C45Binarization_C}
\alias{C45Binarization_C}
\alias{R6_C45Binarization_C}
\title{C45Binarization_C KEEL Classification Algorithm}
\description{
C45Binarization_C Classification Algorithm from KEEL. 
}
\usage{
C45Binarization_C(train, test, pruned, confidence, instancesPerLeaf,
   binarization, scoreFunction, bts)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{pruned}{pruned. Default value = TRUE}
\item{confidence}{confidence. Default value = 0.25}
\item{instancesPerLeaf}{instancesPerLeaf. Default value = 2}
\item{binarization}{binarization. Default value = "OVO"}
\item{scoreFunction}{scoreFunction. Default value = "WEIGHTED"}
\item{bts}{bts. Default value = 0.05}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")
  
#Create algorithm
algorithm <- RKEEL::C45Binarization_C(data_train, data_test)
  
#Run algorithm
algorithm$run()
  
#See results
algorithm$testPredictions
}
\keyword{classification}

