\docType{class}
\name{SSGA_Integer_knn_FS}
\alias{SSGA_Integer_knn_FS}
\alias{R6_SSGA_Integer_knn_FS}
\title{SSGA_Integer_knn_FS KEEL Preprocess Algorithm}
\description{
SSGA_Integer_knn_FS Preprocess Algorithm from KEEL.
}
\usage{
SSGA_Integer_knn_FS(train, test, paramKNN, nEval, pop_size,
   numFeatures, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{paramKNN}{paramKNN. Default value = 1}
\item{nEval}{nEval. Default value = 5000}
\item{pop_size}{pop_size. Default value = 100}
\item{numFeatures}{numFeatures. Default value = 3}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the preprocessed data for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("car_train")
data_test <- RKEEL::loadKeelDataset("car_test")

#Create algorithm
algorithm <- RKEEL::SSGA_Integer_knn_FS(data_train, data_test)
algorithm <- RKEEL::SSGA_Integer_knn_FS(data_train, data_test, nEval = 10, pop_size = 10)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
\keyword{preprocess}

