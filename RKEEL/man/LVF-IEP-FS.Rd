\docType{class}
\name{LVF_IEP_FS}
\alias{LVF_IEP_FS}
\alias{R6_LVF_IEP_FS}
\title{LVF_IEP_FS KEEL Preprocess Algorithm}
\description{
LVF_IEP_FS Preprocess Algorithm from KEEL.
}
\usage{
LVF_IEP_FS(train, test, paramKNN, maxLoops, inconAllow, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{paramKNN}{paramKNN. Default value = 1}
\item{maxLoops}{maxLoops. Default value = 770}
\item{inconAllow}{inconAllow. Default value = 0}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the preprocessed data for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("car_train")
data_test <- RKEEL::loadKeelDataset("car_test")

#Create algorithm
algorithm <- RKEEL::LVF_IEP_FS(data_train, data_test)
algorithm <- RKEEL::LVF_IEP_FS(data_train, data_test, maxLoops = 30, 
  inconAllow=2)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
\keyword{preprocess}

