\docType{class}
\name{ANR_F}
\alias{ANR_F}
\alias{R6_ANR_F}
\title{ANR_F KEEL Preprocess Algorithm}
\description{
ANR_F Preprocess Algorithm from KEEL.
}
\usage{
ANR_F(train, test, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the preprocessed data for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("zoo")
data_test <- RKEEL::loadKeelDataset("zoo")
  
#Create algorithm
algorithm <- RKEEL::ANR_F(data_train, data_test)
  
#Run algorithm
algorithm$run()
  
#See results
algorithm$preprocessed_test
}
\keyword{preprocess}

