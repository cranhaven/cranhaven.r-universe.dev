\docType{class}
\name{IterativePartitioningFilter_F}
\alias{IterativePartitioningFilter_F}
\alias{R6_IterativePartitioningFilter_F}
\title{IterativePartitioningFilter_F KEEL Preprocess Algorithm}
\description{
IterativePartitioningFilter_F Preprocess Algorithm from KEEL.
}
\usage{
IterativePartitioningFilter_F(train, test, numPartitions,
   filterType, confidence, itemsetsPerLeaf, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{numPartitions}{numPartitions. Default value = 5}
\item{filterType}{filterType. Default value = "consensus"}
\item{confidence}{confidence. Default value = 0.25}
\item{itemsetsPerLeaf}{itemsetsPerLeaf. Default value = 2}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the preprocessed data for both \code{train} and \code{test} datasets.
}
\examples{
\donttest{
data_train <- RKEEL::loadKeelDataset("car_train")
data_test <- RKEEL::loadKeelDataset("car_test")

#Create algorithm
algorithm <- RKEEL::IterativePartitioningFilter_F(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
}
\keyword{preprocess}

