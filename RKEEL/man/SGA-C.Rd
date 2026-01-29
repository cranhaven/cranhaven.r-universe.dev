\docType{class}
\name{SGA_C}
\alias{SGA_C}
\alias{R6_SGA_C}
\title{SGA_C KEEL Classification Algorithm}
\description{
SGA_C Classification Algorithm from KEEL. 
}
\usage{
SGA_C(train, test, mut_prob_1to0, mut_prob_0to1, cross_prob,
   pop_size, evaluations, alfa, selection_type, k,
   distance, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{mut_prob_1to0}{mut_prob_1to0. Default value = 0.01}
\item{mut_prob_0to1}{mut_prob_0to1. Default value = 0.001}
\item{cross_prob}{cross_prob. Default value = 1}
\item{pop_size}{pop_size. Default value = 50}
\item{evaluations}{evaluations. Default value = 10000}
\item{alfa}{alfa. Default value = 0.5}
\item{selection_type}{selection_type. Default value = "orden_based"}
\item{k}{k. Default value = 1}
\item{distance}{distance. Default value = "Euclidean"}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::SGA_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

