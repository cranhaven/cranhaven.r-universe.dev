\docType{class}
\name{CHC_C}
\alias{CHC_C}
\alias{R6_CHC_C}
\title{CHC_C KEEL Classification Algorithm}
\description{
CHC_C Classification Algorithm from KEEL. 
}
\usage{
CHC_C(train, test, pop_size, evaluations, alfa, restart_change,
   prob_restart, prob_diverge, k, distance, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{pop_size}{pop_size. Default value = 50}
\item{evaluations}{evaluations. Default value = 10000}
\item{alfa}{alfa. Default value = 0.5}
\item{restart_change}{restart_change. Default value = 0.35}
\item{prob_restart}{prob_restart. Default value = 0.25}
\item{prob_diverge}{prob_diverge. Default value = 0.05}
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
algorithm <- RKEEL::CHC_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

