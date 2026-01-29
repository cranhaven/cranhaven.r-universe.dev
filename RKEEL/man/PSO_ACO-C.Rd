\docType{class}
\name{PSO_ACO_C}
\alias{PSO_ACO_C}
\alias{R6_PSO_ACO_C}
\title{PSO_ACO_C KEEL Classification Algorithm}
\description{
PSO_ACO_C Classification Algorithm from KEEL. 
}
\usage{
PSO_ACO_C(train, test, max_uncovered_samples, min_saples_by_rule,
   max_iterations_without_converge, enviromentSize, numParticles,
   x, c1, c2, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{max_uncovered_samples}{max_uncovered_samples. Default value = 20}
\item{min_saples_by_rule}{min_saples_by_rule. Default value = 2}
\item{max_iterations_without_converge}{max_iterations_without_converge. Default value = 100}
\item{enviromentSize}{enviromentSize. Default value = 3}
\item{numParticles}{numParticles. Default value = 100}
\item{x}{x. Default value = 0.72984}
\item{c1}{c1. Default value = 2.05}
\item{c2}{c2. Default value = 2.05}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
\donttest{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::PSO_ACO_C(data_train, data_test, 
  max_iterations_without_converge=2, numParticles=5)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
}
\keyword{classification}

