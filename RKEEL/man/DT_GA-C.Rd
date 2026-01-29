\docType{class}
\name{DT_GA_C}
\alias{DT_GA_C}
\alias{R6_DT_GA_C}
\title{DT_GA_C KEEL Classification Algorithm}
\description{
DT_GA_C Classification Algorithm from KEEL. 
}
\usage{
DT_GA_C(train, test, confidence, instancesPerLeaf,
   geneticAlgorithmApproach, threshold, numGenerations,
   popSize, crossoverProb, mutProb, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{confidence}{confidence. Default value = 0.25}
\item{instancesPerLeaf}{instancesPerLeaf. Default value = 2}
\item{geneticAlgorithmApproach}{geneticAlgorithmApproach. Default value = "GA-LARGE-SN"}
\item{threshold}{threshold. Default value = 10}
\item{numGenerations}{numGenerations. Default value = 50}
\item{popSize}{popSize. Default value = 200}
\item{crossoverProb}{crossoverProb. Default value = 0.8}
\item{mutProb}{mutProb. Default value = 0.01}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::DT_GA_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

