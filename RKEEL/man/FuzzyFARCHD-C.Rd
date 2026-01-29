\docType{class}
\name{FuzzyFARCHD_C}
\alias{FuzzyFARCHD_C}
\alias{R6_FuzzyFARCHD_C}
\title{FuzzyFARCHD_C KEEL Classification Algorithm}
\description{
FuzzyFARCHD_C Classification Algorithm from KEEL. 
}
\usage{
FuzzyFARCHD_C(train, test, linguistic_values, min_support,
   max_confidence, depth_max, K, max_evaluations, pop_size,
   alpha, bits_per_gen, inference_type, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{linguistic_values}{linguistic_values. Default value = 5}
\item{min_support}{min_support. Default value = 0.05}
\item{max_confidence}{max_confidence. Default value = 0.8}
\item{depth_max}{depth_max. Default value = 3}
\item{K}{K. Default value = 2}
\item{max_evaluations}{max_evaluations. Default value = 15000}
\item{pop_size}{pop_size. Default value = 50}
\item{alpha}{alpha. Default value = 0.15}
\item{bits_per_gen}{bits_per_gen. Default value = 30}
\item{inference_type}{inference_type. Default value = 1}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}

}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::FuzzyFARCHD_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

