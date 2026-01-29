\docType{class}
\name{Tan_GP_C}
\alias{Tan_GP_C}
\alias{R6_Tan_GP_C}
\title{Tan_GP_C KEEL Classification Algorithm}
\description{
Tan_GP_C Classification Algorithm from KEEL.
}
\usage{
Tan_GP_C(train, test, population_size, max_generations,
   max_deriv_size, rec_prob, mut_prob, copy_prob, w1, w2,
   elitist_prob, support, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{population_size}{population_size. Default value = 150}
\item{max_generations}{max_generations. Default value = 100}
\item{max_deriv_size}{max_deriv_size. Default value = 20}
\item{rec_prob}{rec_prob. Default value = 0.8}
\item{mut_prob}{mut_prob. Default value = 0.1}
\item{copy_prob}{copy_prob. Default value = 0.01}
\item{w1}{w1. Default value = 0.7}
\item{w2}{w2. Default value = 0.8}
\item{elitist_prob}{elitist_prob. Default value = 0.06}
\item{support}{support. Default value = 0.03}
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
algorithm <- RKEEL::Tan_GP_C(data_train, data_test)
algorithm <- RKEEL::Tan_GP_C(data_train, data_test, population_size = 5, max_generations = 10)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
}
\keyword{classification}

