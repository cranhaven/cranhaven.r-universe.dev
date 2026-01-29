\docType{class}
\name{FCRA_C}
\alias{FCRA_C}
\alias{R6_FCRA_C}
\title{FCRA_C KEEL Classification Algorithm}
\description{
FCRA_C Classification Algorithm from KEEL.
}
\usage{
FCRA_C(train, test, generations, pop_size, length_S_C, WCAR,
   WV, crossover_prob, mut_prob, n1, n2, max_iter,
   linguistic_values, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{generations}{generations. Default value = 50}
\item{pop_size}{pop_size. Default value = 30}
\item{length_S_C}{length_S_C. Default value = 10}
\item{WCAR}{WCAR. Default value = 10.0}
\item{WV}{WV. Default value = 1.0}
\item{crossover_prob}{crossover_prob. Default value = 1.0}
\item{mut_prob}{mut_prob. Default value = 0.01}
\item{n1}{n1. Default value = 0.001}
\item{n2}{n2. Default value = 0.1}
\item{max_iter}{max_iter. Default value = 100}
\item{linguistic_values}{linguistic_values. Default value = 5}
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
algorithm <- RKEEL::FCRA_C(data_train, data_test, generations=10, pop_size=10)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
}
\keyword{classification}

