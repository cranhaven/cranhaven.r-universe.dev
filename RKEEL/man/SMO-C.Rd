\docType{class}
\name{SMO_C}
\alias{SMO_C}
\alias{R6_SMO_C}
\title{SMO_C KEEL Classification Algorithm}
\description{
SMO_C Classification Algorithm from KEEL. 
}
\usage{
SMO_C(train, test, C, toleranceParameter, epsilon,
   RBFKernel_gamma, normalized_PolyKernel_exponent,
   normalized_PolyKernel_useLowerOrder, PukKernel_omega,
   PukKernel_sigma, StringKernel_lambda,
   StringKernel_subsequenceLength,
   StringKernel_maxSubsequenceLength, StringKernel_normalize,
   StringKernel_pruning, KernelType, FitLogisticModels,
   ConvertNominalAttributesToBinary, PreprocessType, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{C}{C. Default value = 1.0}
\item{toleranceParameter}{toleranceParameter. Default value = 0.001}
\item{epsilon}{epsilon. Default value = 1.0e-12}
\item{RBFKernel_gamma}{RBFKernel_gamma. Default value = 0.01}
\item{normalized_PolyKernel_exponent}{normalized_PolyKernel_exponent. Default value = 1}
\item{normalized_PolyKernel_useLowerOrder}{normalized_PolyKernel_useLowerOrder. Default value = FALSE}
\item{PukKernel_omega}{PukKernel_omega. Default value = 1.0}
\item{PukKernel_sigma}{PukKernel_sigma. Default value = 1.0}
\item{StringKernel_lambda}{StringKernel_lambda. Default value = 0.5}
\item{StringKernel_subsequenceLength}{StringKernel_subsequenceLength. Default value = 3}
\item{StringKernel_maxSubsequenceLength}{StringKernel_maxSubsequenceLength. Default value = 9}
\item{StringKernel_normalize}{StringKernel_normalize. Default value = FALSE}
\item{StringKernel_pruning}{StringKernel_pruning. Default value = "None"}
\item{KernelType}{KernelType. Default value = "PolyKernel"}
\item{FitLogisticModels}{FitLogisticModels. Default value = FALSE}
\item{ConvertNominalAttributesToBinary}{ConvertNominalAttributesToBinary. Default value = TRUE}
\item{PreprocessType}{PreprocessType. Default value = "Normalize"}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::SMO_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

