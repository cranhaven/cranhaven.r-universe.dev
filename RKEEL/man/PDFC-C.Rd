\docType{class}
\name{PDFC_C}
\alias{PDFC_C}
\alias{R6_PDFC_C}
\title{PDFC_C KEEL Classification Algorithm}
\description{
PDFC_C Classification Algorithm from KEEL. 
}
\usage{
PDFC_C(train, test, C, d, tolerance, epsilon, PDRFtype,
   nominal_to_binary, preprocess_type, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{C}{C. Default value = 100.0}
\item{d}{d. Default value = 0.25}
\item{tolerance}{tolerance. Default value = 0.001}
\item{epsilon}{epsilon. Default value = 1.0E-12}
\item{PDRFtype}{PDRFtype. Default value = "Gaussian}
\item{nominal_to_binary}{nominal_to_binary. Default value = TRUE}
\item{preprocess_type}{preprocess_type. Default value = "Normalize"}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::PDFC_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

