\docType{class}
\name{NU_SVM_C}
\alias{NU_SVM_C}
\alias{R6_NU_SVM_C}
\title{NU_SVM_C KEEL Classification Algorithm}
\description{
NU_SVM_C Classification Algorithm from KEEL. 
}
\usage{
NU_SVM_C(train, test, KernelType, C, eps, degree, gamma, coef0,
   nu, p, shrinking, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{KernelType}{KernelType. Default value = 1}
\item{C}{C. Default value = "RBF"}
\item{eps}{eps. Default value = 1000.0}
\item{degree}{degree. Default value = 0.001}
\item{gamma}{gamma. Default value = 10}
\item{coef0}{coef0. Default value = 0.01}
\item{nu}{nu. Default value = 0.1}
\item{p}{p. Default value = 1.0}
\item{shrinking}{shrinking. Default value = 1}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::NU_SVM_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

