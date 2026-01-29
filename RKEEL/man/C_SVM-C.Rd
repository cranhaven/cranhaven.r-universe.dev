\docType{class}
\name{C_SVM_C}
\alias{C_SVM_C}
\alias{R6_C_SVM_C}
\title{C_SVM_C KEEL Classification Algorithm}
\description{
C_SVM_C Classification Algorithm from KEEL. 
}
\usage{
C_SVM_C(train, test, KernelType, C, eps, degree, gamma, coef0,
   nu, p, shrinking, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{KernelType}{KernelType. Default value = "RBF"}
\item{C}{C. Default value = 100.0}
\item{eps}{eps. Default value = 0.001}
\item{degree}{degree. Default value = 1}
\item{gamma}{gamma. Default value = 0.01}
\item{coef0}{coef0. Default value = 0.0}
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
algorithm <- RKEEL::C_SVM_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

