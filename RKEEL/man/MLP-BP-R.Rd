\docType{class}
\name{MLP_BP_R}
\alias{MLP_BP_R}
\alias{R6_MLP_BP_R}
\title{MLP_BP_R KEEL Regression Algorithm}
\description{
MLP_BP_R Regression Algorithm from KEEL. 
}
\usage{
MLP_BP_R(train, test, hidden_layers, hidden_nodes, transfer,
   eta, alpha, lambda, test_data, validation_data,
   cross_validation, cycles, improve, tipify_inputs,
   save_all, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{hidden_layers}{hidden_layers. Default value = 2}
\item{hidden_nodes}{hidden_nodes. Default value = 15}
\item{transfer}{transfer. Default value = "Htan"}
\item{eta}{eta. Default value = 0.15}
\item{alpha}{alpha. Default value = 0.1}
\item{lambda}{lambda. Default value = 0.0}
\item{test_data}{test_data. Default value = TRUE}
\item{validation_data}{validation_data. Default value = FALSE}
\item{cross_validation}{cross_validation. Default value = FALSE}
\item{cycles}{cycles. Default value = 10000}
\item{improve}{improve. Default value = 0.01}
\item{tipify_inputs}{tipify_inputs. Default value = TRUE}
\item{save_all}{save_all. Default value = FALSE}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted values for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("autoMPG6_train")
data_test <- RKEEL::loadKeelDataset("autoMPG6_test")

#Create algorithm
algorithm <- RKEEL::MLP_BP_R(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{regression}

