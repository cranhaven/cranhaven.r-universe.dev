\docType{class}
\name{GANN_C}
\alias{GANN_C}
\alias{R6_GANN_C}
\title{GANN_C KEEL Classification Algorithm}
\description{
GANN_C Classification Algorithm from KEEL.
}
\usage{
GANN_C(train, test, hidden_layers, hidden_nodes, transfer, eta,
   alpha, lambda, test_data, validation_data, cross_validation,
   BP_cycles, improve, tipify_inputs, save_all, elite,
   num_individuals, w_range, connectivity, P_bp, P_param,
   P_struct, max_generations, seed)
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
\item{BP_cycles}{BP_cycles. Default value = 10000}
\item{improve}{improve. Default value = 0.01}
\item{tipify_inputs}{tipify_inputs. Default value = TRUE}
\item{save_all}{save_all. Default value = FALSE}
\item{elite}{elite. Default value = 0.1}
\item{num_individuals}{num_individuals. Default value = 100}
\item{w_range}{w_range. Default value = 5.0}
\item{connectivity}{connectivity. Default value = 0.5}
\item{P_bp}{P_bp. Default value = 0.25}
\item{P_param}{P_param. Default value = 0.1}
\item{P_struct}{P_struct. Default value = 0.1}
\item{max_generations}{max_generations. Default value = 100}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::GANN_C(data_train, data_test, hidden_layers=1, 
  hidden_nodes=5, max_generations=5)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

