\docType{methods}
\name{runParallel}
\alias{runParallel}
\title{Run Parallel}
\description{
Run a set of RKEEL algorithms in parallel
}
\usage{
runParallel(algorithmList, cores)
}
\arguments{
\item{algorithmList}{List of RKEEL Algorithms to be executed}
\item{cores}{Number of cores to execute in parallel. If it is not specified, it detects the cores automatically and execute the experiment in all of them}
}
\value{
Returns a list with the executed algorithms
}
\examples{
#Load datasets
iris_train <- RKEEL::loadKeelDataset("iris_train")
iris_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithms
learner_C45_C <- RKEEL::C45_C(iris_train, iris_test)
learner_KNN_C <- RKEEL::KNN_C(iris_train, iris_test)
learner_Logistic_C <- RKEEL::Logistic_C(iris_train, iris_test)
learner_LDA_C <- RKEEL::LDA_C(iris_train, iris_test)

#Create list
algorithms <- list(learner_C45_C, learner_KNN_C, learner_Logistic_C, 
  learner_LDA_C)

#Run algorithms in parallel in two cores
par <- RKEEL::runParallel(algorithms, 2)
}
\keyword{utils}

