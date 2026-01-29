\docType{methods}
\name{runSequential}
\alias{runSequential}
\title{Run Sequential}
\description{
Run a set of RKEEL algorithms in sequential.
}
\usage{
runSequential(algorithmList)
}
\arguments{
\item{algorithmList}{List of RKEEL Algorithms to be executed}
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

#Run algorithms
seq <- RKEEL::runSequential(algorithms)
}
\keyword{utils}

