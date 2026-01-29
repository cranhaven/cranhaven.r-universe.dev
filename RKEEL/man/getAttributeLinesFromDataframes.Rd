\docType{methods}
\name{getAttributeLinesFromDataframes}
\alias{getAttributeLinesFromDataframes}
\title{Get attribute lines from data.frames}
\description{
Method for getting the attribute lines from data.frame objects
}
\usage{
getAttributeLinesFromDataframes(trainData, testData)
}
\arguments{
\item{trainData}{Train dataset as data.frame}
\item{testData}{Test dataset as data.frame}
}
\value{
Returns a list with the attribute names and types
}
\examples{
iris_train <- RKEEL::loadKeelDataset("iris_train")
iris_test <- RKEEL::loadKeelDataset("iris_test")

attributeLines <- getAttributeLinesFromDataframes(iris_train, iris_test)
}
\keyword{utils}

