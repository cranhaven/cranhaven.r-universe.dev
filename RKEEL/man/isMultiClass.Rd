\docType{methods}
\name{isMultiClass}
\alias{isMultiClass}
\title{Is Multi-class}
\description{
Method for check if a dataset is multi-class
}
\usage{
isMultiClass(data)
}
\arguments{
\item{data}{Dataset as data.frame}
}
\value{
Returns TRUE if the dataset is multi-class and FALSE if it is not.
}
\examples{
iris <- RKEEL::loadKeelDataset("iris")
isMultiClass(iris)
}
\keyword{utils}

