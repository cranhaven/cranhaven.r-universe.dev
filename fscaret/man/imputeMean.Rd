\name{imputeMean}
\alias{impute.mean}
\title{
imputeMean
}
\description{
Secondary function imputes the mean to columns with NA data.
}
\usage{
impute.mean(x)
}
\arguments{
  \item{x}{
a vector to calculate mean
}
}

\author{
Jakub Szlek and Aleksander Mendyk
}
\examples{

library(fscaret)

# Make sample matrix
testData <- matrix(data=rep(1:5),ncol=10,nrow=15)

# Replace random values with NA's
n <- 15
replace <- TRUE
set.seed(1)

rand.sample <- sample(length(testData), n, replace=replace)
testData[rand.sample] <- NA 

# Print out input matrix
testData

# Record cols with missing values
missing.colsTestMatrix <- which(colSums(is.na(testData))>0)

for(i in 1:length(missing.colsTestMatrix)){

rowToReplace <- missing.colsTestMatrix[i]
testData[,rowToReplace] <- impute.mean(testData[,rowToReplace])

}

# Print out matrix with replaced NA's by column mean 
testData

}

\keyword{ math }
\keyword{ logic }