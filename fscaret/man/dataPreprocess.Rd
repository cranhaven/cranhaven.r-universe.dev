\name{dataPreprocess}
\alias{dataPreprocess}
\title{
dataPreprocess
}
\description{
The functionality is realized in two main steps:
\enumerate{
\item Check for near zero variance predictors and flag as near zero if:
\enumerate{
\item the percentage of unique values is less than 20% and
\item the ratio of the most frequent to the second most frequent value is greater than 20,
    }
\item Check for susceptibility to multicollinearity
\enumerate{
\item Calculate correlation matrix
\item Find variables with correlation 0.9 or more and delete them
    }
  }
}
\usage{
dataPreprocess(trainMatryca_nr, testMatryca_nr, labelsFrame, lk_col, lk_row, with.labels)
}

\arguments{
  \item{trainMatryca_nr}{
Input training data matrix
}
  \item{testMatryca_nr}{
Input testing data matrix
}
  \item{labelsFrame}{
Transposed data frame of column names
}
  \item{lk_col}{
Number of columns 
}
  \item{lk_row}{
Number of rows
}
  \item{with.labels}{
If with.labels=TRUE, additional data frame with preprocessed inputs corresponding to original data set column numbers as output is generated
}
}
\author{
Jakub Szlek and Aleksander Mendyk
}
\references{
Kuhn M. (2008) Building Predictive Models in R Using the caret Package \emph{Journal of Statistical Software} \bold{28(5)} \url{http://www.jstatsoft.org/}.
}
\examples{


library(fscaret)

# Create data sets and labels data frame
trainMatrix <- matrix(rnorm(150*120,mean=10,sd=1), 150, 120)

# Adding some near-zero variance attributes

temp1 <- matrix(runif(150,0.0001,0.0005), 150, 12)

# Adding some highly correlated attributes

sampleColIndex <- sample(ncol(trainMatrix), size=10)

temp2 <- matrix(trainMatrix[,sampleColIndex]*2, 150, 10)

# Output variable

output <- matrix(rnorm(150,mean=10,sd=1), 150, 1)

trainMatrix <- cbind(trainMatrix,temp1,temp2, output)

colnames(trainMatrix) <- paste("X",c(1:ncol(trainMatrix)),sep="")

# Subset test data set

testMatrix <- trainMatrix[sample(round(0.1*nrow(trainMatrix))),]

labelsDF <- data.frame("Labels"=paste("X",c(1:(ncol(trainMatrix)-1)),sep=""))

lk_col <- ncol(trainMatrix)
lk_row <- nrow(trainMatrix)

with.labels = TRUE

testRes <- dataPreprocess(trainMatrix, testMatrix,
			  labelsDF, lk_col, lk_row, with.labels)
			  
summary(testRes)

# Selected attributes after data set preprocessing
testRes$labelsDF

# Training and testing data sets after preprocessing
testRes$trainMatryca
testRes$testMatryca

}

\keyword{ univar }
\keyword{ robust }
