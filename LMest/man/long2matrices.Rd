\name{long2matrices}
\alias{long2matrices}

\title{From data in the long format to data in array format}
\description{Function that transforms data in the long format to data in array format.}
\usage{
long2matrices(id, time = NULL, X = NULL, Y)
}
\arguments{
  \item{id }{vector of subjects id}
  \item{time }{vector of time occasions}
  \item{X }{matrix of covariates in long format}
  \item{Y }{matrix of responses in long format}
}
\value{
  \item{XX }{array of covariates (n x TT x nc)}
  \item{YY }{array of responses (n x TT x r)}
}

\author{Francesco Bartolucci, Silvia Pandolfi, University of Perugia (IT), http://www.stat.unipg.it/bartolucci}

\examples{
# Example based on SRHS data

# load SRHS data
data(data_SRHS_long)
dataSRHS <- data_SRHS_long[1:1600,]
head(dataSRHS)
X <- cbind(dataSRHS$gender-1, dataSRHS$race == 2 | dataSRHS$race == 3,
dataSRHS$education == 4,dataSRHS$education == 5, dataSRHS$age-50,
(dataSRHS$age-50)^2/100)
Y <- dataSRHS$srhs
res <- long2matrices(dataSRHS$id, X = X, Y = Y)
}

