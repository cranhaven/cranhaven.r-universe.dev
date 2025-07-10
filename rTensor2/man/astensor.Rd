\name{as.tensor}
\alias{as.tensor}
\title{Tensor Conversion}

\description{
Create a \href{/library/rTensor/help/Tensor-class}{Tensor-class} object from an array, matrix, or vector.
}
\usage{
  as.tensor(x, drop = FALSE)
}
\arguments{
  \item{x}{: an instance of array, matrix, or vector}
  \item{drop}{: whether or not modes equal to 1 should be dropped}
}
\value{
  a \href{/library/rTensor/help/Tensor-class}{Tensor-class} object

}
\author{
  Kyle Caudle

  Randy Hoover

  Jackson Cates
}
\references{
Imported from rTensor package version 1.4.8.
}
\examples{
#From vector
vec <- runif(3); vecT <- as.tensor(vec); vecT
#From matrix
mat <- matrix(runif(2*3),nrow=2,ncol=3)
matT <- as.tensor(mat); matT
#From array
indices <- c(2,3,4)
arr <- array(runif(prod(indices)), dim = indices)
arrT <- as.tensor(arr); arrT
}
