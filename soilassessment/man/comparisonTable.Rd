\name{comparisonTable}
\alias{comparisonTable}
\title{
A function for normalizing decision ranking table
}
\description{
This function normalizes the decision ranking table and determines consistency of the decisions
}
\usage{
comparisonTable(df)
}
\arguments{
  \item{df}{
A matrix of rank decisions with complete column names.
}
}
\details{
The column names of the rank-decision table should correspond with the names of the criteria maps
}
\value{
nmtx: a normalized pairwise comparison matrix
crt: consistency index and message on whether the input decisions are consistent for analysis
}
\references{
Barzilai J. and Golany B., 1990. Deriving Weights from Pairwise Comparison Matrices: the Additive Case. Operations Research Letters 9: 407–410.
}
\author{
Christian Thine Omuto
}
\seealso{
\code{\link{suitability}}, \code{\link{fertilitySuit}}
}
\examples{
data(nutrient)
library(FuzzyAHP)
comparisonTable(nutrient)
}
