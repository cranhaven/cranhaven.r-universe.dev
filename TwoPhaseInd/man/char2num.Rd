\name{char2num}
\alias{char2num}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function used in acoarm to transform categorical variable to integers
}
\description{
Transform category data to integers 0..levels(data)-1. The the numeric 
variable can be then used in acoarm models.
}
\usage{
char2num(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data is a dataframe composed of categorical variables. 
}
}

\details{
The function transforms a categorical variable to integers.
}
\value{
A data frame of transformed values. For each column, each category is 
transformed to an integer, from 0 to levels(data[,column])-1.
}

\author{
James Y. Dai
}

\examples{
## Load the example data
data(acodata)
result <- char2num(acodata[, "fcgr2a.3"])
}

\keyword{ Transformation }