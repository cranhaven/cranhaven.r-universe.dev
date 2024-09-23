\name{remove_missingdata}
\alias{remove_missingdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function used to remove missing data
}
\description{
It is used to remove samples which have NA/missing data in covariates.
}
\usage{
remove_missingdata(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data is a dataframe.
}
}
\value{
row index of the input data which don't have missing values. 
}
