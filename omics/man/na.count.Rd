\name{na.count}
\alias{na.count}
\title{Count Missing Values}
\description{
    Counts the number of missing values.
}
\usage{
    na.count(X, margin, fraction=TRUE)
}
\arguments{
    \item{X}{an array, including a matrix.}
    \item{margin}{a vector giving the subscripts to count over.}
    \item{fraction}{whether to return the fraction of missing values.}
}
\value{
    The number (or fraction) of missing values.
}
