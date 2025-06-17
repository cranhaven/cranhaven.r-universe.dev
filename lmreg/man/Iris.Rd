\name{Iris}
\alias{Iris}
\docType{data}
\title{
Fisher's Iris data
}
\description{
Measurements of four dimensions of flowers of three species of the plant Iris (Iris setosa, Iris versicolor, and Iris virginica).
}
\usage{data(Iris)}
\format{
  A data frame with 150 observations on the following 6 variables.
  \describe{
    \item{\code{Species_No}}{Species number}
    \item{\code{Petal_width}}{Petal width (in cm)}
    \item{\code{Petal_length}}{Petal length (in cm)}
    \item{\code{Sepal_width}}{Sepal width (in cm)}
    \item{\code{Sepal_length}}{Sepal length (in cm)}
    \item{\code{Species_name}}{Species names: \code{Setosa}, \code{Verginica} or \code{Versicolor}, a character vector}
  }
}

\source{
Fisher, R.A. (1936) The use of multiple measurements in taxonomic problems. Ann. Eugenics, 7, pp.179-188.
}

\examples{
data(Iris)
head(Iris)
}
\keyword{datasets}
