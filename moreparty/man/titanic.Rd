\name{titanic}
\alias{titanic}
\docType{data}
\title{Titanic dataset}

\description{
A dataset describing the passengers of the Titanic and their survival
}

\usage{data("titanic")}

\format{
  A data frame with 1309 observations and the following 5 variables.
  \describe{
    \item{\code{Survived}}{Factor. Whether one survived or not}
    \item{\code{Pclass}}{Factor. Passenger class}
    \item{\code{Sex}}{Factor. Sex}
    \item{\code{Age}}{Numeric vector. Age}
    \item{\code{Embarked}}{Factor. Port of embarkation}
  }
}

\examples{
data(titanic)
str(titanic)
}

\keyword{datasets}
