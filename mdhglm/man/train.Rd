\name{train}
\alias{train}
\docType{data}
\title{Train Accident Data
}
\description{
Agresti (2007) presented the train accident data in the UK between 1975 and 2003.
The respone variable y is annual number of collisions between trains
and road vehicles, for t million kilometers of train travel. 
The covariate x is number of years since 1975.
}
\usage{data("train")}
\format{
  A data frame with 29 observations on the following 4 variables.
  \describe{
    \item{\code{x}}{number of years since 1975}
    \item{\code{y}}{annual number of collisions between trains and road vehicles}
    \item{\code{t}}{million kilometers of annual train travel}
    \item{\code{id}}{oberservations identifier}
  }
}
\references{
Agresti, A. (2007). An Introduction to Categorical Data Analysis. John
Wiley & Sons, New York, 2nd edition.
}

