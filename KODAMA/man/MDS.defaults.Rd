\docType{data}
\name{MDS.defaults}
\alias{MDS.defaults}
\title{Default configuration for RMDS}
\format{
  An object of class \code{MDS.defaults} of length 1.
}
\usage{
  MDS.defaults
}
\description{
  A list with parameters customizing an MDS embedding. 
}
\details{
  dims: integer, Output dimensionality 
  }
\examples{
  # display all default settings
  MDS.defaults
  
  # create a new settings object with perplexity set to 100
  custom.settings = MDS.defaults
  custom.settings$dims = 3
  custom.settings
  
}
\keyword{datasets}
