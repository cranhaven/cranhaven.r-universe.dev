\name{if.0.rm}
\alias{if.0.rm}
\title{
  A helper function-- remove if zero.
}
\description{
  A helper function-- remove if zero. Included at the user level because
  it's useful for setting up batch jobs. 
}
\usage{
  if.0.rm(x)
}
\arguments{
  \item{x}{
    A numeric vector.
  }
}
\value{
  A numeric vector, equal to the input vector, \code{x}, except with 0's
  removed.
}
\author{
  Grant Izmirlian <izmirlian at nih dot gov>  
}
