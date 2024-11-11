\name{if.na.x}
\alias{if.na.x}
\title{
  A helper function -- substitute 'NA's with a specified 'x'.
}
\description{
  A helper function -- substitute 'NA's with a specified 'x'. Included
  at the user level because it's useful for setting up batch jobs.
}
\usage{
  if.na.x(x, x0 = FALSE)
}
\arguments{
  \item{x}{
    A numeric or boolean vector.
  }
  \item{x0}{
    Value with which to replace \code{NA}'s. Defaults to 0 or \code{FALSE}.
  }
}
\value{
  A numeric vector, equal to the input vector, \code{x}, except with \code{NA}'s
  replaced by the value, \code{x0}, which the user suplied. 
}
\author{
  Grant Izmirlian <izmirlian at nih dot gov>  
}
