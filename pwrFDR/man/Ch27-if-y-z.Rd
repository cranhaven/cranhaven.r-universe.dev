\name{if.y.z}
\alias{if.y.z}
\title{
  A helper function -- substitute \code{y}'s with a specified 'z'.

}
\description{
  A helper function -- in a numeric vector, substitute values equal to
  'y' with user specified 'z'. Included at the user level because it's
  useful for setting up batch jobs.
}
\usage{
  if.y.z(x, y = 0, z = 1)
}
\arguments{
  \item{x}{
    A numeric, character or boolean vector
  }
  \item{y}{
    The valued to be swapped out
  }
  \item{z}{
    The value which replaces swapped out values
  }
}
\value{
  A numeric, character or boolean vector, equal to the input vector,
  \code{x}, except with occurences \code{y} replaced with the value \code{z}
}
\author{
  Grant Izmirlian <izmirlian at nih dot gov>  
}
