\name{makeJackControl}
\alias{makeJackControl}

\title{
  Construct a control object for delete-d estimation.
}
\description{
  A utility function for using the \code{delete-d} option of the \code{\link{Estimate.b.k}} function.
}
\usage{
makeJackControl(d, B)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
    The number of (random) arrivals in the sample to delete.
}
  \item{B}{
    The number of deleted samples to generate.
}
}

\value{
  A list with named control parameters.
}
\references{
  Musa, John D., and A. Iannino. 
  "Estimating the Total Number of Software Failures Using an Exponential Model."
  SIGSOFT Softw. Eng. Notes 16, no. 3 (July 1991): 80-84. doi:10.1145/127099.127123.
  }


\seealso{
    \code{\link{Estimate.b.k}}
}


