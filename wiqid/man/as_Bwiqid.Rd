\name{Bwiqid-class}
\alias{Bwiqid-class}
\alias{as.Bwiqid}
\alias{as.Bwiqid.default}

\title{
Conversion to class Bwiqid (deprecated)
}
\description{
Convert output containing MCMC chains to the class \code{mcmcOutput}, with a warning. The class \code{Bwiqid} is deprecated, use \code{mcmcOutput} instead.
}
\usage{
as.Bwiqid(object, ...)

\method{as.Bwiqid}{default}(object, ...)
}

\arguments{
  \item{object}{
an object containing the MCMC chains; see Details.
}
  \item{\dots}{
named parameters to be passed to other methods.
}
}

\value{
An object of class \code{mcmcOutput}.

}

\author{
Mike Meredith.
}


\keyword{methods}

