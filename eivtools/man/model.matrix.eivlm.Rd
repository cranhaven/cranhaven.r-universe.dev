\name{model.matrix.eivlm}
\alias{model.matrix.eivlm}

\title{\code{model.matrix} method for objects of class \code{eivlm}.}

\description{
    Extract model matrix from \code{eivlm} object. Analogous to \code{model.matrix} method for \code{\link{lm}} objects.
}

\usage{
\method{model.matrix}{eivlm}(object, ...)
}

\arguments{
  \item{object}{
    A model object of class \code{eivlm}.
  }

  \item{\ldots}{
    See help for \code{\link{model.matrix}}.
  }
}

\value{
  Design matrix used in EIV regression.
}

\keyword{methods}

