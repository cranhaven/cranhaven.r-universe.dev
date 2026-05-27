\name{galaxy}
\alias{galaxy}
\docType{data}
\title{ Galaxy Data
}
\description{The observed rotation velocities and their uncertainties of Low Surface Brightness (LSB) galaxies, along with the physical radius of the galaxy.}

\usage{data("galaxy")}
\format{
  A data frame of \eqn{(y_i, se_i, X_i)} for \eqn{i = 1,...,318}.
  \describe{
    \item{\code{y}}{actual observed (smoothed) velocity}
    \item{\code{se}}{uncertainty of observed velocity}
    \item{\code{X}}{physical radius of the galaxy}
  }
}

\source{De Blok, W.J.G., McGaugh, S.S., and Rubin, V. C., 2001. "High-resolution rotation curves of low surface brightness galaxies. II. Mass models," \emph{The Astronomical Journal, 122}(5), p. 2396. 
}

\keyword{datasets}
