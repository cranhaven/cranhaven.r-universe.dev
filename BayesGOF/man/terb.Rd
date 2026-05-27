\name{terb}
\alias{terb}
\docType{data}
\title{ Terbinafine trial data
}
\description{ During several studies of the oral antifungal agent terbinafine, a proportion of the patients in the trial terminated treatment due to some adverse effects.  In the data set, \eqn{y_i} is the number of terminated treatments and \eqn{n_i} is the total number of patients in the in the \eqn{i^{th}} trial.
}
\usage{data("terb")}
\format{
 A data frame of \eqn{(y_i, n_i)} for \eqn{i = 1,...,41}.
  \describe{
    \item{\code{y}}{number of patients who terminated treatment early in the \eqn{i^{th}} trial}
    \item{\code{n}}{total number of patients in the \eqn{i^{th}} clinical trial}
  }
}

\source{ Young-Xu, Y. and Chan, K.A., 2008. "Pooling overdispersed binomial data to estimate event rate," \emph{BMC Medical Research Methodology, 8}(1), p. 58.
}
\keyword{datasets}
