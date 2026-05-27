\name{surg}
\alias{surg}
\docType{data}
\title{Intestinal surgery data
}
\description{ Data involves the number of malignant lymph nodes removed during intestinal surgery for \eqn{k=844} cancer patients.  For each patient, \eqn{n} is the total number of satellite nodes removed during surgery from a patient and \eqn{y} is the number of malignant nodes.}
\usage{data("surg")}
\format{
  A data frame of \eqn{(y_i, n_i)} for \eqn{i = 1,...,844}.
  \describe{
    \item{\code{y}}{number of malignant lymph nodes removed from the \eqn{i^{th}} patient}
    \item{\code{n}}{total number of lymph nodes removed from the \eqn{i^{th}} patient}
  }
}

\source{
Efron, B., 2016. "Empirical Bayes deconvolution estimates," \emph{Biometrika, 103}(1), pp. 1-20.
}

\keyword{datasets}
