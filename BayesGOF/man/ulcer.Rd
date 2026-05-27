\name{ulcer}
\alias{ulcer}
\docType{data}
\title{
Recurrent Bleeding of Ulcers}
\description{
The data consist of \eqn{k=40} randomized trials between 1980 and 1989 of a surgical treatment for stomach ulcers.  Each of the trials has an estimated log-odds ratio that measures the rate of occurrence of recurrent bleeding given the surgical treatment.
}
\usage{data("ulcer")}
\format{
  A data frame of \eqn{(y_i,} se\eqn{_i)} for \eqn{i = 1,...,40}.
  \describe{
    \item{\code{y}}{log-odds of the occurrence of recurrent bleeding in the \eqn{i^{th}} study}
    \item{\code{se}}{standard error of the log-odds for the \eqn{i^{th}} study}
  }
}
\source{ 
Sacks, H.S., Chalmers, T.C., Blum, A.L., Berrier, J., and Pagano, D., 1990. "Endoscopic hemostasis: an effective therapy for bleeding peptic ulcers," \emph{Journal of the American Medical Association, 264}(4), pp. 494-499.
}
\references{
Efron, B., 1996. "Empirical Bayes methods for combining likelihoods," \emph{Journal of the American Statistical Association, 91}(434), pp. 538-550.
}
\keyword{datasets}
