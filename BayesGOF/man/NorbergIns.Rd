\name{NorbergIns}
\alias{NorbergIns}
\docType{data}
\title{ Norberg life insurance data 
}
\description{The number of claims \eqn{y_i} on a life insurance policy for each of \eqn{k=72} Norwegian occupational categories and the total number of years the workers in each category were exposed to risk (\eqn{E_i}).}
\usage{data("NorbergIns")}
\format{
  A data frame of the occupational group number (\code{group}), the number of deaths (\code{deaths}), and the years of exposure (\code{exposure}) for \eqn{i = 1,...,72}.
  \describe{
    \item{\code{group}}{Occupational group number}
    \item{\code{deaths}}{The number of deaths in the occupational group resulting in a claim on a life insurance policy.}
	\item{\code{exposure}}{The total number of years of exposure to risk for those who passed.}
	}
}
\source{ 
Norberg, R., 1989. "Experience rating in group life insurance," \emph{Scandinavian Actuarial Journal, 1989}(4), pp. 194-224.
}
\references{
Koenker, R. and Gu, J., 2017. "REBayes: An R Package for Empirical Bayes Mixture Methods," \emph{Journal of Statistical Software, Articles, 82}(8), pp. 1-26.
}
\keyword{datasets}
