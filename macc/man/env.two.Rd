\name{env.two}
\alias{env.two}
\docType{data}
\title{ Simulated two-level dataset
}
\description{ "env.three" is an R environment containing a data list generated from 50 subjects, and the parameter settings used to generate the data.
}
\usage{data("env.two")}
\format{ An R environment.
	\describe{
		\item{\code{data2}}{a list of length 50, each contains a data frame with 3 variables.}
		\item{\code{Theta}}{a 2 by 2 matrix, which is the population level model coefficients.}
		\item{\code{Sigma}}{the covariance matrix of the model error terms for the single level model.}
		\item{\code{n}}{a 50 by 1 matrix, is the number of trials for each subject.}
		\item{\code{Lambda}}{the covariance matrix of the model errors in the coefficient regression model.}
		\item{\code{A}}{a vector of length 50, is the \code{A} value in the single-level for each subject each session.}
		\item{\code{B}}{a vector of length 50, is the \code{B} value in the single-level for each subject each session.}
		\item{\code{C}}{a vector of length 50, is the \code{C} value in the single-level for each subject each session.}
	}
}
\details{ The number of subjects is \eqn{N = 50}. For each subject, the number of trials is a random draw from a Poisson distribution with mean 100. The population level coefficients are set to be \eqn{A = 0.5}, \eqn{C = 0.5} and \eqn{B = -1}, and the variances of the model errors are \eqn{\sigma_{1_{i}}^2 = 1}, \eqn{\sigma_{2_{i}}^2 = 4} and the correlation is \eqn{\delta = 0.5}. See Section 5.2 of the reference for details. This is a special case of the three-level data with \eqn{K=1}.
}
\references{Zhao, Y., & Luo, X. (2014). \emph{Estimating Mediation Effects under Correlated Errors with an Application to fMRI.} arXiv preprint arXiv:1410.7217.
}
\examples{
data(env.two)
dt<-get("data2",env.two)
}
\keyword{datasets}
