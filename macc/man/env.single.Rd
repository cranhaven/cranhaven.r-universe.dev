\name{env.single}
\alias{env.single}
\docType{data}
\title{ Simulated single-level dataset
}
\description{ "env.single" is an R environment containing a data frame of data generated from 100 trials, the true coefficients and the coavariance matrix of the model errors.
}
\usage{data("env.single")}
\format{ An R environment.
	\describe{
		\item{\code{data1}}{a data frame with \code{Z} the treatment assignment, \code{M} the mediator and \code{R} the interested outcome.}
		\item{\code{Theta}}{a 2 by 2 matrix, which is the coefficients of the model.}
		\item{\code{Sigma}}{a 2 by 2 matrix, which is the covariance matrix of the model errors.}
		}
}
\details{ The number of subjects is 100. The coefficients are set to be \eqn{A = 0.5}, \eqn{C = 0.5} and \eqn{B = -1}. The variances of the model errors are \eqn{\sigma_1^2 = 1}, \eqn{\sigma_2^2 = 4} and the correlation is \eqn{\delta = 0.5}. See Section 5.1 of the reference.
}
\references{Zhao, Y., & Luo, X. (2014). \emph{Estimating Mediation Effects under Correlated Errors with an Application to fMRI.} arXiv preprint arXiv:1410.7217.
}
\examples{
data(env.single)
dt<-get("data1",env.single)
}
\keyword{datasets}
