\name{env.single}
\alias{env.single}
\docType{data}
\title{ Simulated single-level dataset
}
\description{ "env.single" is an R environment containing a data frame of data generated from 500 time points, and the true model parameters.
}
\usage{data("env.single")}
\format{ An R environment
	\describe{
		\item{\code{data1}}{a data frame with \code{Z} the treatment assignment, \code{M} the mediator and \code{R} the interested outcome.}
		\item{\code{error1}}{a data frame with \code{E1} and \code{E2} the error time series of \code{M} and \code{R}, respectively.}
		\item{\code{theta}}{a 3 by 1 vector, which is the coefficients \code{(A,B,C)} of the model.}
		\item{\code{Sigma}}{a 2 by 2 matrix, which is the covariance matrix of two Gaussian white noise processes.}
		\item{\code{p}}{the order of the vector autoregressive (VAR) model.}
		\item{\code{W}}{a 2\code{p} by 2 matrix, which is the transition matrix of the VAR(\code{p}) model.}
		\item{\code{Delta}}{a 2 by 2 matrix, which is the covariance matrix of the initial condition of the Gaussian white noise processes.}
	}
}
\details{ The true parameters are set as follows. The number of time points is 500. The coefficients are set to be \eqn{A = 0.5}, \eqn{C = 0.5} and \eqn{B = -1}. The variances of the model errors are \eqn{\sigma_1^2 = 1}, \eqn{\sigma_2^2 = 4} and the correlation is \eqn{\delta = 0.5}. For the VAR model, we consider the case \eqn{p = 1}, and the parameter settings satisfy the stationarity condition.
}
\references{Zhao, Y., & Luo, X. (2017). \emph{Granger Mediation Analysis of Multiple Time Series with an Application to fMRI.} arXiv preprint arXiv:1709.05328.
}
\examples{
data(env.single)
dt<-get("data1",env.single)
}
\keyword{datasets}
