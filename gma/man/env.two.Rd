\name{env.two}
\alias{env.two}
\docType{data}
\title{ Simulated two-level dataset
}
\description{ "env.two" is an R environment containing a data list generated from 50 subjects, and the parameter settings used to generate the data.
}
\usage{data("env.two")}
\format{ An R environment.
	\describe{
		\item{\code{data2}}{a list of length 50, each contains a data frame with 3 variables.}
		\item{\code{error2}}{a list of length 50, each contains a data frame with 2 columns.}
		\item{\code{theta}}{a 3 by 1 vector, which is the population level coefficients \code{(A,B,C)} of the model.}
		\item{\code{Sigma}}{a 2 by 2 matrix, which is the covariance matrix of the two Gaussian white noise processes.}
		\item{\code{p}}{the order of the vector autoregressive (VAR) model.}
		\item{\code{W}}{a 2\code{p} by 2 matrix, which is the transition matrix of the VAR(\code{p}) model.}
		\item{\code{Delta}}{a 2 by 2 matrix, which is the covariance matrix of the initial condition of the Gaussian white noise processes.}
		\item{\code{n}}{a 50 by 1 matrix, is the number of time points for each subject.}
		\item{\code{Lambda}}{the covariance matrix of the model errors in the coefficient regression model.}
		\item{\code{A}}{a vector of length 50, is the \code{A} value in the single-level for each subject.}
		\item{\code{B}}{a vector of length 50, is the \code{B} value in the single-level for each subject.}
		\item{\code{C}}{a vector of length 50, is the \code{C} value in the single-level for each subject.}
	}
}
\details{ The true parameters are set as follows. The number of subjects i \eqn{N = 50}. For each subject, the number of time points is a random draw from a Poisson distribution with mean 100. The population level coefficients are set to be \eqn{A = 0.5}, \eqn{C = 0.5} and \eqn{B = -1}, and the variances of the Gaussian white noise process are assumed to be the same across participants with \eqn{\sigma_{1_{i}}^2 = 1}, \eqn{\sigma_{2_{i}}^2 = 4} and the correlation is \eqn{\delta = 0.5}. For the VAR model, we consider the case \eqn{p = 1}, and the parameter settings satisfy the stationarity condition.
}
\references{Zhao, Y., & Luo, X. (2017). \emph{Granger Mediation Analysis of Multiple Time Series with an Application to fMRI.} arXiv preprint arXiv:1709.05328.
}
\examples{
data(env.two)
dt<-get("data2",env.two)
}
\keyword{datasets}
