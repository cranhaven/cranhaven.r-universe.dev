\name{env.three}
\alias{env.three}
\docType{data}
\title{ Simulated three-level dataset
}
\description{ "env.three" is an R environment containing a data \code{list} generated from 50 subjects and 4 sessions, and the parameter settings used to generate the data.
}
\usage{data("env.three")}
\format{ An R environment.
	\describe{
		\item{\code{data3}}{a list of length 50, each contains a list of length 4 of a data frame with 3 variables.}
		\item{\code{Theta}}{a 2 by 2 matrix, which is the value of the fixed effects.}
		\item{\code{Sigma}}{the covariance matrix of the model error terms for the single level model.}
		\item{\code{n}}{a 50 by 4 matrix, is the number of trials for each subject each session.}
		\item{\code{Psi}}{the covariance matrix of the random effects in the mixed effects model.}
		\item{\code{Lambda}}{the covariance matrix of the model errors in the mixed effects model.}
		\item{\code{A}}{a 50 by 4 matrix, is the \code{A} value in the single-level for each subject each session.}
		\item{\code{B}}{a 50 by 4 matrix, is the \code{B} value in the single-level for each subject each session.}
		\item{\code{C}}{a 50 by 4 matrix, is the \code{C} value in the single-level for each subject each session.}
	}
}
\details{ The number of subjects is \eqn{N = 50} and the number of sessions is \eqn{K = 4}. Under each session of each subject, the number of trials is a random draw from a Poisson distribution with mean 100. The fixed effects are set to be \eqn{A = 0.5}, \eqn{C = 0.5}, and \eqn{B = -1}, and the variances of the model errors are \eqn{\sigma_{1_{ik}}^2 = 1}, \eqn{\sigma_{2_{ik}}^2 = 4} and the correlation is \eqn{\delta = 0.5}. See Section 5.2 of the reference for details.
}
\references{Zhao, Y., & Luo, X. (2014). \emph{Estimating Mediation Effects under Correlated Errors with an Application to fMRI.} arXiv preprint arXiv:1410.7217.
}
\examples{
data(env.three)
dt<-get("data3",env.three)
}
\keyword{datasets}
