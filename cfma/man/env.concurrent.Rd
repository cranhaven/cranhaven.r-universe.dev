\name{env.concurrent}
\alias{env.concurrent}
\docType{data}
\title{ Simulated data from the concurrent mediation model
}
\description{ "env.concurrent" is an R environment containing the data generated from a concurrent mediation model.
}
\usage{data("env.concurrent")}
\format{An R environment
	\describe{
		\item{\code{Z}}{a \eqn{n\times T} data matrix, treatment trajectory of \eqn{n} subjects for \eqn{T} time points.}
		\item{\code{M}}{a \eqn{n\times T} data matrix, mediator trajectory of \eqn{n} subjects for \eqn{T} time points.}
		\item{\code{Y}}{a \eqn{n\times T} data matrix, outcome trajectory of \eqn{n} subjects for \eqn{T} time points.}
		\item{\code{alpha}}{a length \eqn{T} vector model coefficient.}
		\item{\code{beta}}{a length \eqn{T} vector model coefficient.}
		\item{\code{gamma}}{a length \eqn{T} vector model coefficient.}
	}
}
\details{ The data was generated from the concurrent mediation model
	\deqn{M(t)=Z(t)\alpha(t)+\epsilon_{1}(t),}
	\deqn{R(t)=Z(t)\gamma(t)+M(t)\beta(t)+\epsilon_{2}(t).}
	\eqn{Z(t)} is the convolution of hemodynamic response function (HRF) and event onsets. 
}
\examples{
data(env.concurrent)
Z<-get("Z",env.concurrent)
M<-get("M",env.concurrent)
Y<-get("Y",env.concurrent)
}
\keyword{datasets}
