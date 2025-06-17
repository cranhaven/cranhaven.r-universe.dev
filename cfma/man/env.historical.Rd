\name{env.historical}
\alias{env.historical}
\docType{data}
\title{ Simulated data from the historical influence mediation model
}
\description{ "env.historical" is an R environment containing the data generated from a historical influence mediation model.
}
\usage{data("env.historical")}
\format{An R environment
	\describe{
		\item{\code{Z}}{a \eqn{n\times T} data matrix, treatment trajectory of \eqn{n} subjects for \eqn{T} time points.}
		\item{\code{M}}{a \eqn{n\times T} data matrix, mediator trajectory of \eqn{n} subjects for \eqn{T} time points.}
		\item{\code{Y}}{a \eqn{n\times T} data matrix, outcome trajectory of \eqn{n} subjects for \eqn{T} time points.}
		\item{\code{alpha}}{a \eqn{T\times T} matrix model coefficient.}
		\item{\code{beta}}{a \eqn{T\times T} matrix model coefficient.}
		\item{\code{gamma}}{a \eqn{T\times T} matrix model coefficient.}
	}
}
\details{ The data was generated from the historical influence mediation model
	\deqn{M(t)=\int_{\Omega_{t}^{1}}Z(s)\alpha(s,t)ds+\epsilon_{1}(t),}
    \deqn{Y(t)=\int_{\Omega_{t}^{2}}Z(s)\gamma(s,t)ds+\int_{\Omega_{t}^{3}}M(s)\beta(s,t)ds+\epsilon_{2}(t),}
    where \eqn{\alpha(s,t)}, \eqn{\beta(s,t)}, \eqn{\gamma(s,t)} are coefficient curves; \eqn{\Omega_{t}^{j}=[(t-\delta_{j})\vee 0,t]} for \eqn{j=1,2,3}. \eqn{Z(t)} is the convolution of hemodynamic response function (HRF) and event onsets. 
}
\examples{
data(env.historical)
Z<-get("Z",env.historical)
M<-get("M",env.historical)
Y<-get("Y",env.historical)
}
\keyword{datasets}
