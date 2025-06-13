\name{mtarmissing}
\alias{mtarmissing}

\title{Estimation of missing values of observed, covariate and threshold processes}
\description{
Estimation using Bayesian methodology of missing values in observed(output), covariate and threshold processes.
}
\usage{
mtarmissing(ini_obj,niter = 1000, chain = FALSE, level = 0.95,
burn = NULL, cU = 0.5, b = NULL)
}

\arguments{
  \item{ini_obj}{class \dQuote{\code{regime_inipars}} object, here specificate in pars: l, orders and r known. Not NULL}
  \item{niter}{numeric type, number of runs of MCMC. Default 1000}
  \item{chain}{logical type, if return chains of parameters. Default FALSE}
  \item{level}{numeric type, confident interval for estimations. Default 0.95}
  \item{burn}{numeric type, number of initial runs. Default NULL (10\% of niter)}
  \item{cU}{numeric type, coefficient of the diagonal covariance matrix of process Ut = (Zt,Xt). Default 0.5}
  \item{b}{numeric type greater or equal 1, autoregressive order of Ut = (Zt,Xt). Default NULL meaning 1}
}

\details{
  The MTAR model

  \eqn{Y_t= \phi^{(j)}_{0}+ \sum_{i=1}^{p_j} \phi_{i}^{(j)}Y_{t-i}+  \sum_{i=1}^{q_j}\beta_{i}^{(j)}X_{t-i} + \sum{i=1}^{d_j}\delta_{i}^{(j)}Z_{t-i} +\Sigma_{(j)}^{1/2} \epsilon_{t}  if  r_{j-1}< Z_t \le r_{j},}

is written into a state space model with regime-switching where the matrices depend on the threshold variable. In order to estimate the missing data in the observed vector \eqn{Y_t}, it is necessary to obtain samples of the full conditional distribution of the state vector \eqn{\alpha_t}, for all times \eqn{t=1,\cdots,T} using Kalman Filter. It is assumed that the process \eqn{U_t=(X_t,Z_t)} is a Markov chain, and in order to get samples of the full conditional distribution of \eqn{U_t, t=1,\cdots,T}, it is supposed that kernel and initial distribution
are Gaussian for simplicity. However, in the next updates, we are going to get flexibility at this point.
}
\value{
  Return list type object of class \dQuote{\code{regime_missing}}
  \item{tsregime}{ini_obj$tsregime_obj with estimated observations}
  \item{estimates}{confident interval and mean of estimated missing values}
  \item{Chain}{if chain TRUE, chains of the estimated missing values}
}
\references{
Calderon, S. and Nieto, F. (2017) \emph{Bayesian analysis of multivariate threshold autoregress models with missing data.} Communications in Statistics - Theory and Methods 46 (1):296–318. doi:10.1080/03610926.2014.990758.
}
\author{Valeria Bejarano \email{vbejaranos@unal.edu.co}, Sergio Calderon \email{sacalderonv@unal.edu.co} & Andrey Rincon \email{adrincont@unal.edu.co}}

\examples{
data("datasim")
yt = datasim$Sim
# some missing data
data_yt = yt$Yt
data_zt = yt$Zt
posNA = sample(c(1:500),8)
data_yt[c(posNA),] = c(NA,NA)
posNA = sample(c(1:500),8)
data_zt[c(posNA)] = NA
data_final = tsregime(data_yt,data_zt,r = yt$r)
autoplot.tsregime(data_final,1)
autoplot.tsregime(data_final,2)

initial = mtarinipars(tsregime_obj = data_final,
list_model = list(pars = list(l = 2,r = datasim$Sim$r,
orders = list(pj = c(1,1), qj = c(0,0),dj = c(0,0)))))
\donttest{
missingest = mtarmissing(ini_obj = initial,chain = TRUE,
niter = 500,burn = 500)
print(missingest)
autoplot.regime_missing(missingest,1)
datasim$Sim$Yt[is.na(data_yt[,1]),]
missingest$tsregime$Yt[is.na(data_yt[,1]),]
}
}

\keyword{Bayesian estimation}
\keyword{State Space Form}
