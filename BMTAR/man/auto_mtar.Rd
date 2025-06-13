\name{auto_mtar}
\alias{auto_mtar}

\title{Estimation of a MTAR model for some data}
\description{
Compute by Bayesian methodology a MTAR model for some data
}
\usage{
auto_mtar(Yt, Zt = NULL, Xt = NULL, l0_min = 2, l0_max = 3,
maxorders = list(pj = 2,qj = 0,dj = 0),
niter = 3000, chain = FALSE, method = 'KUO',parallel = FALSE)
}

\arguments{
  \item{Yt}{matrix type object, observed process. Not NULL}
  \item{Zt}{matrix type object, threshold process. Default NULL}
  \item{Xt}{matrix type object, covariate process. Default NULL}
  \item{l0_min}{numeric type between 1 and 4, number of regimes minimum to consider. Default 2}
  \item{l0_max}{numeric type between 1 and 4, number of regimes maximum to consider. Default 3}
  \item{maxorders}{list type object with names (pj,qj,dj), maximum lags consider for the processes in each regime. Default pj = 2, qj = 0,dj = 0}
  \item{niter}{numeric type, number of runs for every estimation. Default 3000}
  \item{chain}{logical type, if return chains of estimations parameters and values (if missing)}
  \item{method}{character type, must be one ``KUO'' or ``SSVS''}
  \item{parallel}{logical, if \code{parallel} package should be used. Default FALSE}
}

\details{
  The default arguments are designed for rapid estimation of models for any data (Yt, Zt and Xt). Returns the fit of MTAR model.
  The function conducts Bayesian estimation with ``niter'' chains of the number of regimes with maximum ``l0'' and within the maximum lags orders provided ``maxorders''.
  It can be a little be faster when used ``parallel'' for \code{parallel} package.
}
\value{
  Return list type object

  \item{tsregime}{class \dQuote{\code{tsregime}} object, if missing values completed with estimations}
  \item{numreg}{class \dQuote{\code{regime_number}}, number of regimes estimated}
  \item{pars}{class \dQuote{\code{regime_model}} object with final estimations of parameters}
}
\references{
Calderon, S. and Nieto, F. (2017) \emph{Bayesian analysis of multivariate threshold autoregress models with missing data.} Communications in Statistics - Theory and Methods 46 (1):296–318. doi:10.1080/03610926.2014.990758.
}
\author{Valeria Bejarano \email{vbejaranos@unal.edu.co}, Sergio Calderon \email{sacalderonv@unal.edu.co} & Andrey Rincon \email{adrincont@unal.edu.co}}

\examples{
  data('datasim')
  data = datasim$Sim
  \donttest{auto = auto_mtar(Yt = data$Yt, Zt = data$Zt,niter = 1000)}
}

\keyword{Bayesian estimation}
