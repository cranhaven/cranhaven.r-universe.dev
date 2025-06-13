\name{mtarnumreg}
\alias{mtarnumreg}

\title{Estimation of the number of regimes in a MTAR model}
\description{
Compute estimation of number of regimes by NAIC or Carlin and Chib methodology for a MTAR model
}
\usage{
mtarnumreg(ini_obj, level = 0.95, burn_m = NULL,niter_m = 1000,
iterprev = 500, chain_m = FALSE, list_m = FALSE,
NAIC = FALSE,ordersprev = list(maxpj = 2,maxqj = 0,maxdj = 0),
parallel = FALSE)
}

\arguments{
  \item{ini_obj}{class \dQuote{\code{regime_inipars}} object, here specificate l0_min, l0_max and method. Not NULL. Default l0_min = 2, l0_max = 3, method = 'KUO'}
  \item{level}{numeric type, confident interval for estimations. Default 0.95}
  \item{burn_m}{numeric type, number of initial runs. Default NULL (10\% of niter)}
  \item{niter_m}{numeric type, number of runs of MCMC. Default 1000}
  \item{iterprev}{numeric type, number of runs for pseudo values. Default 500}
  \item{chain_m}{logical type, if return chains of parameters. Default FALSE}
  \item{list_m}{logical type, if return list of regimes considered. Default FALSE}
  \item{NAIC}{logical type, if return estimation of number of regimes by NAIC (not run Carlin and Chip for l). Default FALSE}
  \item{ordersprev}{list type object with names (maxpj,maxqj,maxdj), maximum number of lags of each process consider in the pseudo values for each number of regimes considered . Default maxpj = 2,maxqj = 0, maxdj = 0}
  \item{parallel}{logical type, if package \code{parallel} should be used. Default FALSE}
}

\details{
  Two proposals to identify or estimate the number of regimes l are implemented. Metropolised Carlin and Chib methodology takes into account the changing dimension in the parameter vector when the number of regimes changes, that proposal is  Bayesian model selection. Other methodology consists in calculating the information criterion NAIC.
}
\value{
  Return a list type object of class \dQuote{\code{regime_number}}
  \item{tsregime}{ini_obj$tsregime_obj}
  \item{list_m}{if list_m TRUE list of models considered}
  \item{m_chain}{if chain_m TRUE chains of m}
  \item{estimates}{table of the proportions of m estimated}
  \item{final_m}{numeric type, final number of regimes estimated}

  If NAIC TRUE
  \item{tsregime}{ini_obj$tsregime_obj}
  \item{list_m}{list of consider models}
  \item{NAIC}{list type of NAIC for each considered model}
  \item{NAIC_final_m}{numeric type, final number of regimes by this criteria}
}
\references{
Calderon, S. and Nieto, F. (2017) \emph{Bayesian analysis of multivariate threshold autoregress models with missing data.} Communications in Statistics - Theory and Methods 46 (1):296–318. doi:10.1080/03610926.2014.990758.
}
\author{Valeria Bejarano \email{vbejaranos@unal.edu.co}, Sergio Calderon \email{sacalderonv@unal.edu.co} & Andrey Rincon \email{adrincont@unal.edu.co}}

\examples{
data("datasim")
data = datasim
initial = mtarinipars(tsregime_obj = data$Sim,
list_model = list(l0_max = 3),method = 'KUO')
\donttest{
estim = mtarnumreg(ini_obj = initial,iterprev = 500,niter_m = 500,
burn_m = 500, list_m = TRUE,ordersprev = list(maxpj = 2))
estim$final_m
}
}

\keyword{Bayesian estimation}
\keyword{Carlin and Chib}

