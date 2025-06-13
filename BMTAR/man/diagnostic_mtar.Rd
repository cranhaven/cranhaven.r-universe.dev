\name{diagnostic_mtar}
\alias{diagnostic_mtar}

\title{Residual diagnosis for model MTAR}
\description{
Tests to help evaluate some assumptions about the MTAR model. calculating some tests and graphs.
}
\usage{
diagnostic_mtar(regime_model,lagmax = NULL,alpha = '0.05')
}
\arguments{
 \item{regime_model}{Object of class \dQuote{\code{regime_model}}. Not NULL}
 \item{lagmax}{maximum lag at which to calculate the acf and pacf. Default NULL}
 \item{alpha}{level of significance for the graphs, should take values in
  c('0.10','0.05','0.025','0.01','0.005'). Default '0.05'}
}

\details{
  For the graphical tests it returns: \dQuote{\code{Residuals plot}} and \dQuote{\code{Residuals density plot}} (overlaps a standard normal density),\dQuote{\code{Residuals plot}} and \dQuote{\code{Residuals plot}}, \dQuote{\code{CUSUM}} statistic for residuals, \dQuote{\code{ACF}} and \dQuote{\code{PACF}} plots for residuals series.
}
\value{
Returns a list of ggplot objects with the graphics mentioned before.
}
\references{
Calderon, S. and Nieto, F. (2017) \emph{Bayesian analysis of multivariate threshold autoregress models with missing data.} Communications in Statistics - Theory and Methods 46 (1):296–318. doi:10.1080/03610926.2014.990758.
}
\author{Valeria Bejarano \email{vbejaranos@unal.edu.co}, Sergio Calderon \email{sacalderonv@unal.edu.co} & Andrey Rincon \email{adrincont@unal.edu.co}}
\examples{
library(ggplot2)
data("datasim")
data = datasim$Sim$Z
parameters = list(l = 1,orders = list(pj = 1))
initial = mtarinipars(tsregime_obj = tsregime(data),
                      list_model = list(pars = parameters))
estim1 = mtarns(ini_obj = initial,niter = 500,chain = TRUE,burn = 500)
diagnostic_mtar(estim1)
}

