\name{autoplot.regime_model}
\alias{autoplot.regime_model}

\title{regime_model object ggplot for the outputs on the function outputs mtarns and mtastr}

\description{
Produces a ggplot object for the results of the mtarns and mtarstr functions.
}
\usage{
\method{autoplot}{regime_model}(object, type = 1,...)
}

\arguments{
\item{object}{Object of class \dQuote{\code{regime_model}}. Not NULL}
\item{type}{character string giving the type of plot to be computed. Allowed
values are 1 for "\code{Thresholds chains}" (the default), 2 for \dQuote{\code{Sigma chains}}, 3 for \dQuote{\code{Theta chains}}, 4 for \dQuote{\code{Gamma chains}} or 5 for \dQuote{\code{Output process fit}}
}
\item{...}{other arguments passed to specific methods}
}

\details{
Graph the strings for the outputs corresponding to the functions \dQuote{\code{mtarns}} and \dQuote{\code{mtarstr}} which return an object of class \dQuote{\code{regime_model}}. The chains corresponding to the samplings in each case do not contain the burning period.
}
\value{
  Return a ggplot object.
}
\references{
Calderon, S. and Nieto, F. (2017) \emph{Bayesian analysis of multivariate threshold autoregress models with missing data.} Communications in Statistics - Theory and Methods 46 (1):296–318. doi:10.1080/03610926.2014.990758.
}
\seealso{\code{\link[BMTAR]{mtarns}}, \code{\link[BMTAR]{mtarstr}}}
\author{Valeria Bejarano \email{vbejaranos@unal.edu.co} & Andrey Rincon \email{adrincont@unal.edu.co}}

\examples{
library(ggplot2)
data("datasim")
data = datasim$Sim$Zt
parameters = list(l = 1,orders = list(pj = 1))
initial = mtarinipars(tsregime_obj = tsregime(data),
                      list_model = list(pars = parameters))
estim1 = mtarns(ini_obj = initial,niter = 500,chain = TRUE)
\donttest{
autoplot.regime_model(estim1,2)
autoplot.regime_model(estim1,3)
}
autoplot.regime_model(estim1,5)
}
