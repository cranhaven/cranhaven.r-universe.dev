\name{print.regime_model}
\alias{print.regime_model}

\title{print regime_model object for the function outputs mtarns and mtastr}

\description{
Print estimates for the results of the mtarns and mtarstr functions.
}
\usage{
\method{print}{regime_model}(object, ...)
}

\arguments{
\item{object}{Object of class \dQuote{\code{regime_model}}. Not NULL}
\item{...}{Other print parameters that affect.}
}

\details{
Print estimates outputs corresponding to functions \dQuote{\code{mtarns}} and \dQuote{\code{mtarstr}} which return an object of class \dQuote{\code{regime_model}}.
}
\value{
  Return to console.
}
\references{
Calderon, S. and Nieto, F. (2017) \emph{Bayesian analysis of multivariate threshold autoregress models with missing data.} Communications in Statistics - Theory and Methods 46 (1):296–318. doi:10.1080/03610926.2014.990758.
}
\seealso{\code{\link[BMTAR]{mtarns}}, \code{\link[BMTAR]{mtarstr}}}
\author{Valeria Bejarano \email{vbejaranos@unal.edu.co} & Andrey Rincon \email{adrincont@unal.edu.co}}

\examples{
data("datasim")
data = datasim$Sim$Zt
parameters = list(l = 1,orders = list(pj = 1))
initial = mtarinipars(tsregime_obj = tsregime(data),
                      list_model = list(pars = parameters))
estim1 = mtarns(ini_obj = initial,niter = 500,chain = TRUE,burn = 500)
print.regime_model(estim1)
}
