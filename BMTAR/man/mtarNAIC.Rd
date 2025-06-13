\name{mtarNAIC}
\alias{mtarNAIC}

\title{Compute NAIC of a MTAR model}
\description{
Compute the Non-linear Akaike information criterion (NAIC) of a \dQuote{\code{regime_model}} class object.
}
\usage{
mtarNAIC(regimemodel)
}

\arguments{
  \item{regimemodel}{object of class \dQuote{\code{regime_model}}}
}

\details{
Estimation of thresholds was made before starting the Bayesian procedure via the Non-linear Akaike information criterion (NAIC) (Tong, 1990), in MTAR model. The NAIC for a MTAR model with l regimes is:
 \deqn{
 NAIC = {\sum_{j = 1}^l AICj(r)}/{\sum_{j = 1}^l N_j}
 }
  \deqn{
  AICj(r) = Nj ln(|Sj/Nj|) + 2k \eta_j
 }
 Nj: number of observations in each regime.
 \deqn{
 S_{j}=\sum_{t:j_t = j}(y_t-YP)'(y_t-YP)
 }
  \deqn{
  YP =  \Phi_{0}^(j)+\sum_{i=1}^{p_j}\Phi_{i}^{(j)} Y_{t-i}+\sum_{i=1}^{q_j} \beta_{i}^{(j)} X_{t-i} + \sum_{i=1}^{d_j} \delta_{i}^{(j)} Z_{t-i}
 }
}
\value{
  Return a list type object:
 \item{AICj}{numeric type, AIC for each regime}
 \item{NAIC}{numeric type, NAIC value}
}
\references{
Calderon, S. and Nieto, F. (2017) \emph{Bayesian analysis of multivariate threshold autoregress models with missing data.} Communications in Statistics - Theory and Methods 46 (1):296–318. doi:10.1080/03610926.2014.990758.
}
\author{Valeria Bejarano \email{vbejaranos@unal.edu.co}, Sergio Calderon \email{sacalderonv@unal.edu.co} & Andrey Rincon \email{adrincont@unal.edu.co}}

\examples{
data("datasim")
data = datasim
# Estimate number of regimes with NAIC
initial1 = mtarinipars(tsregime_obj = data$Sim,
list_model = list(pars = list(l = 2,
orders = list(pj = c(1,1),dj = c(1,1)), r = 0.2)))
estruc1 = mtarns(ini_obj = initial1,niter = 100,chain = TRUE,burn = 100)

initial2 = mtarinipars(tsregime_obj = data$Sim,
list_model = list(pars = list(l = 2,
orders = list(pj = c(1,1),dj = c(1,1)), r = 0.3)))
estruc2 = mtarns(ini_obj = initial2,niter = 100,chain = TRUE,burn = 100)
#NAIC
mtarNAIC(estruc1)
mtarNAIC(estruc2)
}
\keyword{AIC}
\keyword{MTAR}
\keyword{NAIC}
