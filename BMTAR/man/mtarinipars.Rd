\name{mtarinipars}
\alias{mtarinipars}

\title{Organization and check model specification}
\description{
Model specification of data, known or unknown parameters and its prior distributions
}
\usage{
mtarinipars(tsregime_obj, list_model = list(pars = list(l = 2,
orders = list(pj = c(1,1), qj = c(0,0), dj = c(0,0)), r = NULL, Sigma = NULL),
orders = NULL,l0_min = NULL,l0_max = NULL), method = NULL, theta_prior = NULL,
sigma_prior = NULL,gamma_prior = NULL, r_prior = NULL)
}
\arguments{
  \item{tsregime_obj}{class \dQuote{\code{tsregime}} object. Not NULL}
  \item{list_model}{list type object with at least one of the names (pars, orders, l0).

  \{pars: list type object with one of the names (l, orders, r, Sigma) for parameters of the model known

    (l: number of regimes of the model (not known use l0). Default 2

    orders:list type object with names (pj,qj,dj) each a vector of length l, specificate lags orders in each regime. Default list(pj = c(1,1), qj = c(0,0), dj = c(0,0))

    r: threshold value. Default NULL

    Sigma: list type object with names (R1, ..., Rl) each a matrix type object, specification of error covariate matrix. Default NULL)

  orders: list type object with names (pj,qj,dj) each a vector of length l,specificate maximum lags orders in each regime if not known. Default NULL

  l0_min: numeric type, number minimum of regimes to consider for the model if not known. Default NULL

  l0_max: numeric type, number maximum of regimes to consider for the model if not known. Default NULL\}
  }
  \item{method}{character type object, if orders not known or enter l0 it must be one ``KUO'' or ``SSVS'', when known it is not necessary. Default NULL}
  \item{theta_prior}{list type object with names (R1, ..., Rl), each one a list type object with at least one of the names (theta0j,cov0j)(if method ``SSVS'' (theta0j,cov0j,Cij,Tauij,R)),specification of mean and covariate matrix of the prior distribution for \eqn{\theta} parameter. Default NULL}
  \item{sigma_prior}{list type object with names (R1, ..., Rl), each one a list type object with at least one of the names (S0j,nu0j) specification of matrix and degrees of freedom of the prior distribution for \eqn{\Sigma} parameter. Default NULL}
  \item{gamma_prior}{list type object with names (R1, ..., Rl), each one a vector of prior probabilities for \eqn{\gamma} parameter. Default NULL}
  \item{r_prior}{list type object with at least one name (za, zb, val_rmh), each one a numeric type object with the minimum, maximum value for r and its parameter for Metropolis-Hasting respectively. Default NULL}
}

\details{
 list_model its a easy way to identify what need to be estimated in the MTAR model.
 First, pars refers to known parameters in the model like ``l'' number of regimes, ``orders'' lags for output, covariate and threshold processes, ``r'' threshold value and ``Sigma'' covariance error matrix for each regime.
 Also when lags orders or l0 are unknown this could be added to this list.
 Second, in order to identify autoregressive orders in MTAR models, two methods for stochastic search are selected because it permits us that the estimation is done in only one step.
 The first method, called Kuo and Mallick (Kuo), was introduced in (Kuo & Mallick, 1998) for variable selection in regression models. The second one was proposed in (George & McCulloch, 1993) and it is called Stochastic Search Variable Selection (SSVS).
 Then ``method'' refers to one of this two for estimating structural parameters of the MTAR model.
 Third, all related to prior distributions of our parameters of interest
  \deqn{\theta_j \sim N(\theta_0j,\Sigma_0j) in Regime j and \theta_j = vec(A_j) A_j = [\Phi_0 \Phi_1:p \beta_1:q \delta_1:d]}
  \deqn{\Sigma_j \sim W(S_0j,\nu_0j)}
  \deqn{\gamma_ij \sim Ber(p_ij0) where i = 1,..., k*p_j + nu*q_j + d}
  \deqn{ r \sim U(za,zb) and its proposal for MH algorithm U(-val_rmh,val_rmh)}
}
\value{
  Return a list type object of class \dQuote{\code{regime_inipars}}
  \item{tsregime_obj}{ = tsregime_obj}
  \item{pars}{= list_model$pars}
  \item{orders}{ = list_model$orders or list_model$pars$orders}
  \item{method}{ = method}
  \item{init$r}{ = r_prior}
  \item{init$Theta}{ = theta_prior}
  \item{init$Sigma}{ = sigma_prior}
  \item{init$Gamma}{ = gamma_prior}
}
\references{
Calderon, S. and Nieto, F. (2017) \emph{Bayesian analysis of multivariate threshold autoregress models with missing data.} Communications in Statistics - Theory and Methods 46 (1):296–318. doi:10.1080/03610926.2014.990758.
}
\author{Valeria Bejarano \email{vbejaranos@unal.edu.co} & Andrey Rincon \email{adrincont@unal.edu.co}}


\examples{
data("datasim")
tsregime_obj = datasim$Sim
# for estimate non-structural parameters:
# mtarns: l always known,
# Sigma = NULL = list(R1,R2) can be known, r = NULL can be known
# Sigma and r known
parameters = list(l = length(datasim$Reg),
Sigma = list(R1 = datasim$Reg$R1$sigma,R2 = datasim$Reg$R2$sigma),
r = tsregime_obj$r,
orders = list(pj = datasim$pj, qj = datasim$qj, dj = datasim$dj))
initpars_Sr = mtarinipars(tsregime_obj,list_model = list(pars = parameters))
#only r known
parameters = list(l = length(datasim$Reg),Sigma = NULL, r = tsregime_obj$r,
orders = list(pj = datasim$pj, qj = datasim$qj, dj = datasim$dj))
initpars_r = mtarinipars(tsregime_obj,list_model = list(pars = parameters))
#r and Sigma unknown
parameters = list(l = length(datasim$Reg),Sigma = NULL, r = NULL,
orders = list(pj = datasim$pj, qj = datasim$qj, dj = datasim$dj))
initpars = mtarinipars(tsregime_obj,list_model = list(pars = parameters))
# for estimate structural and non-structural parameters
# mtarstr: l always known
parameters = list(l = length(datasim$Reg))
orders = list(pj = c(2,2),dj = c(1,1))
initpars_KUO = mtarinipars(tsregime_obj,
list_model = list(pars = parameters,orders = orders),method = 'KUO')
initpars_SSVS = mtarinipars(tsregime_obj,
list_model = list(pars = parameters,orders = orders),method = 'SSVS')
# mtarnumreg l0_min or l0_max and method always
initpars_l = mtarinipars(tsregime_obj,list_model = list(l0_max = 3),method = 'KUO')
}

\keyword{prior distribution}
\keyword{Metropolis - Hasting}


