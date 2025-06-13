\name{mtarsim}
\alias{mtarsim}

\title{Multivariate threshold autoregressive process simulation}
\description{
Given an list object of the class \dQuote{\code{regime}} (length = l) with the model specification simulates N observations for a MTAR (Multivariate threshold autoregressive process) process.
}
\usage{
mtarsim(N, Rg, r = NULL, Xt = NULL, Zt = NULL, seed = NULL)
}
\arguments{
  \item{N}{numeric type greater than 0. Number of observation to simulate. Not NULL}
  \item{Rg}{list type object of length l number of regimes of the process with names (R1, ..., Rl), each a class \dQuote{\code{regime}} object. Not NULL}
  \item{r}{numeric type of length l - 1, threshold value (within the range of Z_t). Default NULL}
  \item{Xt}{matrix \eqn{(Nx\nu)} type object, covariate process (admit NA values). Default NULL}
  \item{Zt}{matrix \eqn{(Nx1)} type object, threshold process (admit NA values). Default NULL}
  \item{seed}{numeric type, set a seed for simulation}
}

\details{
Given a list of length l of object of class \dQuote{\code{regime}} (model specification), it simulates observations of a MTAR process ($ Sim) and returns them an object of the class \dQuote{\code{mtarsim}}. We have an MTAR process is given by:
  \deqn{
Y_t= \Phi_{0}^(j)+\sum_{i=1}^{p_j}\Phi_{i}^{(j)} Y_{t-i}+\sum_{i=1}^{q_j} \beta_{i}^{(j)} X_{t-i} + \sum_{i=1}^{d_j} \delta_{i}^{(j)} Z_{t-i}+ \Sigma_{(j)}^{1/2} \epsilon_{t}
}
\deqn{if r_{j-1}< Z_t \leq r_{j}}
The simulation has 100 burn observations to stabilize the process. It is possible to simulate univariate (TAR, SETAR, etc.) or multivariate (VAR) processes, properly specifying the regime type object according to the model.
}
\value{
  Return a list type object of class \dQuote{\code{mtarsim}}:

  \item{Sim}{object class \dQuote{\code{tsregime}}}
  \item{Reg}{list type object with names (R1, ..., Rl) each one class \dQuote{\code{regime}}}
  \item{pj}{vector of autoregressive orders in each regime}
  \item{qj}{vector of covariate lags orders in each regime}
  \item{dj}{vector of lags orders of threshold process in each regime}
}
\references{
Calderon, S. and Nieto, F. (2017) \emph{Bayesian analysis of multivariate threshold autoregress models with missing data.} Communications in Statistics - Theory and Methods 46 (1):296–318. doi:10.1080/03610926.2014.990758.
}
\author{Valeria Bejarano \email{vbejaranos@unal.edu.co}, Sergio Calderon \email{sacalderonv@unal.edu.co} & Andrey Rincon \email{adrincont@unal.edu.co}}

\examples{
## get Ut data process
Tlen = 500
Sigma_ut = 2
Phi_ut = list(phi1 = 0.3)
R_ut = list(R1 = mtaregime(orders = list(p = 1,q = 0,d = 0),Phi = Phi_ut,Sigma = Sigma_ut))
Ut = mtarsim(N = Tlen,Rg = R_ut,seed = 124)
Zt = Ut$Sim$Yt

# Yt process
k = 2
## R1 regime
Phi_R1 = list(phi1 = matrix(c(0.1,0.6,-0.4,0.5),k,k,byrow = TRUE))
Sigma_R1 = matrix(c(1,0,0,1),k,k,byrow = TRUE)
R1 = mtaregime(orders = list(p = 1,q = 0,d = 0),Phi = Phi_R1,Sigma = Sigma_R1)
## R2 regime
Phi_R2 = list(phi1 = matrix(c(0.3,0.5,0.2,0.7),2,2,byrow = TRUE))
Sigma_R2 = matrix(c(2.5,0.5,0.5,1),2,2,byrow = TRUE)
R2 = mtaregime(orders = list(p = 1,q = 0,d = 0),
Phi = Phi_R2,Sigma = Sigma_R2)
## create list of regime-type objects
Rg = list(R1 = R1,R2 = R2)
r = 0.3

# get the simulation
datasim = mtarsim(N = Tlen,Rg = Rg,r = r,Zt = Zt,seed = 124)
autoplot.tsregime(datasim$Sim,1)
autoplot.tsregime(datasim$Sim,2)
}
\seealso{
\code{\link[BMTAR]{mtaregime}},
\code{\link[BMTAR]{mtarns}},
\code{\link[BMTAR]{mtarstr}},
\code{\link[BMTAR]{mtarmissing}},
\code{\link[BMTAR]{mtarnumreg}}
}
\keyword{MTAR}
\keyword{Covariate process}
\keyword{Threshold process}

