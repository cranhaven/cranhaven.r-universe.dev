\name{penetrance}
\alias{penetrance}
\title{
  Penetrance function and confidence intervals
}
\description{
  Estimates the cumulative disease risks (penetrances) and confidence intervals at given age(s) based on the fitted penetrance model.
}
\usage{
  penetrance(fit, fixed, age, CI = TRUE, MC = 100)
}

\arguments{
  \item{fit}{
    An object class of \code{'penmodel'}, a fitted model by \code{\link{penmodel}} or \code{\link{penmodelEM}} functions.
  }
  \item{fixed}{
    Vector of fixed values of the covariates used for penetrance calculation.
  }
  \item{age}{
    Vector of ages used for penetrance calculation.
  }
  \item{CI}{
    Logical; if \code{TRUE}, the 95\% confidence interval will be obtained using a Monte-Carlo method, 
    otherwise no confidence interval will be provided. Default is \code{TRUE}.
  }
  \item{MC}{
    Number of simulated samples used to calculate confidence intervals with a Monte-Carlo method. 
    If \code{MC=0}, no confidence intervals will be calculated. Default value is 100.
  }

}
\details{
  The penetrance function is defined as the probability of developing a disease by age \eqn{t} given fixed values of covariates \eqn{x}, 
\deqn{ P(T < t | x) = 1 - S(t; x),}
where \eqn{t} is greater than the minimum age \ifelse{html}{\out{t<sub>0</sub>}}{\eqn{t_0}{t0}} and \eqn{S(t; x)}  is the survival distribution based on a proportional hazards model with a specified baseline hazard distribution.

The proportional hazards model is specified as:
\ifelse{html}{\out{h(t|x) = h<sub>0</sub>(t) exp(&beta;*x),}}{
\deqn{ h(t|x) = h_0(t) \exp(\beta x) ,}{h(t|x) = h0(t) exp(b*x) ,}}
where \ifelse{html}{\out{h<sub>0</sub>(t)}}{\eqn{h_0(t)}{h0(t)}} is the baseline hazards function, \eqn{x} is the vector of covariates and \ifelse{html}{\out{&beta;}}{\eqn{\beta}{b}} is the vector of corresponding regression coefficients.

  Calculations of standard errors of the penetrance estimates and 95\% confidence intervals (CIs) for the penetrance at a given age are based on Monte-Carlo simulations of the estimated penetrance model. 
  
  A multivariate normal distribution is assumed for the parameter estimates, and \code{MC = n} sets of the parameters are generated from the multivariate normal distribution with the parameter estimates and their variance-covariance matrix.
  For each simulated set, a penetrance estimate is calculated at a given age by substituting the simulated parameters into the penetrance function.
  
  The standard error of the penetrance estimate at a given age is calculated by the standard deviation of penetrance estimates obtained from \eqn{n} simulations.
  
  The 95\% CI for the penetrance at a given age is calculated using the 2.5th and 97.5th percentiles of the penetrance estimates obtained from \eqn{n} simulations.
}
\value{
  Returns the following values:
  
  \item{age}{Ages at which the penetrances are calculated.}
  \item{penetrance}{Penetrance estimates at given ages.}
  \item{lower}{Lower limit of the 95\% confidence interval; simulation-based 2.5th percentile of the penetrance estimates.}
  \item{upper}{Upper limit of the 95\% confidence interval; simulation-based 97.5th percentile of the penetrance estimates.}
   \item{se}{Simulation-based standard errors of the penetrance estimates.}
  
}

\author{
  Yun-Hee Choi
}
\seealso{
  \code{\link{simfam}, \link{penmodel}, \link{penmodelEM}}
}
\examples{
  set.seed(4321)
  fam <- simfam(N.fam = 100, design = "pop+", base.dist = "Weibull", allelefreq = 0.02, 
         base.parms = c(0.01,3), vbeta = c(-1.13, 2.35))
	
  fit <- penmodel(Surv(time, status) ~ gender +  mgene, cluster = "famID", 
         parms = c(0.01, 3, -1.13, 2.35),  data = fam, base.dist = "Weibull", design = "pop+")
 
 # Compute penetrance estimates for male carriers at age 40, 50, 60, and 70 and
 # their 95% CIs based on 100 Monte Carlo simulations.
 
 penetrance(fit, fixed = c(1,1), age = c(40, 50, 60, 70), CI = TRUE, MC = 100)

}
\keyword{Penetrance estimation}