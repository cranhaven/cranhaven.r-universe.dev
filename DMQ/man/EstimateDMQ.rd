\name{EstimateDMQ}
\alias{EstimateDMQ}
\title{
	Estimate the Dynamic Multiple Quantile (DMQ) model.
}
\description{
	Estimate the parameters of the DMQ model using the estimator detailed in Catania and Luati (2023).
}
\usage{
EstimateDMQ(vY, vTau, iTau_star = NULL, vPn_Starting = NULL, 
                        FixReference = FALSE, FixOthers = FALSE, 
                        ScalingType = "InvSqrt",
                         vQ_0 = NULL,  
                         fn.optimizer = fn.DEoptim, 
                        cluster = NULL, smooth = NULL, ...)
}
\arguments{
\item{vY}{\code{numeric} vector of length Tx1 containing the time series of observations.}
%
\item{vTau}{\code{numeric} vector of length Jx1 containing probability levels at which quantiles are estimated.}
%
\item{iTau_star}{Integer indicating the position in \code{vTau} where the reference quantile is placed. For instance, if \code{vTau = seq(0.01, 0.99, 0.01)} then \code{iTau_star = 50} means that the median is used as the reference quantile.}
%
\item{vPn_Starting}{\code{numeric} named vector of length 4x1 with starting values for the optimizer. For example \code{vPn_Starting = c("phi" = 0.9, "gamma" = 0.05, "alpha" = 0.01, "beta" = 0.7)}.}
%
\item{FixReference}{\code{logical}. Should the reference quantile be fixed? By default \code{FixReference = FALSE}.}
%
\item{FixOthers}{\code{logical}. Should the quantiles other than the reference quantile be fixed? By default \code{FixOthers = FALSE}.}
%
\item{ScalingType}{\code{character} Indicating the scaling mechanism for the conditional quasi score. Possible choices are \code{"Identity"}, \code{"Inv"},\code{"InvSqrt"}. When  \code{ScalingType = "InvSqrt"} quasi scores are scaled by their standard deviation. When  \code{ScalingType = "Inv"} quasi scores are scaled by their variance. When  \code{ScalingType = "Identity"} quasi scores are not scaled. Default value \code{ScalingType = "InvSqrt"}.}
%
\item{vQ_0}{\code{numeric}. Vector of limiting quantiles evaluated at \code{vTau}. By default \code{FixOthers = NULL} meaning that empirical unconditional quantilies are used.}
%
\item{fn.optimizer}{\code{function}. This is a generic optimization function that can be provided by the user. By default \code{fn.optimizer = fn.DEoptim} where \link{fn.DEoptim} is a wrapper to the \link{DEoptim} function of the package \code{DEoptim}. See the Details and Examples sections for user defined optimization routines.}
%
\item{cluster}{A \code{cluster} object created calling using the \code{paralell} package. If supplied parallel processing is used to speed up the computations if \code{fn.optimizer} makes use of it. When \code{fn.optimizer = fn.DEoptim} parallel computation can be used.}
%
\item{smooth}{\code{logical}. Should a smooth version of the objective function should be used? If using a gradient based optimizer like \code{fn.optimizer = fn.optim} is it advised to set \code{smooth = TRUE}. By default, when \code{fn.optimizer = fn.DEoptim} we set \code{smooth = FALSE} and when \code{fn.optimizer = fn.optim} or \code{fn.optimizer = fn.solnp} we set \code{smooth = TRUE}.}
%
\item{...}{Additional arguments to be passed to \code{fn.optimizer}.}
}
\details{
%
Starting values for the optimizer are by default set as \code{c("phi" = 0.94, "gamma" = 0.10, "alpha" = 0.05, "beta" = 0.95)}.\cr
%
The user is free to employ his/her own optimization routine via the \code{fn.optimizer} argument. \code{fn.optimizer} accepts a \code{function} object. The user provided optimizer has to satisfy strict requirements. The arguments of the \code{fn.optimizer} are:
%
\describe{
\item{\code{par0}}{ a vector of starting values,}
\item{\code{vY}}{ the data provided,}
\item{\code{FUN}}{ the objective function,}
\item{\code{LB}}{ vector of lower bounds for the parameters,}
\item{\code{UB}}{ vector of upper bounds for the parameters.}
\item{\code{...}}{ additional arguments.}
}
%
The output of \code{fn.optimizer} has to be an object of the class \code{list} with four named elements:
\describe{
\item{\code{pars}}{ a \code{numeric} vector where the estimated parameters are stored,}
\item{\code{value}}{ a \code{numeric} containing the value of the objective function evaluated at its minimum,}
\item{\code{hessian}}{ a \code{numeric} matrix containing the Hessian matrix evaluated at
the minimum of the objective function, this is used for inferential purposes,}
\item{\code{convergence}}{ a \code{numeric} variable  reporting information about the convergence of the optimization. \code{convergence = 0} has to indicate successful completion.}
}
The user is allowed to not include the last two elements of the output of the \code{fn.optimizer} function, that is, the values \code{hessian = NULL} and \code{convergence = NULL} are admissible. In the case of \code{hessian = NULL}, no standard errors will be computed.
}
\value{
A \code{list} with, among others, elements:
%
\item{lFilter}{A \code{list} containing the output from the filtering procedure. For instance filtered quantiles, hit variables, and losses.}
%
\item{vPn}{\code{numeric} named vector of estimated parameters.}
%
\item{optimizer}{A \code{list} with the output from \code{fn.optimizer}.}
%
\item{Inference}{A \code{list} with output from the inferential procedure.}
}
\references{
Catania, L, and Luati, A. (2023). 
"Semiparametric modeling of multiple quantiles."
Journal of Econometrics
\doi{10.1016/j.jeconom.2022.11.002}.
}
\author{Leopoldo Catania}
\examples{
# Load Microsoft Corporation logarithmic percentage returns from December 8, 
# 2010 to November 15, 2018 for a total of T = 2000 observation
data("MSFT")

##############################################################
######################## Estimate DMQ ########################
##############################################################

# Deciles
vTau = seq(0.1, 0.9, 0.1)

# Reference quantile to the median
iTau_star = 5

# Fix the reference quantile to a constant
FixReference = TRUE

# Estimate DMQ
Fit_solnp = EstimateDMQ(vY = vY,
                  vTau = vTau,
                  iTau_star = iTau_star,
                  FixReference = FixReference,
                  fn.optimizer = fn.solnp,
                  cluster = cluster)

Fit_solnp$vPn
Fit_solnp$optimizer$value

\dontrun{
#### Estimate DMQ using different optimizers

# With the DEoptim optimizer

# parallel computation
iG = 7
cluster = makeCluster(iG)

set.seed(123)

# Estimate DMQ
Fit_DEoptim = EstimateDMQ(vY = vY,
                  vTau = vTau,
                  iTau_star = iTau_star,
                  FixReference = FixReference,
                  fn.optimizer = fn.DEoptim,
                  cluster = cluster)

Fit_DEoptim$vPn
Fit_DEoptim$optimizer$value

# Estimate the model with a user defined optimizer.
# Let's use the gosolnp() optimizer from the Rsolnp package.

library("Rsolnp")
fn.gosolnp <- function(par0, vY, FUN, LB, UB, ...) {
  
  foo = list(...)
  if (!is.null(foo$cluster)) {
    cluster = foo$cluster
    clusterEvalQ(cluster, library(DMQ))
  } 
  
  optimiser = gosolnp(
    pars = par0,
    fun = FUN, vY = vY, 
    n.sim = 1000,
    n.restarts = 5,
    LB = LB,
    UB = UB, control = list(trace = 1), 
    ...)
  
  out = list(pars = optimiser$pars,
             value = tail(optimiser$values, 1),
             hessian = optimiser$hessian,
             convergence = optimiser$convergence)
  
  return(out)
  
}

set.seed(123)
# Estimate DMQ
Fit_gosolnp = EstimateDMQ(vY = vY,
                  vTau = vTau,
                  iTau_star = iTau_star,
                  FixReference = FixReference,
                  fn.optimizer = fn.gosolnp,
                  cluster = cluster,
                  smooth = TRUE) 

Fit_gosolnp$vPn
Fit_gosolnp$optimizer$value

stopCluster(cluster)

}
}
