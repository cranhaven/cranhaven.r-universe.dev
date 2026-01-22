\docType{data}
\name{lvaproxdata}
\alias{lvaproxdata}
\title{Prey-Predator (Lotka-Volterra) example in LV dataset}
\format{\strong{lvaproxdata}: a data.frame for approximation (evaluated on (20 x 20) Chebyshev nodes)
\itemize{
\item \code{xs} prey stock
\item \code{ys} predator stock
\item \code{xdot} evaluated xdot \eqn{\frac{dx}{dt}}
\item \code{ydot} evaluated ydot \eqn{\frac{dy}{dt}}
\item \code{wval} profit (W in Fenichel and Abtott (2014))
}}
\usage{
## Load dataset
data("lvdata")
}
\description{
The \code{lvaproxdata} provides the data in \code{LV} dataset to simulate prey-predator (Lotka-Volterra) model. The original code was written by Joshua Abbott in MATLAB and Seong Do Yun adapted it to a package example. The prey-predator model is:\cr
\cr
Prey (\eqn{X}): \eqn{ \dot{X} = rX \left( 1 - \frac{X}{K} \right) - aXY - \theta X   }, and\cr
\cr
Predator (\eqn{Y}): \eqn{ \dot{Y} = bXY - mY - \gamma Y   }.\cr
\cr
The parameters are given as:\cr
\eqn{r = 0.025}: intrinsic growth rate for prey,\cr
\eqn{K = 1}: carrying capacity for prey,\cr
\eqn{a = 0.08}: predator-related mortality parameter for prey,\cr
\eqn{b = 0.05}: predator/prey uptake parameter for predator,\cr
\eqn{m = 0.01}: natural mortality for predator,\cr
\eqn{\gamma = 0.005}: slope for linear predator harvest control rule, and\cr
\eqn{\theta = 0.005}: slope for linear prey harvest control rule\cr
\cr
The predator with no economic value (unharvested) is designed for the economic program as:\cr
\cr
\eqn{ W = harv.prey(p.prey-c.prey/X)\theta X + harv.pred*(p.pred-c.pred/Y)\gamma Y}.\cr
\cr
The paramters are:\cr
\eqn{p.pred = 0}: price per unit harvest of predator,\cr
\eqn{p.prey = 25}:  price per unit harvest of prey,\cr
\eqn{c.prey = 0.1 p_prey}: cost /per unit of prey effort in Schaefer model (really c/q with q=1), and\cr
\eqn{c.pred = c_prey}: cost per unit of predator effort in Schaefer model (really c/q with q=1).\cr
}
\seealso{
\code{\link{LV},\link{vsim}}
}
