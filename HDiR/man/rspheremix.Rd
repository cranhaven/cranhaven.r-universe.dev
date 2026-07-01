\encoding{utf8}
\name{rspheremix}
\alias{rspheremix}
\title{
Random generation functions for mixtures of spherical von Mises-Fisher
}
\description{
Random generation functions for nine finite mixtures of spherical von Mises-Fisher allowing different numbers of modes.
}
\usage{
rspheremix(n, model = NULL)
}
\arguments{
  \item{n}{
Number of observations to generate.
}
  \item{model}{
Number between 1 and 9, corresponding with a model defined in Saavedra-Nieves and Crujeiras (2021). See Details.
}
}
\details{
These nine spherical models are obtained as mixtures of von Mises distributions where the density \eqn{f} is given by:
\deqn{
f=\sum_{i=1}^I w_i K_{vM}(x;m_i;k_i), w_i\geq 0;\sum_{i=1}^I w_i=1
} with \eqn{K_vM} denoting the von Mises-Fisher kernel density; \eqn{m_i}, \eqn{k_i} and \eqn{w_i} the mean, concentration and weight corresponding to each component. More details can be found in Hornik and Grun (2014) and Wood (1994). The combination of means, concentration parameters and the weights of spherical models from Saavedra-Nieves and Crujeiras (2021) are specified below:\cr \cr
S1: (0, 0, 1) (\eqn{m}); 10 (\eqn{k}); 1 (\eqn{w}).\cr \cr
S2: (0, 0, 1), (0, 0, -1) (\eqn{m}); 1, 1 (\eqn{k}); 1/2, 1/2 (\eqn{w}).\cr \cr
S3: (0, 0, 1), (0, 0, -1)  (\eqn{m}); 10, 1 (\eqn{k}); 1/2, 1/2 (\eqn{w}).\cr \cr
S4: (0, 0, 1); (0, 1/\eqn{\sqrt2}, 1/\eqn{\sqrt2}) (\eqn{m}); 10, 10 (\eqn{k}); 1/2, 1/2 (\eqn{w}).\cr \cr
S5: (0, 0, 1); (0, 1/\eqn{\sqrt2}, 1/\eqn{\sqrt2}) (\eqn{m}); 10, 10 (\eqn{k}); 2/5, 3/5 (\eqn{w}).\cr \cr
S6: (0, 0, 1); (0, 1/\eqn{\sqrt2}, 1/\eqn{\sqrt2 }) (\eqn{m}); 10, 5 (\eqn{k}); 1/5, 4/5 (\eqn{w}).\cr \cr
S7: (0, 0, 1), (0, 1, 0), (1, 0, 0) (\eqn{m}); 5, 5, 5 (\eqn{k}); 1/3, 1/3, 1/3 (\eqn{w}).\cr \cr
S8:  (0, 0, 1), (0, 1, 0), (1, 0, 0) (\eqn{m}); 5, 5, 5 (\eqn{k}); 2/3, 1/6, 1/6 (\eqn{w}).\cr \cr
S9: (0, 0, 1); (0, 1/\eqn{\sqrt 2}, 1/\eqn{\sqrt 2}), (0, 1, 0) (\eqn{m}); 10, 10, 10 (\eqn{k}); 1/3, 1/3, 1/3 (\eqn{w}).\cr \cr
}
\value{
A matrix with \eqn{n} unit length rows representing the generated values from a finite mixture of spherical von Mises-Fisher.\cr%%  If it is a LIST, use
}
\references{
Hornik, K. and Grun, B. (2014). movMF: an R package for fitting mixtures of von Mises-Fisher distributions. Journal of Statistical Software, 58(10), 1-31.\cr
Saavedra-Nieves, P. and Crujeiras, R. M. (2021). Nonparametric estimation of directional highest density regions. Advances in Data Analysis and Classification, 1-36.\cr
Wood, A. T. (1994). Simulation of the von Mises Fisher distribution. Communications in Statistics-Simulation and Computation, 23(1), 157-164.}
\author{
Paula Saavedra-Nieves and Rosa M. Crujeiras.
}
\examples{
# Random generation from model 1 in library HDiR
data <- rspheremix(500, model=1)
library(Directional)
sphereplot(data)
}

