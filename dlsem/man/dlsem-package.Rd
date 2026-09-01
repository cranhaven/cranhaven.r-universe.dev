\name{dlsem-package}
\alias{dlsem-package}
\docType{package}
\title{Distributed-lag linear structural equation models}
\description{
Inference functionalities for distributed-lag linear structural equation models (DLSEMs).
DLSEMs are Markovian structural causal models where each factor of the joint probability distribution
is a distributed-lag linear regression with constrained lag shapes (Magrini, 2018; Magrini et. al, 2019; Magrini, 2020).
DLSEMs account for temporal delays in the dependence relationships among the variables through a single parameter per covariate, thus allowing to perform dynamic causal inference in a feasible fashion.
Endpoint-constrained quadratic ('ecq'), quadratic decreasing ('qd'), linearly decreasing ('ld') and gamma ('gam') lag shapes are available.
The main functions of the package are:
\itemize{
  \item{\link{dlsem}, }{to perform parameter estimation;}
  \item{\link{causalEff}, }{to compute all the pathwise causal lag shapes and the overall one connecting two or more variables;}
  \item{\link{lagPlot}, }{to display a pathwise or an overall causal lag shape.}
  }
}
\details{
  \tabular{ll}{
  Package: \tab dlsem\cr
  Type: \tab Package\cr
  Version: \tab 2.4.6\cr
  Date: \tab 2020-03-22\cr
  License: \tab GPL-2
  }
}
\author{Alessandro Magrini <alessandro.magrini@unifi.it>}
\references{
A. Magrini (2018). Linear Markovian models for lag exposure assessment. Biometrical Letters, 55(2): 179-195. DOI: 10.2478/bile-2018-0012.

A. Magrini, F. Bartolini, A. Coli, B. Pacini (2019). A structural equation model to assess the
impact of agricultural research expenditure on multiple dimensions. Quality and Quantity, 53(4):
2063-2080. DOI: 10.1007/s11135-019-00855-z

A. Magrini (2020). A family of theory-based lag shapes for distributed-lag linear regression. To be appeared on \emph{Italian Journal of Applied Statistics}.
}        
