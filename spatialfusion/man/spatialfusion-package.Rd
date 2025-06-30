\docType{package}
\name{spatialfusion-package}
\alias{spatialfusion}
\alias{spatialfusion-package}
\title{Multivariate Analysis of Spatial Data Using a Unifying Spatial Fusion Framework }
\description{
Multivariate modelling of geostatistical (point), lattice (areal) and point pattern data in a unifying spatial fusion framework. Details are given in Wang and Furrer (2021) <\doi{10.1016/j.csda.2021.107240}>. Model inference is done using either 'Stan' <https://mc-stan.org/> or 'INLA' <https://www.r-inla.org/>.
}
\section{Data analysis pipeline}{
\subsection{Preparing data}{
\code{fusionData()} is used to set up the data structure needed for spatial fusion modelling. Depending on the chosen \code{'method'}, either a \code{dstan} or a \code{dinla} object is returned. This object is then suppied to the \code{'data'} argument in \code{fusion()} for fitting a spatial fusion model.

In terms of the \code{'method'}, Stan provides Hamiltonian Monte Carlo-based full Bayesian inference, while INLA provides approximate Bayesian inferece at a much faster computation speed. Their results are very similar in our simulation studies (Wang and Furrer, 2021 <\doi{10.1016/j.csda.2021.107240}>).

IMPORTANT: Users should be familiar with either \pkg{rstan} or \pkg{INLA} packages themselves. For Stan, users should know how to choose priors appropriately. For INLA, users should know how to set up an appropriate mesh.
}
\subsection{Fitting model}{
\code{fusion()} is used to fit a spatial fusion model. The most related publication is by Wang and Furrer (2021) <\doi{10.1016/j.csda.2021.107240}>, which introduced the framework.

We suggest users to test their model on smaller sub-sampled dataset first, to check model fitting issues such as convergence, identifiability etc. It also helps to get an idea of the computation time required. Afterwards, users can fit the model to their full dataset. The output has a class \code{fusionModel}.
}
\subsection{Model diagnostics}{
Common generic functions such as \code{fitted()}, \code{predict()}, \code{summary()} and \code{plot()} are available for \code{fusionModel} objects. Diagnostics of spatial fusion models should be done in the same way as for a Stan or a INLA model, depending on the chosen method.
}
}

\details{
\tabular{ll}{
Package: \tab spatialfusion\cr
Type: \tab Package\cr
Version: \tab 0.7-2\cr
Date: \tab 2025-06-22\cr
License: \tab GPL (>= 3) \cr
LazyLoad: \tab yes\cr
}}

\author{
Craig Wang \email{craigwang247@gmail.com} \cr
}

\examples{
\donttest{
## Citations
citation('spatialfusion')

## Vignette
vignette("spatialfusion_vignette", package = "spatialfusion")
}}

\keyword{package}
