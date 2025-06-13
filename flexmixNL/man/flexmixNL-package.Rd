\name{flexmixNL-package}
\alias{flexmixNL-package}
\alias{flexmixNL}
\docType{package}
\title{Mixtures of Generalized Nonlinear Models}
\description{Extension of package \code{flexmix} for fitting mixtures of 
Generalized Nonlinear Models.}
\details{The package \code{flexmixNL} implements an extension for the package 
\code{\link{flexmix}} for fitting mixtures of Generalized Nonlinear Models 
(GNMs). The package provides a specified M-step for the EM-algorithm within the 
FlexMix framework (see also \code{\link{flexmix}}) for fitting 
GNMs for the normal and gamma distribution. 
The mixture model is specified by the function \code{\link{FLXMRnlm}}.
}
\author{Sanela Omerovic}


\keyword{cluster}
\keyword{regression}
\keyword{nonlinear}
\seealso{
See also \code{\link{flexmix}} for finite mixtures of regression models and 
\code{\link{gnm}} for the fitting of Generalized Nonlinear Models (GNMs) for 
further information.}

\examples{
# example 1.
data("NReg", package = "flexmixNL")
# mixture of two nonlinear regression models (normal distribution).
start1 <- list(a = 170, b = 5)
start2 <- list(a = 130, b = 5)
model <- flexmix(yn ~ x, k = 2, data = NReg,
                     model = list(FLXMRnlm(formula = yn ~ a*x / (b+x),
                                           family = "gaussian", 
                                           start = list(start1, start2))))
# final cluster assignments.                                           
plot(yn ~ x, col = clusters(model), data = NReg)

# example 2.
data("GReg", package = "flexmixNL")
# mixture of two nonlinear regression models (gamma distribution).
exp.1 = function(x,predictors){
  list(predictors = list(a = 1, b = 1),
       variables = list(substitute(x)),
       term = function(predictors, variables){
         sprintf("exp( \%s + \%s * \%s)", 
         predictors[1], predictors[2], variables)
       })
}
class(exp.1) = "nonlin"

start1 <- list(a = -0.4, b = 0.3)
start2 <- list(a = -0.1, b = 0.4)
model2 <- flexmix(yg ~ x, k = 2, data = GReg,
                      model = list(FLXMRnlm(formula = yg ~ -1 + exp.1(x),
                                            family = "Gamma", 
                                            start = list(start1, start2))))
# final cluster assignments.  
plot(yg ~ x, col = clusters(model2), data = GReg)
}