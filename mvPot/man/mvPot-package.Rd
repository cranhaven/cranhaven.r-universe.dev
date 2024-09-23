\name{mvPot-package}
\alias{mvPot-package}
\alias{mvPot}
\docType{package}
\title{
Multivariate Peaks-over-Threshold Modelling for Extreme Events Analysis
}
\description{
The mvPot package provides functions to perform high-dimensional peaks-over-threshold inference
of spatial processes such as the Brown--Resnick. Parallel implementation for censored likelihood
allows up to 500 locations, whereas the gradient score can handle thousands of locations. The package also
includes simulations algorithms for the Brown-Resnick max-stable process as well as its associated Pareto process.
A tutorial describing a complete case study of Red Sea temperature anomalies extremes can be found at \url{https://github.com/r-fndv/mvPot_tutorial}.
}
\details{

The mvPot package provides functions to perform high-dimensional peaks-over-threshold inference of spatial processes such as the Brown--Resnick.

\code{spectralLikelihood} relies on the spectral likelihood as developed by Engelke et al. (2015). This methods is fast to compute, however it is not robust with regard to non-extreme components.

\code{censoredLikelihoodBR} (Wadsworth and Tawn, 2013) is a likelihood function for exceedances with at least one component exceeding a threshold and where low components, i.e., components under their threshold,. This approach is robust and performs best but requires heavy computations. The implementation in this package makes use of quasi-Monte Carlo estimation and thus can handle 100 locations in a reasonable time and up to 500 when parallelized. The analog function for extremal Student processes is \code{censoredLikelihoodXS}.

\code{scoreEstimation} is a faster alternative to the \code{censoredLikelihood}, which is more robust than \code{spectralLikelihood}. This method can also be used with any kind of differentiable risk functional (Fondeville and Davison, 2016). Here the algorithm is limited only by matrix inversion and thus thousands of locations can be used.

\code{simulBrownResnick} is an exact algorithm for simulation of Brown-Resnick max-stable processes as described in Dombry et al. (2015).

\code{simulPareto} allows for simulation of Pareto processes associated to log-Gaussian random functions.

\code{rExtremalStudentParetoProcess} allows for simulation of Pareto processes associated to Student random functions, using the accept-reject algorithm of Thibaud and Opitz (2015).

\code{mvtNormQuasiMonteCarlo} and \code{mvTProbQuasiMonteCarlo} are Cpp functions to evaluate the distribution function of Gaussian and t integrals, using a quasi-Monte Carlo algorithm based on randomly shifted lattice rules.

}
\author{
Raphael de Fondeville

Maintainer: Raphael de Fondeville <raphael.de-fondeville@epfl.ch>
}
\references{
de Fondeville, R. and Davison A. (2018). High-dimensional peaks-over-threshold inference. Biometrika, 105(3), 575-592.

Engelke, S. et al. (2015). Estimation of Huesler-Reiss Distributions and Brown-Resnick Processes. Journal of the Royal Statistical Society: Series B, 77(1), 239-265

Wadsworth, J.L. and Tawn, J.A. (2013). Efficient inference for spatial extreme value processes associated to log-Gaussian random functions. Biometrika, 101(1), 1-15.

Thibaud, E. and T. Opitz (2015). Efficient inference and simulation for elliptical Pareto processes. Biometrika, 102(4), 855-870.

Dombry, C., Engelke, S. and Oesting, M. (2016). Exact simulation of max-stable processes. Biometrika, 103(2), 303-317.

Genz, A. and Bretz, F. (2009). Computations of Multivariate Normal and t Probabilities, volume 105. Springer: Dordrecht.

Genz, A. (2013). QSILATMVNV \url{http://www.math.wsu.edu/faculty/genz/software/software.html}
}


\examples{
#Define semi-variogram function
vario <- function(h, alpha = 1.5){
    norm(h,type = "2")^alpha
}

#Define locations
loc <- expand.grid(1:4, 1:4)

#Simulate data
obs <- simulPareto(1000, loc, vario)

#Evaluate risk functional
sums <- sapply(obs, sum)

#Define weighting function
weigthFun <- function(x, u){
 x * (1 - exp(-(sum(x) / u - 1)))
}

#Define partial derivative of weighting function
dWeigthFun <- function(x, u){
 (1 - exp(-(sum(x) / u - 1))) + (x / u) * exp( - (sum(x) / u - 1))
}


#Select exceedances
threshold <- quantile(sums, 0.9)
exceedances <- obs[sums > threshold]

#Define objective function
objectiveFunction = function(parameter, exceedances, loc, vario, weigthFun, dWeigthFun, threshold){

 #Define semi-variogram for the corresponding parameters
 varioModel <- function(h){
  vario(h, parameter[1])
 }

 #Compute score
 scoreEstimation(exceedances, loc, varioModel, weigthFun, dWeigthFun, u = threshold)
}

#Estimate the parameter by optimization of the objective function
est <- optim(par = c(1.5),
             fn = objectiveFunction,
             exceedances = exceedances,
             loc = loc,
             vario = vario,
             weigthFun = weigthFun,
             dWeigthFun = dWeigthFun,
             threshold = threshold,
             control = list(maxit = 100, trace = 1),
             lower = c(0.01),
             upper = c(1.99),
             method = "L-BFGS-B")
}
