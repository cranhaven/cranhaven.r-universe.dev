#' @details Details
#' 
#' The first algorithm (DEN) calculates the needed equations using the \code{\link{Deriv}}
#' function of the \pkg{Deriv} package. The process is implemented through the use
#' of the S4 class \code{\link{odeEquations-class}}. 
#' 
#' The conjugate gradient based algorithm uses a greedy algorithm to estimate a 
#' sparse control that tries to minimize the discrepancies between a given 
#' 'nominal model given the measurements (e.g from an experiment). The algorithm
#' the \code{\link{ode}} uses \pkg{deSolve} to calculate the hidden inputs w 
#' based on the adjoint equations of the ODE-System. 
#' 
#' The adjoint equations are calculated using the \code{\link{ode}} function of the 
#' \pkg{deSolve} package. For the usage of the algorithm please look into the 
#' examples and documentation given for the functions.
#' 
#' The second algorithm is called Bayesian Dynamic Elastic Net (BDEN).
#' The BDEN as a new and fully probabilistic approach, supports the modeler in an 
#' algorithmic manner to identify possible sources of errors in ODE based models on 
#' the basis of experimental data.  THE BDEN does not require pre-specified hyper-parameters. 
#' BDEN thus provides a systematic Bayesian computational method to identify target nodes and 
#' reconstruct the corresponding error signal including detection of missing and 
#' wrong molecular interactions within the assumed model. The method works for ODE 
#' based systems even with uncertain knowledge and noisy data. 
#' 
#' \describe{
#'    \item{\code{\link[seeds]{DEN}}}{a greedy algorithm to calculate a sparse control}
#'    \item{\code{\link[seeds]{BDEN}}}{a basian mcmc approach}
#' }
#' 
#' @references \strong{Benjamin Engelhardt, Holger Froehlich, Maik Kschischo} 
#' Learning (from) the errors of a systems biology model, \emph{Nature Scientific Reports},
#'  6, 20772, 2016 \url{https://www.nature.com/articles/srep20772}
"_PACKAGE"
