#' @title Diagnostics from a fitll object
#' @name lldiagnostics
#' @description Prints diagnostics or extract those diagnostics from a fitll object.
#' @param object Object of "fitll", that the user provided the y, X, and W arguments for the llbayesireg function.
#'
#' @return lldiagnostics(object) prints diagnostics or extract those diagnostics from a fitll object.
#'
#' @source The L-Losgistic distribution was introduced by Tadikamalla and Johnson (1982), which refer to this distribution as Logit-Logistic
#' distribution. Here, we have a new parameterization of the Logit-Logistic with the median as a parameter.
#'
#'
#' @references Paz, R.F., Balakrishnan, N and Bazán, J.L. (2018). L-Logistic Distribution: Properties, Inference and an Application to Study Poverty and Inequality in Brazil.
#' The Stan Development Team Stan Modeling Language User's Guide and Reference Manual. http://mc-stan.org/.
#' Plummer, M., Best, N., Cowles, K., and Vines, K. (2006). Coda: Convergence diagnosis and output analysis for mcmc. R News, 6(1):7-11.
#'
#' @details The function calls the check_* functions and the get_* functions are for access to the diagnostics. If the matrix X and W are missing, the coda package is used by test the convergence
#' of the chains by  Cramer-von-Mises statistic and an image of the correlation is show for both
#' of generated chains.#'
#' @examples
#' # Modelation the coeficient with generated data
#'
#' library(llbayesireg)
#' library(llogistic)
#'
#' # Number of elements to be generated
#'
#' n=50
#'
#' # Generated response
#'
#' bin=2005
#' set.seed(bin)
#' y=rllogistic(n,0.5, 2)
#'
#' fitll = llbayesireg(y, niter=100, jump=10)
#'
#' lldiagnostics(fitll$object)
#'
#' \donttest{
#' # Modelation the coeficient with real data
#'
#' library(llbayesireg)
#'
#' data("Votes","MHDI")
#'
#' y = Votes[,4]
#' X = MHDI
#'
#' fitll = llbayesireg(y,X)
#'
#' lldiagnostics(fitll$object)
#' }
#'
#' @importFrom coda as.mcmc.list
#' @importFrom coda mcmc
#' @importFrom coda autocorr.plot
#' @importFrom coda heidel.diag
#'
#' @import rstan
#'
#' @export lldiagnostics
lldiagnostics = function (object){

  if(is.null(object)){
    stop("There is no object.")
  }

  if(typeof(object) == "S4"){
    print("Check HMC Diagnostics After Sampling.")
    check_hmc_diagnostics(object)

    print("A logical vector indicating problems for individual iterations.")
    print(get_divergent_iterations(object))

    print("The number of offending interations.")
    print(get_num_max_treedepth(object))

    print("An integer vector with the number of leapfrog evalutions for each iteration.")
    print(get_num_leapfrog_per_iteration(object))

    print("Per-chain E-BFMI values.")
    print(get_bfmi(object))

    print("The indices of chains with low E-BFMI.")
    print(get_low_bfmi_chains(object))
  }else{
    mcmc.object= as.mcmc.list(lapply(as.data.frame(object), mcmc))
    autocorr.plot(mcmc.object)
    heidel.diag(mcmc.object, eps=0.1, pvalue=0.05)
  }





}
