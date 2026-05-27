gMLE.bb <-
function (success, trials, start = NULL, optim.method = "default", 
                        lower = 0, upper = Inf) {
  #################################################################################
  # MLE estimate of Beta-Binomial parameters
  # SOURCE: https://github.com/SupplyFrame/EmpiricalBayesR/blob/master/EmpiricalBayesEstimation.R
  # Args:  
  #      success: vector of #success; trials:=vector of #trials; 
  #      start: initial parameters (must be a list with name shape1, shape2)
  #      optim.method: optimization methods in optim(){stats}
  #      lower(upper): lower(upper) bound for parameters
  #
  # Returns: 
  #         $estimate: MLE estimate for beta parameters   
  #         $convergence: convergence code from optim(). 0 means good.
  #         $loglik: Loglikelihood with estimated parameters
  #         $starting: initial parameters from the method of moments
  #
  # Dependent package: VGAM
  # 
  # Note: The structure of the function heavily relies on 
  #       mledist(){fitdistrplus} by Marie Laure Delignette-Muller.
  #
  # Summer2013 @Supplyframe
  #################################################################################
  
  if(!is.element("VGAM", installed.packages()[,1])){
    stop("Please install and load package VGAM before using this function.")
  }
  
  distname <- "betabinom.ab"
  ddistname <- paste("d", distname, sep = "")
  
  if (is.null(start)) {
    if (distname == "betabinom.ab") {
      if (any(success/trials < 0) | any(success/trials > 1)) {
        stop("Proportion must be in [0-1] to fit a betabinom distribution")
      } 
      start.mu <- mean(success/trials)
      start.var <- var(success/trials)
      start.a <- ((1 - start.mu) / start.var - 1 / start.mu) * start.mu ^ 2
      start.b <- start.a * (1 / start.mu - 1)
      start <- list(shape1 = start.a, shape2 = start.b)
    } 
    if (!is.list(start)) 
      stop("'start' must be defined as a named list for this distribution")
  }
  
  vstart <- unlist(start)
  argddistname <- names(formals(ddistname))
  m <- match(names(start), argddistname)
  
  if (any(is.na(m)) || length(m) == 0) 
    stop("'start' must specify names which are arguments to 'distr'")
  
  fnobj <- function(par, x, n, ddistnam) {
    -sum(do.call(ddistnam, c(list(x), list(n), par, log = TRUE)))
  }
  
  if (optim.method == "default") {
    if (is.infinite(lower) && is.infinite(upper)) {
      if (length(vstart) > 1) 
        meth <- "Nelder-Mead"
      else meth <- "BFGS"
    }
    else meth <- "L-BFGS-B"
  }
  else meth <- optim.method
  
  opttryerror <- try(opt <- optim(par = start, fn = fnobj, 
                                  x = success, n = trials, ddistnam = ddistname, 
                                  hessian = TRUE, method = meth, lower = lower, 
                                  upper = upper), silent = TRUE)
  
  if (inherits(opttryerror, "try-error")) {
    warnings("The function optim encountered an error and stopped")
    #print(opttryerror)
    return(list(estimate = rep(NA, length(vstart)), convergence = 100, 
                loglik = NA, hessian = NA))
  }
  if (opt$convergence > 0) {
    warnings("The function optim failed to converge, with the error code ", 
             opt$convergence)
    return(list(estimate = rep(NA, length(vstart)), convergence = opt$convergence, 
                loglik = NA, hessian = NA, message = opt$message))
  }
  
  res <- list(estimate = opt$par, convergence = opt$convergence, 
              loglik = -opt$value, initial = vstart, hessian = opt$hessian)
  
  return(res)
}
