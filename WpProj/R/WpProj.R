#' p-Wasserstein Linear Projections
#'
#' @param X An \eqn{n \times p} matrix of covariates
#' @param eta An \eqn{n \times s} matrix of predictions from a model
#' @param theta An optional An \eqn{p \times s} parameter matrix for selection methods. Only makes sense if the original model is a linear model.
#' @param power The power of the Wasserstein distance to use. Must be `>= 1.0`. Will default to `2.0`.
#' @param method The algorithm to calculate the Wasserstein projections. One of "L1", "binary program", "IP", "stepwise","simulated annealing", or "L0". Will default to "L1" if not provided. See details for more information.
#' @param solver Which solver to use? One of "lasso", "ecos", "lpsolve", or "mosek". See details for more information
#' @param options Options passed to the particular method and desired solver. See details for more information.
#'
#' @returns object of class `WpProj`, which is a list with the following slots:
#' \itemize{
#' \item{`call`: The call to the function}
#' \item{`theta`: A list of the final parameter matrices for each returned model}
#' \item{`fitted.values`: A list of the fitted values for each returned model}
#' \item{`power`: The power of the Wasserstein distance used}
#' \item{`method`: The method used to calculate the Wasserstein projections}
#' \item{`solver`: The solver used to calculate the Wasserstein projections}
#' \item{`niter`: The number of iterations used to calculate the Wasserstein projections. Not all methods return a number of iterations so this may be `NULL`}
#' \item{`nzero`: The number of non zero coefficients in the final models}
#' }
#' 
#' @description 
#' `r lifecycle::badge("experimental")`
#' This function will calculate linear projections from a set of predictions into the space of the covariates in terms of the p-Wasserstein distance.
#'
#' @details
#' ## Methods
#' The `WpProj` function is a wrapper for the various Wasserstein projection methods. It is designed to be a one-stop shop for all Wasserstein projection methods. It will automatically choose the correct method and solver based on the arguments provided. It will also return a standardized output for all methods. Each method has its own set of options that can be passed to it. See the documentation for each method for more information.
#' 
#' For the L1 methods, see [L1_method_options()] for more information. For the binary program methods, see [binary_program_method_options()] for more information. For the stepwise methods, see [stepwise_method_options()] for more information. For the simulated annealing methods, see [simulated_annealing_method_options()] for more information.
#' 
#' In most cases, we recommend using the L1 methods or binary program methods. The L1 methods are the fastest and applicable to Wasserstein powers of any value greater than 1 and function as direct linear projections into the space of the covariates. The binary program methods instead preserve the coefficients of the original model if this is of interest, such as when the original model was already a linear model. The binary program will instead function as a way of turning on and off certain coefficients in a way that minimizes the Wasserstein distance between reduced and original models. Of note, we also have available an approximate binary program method using a lasso solver. This method is faster than the exact binary program method but is not guaranteed to find the optimal solution. It is recommended to use the exact binary program method if possible. See [binary_program_method_options()] for more information on how to set up the approximate method as some arguments for the lasso solver should be specified. For more information on how this works, please also see the referenced paper.
#' 
#' The stepwise, simulated annealing, and L0 methods also select covariates like the binary program methods but they can be slower. They are presented merely for comparison purposes given they were used in the original paper.
#' 
#' ## Wasserstein distances and powers
#' The Wasserstein distance is a measure of distance between two probability distributions. It is defined as:
#' \deqn{W_p(\mu,\nu) = \left(\inf_{\pi \in \Pi(\mu,\nu)} \int_{\mathbb{R}^d \times \mathbb{R}^d} \|x-y\|^p d\pi(x,y)\right)^{1/p},}
#' where \eqn{\Pi(\mu,\nu)} is the set of all joint distributions with marginals \eqn{\mu} and \eqn{\nu}. The Wasserstein distance is a generalization of the Euclidean distance, which is the case when \eqn{p=2}. In our function we have argument `power` that corresponds to the \eqn{p} of the equation above. The default `power` is `2.0` but any value greater than or equal to `1.0` is allowed. For more information, see the references.
#' 
#' The particular implementation of the Wasserstein distance is as follows. If  \eqn{\mu} is the original prediction from the original model, then we seek to find a new prediction \eqn{\nu} that minimizes the Wasserstein distance between the two: \eqn{\text{argmin}_\nu W_p(\mu,\nu)}. 
#' 
#' @references Dunipace, Eric and Lorenzo Trippa (2020) <https://arxiv.org/abs/2012.09999>.
#' 
#' @export
#' 
#' @examples
#' if(rlang::is_installed("stats")) {
#' # note we don't generate believable data with real posteriors
#' # these examples are just to show how to use the function
#' n <- 32
#' p <- 10
#' s <- 21
#' 
#' # covariates and coefficients
#' x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
#' beta <- (1:10)/10
#' 
#' #outcome
#' y <- x %*% beta + stats::rnorm(n)
#' 
#' # fake posterior
#' post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
#' post_mu <- x %*% post_beta #posterior predictive distributions
#' 
#' # fit models
#' ## L1 model
#' fit.p2     <-  WpProj(X=x, eta=post_mu, power = 2.0,
#'                    method = "L1", #default
#'                    solver = "lasso" #default
#' )
#' 
#' ## approximate binary program
#' fit.p2.bp <-  WpProj(X=x, eta=post_mu, theta = post_beta, power = 2.0,
#'                    method = "binary program",
#'                    solver = "lasso" #default because approximate algorithm is faster
#' )
#' 
#' ## compare performance by measuring distance from full model
#' dc <- distCompare(models = list("L1" = fit.p2, "BP" = fit.p2.bp))
#' plot(dc)
#' 
#' ## compare performance by measuring the relative distance between a null model 
#' ## and the predictions of interest as a pseudo R^2
#' r2.expect <- WPR2(predictions = post_mu, projected_model = dc) # can have negative values
#' r2.null  <- WPR2(projected_model = dc) # should be between 0 and 1
#' plot(r2.null)
#' 
#' ## we can also examine how predictions change in the models for individual observations
#' ridgePlot(fit.p2, index = 21, minCoef = 0, maxCoef = 10)
#' }
WpProj <- function(X, eta=NULL, theta = NULL, power = 2.0,
                 method = c("L1", "binary program", "stepwise","simulated annealing","L0"),
                 solver = c("lasso", "ecos", "lpsolve", "mosek"),
                 options = NULL)
{
  # save call
  this.call <- as.list(match.call()[-1])
  
  # make sure specific combo choen makes sense
  args    <- method_lookup(power, this.call$method, 
                           this.call$solver, options) # makes sure combo makes sense
  
  # setup function args
  model_args  <- arrange_args(model_function = args$fun, 
                             X = X, Y = eta, theta = theta,
                             power = args$power,
                             method = args$method,
                             solver = args$solver,
                             options = args$options)
  
  # eval function
  result <- do.call(args$fun, model_args)
  
  # standardize outputs from various function calls
  output <- standardize_output(result, args, this.call)
     
  return(output)
  
}


method_lookup <- function(power, method, solver, options) {
  # lookup: power, method, solver
  
  # check power
  stopifnot("Argument `power` must be a number >= 1.0" = is.numeric(power))
  stopifnot("Argument `power` must be >= 1.0" = isTRUE(power >= 1.0))
  
  # don't want visible function everywhere but keeping independent to not pollute namespace
  warn_for_overuse_of_lasso <- function(out) {
    if (out$power == 1.0 && out$method == "L1" && out$solver == "lasso") {
      warning("Using the lasso solver for Wasserstein power == 1 is inefficient. We recommend using 'ecos', which is a free solver already imported by this package, or 'mosek' instead.")
    }
    
    if ( is.infinite(out$power) && out$method == "L1" && out$solver == "lasso") {
      warning("Using the lasso solvers for Wasserstein power == Inf (max norm) is inefficient and likely to fail. We recommend using 'ecos', which is a free solver already imported by this package, or 'mosek' instead.")
    }
  }
  
  if(is.finite(power) && power != 1.0 && power != 2.0) {
    power_check <- 3.0
  } else {
    power_check <- power
  }
  
  pwr <- table_lookup %>% dplyr::filter(power == power_check | is.na(power))
  
  if (nrow(pwr) > 0) {
    # all combos should pass this stage
    
    # input default methods (L1, IP, etc)
    if ( is.null(method) )  method <- pwr$method[1L]
    meth <- pwr %>% dplyr::filter(method == !!method)
    
    if (nrow(meth) == 0) {
      meth <- pwr %>% dplyr::filter(method == pwr$method[1L])
      warning(sprintf("Using method %s with Wasserstein power %s is not allowed. Switching to method %s.", method, power,  pwr$method[1L]))
    }
    
    if ( is.null(solver) ) solver <- meth$solver[1L]
    sol <- meth %>% dplyr::filter(solver == !!solver | is.na(solver))
    
    if ( nrow(sol) == 0 ) {
      sol <- meth %>% dplyr::filter(solver == meth$solver[1L] | is.na(solver))
      warning(sprintf("Using solver %s with Wasserstein power %s and method %s is not allowed. Switching to solver %s.", solver, power, method, meth$solver[1L]))
    }
    
  } else {
    # if for some reason the power check fails
    stop(sprintf("Methods using Wasserstein power %s not found", power))
  }
  
  solver <- switch(sol$solver,
                   "ecos" = "cone",
                   "lpsolve" = "lp",
                   sol$solver)
  
  out <- list(power = power,
              method = sol$method,
              solver = sol$solver,
              options = options,
              fun = sol$fun)
  
  warn_for_overuse_of_lasso(out) #W1 and WInf should use linear program solvers
  
  return(out)
  
  
}


table_lookup <- list(
  # all L1 penalized methods
  data.frame(method = "L1",
             solver = c("ecos","mosek","lasso"),
             power = 1.0,
             penalty = TRUE,
             fun = c("W1L1","W1L1","WPL1")),
  data.frame(method = "L1",
             solver = c("lasso"),
             power = 2.0,
             penalty = TRUE,
             fun = "WPL1"),
  # data.frame(method = "L1",
  #            solver = c("lasso","ecos","mosek"),
  #            power = 2.0,
  #            penalty = TRUE,
  #            fun = c("WPL1","WlPL1","WlPL1")),
  # data.frame(method = "L1",
  #            solver = c("lasso","ecos","mosek"),
  #            power = 3.0,
  #            penalty = TRUE,
  #            fun = c("WPL1","WlPL1","WlPL1")),
  data.frame(method = "L1",
             solver = c("lasso"),
             power = 3.0,
             penalty = TRUE,
             fun = c("WPL1")),
  data.frame(method = "L1",
             solver = c("ecos","mosek","lasso"),
             power = Inf,
             penalty = TRUE,
             fun = c("WInfL1","WInfL1","WPL1")),
  data.frame(method = "binary program",
             solver = c("lasso","lpsolve","ecos","mosek"),
             power = 2.0,
             penalty = c(FALSE, FALSE, FALSE, TRUE),
             fun = c("WPL1", "W2IP","W2IP","W2IP")),
  
  # other methods
  data.frame(method = "stepwise",
             solver = NA_real_,
             power = NA_real_,
             penalty = FALSE,
             fun = "WPSW"),
  
  data.frame(method = "simulated annealing",
             solver = NA_real_,
             power = NA_real_,
             penalty = FALSE,
             fun = "WPSA"),
  
  data.frame(method = "L0",
             solver = NA_real_,
             power = NA_real_,
             penalty = FALSE,
             fun = "WPL0")

) %>% 
  do.call("rbind",.)


arrange_args <- function(model_function, 
                         X, Y, theta,
                         power,
                         method,
                         solver,
                         options) {
  
  solver <- switch(solver,
                   "ecos" = "cone",
                   "lpsolve" = "lp",
                   "mosek" = "mosek",
                   "lasso" = "lasso",
                   solver)
  
  if(!is.list(options)) options <- as.list(options)
  
  stopifnot(is.list(options))
  
  #special checks for binary program
  if (solver == "lasso" && method == "binary program") {
    # needs lasso params and special handling for approximate method
    if(!is.list(options$solver.options)) options$solver.options <- as.list(options$solver.options)
    options$solver.options <- do.call(L1_method_options, options$solver.options)
    options$solver.options$method <- "selection.variable"
  }
  
  # must supply theta for binary program
  if (method == "binary program") {
    theta_problem <- !(missing(theta) || is.null(theta))
    stopifnot('Argument `theta` must be provided for method = "binary program"'=theta_problem)
  }
  
  options <- switch(method,
                       "L1" = do.call(L1_method_options, options),
                       "binary program" = do.call(binary_program_method_options, options),
                       "stepwise" = do.call(stepwise_method_options, options),
                       "simulated annealing" = do.call(simulated_annealing_method_options, options),
                       "L0" = do.call(L0_method_options, options)
                    )
  
  args <- c(list(X = X, Y = Y, theta = theta, power = power,
               # method = method,
               solver = solver), options)
  
  # no need to pass power to these functions and in fact, it will mess them up
  if(model_function %in% c("W1L1", "WInfL1")) args$power <- NULL
    
  # this will remove dots which I don't want
  # matched.args <- which(names(args) %in% methods::formalArgs(model_function))
  # return(args[matched.args])
  
  dup_names <- duplicated(names(args))
  if(any(dup_names)) warning("Duplicate arguments found. Using first instance of each.")
  non_dup <- which(!dup_names)
  return(args[non_dup])
  
  
} 

standardize_output <- function(res, args, call) {
  
  
  output <- list(
    call = call,
    theta = res$theta,
    fitted.values = res$eta,
    power = args$power,
    method = args$method,
    solver = args$solver,
    niter = res$niter,
    nzero = res$nzero
  )
  
  class(output) <- c("WpProj")
  
  return(output)
}
