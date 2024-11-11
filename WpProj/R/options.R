#' Available Wasserstein Distance Methods
#'
#' @return A character vector of available methods
#' @export
#' 
#' @details
#' This function features several methods of calculating approximate optimal transport methods in addition to the exact method. The first is the "sinkhorn" method of Cuturi (2013). The second is the "greenkhorn" method of Altschuler et al. (2017). The third is the Hilbert sorting method of Bernton et al (2017). Then there are two novel approximation methods based on the univariate ranks of each covariate to obtain the average rank "rank", and a method based on the univariate distances for each covariate "univariate.approximation.pwr"
#' 
#'
#' @examples
#' transport_options()
transport_options <- function() {
  return(c("exact", "sinkhorn", "greenkhorn",
           # "randkhorn", "gandkhorn",
           "hilbert", "rank",
           "univariate.approximation.pwr"))
}


#' Recognized L1 Penalties
#'
#' @return A character vector with the possible penalties for L1 methods
#' @export
#'
#' @examples
#' L1_penalty_options()
#' # [1] "lasso"            "ols"              "mcp"              "elastic.net"      "scad"            
#' # [6] "mcp.net"          "scad.net"         "grp.lasso"        "grp.lasso.net"    "grp.mcp"         
#' # [11] "grp.scad"         "grp.mcp.net"      "grp.scad.net"     "sparse.grp.lasso"
L1_penalty_options <- function() {
  c("lasso", "ols", "mcp", "elastic.net",
    "scad", "mcp.net",
    "scad.net",
    "grp.lasso",
    "grp.lasso.net", "grp.mcp",
    "grp.scad", "grp.mcp.net",
    "grp.scad.net",
    "sparse.grp.lasso")
}

verify_solver_options <- function(solver.options, function_name) {
  if(!is.null(solver.options)) {
    stopifnot(is.list(solver.options))
    argnames <- methods::formalArgs(function_name)
    argnames <- argnames[!argnames %in% c("solver.options")]
    solver_names <- names(solver.options)[!(names(solver.options) %in% argnames)]
    solver.options <- solver.options[solver_names]
  }
  return(solver.options)
}


#' Options For Use With the L1 Method
#'
#' @param penalty The penalty to use. See [L1_penalty_options()] for more details.
#' @param lambda The penalty parameter to use if method is "L1".
#' @param nlambda The number of lambdas to explore for the "L1" method if `lambda` is not provided 
#' @param lambda.min.ratio The minimum ratio of max to min lambda for "L1" method. Default 1e-4.
#' @param gamma Tuning parameter for SCAD and MCP penalties if method = "L1".
#' @param maxit The maximum iterations for optimization. Default is 500.
#' @param model.size What is the maximum number of coefficients to have in the final model. Default is NULL. If NULL, will find models from the minimum size, 0, to the number of columns in `X`.
#' @param tol The tolerance for convergence
#' @param display.progress Logical. Should intermediate progress be displayed? TRUE or FALSE. Default is FALSE.
#' @param solver.options Options to be passed on to the solver. Only used for "ecos" and "mosek" solvers.
#' 
#' @return A list with names corresponding to each argument above.
#' 
#' @seealso [WpProj()]
#' 
#' @export
#' 
#' @examples
#' L1_method_options()
L1_method_options <- function(penalty =  L1_penalty_options(),
                                 lambda = numeric(0),
                                 nlambda = 500L,
                                 lambda.min.ratio = 1e-4,
                                 gamma = 1, maxit = 500L,
                                 model.size = NULL,
                                 tol = 1e-07,
                              display.progress = FALSE,
                              solver.options = NULL) {
  
  # WPL1(X, Y=NULL, theta = NULL, power = 2.0,
  #                  penalty =  c("lasso", "ols", "mcp", "elastic.net",
  #                               "selection.lasso",
  #                               "scad", "mcp.net",
  #                               "scad.net",
  #                               "grp.lasso",
  #                               "grp.lasso.net", "grp.mcp",
  #                               "grp.scad", "grp.mcp.net",
  #                               "grp.scad.net",
  #                               "sparse.grp.lasso"),
  #                  model.size = NULL,
  #                  lambda = numeric(0),
  #                  nlambda = 100L,
  #                  lambda.min.ratio = 1e-4,
  #                  gamma = 1, maxit = 500L,
  #                  tol = 1e-07, display.progress = FALSE, ...)
  
  # make sure args make sense
  if(length(lambda) > 0 ) stopifnot("lambda can be missing or a numeric vector" = all(lambda >=0))
  stopifnot(nlambda > 0L)
  stopifnot(lambda.min.ratio >= 0)
  stopifnot(gamma >= 0.0)
  stopifnot(maxit > 0L)
  if (!is.null(model.size)) {
    stopifnot(is.numeric(model.size))
    stopifnot("model.size must be NULL or > 0" = model.size > 0)
  }
  stopifnot( tol > 0)
  stopifnot(is.logical(display.progress))
  
  # make sure args are of the right data type
  penalty <- match.arg(penalty, choices = L1_penalty_options(), several.ok = FALSE)
  lambda  <- as.double(lambda)
  nlambda <- as.integer(nlambda)
  lambda.min.ratio <- as.double(lambda.min.ratio)
  gamma <- as.double(gamma)
  maxit <- as.integer(maxit)
  tol <- as.double(tol)
  
  solver.options <- verify_solver_options(solver.options, "L1_method_options")
  
  
  return(c(list(penalty = penalty,
              lambda = lambda,
              nlambda = nlambda,
              lambda.min.ratio = lambda.min.ratio,
              gamma = gamma,
              maxit = maxit,
              model.size = model.size,
              tol = tol,
              display.progress = display.progress),
              solver.options))
  
}

#' Options For Use With the Binary Program Method
#'
#' @param maxit The maximum iterations for the optimizer. Default is 500.
#' @param infimum.maxit Maximum iterations to alternate binary program and Wasserstein distance calculations
#' @param transport.method Method for Wasserstein distance calculation. Should be one the outputs of [transport_options()]
#' @param epsilon A value > 0 for the penalty parameter of if using the Sinkhorn method
#' @param OTmaxit The number of iterations to run the Wasserstein distance solvers.
#' @param model.size What is the maximum number of coefficients to have in the final model. Default is NULL. If NULL, will find models from the minimum size, 0, to the number of columns in `X`.
#' @param nvars The number of variables to explore. Should be an integer vector of model sizes. Default is NULL which will explore all models from 1 to `model.size`.
#' @param tol The tolerance for convergence
#' @param display.progress Logical. Should intermediate progress be displayed? TRUE or FALSE. Default is FALSE.
#' @param parallel A cluster backend to be used by [foreach::foreach()]. See [foreach::foreach()] for details about how to set them up. The `WpProj` functions will register the cluster with the [doParallel::registerDoParallel()] function internally.
#' @param solver.options Options to be passed on to the solver. See details
#'
#' @return A list with names corresponding to each argument above.
#' 
#' @details
#' This function will setup the default arguments used by the binary program method. Of note, for the argument `solver.options`, If using the "lasso" solver, you should provide arguments such as "penalty", "nlambda", "lambda.min.ratio", "gamma", and "lambda" in a list. A simple way to do this is to feed the output of the [L1_method_options()] function to the argument `solver.options.` This will tell the approximate solver, which uses a lasso method that then will project the parameters back to the \eqn{\{0,1\}} space. For the other solvers, you can see the options in the ECOS solver package, [ECOSolveR::ecos.control()], and the options for the mosek solver, [Rmosek::mosek()].
#' 
#' @seealso [WpProj()]
#' 
#' 
#' @export
#'
#' @examples
#' binary_program_method_options()
#' # is using the lasso solver for the binary program method to give an approximate solution
#' binary_program_method_options(solver.options = L1_method_options(nlambda = 50L))
binary_program_method_options <- function(
                                          maxit = 500L,
                                          infimum.maxit = 100L,
                                          transport.method = transport_options(),
                                          epsilon = 0.05,
                                          OTmaxit = 100L,
                                          model.size = NULL,
                                          nvars = NULL,
                                          tol = 1e-7,
                                          display.progress=FALSE, 
                                          parallel = NULL, solver.options = NULL) {
  # W2IP(X, Y=NULL, theta,
  #      transport.method = transport_options(),
  #      model.size = NULL,
  #      maxit = 100L
  #      infimum.maxit = 100,
  #      tol = 1e-7,
  #      solver = c("cone","lp", "cplex", "gurobi","mosek"),
  #      display.progress=FALSE, parallel = NULL, ...)
  
  # make sure args make sense
  stopifnot(is.numeric(epsilon))
  stopifnot(epsilon > 0.0)
  stopifnot(is.numeric(maxit))
  stopifnot(maxit > 0L)
  stopifnot(infimum.maxit > 0L)
  stopifnot(is.numeric(OTmaxit))
  stopifnot(OTmaxit > 0L )
  if (!is.null(model.size)) {
    stopifnot(is.numeric(model.size))
    stopifnot("model.size must be NULL or > 0" = model.size > 0)
    model.size <- as.integer(model.size)
  }
  if (!is.null(nvars)) {
    stopifnot("nvars must be NULL or an integer vector of model sizes to find." = is.numeric(nvars))
    stopifnot("nvars must be NULL or an integer vector of model sizes to find." = all(nvars > 0))
    nvars <- as.integer(nvars)
  }
  stopifnot(is.numeric(tol))
  stopifnot( tol > 0)
  stopifnot(is.logical(display.progress))
  
  # make sure args are of the right data type
  transport.method <- match.arg(transport.method, choices = transport_options(), several.ok = FALSE)
  epsilon <- as.double(epsilon)
  maxit <- as.integer(maxit)
  infimum.maxit <- as.integer(infimum.maxit)
  OTmaxit <- as.integer(OTmaxit)
  tol <- as.double(tol)
  
  # check parallel
  if(!is.null(parallel)) {
    stopifnot(inherits(parallel, "cluster") || is.numeric(parallel))
  }
  
  solver.options <- verify_solver_options(solver.options, "binary_program_method_options")
  
  return(c(list(transport.method = transport.method,
              epsilon = epsilon,
              maxit = maxit,
              infimum.maxit = infimum.maxit,
              OTmaxit = OTmaxit,
              model.size = model.size,
              nvars = nvars,
              tol = tol,
              display.progress=display.progress), 
              parallel = parallel, solver.options))
  
}


#' Options For Use With the Stepwise Selection Method
#'
#' @param force Any covariates to force into the model? Should be by column number or NULL if no variables to force into the model.
#' @param direction "forward" or "backward" selection? Default is "backward"
#' @param method Should covariates be selected as an approximate "binary program" or should a projection method be used. Default is the approximate binary program.
#' @param transport.method Method for Wasserstein distance calculation. Should be one the outputs of [transport_options()]
#' @param epsilon A value > 0 for the penalty parameter of if using the Sinkhorn method for optimal transport
#' @param OTmaxit The number of iterations to run the Wasserstein distance solvers.
#' @param model.size How many coefficients should the maximum final model have?
#' @param display.progress Logical. Should intermediate progress be displayed? TRUE or FALSE. Default is FALSE.
#' @param parallel A cluster backend to be used by [foreach::foreach()]. See [foreach::foreach()] for details about how to set them up. The `WpProj` functions will register the cluster with the [doParallel::registerDoParallel()] function internally.
#' @param calc.theta Return the linear coefficients? Default is TRUE.
#' @param ... Not used
#'
#' @return A named list with the above arguments
#' @export
#'
#' @examples
#' stepwise_method_options()
stepwise_method_options <- function(force = NULL, 
                                    direction = c("backward","forward"), 
                                    method= c("binary program","projection"),
                                    transport.method = transport_options(),
                                    OTmaxit = 100,
                                    epsilon = 0.05,
                                    model.size = NULL,
                                    display.progress = FALSE,
                                    parallel = NULL,
                                    calc.theta = TRUE,
                                    ...
                                    ) {
  # WPSW(X, Y, theta, power = 2,
  #                  force = NULL, 
  #                  direction = c("backward","forward"), 
  #                  method=c("selection.variable","scale","projection"),
  #                  transport.method = transport_options(),
  #                  OTmaxit = 100,
  #                  epsilon = 0.05,
  #                  calc.theta = TRUE,
  #                  model.size = NULL,
  #                  parallel = NULL,
  #                  display.progress = FALSE)
  
  method <- match.arg(method)
  method <- switch(method,
                   "binary program" = "selection.variable",
                   "projection" = "projection",
                   "selection.variable"
                   )
  
  direction <- match.arg(direction)
  
  if( !is.null(force) ) {
    stopifnot("force should be NULL or integer vector of covariates to keep in the model" = isTRUE(all(is.numeric(force))))
    stopifnot("force should be NULL or integer vector of covariates to keep in the model" = isTRUE(all(force > 0)))
    stopifnot("force should be NULL or integer vector of covariates to keep in the model"= isTRUE(all(is.wholenumber(force))))
  }
  
  
  # make sure args make sense
  stopifnot(is.numeric(epsilon))
  stopifnot(epsilon > 0.0)
  stopifnot(is.numeric(OTmaxit))
  stopifnot(OTmaxit > 0L )
  if (!is.null(model.size)) {
    stopifnot(is.numeric(model.size))
    stopifnot("model.size must be NULL or > 0" = model.size > 0)
  }
  stopifnot(is.logical(display.progress))
  stopifnot(is.logical(calc.theta))
  
  # make sure args are of the right data type
  transport.method <- match.arg(transport.method, choices = transport_options(), several.ok = FALSE)
  epsilon <- as.double(epsilon)
  OTmaxit <- as.integer(OTmaxit)
  
  # check parallel
  if(!is.null(parallel)) {
    stopifnot(inherits(parallel, "cluster") || is.numeric(parallel))
    if (is.numeric(parallel)) parallel <- as.integer(parallel)
  }
  
  return(list(
    force = force, 
    direction = direction, 
    method = method,
    epsilon = epsilon,
    transport.method = transport.method,
    OTmaxit = OTmaxit,
    model.size = model.size,
    display.progress=display.progress, 
    parallel = parallel,
    calc.theta = calc.theta))
  
}

#' Options For Use With the Simulated Annealing Selection Method
#'
#' @param force Any covariates to force into the model? Should be by column number or NULL if no variables to force into the model. 
#' @param method Should covariates be selected as an approximate "binary program" or should a projection method be used. Default is the approximate binary program.
#' @param transport.method Method for Wasserstein distance calculation. Should be one the outputs of [transport_options()]
#' @param epsilon A value > 0 for the penalty parameter of if using the Sinkhorn method for optimal transport
#' @param OTmaxit The number of iterations to run the Wasserstein distance solvers.
#' @param maxit Maximum number of iterations per temperature
#' @param temps Number of temperatures to try
#' @param max.time Maximum time in seconds to run the algorithm
#' @param proposal.method The method to propose the next covariate to add. One of "covariance" or "random". "covariance" will randomly select from covariates with probability proportional to the absolute value of the covariance. "uniform" will select covariates uniformly at random.
#' @param energy.distribution The energy distribution to use for evaluating proposals. One of "boltzman" or "bose-einstein". Default is "boltzman".
#' @param cooling.schedule The schedule to use for cooling temperatures. One of "Geman-Geman" or "exponential". Default is "Geman-Geman".
#' @param model.size How many coefficients should the maximum final model have?
#' @param display.progress Logical. Should intermediate progress be displayed? TRUE or FALSE. Default is FALSE.
#' @param parallel A cluster backend to be used by [foreach::foreach()]. See [foreach::foreach()] for details about how to set them up. The `WpProj` functions will register the cluster with the [doParallel::registerDoParallel()] function internally.
#' @param calc.theta Return the linear coefficients? Default is TRUE.
#' @param ... Not used.
#'
#' @return A named list with the above arguments
#' @export
#'
#' @examples
#' simulated_annealing_method_options()
simulated_annealing_method_options <- function(
    force = NULL,
    method= c("binary program","projection"),
    transport.method = transport_options(),
    OTmaxit = 100L,
    epsilon = 0.05,
    maxit = 1L,
    temps = 1000L,
    max.time = 3600,
    proposal.method =  c("covariance","uniform"),
    energy.distribution = c("boltzman","bose-einstein"),
    cooling.schedule = c("Geman-Geman","exponential"),
    model.size = NULL,
    display.progress = FALSE,
    parallel = NULL,
    calc.theta = TRUE,
    ...
  ) {
  # WPSA(X, Y=NULL, theta, 
  #                  power = 2, force = NULL, 
  #                  model.size = 3,
  #                  # groups = NULL,
  #                  maxit=1, temps = 1000,
  #                  max.time = 3600, const = NULL,
  #                  proposal = proposal.fun,
  #                  options = list(method = c("selection.variable","scale","projection"),
  #                                 transport.method = transport_options(),
  #                                 energy.distribution = "boltzman",
  #                                 cooling.schedule = "Geman-Geman",
  #                                 proposal.method = "covariance",
  #                                 epsilon = 0.05,
  #                                 OTmaxit = 100),
  #                  display.progress = FALSE,
  #                  parallel = NULL,
  #                  calc.theta = TRUE,
  #                  xtx = NULL,
  #                  xty = NULL
  # ) 
  
  method <- match.arg(method)
  method <- switch(method,
                   "binary program" = "selection.variable",
                   "projection" = "projection",
                   "selection.variable"
  )
  
  
  if( !is.null(force) ) {
    stopifnot("force should be NULL or integer vector of covariates to keep in the model" = isTRUE(all(is.numeric(force))))
    stopifnot("force should be NULL or integer vector of covariates to keep in the model" = isTRUE(all(force > 0)))
    stopifnot("force should be NULL or integer vector of covariates to keep in the model"= isTRUE(all(is.wholenumber(force))))
  }
  
  
  # make sure args make sense
  stopifnot(is.numeric(epsilon))
  stopifnot(epsilon > 0.0)
  stopifnot(is.numeric(maxit))
  stopifnot(maxit > 0L)
  stopifnot(is.numeric(OTmaxit))
  stopifnot(OTmaxit > 0L )
  if (!is.null(model.size)) {
    stopifnot(is.numeric(model.size))
    stopifnot("model.size must be NULL or > 0" = model.size > 0)
  }
  stopifnot(is.logical(display.progress))
  stopifnot(is.logical(calc.theta))
  stopifnot(max.time > 0)
  stopifnot(temps > 0)
  
  # make sure character args are correct
  transport.method <- match.arg(transport.method, choices = transport_options(), several.ok = FALSE)
  energy.distribution <- match.arg(energy.distribution)
  cooling.schedule <- match.arg(cooling.schedule)
  proposal.method <- match.arg(proposal.method)
  proposal.method <- switch(proposal.method,
                            "uniform" = "random",
                            "covariance" = "covariance",
                            proposal.method)
  
  # make sure args are of the right data type
  epsilon <- as.double(epsilon)
  maxit <- as.integer(maxit)
  OTmaxit <- as.integer(OTmaxit)
  max.time <- as.double(max.time)
  temps <- as.integer(temps)
  
  # check parallel
  if(!is.null(parallel)) {
    stopifnot(inherits(parallel, "cluster") || is.numeric(parallel))
    if (is.numeric(parallel)) parallel <- as.integer(parallel)
  }
  
  if (!is.null(model.size)) {
    model.size <- as.integer(model.size)
  }
  
  return(list(
    force = force,
    model.size  = model.size,
    maxit = maxit,
    temps = temps,
    max.time = max.time,
    options = list(
      method = method,
      transport.method = transport.method,
      energy.distribution = energy.distribution,
      cooling.schedule = cooling.schedule,
      proposal.method = proposal.method,
      epsilon = epsilon,
      OTmaxit = OTmaxit),
    display.progress = display.progress,
    parallel = parallel,
    calc.theta = calc.theta
    )
  )
  
}

#' Options For Use With the L0 Method
#' 
#' @param method Should covariates be selected as an approximate "binary program" or should a projection method be used. Default is the approximate binary program.
#' @param transport.method Method for Wasserstein distance calculation. Should be one the outputs of [transport_options()].
#' @param epsilon A value > 0 for the penalty parameter if using the Sinkhorn method for optimal transport
#' @param OTmaxit The number of iterations to run the Wasserstein distance solvers.
#' @param parallel A cluster backend to be used by [foreach::foreach()] if parallelization is desired.
#' @param ... Not used
#' 
#' @export
#' 
#' @returns a named list corresponding to the above arguments
#' 
#' @examples
#' L0_method_options()
L0_method_options <- function(method = c("binary program", "projection"),
                              transport.method = transport_options(),
                              epsilon = 0.05,
                              OTmaxit = 100,
                              parallel = NULL,
                              ...){
  # WPL0(X, Y = NULL, theta, power = 2,
  #      method = c("selection.variable", "projection"),
  #      transport.method = transport_options(),
  #      epsilon = 0.05, maxit = 100,
  #      parallel = NULL) 
  
  method <- match.arg(method)
  method <- switch(method,
                   "binary program" = "selection.variable",
                   "projection" = "projection",
                   "selection.variable"
  )
  
  stopifnot(is.numeric(epsilon))
  stopifnot(epsilon > 0.0)
  stopifnot(is.numeric(OTmaxit))
  stopifnot(OTmaxit > 0L )
  
  epsilon <- as.double(epsilon)
  OTmaxit <- as.integer(OTmaxit)
  
  # check parallel
  if(!is.null(parallel)) {
    stopifnot( inherits(parallel, "cluster") || is.numeric(parallel) )
    if (is.numeric(parallel)) {
      parallel <- as.integer(parallel)
      stopifnot("parallel should be a cluster object or an integer > 0" = parallel > 0)
    }
  }
  
  return(list(
    method = method,
    transport.method = transport.method,
    epsilon = epsilon,
    OTmaxit = OTmaxit,
    parallel = parallel
  ))
}

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
