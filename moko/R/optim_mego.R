#' Augmented Tchebycheff function
#'
#' The Augmented Tchebycheff function (KNOWLES, 2006) is a scalarizing function
#' witch the advantages of having a non-linear term. That causes points on
#' nonconvex regions of the Pareto front can be minimizers of this function
#' and, thus, nonsupported solutions can be obtained.
#'
#' @references Knowles, J. (2006). ParEGO: a hybrid algorithm with on-line
#'   landscape approximation for expensive multiobjective optimization problems.
#'   \emph{IEEE Transactions on Evolutionary Computation}, 10(1), 50-66.
#'
#' @param y Numerical matrix or data.frame containing the responses (on each
#'   column) to be scalarized.
#' @param s Numerical integer (default: 100) setting the number of partitions
#'   the vector lambda has.
#' @param rho A small positive value (default: 0.1) setting the "strength" of
#'   the non-linear term.
#'
#' @export
#' @examples
#' grid <- expand.grid(seq(0, 1, , 50),seq(0, 1, , 50))
#'  res <- t(apply(grid, 1, nowacki_beam))
#'  plot(nowacki_beam_tps$x, xlim=c(0,1), ylim=c(0,1))
#'  grid <- grid[which(as.logical(apply(res[,-(1:2)] < 0, 1, prod))),]
#'  res <- res[which(as.logical(apply(res[,-(1:2)] < 0, 1, prod))),1:2]
#' for (i in 1:10){
#' sres <- Tchebycheff(res[,1:2], s=100, rho=0.1)
#' points(grid[which.min(sres),], col='green')
#' }
Tchebycheff <- function(y, s=100, rho=0.1){ #add lambda as parameter or someway to define the wheighting
  if (round(s) != s)
    stop("'s' must be integer.")
  if (rho < 0)
    stop("'rho' must be positive.")
  lambda <- sample(0:s, ncol(y))/s
  lambda <- lambda/sum(lambda)
  y <- t(normalize(y))
  return((1 - rho) * apply(lambda * y, 2, max) + rho * apply(lambda * y, 2, sum))
}


DiceOptim_EI <- function (x, model, plugin = NULL, type = "UK", minimization = TRUE,
          envir = NULL)
{
  # Direct import from Archived package DiceOptim
  #
  # Reason:
  # Thus, package DiceOptim is now scheduled for archival on 2020-05-03,
  # and archiving this will necessitate also archiving its strong reverse
  # dependencies.
  #
  if (is.null(plugin)) {
    if (minimization) {
      plugin <- min(model@y)
    }
    else {
      plugin <- -max(model@y)
    }
  }
  m <- plugin
  d <- length(x)
  if (d != model@d) {
    stop("x does not have the right size")
  }
  newdata.num <- as.numeric(x)
  newdata <- data.frame(t(newdata.num))
  colnames(newdata) = colnames(model@X)
  predx <- DiceKriging::predict(object = model, newdata = newdata, type = type, checkNames = FALSE)
  kriging.mean <- predx$mean
  if (!minimization) {
    kriging.mean <- -kriging.mean
  }
  kriging.sd <- predx$sd
  xcr <- (m - kriging.mean)/kriging.sd
  if (kriging.sd/sqrt(model@covariance@sd2) < 1e-06) {
    res <- 0
    xcr <- xcr.prob <- xcr.dens <- NULL
  }
  else {
    xcr.prob <- stats::pnorm(xcr)
    xcr.dens <- stats::dnorm(xcr)
    res <- (m - kriging.mean) * xcr.prob + kriging.sd * xcr.dens
  }
  if (!is.null(envir)) {
    assign("xcr", xcr, envir = envir)
    assign("xcr.prob", xcr.prob, envir = envir)
    assign("xcr.dens", xcr.dens, envir = envir)
    assign("kriging.sd", kriging.sd, envir = envir)
    assign("c", predx$c, envir = envir)
    assign("Tinv.c", predx$Tinv.c, envir = envir)
  }
  return(res)
}


#' Constrained Expected Improvement
#'
#' This functions extends the EI function supplied by the package
#' archive package DiceOptim. This extension allows usage of multiple
#' expensive constraints. The constraints are passed to the revamped EI function
#' embedded inside the \code{\link{mkm}} object. Currently low-cost (explicit)
#' constraints are not allowed.
#'
#' The way that the constraints are handled are based on the probability of
#' feasibility. The strong assumption here is that the cost functions and the
#' constraints are uncorrelated. With that assumption in mind, a simple
#' closed-form solution can be derived that consists in the product of the
#' probability that each constraint will be met and the expected improvement of
#' the objective. Another important consideration is that, by default, the value
#' of the plugin passed to the EI is the best
#' \emph{feasible} observed value.
#'
#' @param x A vector representing the input for which one wishes to calculate EI.
#' @param model An object of class \code{\link{mkm}}. This \code{model} must have a single
#'  objective (\code{model@m == 1}).
#' @param control An optional list of control parameters, some of them passed to
#' the EI function. One can control:
#'   \describe{
#'    \item{\code{minimization}}{logical specifying if EI is used in minimization or in maximization
#'    (default: \code{TRUE})}
#'    \item{\code{plugin}}{optional scalar, if not provided, the minimum (or maximum) of the current
#'     feasible observations. If there isn't any feasible design plugin is set to \code{NA} and the
#'     algorithm returns the value of the probability of constraints be met.}
#'    \item{\code{envir}}{optional environment specifying where to assign intermediate values.
#'     Default: \code{NULL}.}
#'   }
#'
#' @references Forrester, A., Sobester, A., & Keane, A. (2008).
#'   \emph{Engineering design via surrogate modelling: a practical guide.} John
#'   Wiley & Sons.
#'
#' @export
#' @examples
#' # --------------------------------------------
#' # Branin-Hoo function (with simple constraint)
#' # --------------------------------------------
#' n <- 10
#' d <- 2
#' doe <- replicate(d,sample(0:n,n))/n
#' fun_cost <- DiceKriging::branin
#' fun_cntr <- function(x) 0.2 - prod(x)
#' fun <- function(x) return(cbind(fun_cost(x),fun_cntr(x)))
#' res <- t(apply(doe, 1, fun))
#' model <- mkm(doe, res, modelcontrol = list(objective = 1, lower=c(0.1,0.1)))
#' grid <- expand.grid(seq(0,1,,25),seq(0,1,,25))
#' ei <- apply(grid, 1, EI, model) # this computation may take some time
#' contour(matrix(ei,25))
#' points(model@design, col=ifelse(model@feasible,'blue','red'))
#' points(grid[which.max(ei),], col='green')
EI <- function(x, model, control = NULL){
  if (class(model) != 'mkm')
    stop('The class of "model" must be "mkm"')
  if (model@m > 1)
    stop('Model must have a single objective')
  if (is.null(control$minimization))
    control$minimization <- TRUE
  if (model@j == 0)
    probg <- 1
  else {
    model_g <- model@km[-model@objective]
    pred_g <- predict(list2mkm(model_g), data.frame(t(x)), model@control)
    s_g <- pred_g$sd
    m_g <- pred_g$mean
    probg <- prod(stats::pnorm(-m_g/s_g))
  }
  if (is.null(control$plugin)){
    if (any(model@feasible)){
      if (control$minimization)
        control$plugin <- min(model@response[which(model@feasible),model@objective])
      else
        control$plugin <- max(model@response[which(model@feasible),model@objective])
    }
    else
      control$plugin <- NA
  }
  if (is.na(control$plugin))
    ei <- 1
  else
    ei <- DiceOptim_EI(x, model=model@km[[model@objective]],
                        plugin = control$plugin,
                        type = model@control$type,
                        minimization = control$minimization,
                        envir = control$envir)
  return(ei*probg)
}

#devtools::use_package("GenSA")
#' max_EI: Maximization of the Constrained Expected Improvement criterion
#'
#' Given an object of class \code{\link{mkm}} and a set of tuning parameters,
#' max_EI performs the maximization of the Constrained Expected Improvement
#' criterion and delivers the next point to be visited in an MEGO-like
#' procedure.
#'
#' @inheritParams EI
#' @param optimcontrol Optional list of control parameters passed to the
#'   \code{\link[GenSA]{GenSA}} function. Please, note that the values are
#'   passed as the \code{control} parameter inside the \code{GenSA} function (\code{genSA(control = optimcontrol)}).
#' @param lower Vector of lower bounds for the variables to be optimized over
#'   (default: 0 with length = \code{model@d}),
#' @param upper Vector of upper bounds for the variables to be optimized over
#'   (default: 1 with length = \code{model@d}),
#' @return A list with components: \describe{
#'  \item{\code{par}}{The best set of parameters found.}
#'  \item{\code{value}}{The value of expected hypervolume improvement at par.}
#'  }
#'
#' @return Vector. The best set of parameters found.
#' @export
#' @examples
#' # --------------------------------------------
#' # Branin-Hoo function (with simple constraint)
#' # --------------------------------------------
#' n <- 10
#' d <- 2
#' doe <- replicate(d,sample(0:n,n))/n
#' fun_cost <- DiceKriging::branin
#' fun_cntr <- function(x) 0.2 - prod(x)
#' fun <- function(x) return(cbind(fun_cost(x),fun_cntr(x)))
#' res <- t(apply(doe, 1, fun))
#' model <- mkm(doe, res, modelcontrol = list(objective = 1, lower=c(0.1,0.1)))
#' max_EI(model)
max_EI <- function(model, lower = rep(0,model@d), upper = rep(1,model@d),
                   control = NULL, optimcontrol = NULL){
  if (class(model) != 'mkm')
    stop('The class of "model" must be "mkm"')
  if (model@m > 1)
    stop('Model must have a single objective')
  if(is.null(optimcontrol$max.time))
    optimcontrol$max.time <- 2
  fn <- function(x)
    return(-EI(x, model, control))
  res <- GenSA::GenSA(NULL, fn, lower, upper, control=optimcontrol)
  res$value <- -res$value
  return(res[c('value','par')])
}

#' MEGO: Multi-Objective Efficient Global Optimization Algorithm based on
#' scalarization of the objectives
#'
#' Executes \code{nsteps} iterations of the MEGO method to an object of class
#' \code{\link{mkm}}. At each step, a weighted kriging model is re-estimated
#' (including covariance parameters re-estimation) based on the initial design
#' points plus the points visited during all previous iterations; then a new
#' point is obtained by maximizing the Constrained Expected Improvement
#' criterion (EI).
#'
#' Note that since MEGO is works by scalarizing a cost function, this technique
#' is well suited for single objective problems with multiple constraints.
#'
#' @param fun The multi-objective and constraint cost function to be optimized.
#'   This function must return a vector with the size of \code{model@m +
#'   model@j} where \code{model@m} are the number of objectives and
#'   \code{model@j} the number of the constraints,
#' @param nsteps An integer representing the desired number of iterations,
#' @param quiet Logical indicating the verbosity of the routine,
#' @inheritParams EI
#' @inheritParams max_EI
#' @return updated \code{\link{mkm}} model
#'
#' @references Knowles, J. (2006). ParEGO: a hybrid algorithm with on-line
#'   landscape approximation for expensive multiobjective optimization problems.
#'   \emph{IEEE Transactions on Evolutionary Computation}, 10(1), 50-66.
#'
#' @export
#' @examples
#' # ----------------
#' # The Nowacki Beam
#' # ----------------
#' n <- 20
#' d <- 2
#' nsteps <- 1 # value has been set to 1 to save compliation time, change this value to 40.
#' fun <- nowacki_beam
#' doe <- replicate(d,sample(0:n,n))/n
#' res <- t(apply(doe, 1, fun))
#' model <- mkm(doe, res, modelcontrol = list(objective = 1:2, lower = rep(0.1,d)))
#' model <- MEGO(model, fun, nsteps, quiet = FALSE, control = list(rho = 0.1))
#' plot(nowacki_beam_tps$set)
#' points(ps(model@response[which(model@feasible),model@objective])$set, col = 'green', pch = 19)
#'
#' ############################################
#' #### some single objective optimization ####
#' ############################################
#' \dontrun{
#' ## Those examples are flagged as "don't run" only to save compilation time. ##
#' n.grid <- 20
#' x.grid <- y.grid <- seq(0,1,length=n.grid)
#' design.grid <- expand.grid(x.grid, y.grid)
#' response.grid <- apply(design.grid, 1, DiceKriging::branin)
#' z.grid <- matrix(response.grid, n.grid, n.grid)
#'
#' # -----------------------------------
#' # Branin-Hoo function (unconstrained)
#' # -----------------------------------
#' n <- 10
#' d <- 2
#' doe <- replicate(d,sample(0:n,n))/n
#' fun <- DiceKriging::branin
#' res <- apply(doe, 1, fun)
#' model <- mkm(doe, res, modelcontrol = list(lower=rep(0.1,d)))
#' model <- MEGO(model, fun, 10, quiet = FALSE)
#' contour(x.grid,y.grid,z.grid,40)
#' points(model@design, col=ifelse(model@feasible,'blue','red'))
#' # ---------------------------------------
#' # Branin-Hoo function (simple constraint)
#' # ---------------------------------------
#' n <- 10
#' d <- 2
#' doe <- replicate(d,sample(0:n,n))/n
#' fun_cost <- DiceKriging::branin
#' fun_cntr <- function(x) 0.2 - prod(x)
#' fun <- function(x) return(c(fun_cost(x),fun_cntr(x)))
#' res <- t(apply(doe, 1, fun))
#' model <- mkm(doe, res, modelcontrol = list(objective = 1, lower=rep(0.1,d)))
#' model <- MEGO(model, fun, 10, quiet = FALSE)
#' contour(x.grid,y.grid,z.grid,40)
#' points(model@design, col=ifelse(model@feasible,'blue','red'))
#' # ---------------------------------------
#' # Branin-Hoo function (narrow constraint)
#' # ---------------------------------------
#' n <- 10
#' d <- 2
#' doe <- replicate(d,sample(0:n,n))/n
#' fun_cost <- DiceKriging::branin
#' fun_cntr <- function(x){
#'  g1 <- 0.9 - sum(x)
#'  g2 <- sum(x) - 1.1
#'  g3 <- - x[1] + 0.75
#'  g4 <- x[2] - 0.25
#'  return(c(g1,g2,g3,g4))
#' }
#' fun <- function(x) return(c(fun_cost(x),fun_cntr(x)))
#' res <- t(apply(doe, 1, fun))
#' model <- mkm(doe, res, modelcontrol = list(objective = 1, lower=rep(0.1,d)))
#' model <- MEGO(model, fun, 10, quiet = FALSE)
#' contour(x.grid,y.grid,z.grid,40)
#' points(model@design, col=ifelse(model@feasible,'blue','red'))
#' # ---------------------------------------------
#' # Branin-Hoo function (disconnected constraint)
#' # ---------------------------------------------
#' n <- 10
#' d <- 2
#' doe <- replicate(d,sample(0:n,n))/n
#' Griewank <-  function(x) {
#'  ii <- c(1:length(x))
#'   sum <- sum(x^2/4000)
#'   prod <- prod(cos(x/sqrt(ii)))
#'   y <- sum - prod + 1
#'   return(y)
#' }
#' fun_cost <- DiceKriging::branin
#' fun_cntr <- function(x) 1.6 - Griewank(x*10-5)
#' fun <- function(x) return(c(fun_cost(x),fun_cntr(x)))
#' res <- t(apply(doe, 1, fun))
#' model <- mkm(doe, res, modelcontrol = list(objective = 1, lower=c(0.1,0.1)))
#' model <- MEGO(model, fun, 10, quiet = FALSE)
#' contour(x.grid,y.grid,z.grid,40)
#' points(model@design, col=ifelse(model@feasible,'blue','red'))
#' }
MEGO <- function(model, fun, nsteps, lower = rep(0, model@d), upper = rep(1, model@d), quiet = TRUE,
                 control = NULL, optimcontrol = NULL){
  time <- proc.time()
  if (class(model) != 'mkm')
    stop('The class of "model" must be "mkm"')
  s_modelcontrol <- model@control
  if (is.null(control$s))
    control$s <- 100
  if (is.null(control$rho))
    control$rho <- 0.05

  design <- model@design
  response <- model@response
  if (model@m > 1){
    s_response <- Tchebycheff(response[,model@objective],
                              s=control$s, rho=control$rho)
    s_response <- cbind(s_response, model@response[,-model@objective])
    s_modelcontrol$objective <- 1
  }
  else
    s_response <- model@response
  s_model <- mkm(design, s_response, s_modelcontrol)
  for(n in 1:nsteps){
    x_star <- max_EI(s_model, lower, upper, control, optimcontrol)$par
    y_star <- fun(x_star)
    design <- rbind(design, x_star)
    response <- rbind(response, y_star)
    rownames(response) <- NULL
    if (model@m > 1){
      s_response <- Tchebycheff(response[,model@objective],
                                       s=control$s, rho=control$rho)
      s_response <- cbind(s_response, response[,-model@objective])
    }
    else
      s_response <- response
#    s_model <- mkm(design, s_response, s_modelcontrol)
    .model <- try(mkm(design, s_response, s_modelcontrol), TRUE)
    if (class(.model) == 'mkm')
      s_model <- .model
    else{
      warning("Failed to update the kriging model at iteration number ",n,".")
      break
    }
    if (!quiet){
      cat('Current iteration:', n, '(elapsed', (proc.time()-time)[3], 'seconds)\n')
      cat('Current design:', round(x_star,3), '\n')
      cat('Current response:', round(y_star[model@objective],3),
          ifelse(utils::tail(s_model@feasible,1),'(feasible)','(unfeasible)'),'\n\n')
    }
  }
  model <- mkm(design, response, model@control)
  return(model)
}
