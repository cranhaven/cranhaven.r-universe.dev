
#devtools::use_package("mco")
#' Predicted Pareto front
#'
#' This function creates a predicted pareto front based on the mean of Kriging
#' models. The predicted mean of each objective and constraint is passed to the
#' \code{\link[mco]{nsga2}} algorithm that builds .
#'
#' @param model Object of class \code{\link{mkm}}.
#' @param lower Vector of lower bounds for the variables to be optimized over
#'   (default: 0 with length \code{model@d}).
#' @param upper Vector of upper bounds for the variables to be optimized over
#'   (default: 1 with length \code{model@d}).
#' @param control An optional list of control parameters that controls the
#'   optimization algorithm. One can control: \describe{
#'   \item{\code{popsize}}{(default: \code{200});}
#'   \item{\code{generations}}{(default: \code{30});}
#'   \item{\code{cdist}}{(default: \code{1/model@d});}
#'   \item{\code{mprob}}{(default: \code{15});}
#'   \item{\code{mdist}}{(default: \code{20}).}
#'   }
#' @param modelcontrol An optional list of control parameters to the
#'   \code{mkm} function (default: \code{object@control}).
#'
#' @return object of class \code{\link{ps}} containing the predicted Pareto front
#'
#' @export
#' @examples
#' # ------------------------
#' # The Nowacki Beam
#' # ------------------------
#' n <- 100
#' doe <- cbind(sample(0:n,n),sample(0:n,n))/n
#' res <- t(apply(doe, 1, nowacki_beam))
#' model <- mkm(doe, res, modelcontrol = list(objective = 1:2, lower=c(0.1,0.1)))
#' pf <- predict_front(model, c(0,0), c(1,1))
#' plot(nowacki_beam_tps$set)
#' points(pf$set, col='blue')
predict_front <- function(model, lower, upper, control = NULL, modelcontrol = NULL){

  idim <- model@d
  odim <- model@m
  cdim <- model@j

  if (is.null(modelcontrol))
    modelcontrol <- model@control

  if (is.null(control$popsize))
    control$popsize <- 200
  if (is.null(control$generations))
    control$generations <- 30
  if (is.null(control$cprob))
    control$cprob <- 1
  if (is.null(control$cdist))
    control$cdist <- 1/idim
  if (is.null(control$mprob))
    control$mprob <- 15
  if (is.null(control$mdist))
    control$mdist <- 20

  model_f <- list2mkm(model@km[ model@objective])
  fn <- function(x){
    x <- as.matrix(x)
    m <- predict(model_f, x, modelcontrol)$mean
    return(t(m))
  }
  if (cdim > 0){
    model_g <- list2mkm(model@km[-model@objective])
    cfn <- function(x){
      x <- as.matrix(x)
      pred_g <- predict(model_g, x, modelcontrol)
      s_g <- pred_g$sd
      m_g <- pred_g$mean
      return(-t(m_g))
    }
  }
  else
    cfn <- function(x) return(1)
  res <- mco::nsga2(fn, idim, odim,
              constraints = cfn,
              cdim = cdim,
              lower.bounds = lower,
              upper.bounds = upper,
              popsize = control$popsize,
              generations = control$generations,
              cprob = control$cprob,
              cdist = control$cdist,
              mprob = control$mprob,
              mdist = control$mdist,
              vectorized = TRUE
              )
  return(mco2ps(res))
}

#' MVPF: Minimization of the Variance of the Kriging-Predicted Front
#'
#' Executes \code{nsteps} iterations of the VMPF algorithm to an object of class
#' \code{\link{mkm}}. At each step, a multi-objective kriging model is re-estimated
#' (including covariance parameters re-estimation).
#'
#' The infill point is sampled from the most uncertain design of a predicted
#' Pareto set. This set is predicted using nsga-2 algorithm and the mean value
#' of the mkm predictor.
#'
#' @param model An object of class \code{\link{mkm}},
#' @param fun The multi-objective and constraint cost function to be optimized.
#'   This function must return a vector with the size of \code{model@m +
#'   model@j} where \code{model@m} are the number of objectives and
#'   \code{model@j} the number of the constraints,
#' @param nsteps An integer representing the desired number of iterations,
#' @param lower Vector of lower bounds for the variables to be optimized over
#'   (default: 0 with length \code{model@d}),
#' @param upper Vector of upper bounds for the variables to be optimized over
#'   (default: 1 with length \code{model@d}),
#' @param quiet Logical indicating the verbosity of the routine,
#' @inheritParams predict_front
#'
#' @return an updated object of class \code{mkm}.
#'
#' @export
#' @examples
#' # ----------------
#' # The Nowacki Beam
#' # ----------------
#' n <- 20
#' d <- 2
#' nsteps <- 2 # value has been set to 2 to save compliation time, change this value to 40.
#' fun <- nowacki_beam
#' doe <- replicate(d,sample(0:n,n))/n
#' res <- t(apply(doe, 1, fun))
#' model <- mkm(doe, res, modelcontrol = list(objective = 1:2, lower = rep(0.1,d)))
#' model <- MVPF(model, fun, nsteps, quiet = FALSE)
#' plot(nowacki_beam_tps$set)
#' points(ps(model@response[which(model@feasible),model@objective])$set, col = 'green', pch = 19)
MVPF <- function(model, fun, nsteps, lower = rep(0,model@d) , upper = rep(1,model@d),
                 quiet = TRUE, control = NULL, modelcontrol = NULL){
  time <- proc.time()
  if (class(model) != 'mkm')
    stop('The class of "model" must be "mkm"')
  if (is.null(modelcontrol))
    modelcontrol <- model@control
  for(n in 1:nsteps){
    pf_x <- predict_front(model, lower, upper, control, modelcontrol)$x
    pf_s <- predict(model, pf_x, modelcontrol)$norm_sd
    x_star <- pf_x[which.max(pf_s),]
    y_star <- fun(x_star)
    .model <- try(
      mkm(rbind(model@design,x_star), rbind(model@response,y_star), model@control),
      TRUE)
    if (class(.model) == 'mkm')
      model <- .model
    else{
      warning("Failed to update the kriging model at iteration number ",n,".")
      break
    }
    if (!quiet){
      cat('Current iteration:', n, '(elapsed', (proc.time()-time)[3], 'seconds)\n')
      cat('Current design:', round(x_star,3),'\n')
      cat('Current response:', round(y_star[model@objective],3),
          ifelse(utils::tail(model@feasible,1),'(feasible)','(unfeasible)'),'\n\n')
    }
  }
  return(model)
}

#' Deprecated function
#'
#' This function is deprecated and will be removed in a near future
#'
#' @inheritParams MVPF
#'
#' @rdname package-deprecated
#' @seealso \link{MVPF}
#' @export
VMPF <- function(model, fun, nsteps, lower = rep(0,model@d) , upper = rep(1,model@d),
                 quiet = TRUE, control = NULL, modelcontrol = NULL){
  .Deprecated("MVPF")
  MVPF(model, fun, nsteps, lower = rep(0,model@d), upper = rep(1,model@d),
           quiet = TRUE, control = NULL, modelcontrol = NULL)
}



