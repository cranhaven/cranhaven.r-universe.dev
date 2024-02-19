#devtools::use_package("GPareto")
#' EHVI: Constrained Expected Hypervolume Improvement
#'
#' Multi-objective Expected Hypervolume Improvement with respect to the current
#' Pareto front. It's based on the \code{\link{crit_EHI}} function of the
#' \code{\link{GPareto-package}} package. However, the present implementation accounts
#' for inequality constrains embedded into the \code{mkm} model.
#'
#' The way that the constraints are handled are based on the probability of
#' feasibility. The strong assumption here is that the cost functions and the
#' constraints are uncorrelated. With that assumption in mind, a simple
#' closed-form solution can be derived that consists in the product of the
#' probability that each constraint will be met and the expected improvement of
#' the objective.
#'
#' @inheritParams GPareto::crit_EHI
#' @param model An object of class \code{\link{mkm}}.
#' @param control An optional list of control parameters, some of them passed to
#'   the \code{\link[GPareto]{crit_EHI}} function. One can control:
#'   \describe{
#'   \item{\code{minimization}}{logical indicating if the EHVI is minimizing all
#'   objectives (\code{TRUE}, by default) or maximizing all objectives
#'   (\code{FALSE}). Mixed optimization is not currently accepted, if the user
#'   needs it, the cost functions should be modified prior Kriging modeling
#'   (i.e. inverting or multiplying the output by \code{-1}).}
#'   \item{\code{paretoFront}}{object of class \code{\link{ps}} containing the
#'   actual Pareto set. If not provided a Pareto set is built based on the
#'   current feasible observations (\code{model@response[model@feasible,]}).}
#'   \item{\code{nb.samp}}{number of random samples from the posterior distribution
#'    (with more than two objectives), default to 50, increasing gives more reliable
#'     results at the cost of longer computation time}
#'   \item{\code{seed}}{seed used for the random samples (with more than two objectives);}
#'   \item{\code{refPoint}}{reference point for Hypervolume Expected Improvement.
#'    If not provided, it is set to the maximum or minimum of each objective.}
#'    }
#'
#' @return The constrained expected hypervolume improvement at \code{x}.
#'
#' @references Forrester, A., Sobester, A., & Keane, A. (2008).
#'   \emph{Engineering design via surrogate modeling: a practical guide.} John
#'   Wiley & Sons.
#'
#' @export
#' @examples
#' # ------------------------
#' # The Nowacki Beam
#' # ------------------------
#' n <- 10
#' d <- 2
#' doe <- replicate(d,sample(0:n,n))/n
#' res <- t(apply(doe, 1, nowacki_beam, box = data.frame(b = c(10, 50), h = c(50, 250))))
#' model <- mkm(doe, res, modelcontrol = list(objective = 1:2, lower=rep(0.1,d)))
#' grid <- expand.grid(seq(0, 1, , 10),seq(0, 1, , 10))
#' ehvi <- apply(grid, 1, EHVI, model)
#' contour(matrix(ehvi, 20))
#' points(model@design, col=ifelse(model@feasible,'blue','red'))
#' points(grid[which.max(ehvi),], col='green', pch=19)
EHVI <- function(x, model, control = NULL){
  if (class(model) != 'mkm')
    stop('The class of "model" must be "mkm"')
  if (length(model@objective) == 1)
    stop('Incorrect Number of objectives. Must be more than 1')
  modelcontrol <- model@control
  if (is.null(control$minimization))
    control$minimization <- TRUE
  if (is.null(control$paretoFront))
    control$paretoFront <- ps(model@response[,model@objective], control$minimization)$set
  if (is.null(control$nb.samp))
    control$nb.samp = 50
  if (is.null(control$seed))
    control$seed = 42

  if (model@j == 0)
    probg <- 1
  else {
    model_g <- model@km[-model@objective]
    pred_g <- predict(list2mkm(model_g), data.frame(t(x)), modelcontrol)
    s_g <- pred_g$sd
    m_g <- pred_g$mean
    probg <- prod(stats::pnorm(-m_g/s_g))
  }
  if (is.null(control$refPoint)){
    if (control$minimization)
      control$refPoint <- as.matrix(apply(control$paretoFront, 2, min))
    else
      control$refPoint <- as.matrix(apply(control$paretoFront, 2, max))
  }
  ehvi <- GPareto::crit_EHI(x, model@km[model@objective], control$paretoFront, control, modelcontrol$type)
  return(ehvi*probg)
}

#devtools::use_package("GenSA")
#' max_EHVI: Maximization of the Expected Hypervolume Improvement criterion
#'
#' Given an object of class \code{\link{mkm}} and a set of tuning parameters,
#' max_EHVI performs the maximization of the Expected Hypervolume Improvement
#' criterion and delivers the next point to be visited in an HEGO-like
#' procedure.
#'
#' @inheritParams EHVI
#' @param optimcontrol Optional list of control parameters passed to the
#'   \code{\link[GenSA]{GenSA}} function. Please, note that the values are passed as
#'   the \code{control} parameter inside the \code{GenSA} function (\code{genSA(control = optimcontrol)}).
#' @param lower Vector of lower bounds for the variables to be optimized over
#'   (default: 0 with length \code{model@d}),
#' @param upper Vector of upper bounds for the variables to be optimized over
#'   (default: 1 with length \code{model@d}),
#' @return A list with components:
#' \describe{
#'  \item{\code{par}}{The best set of parameters found.}
#'  \item{\code{value}}{The value of expected hypervolume improvement at par.}
#'  }
#'
#' @export
#' @examples
#' # ------------------------
#' # The Nowacki Beam
#' # ------------------------
#' n <- 10
#' d <- 2
#' doe <- replicate(d,sample(0:n,n))/n
#' res <- t(apply(doe, 1, nowacki_beam, box = data.frame(b = c(10, 50), h = c(50, 250))))
#' model <- mkm(doe, res, modelcontrol = list(objective = 1:2, lower=c(0.1,0.1)))
#' max_EHVI(model)
max_EHVI <- function(model, lower = rep(0, model@d), upper = rep(1, model@d),
                     control = NULL, optimcontrol = NULL){
  if (class(model) != 'mkm')
    stop('The class of "model" must be "mkm"')
  if(is.null(optimcontrol$max.time))
    optimcontrol$max.time <- 2
  fn <- function(x)
    return(-EHVI(x, model, control))
  res <- GenSA::GenSA(NULL, fn, lower, upper, control = optimcontrol)
  res$value <- -res$value
  return(res[c('value','par')])
}

#' HEGO: Efficient Global Optimization Algorithm based on the Hypervolume criteria
#'
#' Executes \code{nsteps} iterations of the HEGO method to an object of class
#' \code{\link{mkm}}. At each step, a kriging model is re-estimated (including
#' covariance parameters re-estimation) based on the initial design points plus
#' the points visited during all previous iterations; then a new point is
#' obtained by maximizing the Expected Hypervolume Improvement criterion (EHVI).
#'
#' @param fun The multi-objective and constraint cost function to be optimized.
#'   This function must return a vector with the size of \code{model@m + model@j}
#'   where \code{model@m} are the number of objectives and
#'   \code{model@j} the number of the constraints,
#' @param nsteps An integer representing the desired number of iterations,
#' @param quiet Logical indicating the verbosity of the routine,
#' @inheritParams EHVI
#' @inheritParams max_EHVI
#' @return updated \code{\link{mkm}} model
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
#' model <- HEGO(model, fun, nsteps, quiet = FALSE)
#' plot(nowacki_beam_tps$set)
#' points(ps(model@response[which(model@feasible),model@objective])$set, col = 'green', pch = 19)
HEGO <- function(model, fun, nsteps, lower = rep(0, model@d), upper = rep(1, model@d), quiet = TRUE,
                 control = NULL, optimcontrol = NULL){
  time <- proc.time()
  if (class(model) != 'mkm')
    stop('The class of "model" must be "mkm"')
  for(n in 1:nsteps){
    x_star <- max_EHVI(model, lower, upper, control)$par
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
