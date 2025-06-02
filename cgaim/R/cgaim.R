################################################################################
#
# The main function: wrapper for cgaim.fit
#
################################################################################

#' Constrained groupwise additive index models
#'
#' Fits constrained groupwise additive index models (CGAIM) to data. CGAIM fits indices subjected to constraints on their coefficients and shape of their association with the outcome. Such constraints can be specified in the formula through \code{\link{g}} for grouped terms and \code{\link{s}} for smooth covariates.
#'
#' @param formula A CGAIM formula with index terms \code{\link{g}}, smooth terms \code{\link{s}} and linear terms. See details.
#' @param data A data.frame containing the variables of the model.    
#' @param weights An optional vector of observation weights. 
#' @param subset An optional vector specifying a subset of observations to be used in the fitting process.
#' @param na.action A function indicating how to treat NAs. The default is set by the \code{na.action} setting of \code{options}. See \code{\link[stats]{na.fail}}.
#' @param Cmat A constraint matrix for index coefficients alpha. Columns must match all variables entering any index through \code{\link{g}}. See details.
#' @param bvec A vector of lower bounds for the constraints in \code{Cmat}. Potentially recycled to match the number of constraints.
#' @param control A list of parameters controlling the fitting process. See \code{\link{cgaim.control}}.
#'    
#' @details The CGAIM is expressed 
#'  \deqn{y_{i} = \beta_{0} + \sum_{j} \beta_{j} g_{j}(\alpha_{j}^{T} x_{ij})
#'    + \sum_{k} \gamma_{k} f_{k}(w_{ik}) + \sum_{l} \theta_{l} u_{il} + e_{i}}
#'  where the \eqn{x_{ij}} are variables entering grouped indices, the \eqn{w_{ik}} are smooth covariates and the \eqn{u_{il}} are linear covariates.
#'  
#'  The formula interface considers \code{\link{g}} to identify index terms, \code{\link{s}} for smooth functions and can also include linear terms as usual. All smooth terms can be shape constrained.
#'
#' The CGAIM allows for linear constraints on the alpha coefficients. Such constraints can be specified through the \code{\link{g}} interface in the formula, or through \code{alpha.control$Cmat}. The \code{\link{g}} interface is used for constraints meant for a specific index only. In this case, common constraints can easily be specified through the \code{acons} argument (see \code{\link{build_constraints}}). Alternatively, more general constraint can be specified by passing a matrix to the \code{Cmat} argument. Constraints encompassing several indices can be specified through an element \code{Cmat} in \code{alpha.control}. Its number of columns must match the total number of index coefficients alpha to estimate. In all cases, arguments \code{bvec} are used to specify the bounds of constraints.
#' 
#' Both indices (\code{\link{g}}) and smooth covariate terms (\code{\link{s}}) allow shape constraints. See dedicated help for the list of constraints allowed.
#' 
#' The CGAIM is fitted through an iterative algorithm that alternates between estimating the ridge functions \eqn{g_{j}} (and other non-index terms) and updating the coefficients \eqn{\alpha_{j}}. The smoothing of ridge functions currently supports three methods: \code{\link[scam]{scam}} (the default), \code{\link[cgam]{cgam}} and \code{\link[scar]{scar}}. The list \code{smooth.control} controls the smoothing with allowed parameters defined in \code{\link{cgaim.control}}.
#'
#' @note 
#' A model without intercept can only be fitted when the smoothing step is performed with \code{scam}.
#' 
#' @return A \code{cgaim} object, i.e. a list with components:
#'  \item{alpha}{A named list of index coefficients.}
#'  \item{gfit}{A matrix containing the ridge and smooth functions evaluated at the observations. Note that column ordering puts indices first and covariates after.}
#'  \item{indexfit}{A matrix containing the indices evaluated at the observations.}
#'  \item{beta}{A vector containing the intercept and the scale coefficient of each ridge and smooth function. Includes the \eqn{\gamma_{k}} of the CGAIM model above. Note that ordering puts indices first and covariates after.}
#'  \item{index}{A vector identifying to which index the columns of the element \code{x} belong.}
#'  \item{fitted}{A vector of fitted responses.}
#'  \item{residuals}{A vector of residuals.}
#'  \item{rss}{The residual sum of squares of the fit.}
#'  \item{flag}{A flag indicating how the algorithm stopped. 1 for proper convergence, 2 when the algorithm stopped for failing to decrease the RSS and 3 when the maximum number of iterations has been reached.}
#'  \item{niter}{Number of iterations performed.}
#'  \item{edf}{Effective degrees of freedom of the estimator.}
#'  \item{gcv}{Generalized cross validation score.}
#'  \item{dg}{A matrix containing derivatives of ridge and smooth functions.}
#'  \item{gse}{A matrix containing standard errors of ridge and smooth functions.}
#'  \item{active}{A logical vector indicating which constraints are active at convergence.}
#'  \item{Cmat}{The constraint matrix used to fit index coefficients alpha. Will include all constraints given through \code{\link{g}} and the \code{Cmat} parameter.}
#'  \item{bvec}{The lower bound vector associated with \code{Cmat}.}
#'  \item{x}{A matrix containing the variables entering the indices. The variables are mapped to each index through the element \code{index}.}
#'  \item{y}{The response vector.}
#'  \item{weights}{The weights used for estimation.}
#'  \item{sm_mod}{A list of model elements for the smoothing step of the estimation. Notably includes the matrix \code{Xcov} that includes the covariates not entering any index. Other elements depend on the method chosen for smoothing.}
#'  \item{control}{The control list used to fit the cgaim.}
#'  \item{terms}{The model terms.}
#'
#' @seealso \code{\link{confint.cgaim}} for confidence intervals,
#'    \code{\link{predict.cgaim}} to predict on new data,
#'    \code{\link{plot.cgaim}} to plot ridge functions.
#'
#' @examples 
#' ## Simulate some data
#' n <- 200
#' x1 <- rnorm(n)
#' x2 <- rnorm(n)
#' x3 <- rnorm(n)
#' x4 <- rnorm(n)
#' mu <- 4 * exp(8 * x1) / (1 + exp(8 * x1)) + exp(x3)
#' y <- mu + rnorm(n)
#' df1 <- data.frame(y, x1, x2, x3, x4)
#' 
#' ## Fit an unconstrained the model
#' ans <- cgaim(y ~ g(x1, x2) + g(x3, x4), data = df1)
#' 
#' # Compute confidence intervals
#' # In practice, higher B values are warranted
#' cia <- confint(ans, B = 100)
#' cia$alpha
#' cia$beta
#' 
#' # Display ridge functions
#' plot(ans, ci = cia)
#' 
#' # Predict
#' newdf <- as.data.frame(matrix(rnorm(100), 25, 4))
#' names(newdf) <- sprintf("x%i", 1:4)
#' yhat <- predict(ans, newdf)
#' 
#' ## Fit constrained model
#' ans2 <- cgaim(y ~ g(x1, x2, acons = list(monotone = -1)) + 
#'   g(x3, x4, fcons = "cvx"), data = df1)
#' 
#' # Check results
#' ans2
#' plot(ans2)
#' 
#' # Same result
#' Cmat <- as.matrix(Matrix::bdiag(list(build_constraints(2, monotone = -1), 
#'   build_constraints(2, first = 1))))
#' ans3 <- cgaim(y ~ g(x1, x2) + g(x3, x4, fcons = "cvx"), data = df1,
#'   Cmat = Cmat)
#' 
#' ## A mis-specified model
#' ans4 <- cgaim(y ~ g(x1, x2, acons = list(monotone = 1)) + 
#'   g(x3, x4, fcons = "dec"), data = df1)
#'
#' @export
cgaim <- function(formula, data, weights, subset, na.action, 
  Cmat = NULL, bvec = NULL, control = list()) 
{
  # Terms
  mt <- stats::terms(formula, specials = c("g", "s"), data = data)
  gind <- attr(mt, "specials")$g
  p <- length(gind)
  ptot <- length(attr(mt, "term.labels"))
  # Extract data
  cl <- match.call(expand.dots = FALSE)
  m <- match(c("data", "weights", "subset", "na.action"), 
    names(cl), 0L)
  cl <- cl[c(1L, m)]
  cl$drop.unused.levels <- TRUE
  cl$formula <- mt
  cl[[1L]] <- quote(stats::model.frame)
  mf <- eval(cl, parent.frame())
  cl$formula <- stats::reformulate(all.vars(mt))
  refordata <- eval(cl, parent.frame())
  # Control parameters
  control <- do.call(cgaim.control, control)
  # Initialize index estimation components
  mod_components <- index.setup(mf, Cmat, bvec, control)
  # Prepare parameters for smoothing
  smooth_components <- smooth.setup(mf, refordata, 
    control$sm_method, control$sm_control)
  # Fit the model
  fitpars <- c(mod_components, 
    list(intercept = as.logical(attr(mt, "intercept")), 
      sm_mod = smooth_components, control = control))
  result <- do.call(cgaim.fit, fitpars)
  # Organize output
  result$alpha <- split(result$alpha, mod_components$index)
  names(result$alpha) <- unique(names(mod_components$index))
  attributes(result$gfit) <- attributes(result$gfit)[c("dim", "dimnames")]
  result$x <- mod_components$x
  result$y <- mod_components$y
  result$weights <- mod_components$w
  result$sm_mod <- smooth_components
  result$control <- control
  result$terms <- mt
  class(result) <- "cgaim"
  return(result)
}

