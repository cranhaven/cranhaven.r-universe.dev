#' Compute model-based forest from model.
#'
#' Input a parametric model and get a forest.
#'
#' @param model a model object. The model can be a parametric model with a single binary covariate.
#' @param data data. If \code{NULL} the data from the model object are used.
#' @param zformula formula describing which variable should be used for partitioning.
#' Default is to use all variables in data that are not in the model (i.e. \code{~ .}).
#' @param ntree number of trees.
#' @param perturb a list with arguments replace and fraction determining which type of
#' resampling with \code{replace = TRUE} referring to the n-out-of-n bootstrap and
#' \code{replace = FALSE} to sample splitting. fraction is the number of observations
#' to draw without replacement.
#' @param mtry number of input variables randomly sampled as candidates at each
#' node (Default \code{NULL} corresponds to \code{ceiling(sqrt(nvar))}).
#' Bagging, as special case of a random forest without random input variable
#' sampling, can be performed by setting mtry either equal to Inf or
#' equal to the number of input variables.
#' @param applyfun see \code{\link[partykit]{cforest}}.
#' @param cores see \code{\link[partykit]{cforest}}.
#' @param control control parameters, see \code{\link[partykit]{ctree_control}}.
#' @param trace a logical indicating if a progress bar shall be printed while
#' the forest grows.
#' @param ... additional parameters passed on to model fit such as weights.
#'
#' @example inst/examples/ex-pmodel.R
#'
#' @return cforest object
#'
#' @export
#' @importFrom partykit ctree_control
pmforest <- function(model, data = NULL, zformula = ~., ntree = 500L,
                     perturb = list(replace = FALSE, fraction = 0.632),
                     mtry = NULL, applyfun = NULL, cores = NULL,
                     control = ctree_control(teststat = "quad", testtype = "Univ",
                                             mincriterion = 0, saveinfo = FALSE,
                                             lookahead = TRUE, ...),
                     trace = FALSE, ...) {

  ### nmax not possible because data come from model
  stopifnot(all(!is.finite(control$nmax)))

  cl <- match.call()
  args <- .prepare_args(model = model, data = data, zformula = zformula,
                        control = control, ntree = ntree, perturb = perturb,
                        applyfun = applyfun, cores = cores,
                        trace = trace)

  ## call cforest
  args$ytrafo <- function(data, weights, control, ...)
    .modelfit(data = data, weights = weights, control = control, model = model, ...)
  args$mtry <- mtry
  ret <- do.call("cforest", args)
  ret$info$model <- model
  ret$info$zformula <- zformula
  ret$info$call <- cl
  class(ret) <- c("pmforest", class(ret))

  return(ret)
}

#' @rdname pmforest
#'
#' @param object an object returned by pmforest.
#' @param tree an integer, the number of the tree to extract from the forest.
#' @param saveinfo logical. Should the model info be stored in terminal nodes?
#' @param coeffun function that takes the model object and returns the coefficients.
#' Useful when coef() does not return all coefficients (e.g. survreg).
#'
#' @seealso \code{\link[partykit]{gettree}}
#' @export
gettree.pmforest <- function(object, tree = 1L, saveinfo = TRUE, coeffun = coef, ...) {
  ret <- gettree.cforest(object = object, tree = tree, ...)
  cl <- class(ret)

  if(saveinfo) {
    which_terminals <- nodeids(ret, terminal = TRUE)
    ret <- .add_modelinfo(ret, nodeids = which_terminals, data = object$data,
                          model = object$info$model, coeffun = coeffun)
  }

  class(ret) <- c("pmtree", cl)
  return(ret)
}
