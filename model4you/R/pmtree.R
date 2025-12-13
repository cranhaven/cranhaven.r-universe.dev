#' Compute model-based tree from model.
#'
#' Input a parametric model and get a model-based tree.
#'
#' @param model a model object. The model can be a parametric model with a binary covariate.
#' @param data data. If NULL (default) the data from the model object are used.
#' @param zformula formula describing which variable should be used for partitioning.
#' Default is to use all variables in data that are not in the model (i.e. \code{~ .}).
#' @param control control parameters, see \code{\link[partykit]{ctree_control}}.
#' @param coeffun function that takes the model object and returns the coefficients.
#' Useful when \code{coef()} does not return all coefficients (e.g. \code{survreg}).
#' @param ... additional parameters passed on to model fit such as weights.
#'
#' @details Sometimes the number of participant in each treatment group needs to 
#' be of a certain size. This can be accomplished by setting \code{control$converged}.
#' See example below.
#'
#' @return ctree object
#'
#' @example inst/examples/ex-pmtree.R
#' @example inst/examples/ex-pmtree-methods.R
#'
#' @export
#' @import partykit
#' @importFrom partykit ctree_control nodeids nodeapply
#' @importFrom stats predict
pmtree <- function(model, data = NULL, zformula = ~.,
                   control = ctree_control(), coeffun = coef,
                   ...) {

  ### nmax not possible because data come from model
  stopifnot(all(!is.finite(control$nmax)))

  args <- .prepare_args(model = model, data = data, zformula = zformula,
                        control = control)

  ## call ctree
  args$ytrafo <- function(data, weights, control, ...)
    .modelfit(data = data, weights = weights, control = control,
              model = model, coeffun = coeffun, ...)
  ret <- do.call("ctree", args)

  ### add modelinfo to teminal nodes if not there yet, but wanted
  which_terminals <- nodeids(ret, terminal = TRUE)
  if(control$saveinfo) {
    tree_ret <- .add_modelinfo(ret, nodeids = which_terminals,
                               data = args$data, model = model,
                               coeffun = coeffun)
  } else {
    tree_ret <- ret
  }

  ## prepare return object
  class(tree_ret) <- c("pmtree", class(ret))
  tree_ret$info$model <- model
  tree_ret$info$zformula <- if(is.null(zformula)) as.formula("~ .") else
    as.formula(zformula)
  # tree_ret$data <- data
  tree_ret$nobs <- sum(unlist(
    nodeapply(tree_ret, ids = which_terminals, function(x) x$info$nobs)
  ))
  return(tree_ret)
}



#' Add model information to a personalised-model-ctree
#'
#' For internal use.
#'
#' @param x constparty object.
#' @param nodeids node ids, usually the terminal ids.
#' @param data data.
#' @param model model.
#' @param coeffun function that takes the model object and returns the coefficients.
#' Useful when coef() does not return all coefficients (e.g. survreg).
#'
#' @return tree with added info. Class still to be added.
.add_modelinfo <- function(x, nodeids, data, model, coeffun) {

  idx <- get_paths(nodeapply(x)[[1]], nodeids)
  names(idx) <- nodeids
  tree_ret <- unclass(x)
  subset_term <- predict(x, type = "node")

  # if(saveinfo) {
  for (i in nodeids) {
    ichar <- as.character(i)
    idn <- idx[[ichar]]

    if(length(idn) > 1) idn <- c(1, idn)
    iinfo <- tree_ret[[idn]]$info
    subsi <- subset_term == i

    if (is.null(iinfo)) {
      di <- data[subsi, ]
      umod <- update(model, data = di, model = TRUE)
      iinfo <- list(estfun = estfun(umod), coefficients = coeffun(umod),
                    objfun = ifelse(class(umod)[[1]] == "lm",
                                    sum(objfun(umod)),
                                    - logLik(umod)),
                    object = umod)
      tree_ret[[idn]]$info <- iinfo
    }
    tree_ret[[idn]]$info$nobs <- sum(subsi)

  }
  # }
  tree_ret
}
