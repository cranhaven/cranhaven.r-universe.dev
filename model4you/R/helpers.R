
#' Fit function when model object is given
#'
#' Use update function to refit model and extract info such as coef, logLik and
#' estfun.
#'
#' @param model model object.
#' @param data data.
#' @param coeffun function that takes the model object and returns the coefficients.
#' Useful when coef() does not return all coefficients (e.g. survreg).
#' @param weights weights.
#' @param control control options from \code{ctree_control}.
#' @param parm which parameters should be used for instability test?
#'
#' @return A function returning a list of
#' \item{coefficients}{ \code{coef}. }
#' \item{objfun}{ \code{logLik}. }
#' \item{object}{ the model object. }
#' \item{converged}{ Did the model converge? }
#' \item{estfun}{ \code{estfun}. }
#'
#' @importFrom sandwich estfun
#' @importFrom stats update coef logLik getCall as.formula residuals
#' @importFrom methods is
.modelfit <- function(model, data, coeffun = coef, weights, control, parm = NULL) {

  fitfun <- function(subset, weights, info = NULL, estfun = TRUE, object = FALSE) {

    data <- model.frame(data)
    ## compute model on data
    if(length(weights) == 0) weights <- rep(1, NROW(data))
    di <- data[subset, ]
    wi <- weights[subset]
    mod <- tryCatch(update(object = model, data = di, weights = wi),
                    warning = function(w) list(converged = FALSE),
                    error = function(e) {
                      list(converged = FALSE)
                    })

    ## stop if error
    if(!(is.null(mod$converged)) && !(mod$converged))
      return(list(converged = FALSE))

    ## <TH> supply converged function to ctree </TH>
    ## get convergence info
    if (is.null(control$converged)) {
      conv <- if (is.null(mod$converged)) TRUE else mod$converged
      if(is(mod, "survreg") && any(is.na(mod$coef))) conv <- FALSE
    } else {
      conv <- control$converged(mod, data, subset)
    }

    ## prepare return list
    ret <- list(coefficients = coeffun(mod), objfun = - logLik(mod),
                object = if(object) mod else NULL,
                converged = conv)

    ## add estfun if wanted
    if(estfun) {
      if(is(mod, "coxph")) {
          ef <- cbind(mod$residuals, mod$residuals * stats::model.matrix(mod))
        # old: ef <- as.matrix(cbind(residuals(mod, "martingale"), ef))
      } else {
          ef <- estfun(mod)
      }
      ret$estfun <- matrix(0, nrow = NROW(data), ncol = NCOL(ef))
      ret$estfun[subset,] <- ef
      if(!is.null(parm)) ret$estfun <- ret$estfun[, parm]
    }

    return(ret)
  }

  return(fitfun)
}


.get_model_data <- function(model) {
  modcall <- getCall(model)

  if(is.null(data <- try(eval(modcall$data), silent = TRUE))) {
    stop("Need a model with data component, if data is NULL. Solutions: specify data in function call of this function or of the model.")
  } else {
    msg <- paste0("No data given. I'm using data set ", modcall$data,
                  " from the current environment parent.frame(). Please check if that is what you want.")
  }
  if(is(data, "try-error")) {
    if(is.null(data <- model$data)){
      stop("Need a model with data component, if data is NULL. Solutions: specify data in function call of this function or of the model.")
    } else {
      msg <- paste("No data given. I'm using data set given in model$data. Please check if that is what you want.")
    }
  }
  message(msg)
  return(data)
}


#' Check if model has only one factor covariate.
#'
#' See https://stackoverflow.com/questions/50504386/check-that-model-has-only-one-factor-covariate/50514499#50514499
#'
#' @param object model.
#'
#' @return Returns TRUE if model has a single factor covariate, FALSE otherwise.
one_factor <- function(object) {
  f <- attr(terms(object), "factors")
  if(length(f) == 0L || NCOL(f) != 1L) return(FALSE)
  d <- attr(terms(object), "dataClasses")
  if(!is.null(d)) {
    if(d[colnames(f)] %in% c("ordered", "factor")) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(TRUE)  ## TODO: make a better check here!
  }
}


#' Prepare input for ctree/cforest from input of pmtree/pmforest
#'
#' @param model model.
#' @param data an optional data frame.
#' @param zformula ormula describing which variable should be used for partitioning.
#' @param control ontrol parameters, see \code{\link[partykit]{ctree_control}}.
#' @param ... other arguments.
#'
#' @return args to be passed to ctree/cforest.
.prepare_args <- function(model, data, zformula, control, ...) {

  if(!one_factor(model))
    warning("Models with anything but single factor covariates are in beta. Please check if this is what you want!")

  if (is.null(modcall <- getCall(model)))
    stop("Need a model with call component, see getCall")

  ## get arguments for cforest call
  args <- list(...)
  # args$ntree <- ntree
  args$control <- control

  # ## arguments used in model
  # modargs <- as.list(modcall)[-1]

  ## formula and data
  if(is.null(data)) {
    data <- .get_model_data(model)
  }
  args$data <- data
  modformula <- eval(formula(model))

  ## in case I switch to mob
  # if(is.null(zformula)) zformula <- formula(~ .)
  # mobformula <- as.Formula(modformula, zformula)

  ## cforest formula
  if(is.null(zformula)) zformula <- "~ ."
  if(!is.character(zformula)) zformula <- paste(as.character(zformula), collapse = " ")
  modvars <- all.vars(modformula)
  args$formula <- as.formula(
    paste(paste(modvars, collapse = " + "), zformula)
  )

  nas <- is.na(args$data[, modvars])
  if(any(nas)) {
    warning("NAs in model variables (", paste(modvars, collapse = ", "), "). Omitting rows with NAs.")
    args$data <- stats::na.omit(args$data)
  }

  return(args)
}
