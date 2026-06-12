# =============================================================================
# marginaleffects compatibility layer
# This file must be processed early so generics are defined before methods
# =============================================================================

# --- S3 methods for mlmodel class ----------------------------------------

#' @importFrom marginaleffects get_predict
#' @export
get_predict.mlmodel <- function(model,
                                newdata = NULL,
                                type = "response",
                                mfx = NULL,
                                newparams = NULL,
                                ndraws = NULL,
                                se.fit = NULL,
                                ...)
{
  pred <- predict(model, newdata = newdata, type = type, ...)
  data.frame(
    rowid = seq_len(length(pred$fit)),
    estimate = pred$fit
  )
}

#' @importFrom marginaleffects get_vcov
#' @export
get_vcov.mlmodel <- function(model,
                             vcov = NULL,
                             cl_var = NULL,
                             repetitions = 999,
                             seed = NULL,
                             progress = FALSE, ...)
{
  if(is.matrix(vcov))
    return(vcov)
  
  valid_types <- c("oim", "robust", "opg", "cluster", "boot", "jack", "jackknife")
  
  if(is.character(vcov) && vcov %in% valid_types)
    return(vcov(model,
                type = vcov,
                cl_var = cl_var,
                repetitions = repetitions,
                seed = seed,
                progress = progress))
  
  return(vcov(model))
}

#' @importFrom marginaleffects get_coef
#' @export
get_coef.mlmodel <- function(model, ...)
{
  coefs <- coef(model)
  # names(coefs) <- gsub("^value::", "", names(coefs))
  # if(!is.null(model$model$scale))
  #   names(coefs) <- gsub("^scale::", "scale_", names(coefs))
  
  coefs
}

#' @importFrom marginaleffects set_coef
#' @export
set_coef.mlmodel <- function(model, coefs, ...)
{
  out <- model
  out$estimate <- coefs
  return(out)
}

#' Extract data used to fit the model (for insight/marginaleffects compatibility)
#'
#' @param x An object of class `"mlmodel"`
#' @param ... Further arguments (currently ignored)
#' @returns The original data frame used when fitting the model
#' @importFrom insight get_data
#' @export
get_data.mlmodel <- function(x, ...) {
  # Data is stored inside the $model element. We return only the observations used
  # in estimation to ensure that bootstrapping is done right.
  if (!is.null(x$model$data) && is.data.frame(x$model$data)) {
    return(x$model$data[x$model$sample, , drop = FALSE])
  }

  # Safety fallback (in case we ever store it at root level in future models)
  if (!is.null(x$data) && is.data.frame(x$data)) {
    return(x$data[x$model$sample, , drop = FALSE])
  }

  cli::cli_abort("Could not recover the original data from the mlmodel object.",
                 call = NULL)
}

#' Extract the predictors used in the model (for insight/marginaleffects compatibility)
#'
#' @param x An object of class `"mlmodel"`
#' @param ... Further arguments passed to methods
#' 
#' @returns A character vector with the names of the predictor variables.
#' 
#' @importFrom insight find_predictors
#' @export
find_predictors.mlmodel <- function(x, ...)
{
  vars <- character(0)
  
  # Value
  if(!is.null(x$model$formula))
  {
    rhs <- rlang::f_rhs(x$model$formula)
    vars <- all.vars(rhs)
  }
  else if(!is.null(x$model$value$blueprint$formula))
  {
    rhs <- rlang::f_rhs(x$model$value$blueprint$formula)
    vars <- all.vars(rhs)
  }
  
  # Scale
  if(!is.null(x$model$scale_formula))
  {
    rhs <- rlang::f_rhs(x$model$scale_formula)
    vars <- unique(c(vars, all.vars(rhs)))
  }
  else if(!is.null(x$model$scale$blueprint$formula))
  {
    rhs <- rlang::f_rhs(x$model$scale$blueprint$formula)
    vars <- unique(c(vars, all.vars(rhs)))
  }
  
  # return(list(conditional = vars))
  vars
}

#' Extract the variables used in the model (for insight/marginaleffects compatibility)
#'
#' @param x An object of class `"mlmodel"`
#' @param ... Further arguments passed to methods
#' 
#' @returns A list with components `response` (character) and `conditional` (character vector).
#' 
#' @importFrom insight find_variables
#' @export
find_variables.mlmodel <- function(x, ...) {
  
  # Start with predictors
  pred_vars <- find_predictors(x, ...)
  
  # Add response variable
  response <- character(0)
  
  if (!is.null(x$model$formula)) {
    lhs <- rlang::f_lhs(x$model$formula)
    response <- all.vars(lhs)
  } else if (!is.null(x$model$value$blueprint$formula)) {
    lhs <- rlang::f_lhs(x$model$value$blueprint$formula)
    response <- all.vars(lhs)
  }
  
  list(
    conditional = pred_vars,
    response    = response
  )
}

#' @rdname get_data.mlmodel
#' @export
get_modeldata.mlmodel <- get_data.mlmodel


# Register mlmodels with marginaleffects onload.
.onLoad <- function(libname, pkgname) {
  
  # Only register with marginaleffects if the user has it installed
  if (requireNamespace("marginaleffects", quietly = TRUE)) {
    
    current <- getOption("marginaleffects_model_classes", default = character(0))
    
    if (!"mlmodel" %in% current) {
      new_classes <- c(current, "mlmodel")
      options("marginaleffects_model_classes" = unique(new_classes))
    }
  }
  
  invisible(NULL)
}