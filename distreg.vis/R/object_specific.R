#' Model data getter
#'
#' Get the data with which the distributional regression model of interest was
#' estimated (see \link{distreg_checker} for a list of supported object
#' classes). By default, only explanatory variables are returned.
#' @importFrom methods is
#' @importFrom stats model.frame
#' @param model A gamlss or bamlss object.
#' @param dep If TRUE, then only the dependent variable is returned.
#' @param varname Variable name in character form that should be returned. If
#'   this is specified, only the desired variable is returned.
#' @param incl_dep Should the dependent variable be included?
#' @return A data.frame object if dep or varname is not specified, otherwise a
#'   vector.
#' @examples
#' library("betareg")
#'
#' # Get some data
#' beta_dat <- model_fam_data(fam_name = "betareg")
#'
#' # Estimate model
#' betamod <- betareg(betareg ~ ., data = beta_dat)
#'
#' # Get data
#' model_data(betamod)
#' @export
model_data <- function(model, dep = FALSE, varname = NULL, incl_dep = FALSE) {

  # Check first if supported distributional regression model
  if (!distreg_checker(model))
    stop("Specified model is not a supported distributional regression object.
See ?distreg_checker for details")

  if (dep & !is.null(varname))
    stop("Combination dep = TRUE and a specified varname is not possible.")

  # GAMLSS
  if (is(model, "gamlss")) {

    # Check all formulas for function evaluations inside model formula and stop if yes - this is due to weird behaviour of gamlss
    form_evals <- sapply(model[grepl("formula", names(model))], FUN = function(x) {
      return(any(grepl("factor\\(|log\\(", as.character(x))))
    })
    if (any(form_evals))
      stop("Please don't use any function evaluations like log() or
factor() inside the model formula. It messes with lots of
things in gamlss and distreg.vis.")

    # This gets unique variables from all parameters
    data_model <- lapply(model$parameters, FUN = model.frame, formula = model)
    data_model <- do.call("cbind", args = data_model)

    # Put dep variable in
    all_data <- cbind(model$y, data_model)
    dep_name <- as.character(model$mu.formula)[2] # this works, because in gamlss we do not have multivariate responses
    colnames(all_data)[1] <- dep_name

    # Here we check whether we have splines or identical columns
    all_data <- gamlss_data_cleaner(all_data)

    # Here we replace the variables we have with ones from the OG data.frame
    # this is really really annoying but necessary because gamlss doesn't store
    # data well when they are included as some kind of splines.
    og_df <- get(as.character(model$call$data), envir = environment(model$mu.terms))
    all_data <- og_df[, colnames(all_data)]

    # Make data.frame out of this
    return_object <- as.data.frame(all_data)

    # If dep then return only dependent variable
    if (dep & is.null(varname))
      return_object <- return_object[[dep_name]]

    # If varname then return varname
    if (!dep & !is.null(varname))
      return_object <- return_object[[varname]]

    # If no varname but no dep then don't return with dep
    if (!dep & is.null(varname)) {
      if (!incl_dep) {
        return_object <- return_object[, !colnames(return_object) %in% dep_name]
      }
      if (incl_dep) {
        return_object <- return_object[, ] # return everything
      }
    }
  }

  # BAMLSS
  if (is(model, "bamlss")) {
    return_object <- model$model.frame

    # Return dependent variable if wanted
    if (dep & is.null(varname))
      return_object <- c(model$y)[[1]]

    # Return a specific variable
    if (!dep & !is.null(varname))
      return_object <- return_object[[varname]]

    # If dep is true but varname not specified then return without dep
    if (!dep & is.null(varname)) {
      if (!incl_dep) {
        return_object <- return_object[, -1]
      }
      if (incl_dep) {
        return_object <- return_object[, ] # don't do anything if dep should be included
      }
    }

  }

  # Betareg / Betatree
  if (is(model, "betareg") | is(model, "betatree")) {
    return_object <- model.frame(model)

    # Return dependent variable if wanted
    if (dep & is.null(varname))
      return_object <- return_object[[1]]

    if (!dep) {
      if (!is.null(varname))
        return_object <- return_object[[varname]]
      else if (is.null(varname)) {
        if (!incl_dep) {
          return_object <- return_object[, c(-1)]
        }
        if (incl_dep) {
          return_object <- return_object[, ]
        }

      }
    }
  }
  return(return_object)
}

#' GAMLSS expl_data cleaner
#'
#' This checks whether we have spline column names and/or duplicate columns
#' @keywords internal
gamlss_data_cleaner <- function(temp_df) {
  cnames <- colnames(temp_df)

  # Clean of spline and other functions
  broken_up_list <- strsplit(cnames, "[(]|[)|,]")
  new_cnames <- sapply(broken_up_list, FUN = function(x) {
    if (length(x) != 1)
      return(x[2])
    else
      return(x[1])
  })

  # Assign new colnames
  new_df <- temp_df
  colnames(new_df) <- new_cnames

  # Only retain unique columns
  new_df <- new_df[, !duplicated(new_cnames)]

  # Return it
  return(new_df)
}
