#' Find typical levels of explanatory variables in a model/dataset.
#' 
#' This function tries to choose sensible values of the explanatory variables 
#' from the data used to build a model or any other specified data. 
#' (or from data specified with the \code{data =} argument.)
#' 
#' @details For categorical variables, the most populated levels are used. For quantitative 
#' variables, a sequence of \code{pretty()} values is generated. 
#' 
#' @return A dataframe containing all combinations of the selected values for 
#' the explanatory variables. If there are p explanatory variables, 
#' there will be about \code{nlevels^p} cases.
#'
#' @param data optional data frame from which to extract levels for explanatory variables
#' @param nlevels how many levels to construct for input variables.
#' For quantitative variables, this is a suggestion. Set to `Inf` to get all levels 
#' for categorical variables and 100 levels for quantitative variables.
#' @param at named list giving specific values at which to hold the variables. Use this to 
#' override the automatic generation of levels for any or all explanatory variables.
#' @param model the model to display graphically
#' @param ... a more concise mechanism to passing desired values for variables
#'
#' @details For categorical variables, will return the nlevels most popular levels, unless 
#' the levels are specified explicitly in an argument.
#'
#' @examples
#' \dontrun{
#' df_typical(mosaicData::Galton, nlevels = 2, father = 70, mother = 68, nkids = 3)
#' df_typical(mosaicData::Galton, nlevels = 2)
#' mod1 <- lm(wage ~ age * sex + sector, data = mosaicData::CPS85)
#' df_typical(model = mod1, nlevels = 3)
#' }
#' @export
df_typical <- function(data = NULL,  
                   nlevels = 3, at = list(), model=NULL, ...) {
  extras <- list(...)
  at <- c(extras, at)
  
  # try to figure out what are the possible levels of variables
  if ( (! is.null(model)) && is.null(data)) data <- data_from_mod(model)
  missing_from_data <- ! names(at) %in% names(data)
  if (any(missing_from_data)) 
    stop("Explanatory variable",
         ifelse(length(at)>1, "s", ""), " ", 
         paste0("'", paste(names(at), collapse=", "), "'"), 
         " not in the data table")
  explan_vars <- if (is.null(model)) names(at) else base::union(explanatory_vars(model), names(at))

  # Set a large number of levels for the first explanatory variable,
  # then nlevels for the remaining ones.
  if (length(nlevels) == 1) {
    # replicate for all the explanatory variables
    how_many <- as.list(c(rep(nlevels, length(explan_vars))))
    names(how_many) <- explan_vars
  } else {
    how_many = nlevels
  }
  eval_levels <- reference_values(data[explan_vars], n = how_many, at = at )
  vnames <- names(eval_levels)
  for (name in vnames) {
    if (inherits(data[[name]], "factor") && !inherits(eval_levels[[name]], "factor"))
      eval_levels[[name]] <- factor(eval_levels[[name]], levels = levels(data[[name]]))
  }

  eval_levels
}

#' Compute sensible values from a data set for use as a baseline
#'
#'
#' @param data a data frame
#' @param n number of values for specified variables: could be a single number or
#' a list assigning a number of levels for individual variables
#' @param at optional values at which to set values: a list whose names are the
#' variables whose values are to be set.
#'
#' @details Variables not listed in \code{at} will be assigned levels using these principles:
#' Categorical variables: the most populated levels.
#' Quantitative variables: central quantiles, e.g. median for n=1, 

reference_values <- function(data, n = 1, at = list()) {
  var_names <- names(data)
  # n might be a list.  If so, the default should be 1
  n_default <- ifelse(inherits(n, "list"), 1, n)
  n_values <- as.list(rep(n_default, length(var_names)))
  names(n_values) <- var_names
  if (inherits(n, "list")) # override any appearing in the n-list
    n_values[names(n)] <- n
  
  ranges <- conversions <- as.list(rep(NA, length(var_names)))
  names(ranges) <- var_names
  for (k in 1:length(var_names)) {
    # get the appropriate number of levels for each variable
    if (var_names[k] %in% names(at)) {
      ranges[[k]] <- at[[var_names[k]]]
      #conversions[[k]] <- ifelse(is.numeric(ranges[[k]]), "as.discrete", NA)
    } else {
      ranges[[k]] <- n_levels(data[[var_names[k]]], n_values[[ var_names[k] ]])
      #conversions[[k]] <- attr(ranges[[k]], "convert")
    }
  }
  res <- do.call(expand.grid, c(ranges, stringsAsFactors = FALSE))
  #attr(res, "convert") <- conversions
  
  vnames <- names(res)
  for (name in vnames) {
    if (inherits(data[[name]], "factor") && !inherits(res[[name]], "factor"))
      res[[name]] <- factor(res[[name]], levels = levels(data[[name]]))
  }
  
  res
}
