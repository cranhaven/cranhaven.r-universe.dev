#' @title Create models with different combinations of variables
#' @description Generates all possible linear models for a given set of
#' predictor variables using the distance matrix as a response variable.
#' The function allows for the user to specify the maximum number of
#' variables in a model, which can be useful in cases where there are
#' many predictors. The output is a data frame containing all the
#' possible models, which can be passed to the fit_models function for
#' fitting using a PERMANOVA approach.
#' @param vars A character vector of variables to use for modeling
#' @param ncores An integer specifying the number of cores to use for parallel processing
#' @param k maximum number of variables in a model, default is NULL
#' @param verbose logical, defaults TRUE, sends messages about processing times
#' @return A data frame containing all the possible linear permanova
#' models
#'
#' @importFrom parallel makeCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom utils combn
#' @importFrom data.table data.table
#' @importFrom data.table rbindlist
#' @importFrom data.table :=
#' @importFrom future plan cluster
#' @importFrom furrr future_map_dfr
#' @export
#'
#' @examples
#' \donttest{
#' make_models(vars = c("A", "B", "C", "D"),
#'             ncores = 2, verbose = FALSE)
#'
#' # using k as a way to limit number of variables
#' make_models(vars = c("A", "B", "C", "D"),
#'             ncores = 2, k = 2, verbose = FALSE)
#'}
#' @references
#' Anderson, M. J. (2001). A new method for non-parametric multivariate analysis of variance. Austral Ecology, 26(1), 32-46.

make_models <- function(vars, ncores = 2, k = NULL, verbose = TRUE) {
  max_vif <- NULL

  # create data table of variables to use for modeling
  vars <- unlist(strsplit(vars, "\\s*,\\s*"))
  dt <- data.table::data.table(vars)

  # set response and dataset variables
  dataset <- "Distance"

  forms <- list()

  if(is.null(k)){
    MaxVars <- length(vars)
  }

  if(!is.null(k)){
    MaxVars <- k
  }

  # loop over different numbers of variables to include in models
  for(i in 1:MaxVars) {
    test <- combn(vars, i, simplify = FALSE)
    cl <- parallel::makeCluster(ncores)
    future::plan(future::cluster, workers = cl)

    # loop over all combinations of variables and create a list of formulas
    formulas <- furrr::future_map_dfr(test, function(x) {
      form <- paste(dataset, "~", paste(x, collapse = " + "))
      data.frame(form = form, stringsAsFactors = FALSE)
    })
    parallel::stopCluster(cl)
    if(verbose){
      message(paste(i, "of", MaxVars, "ready", Sys.time()))
    }
    forms[[i]] <- formulas
  }

  # combine all formulas into a single data table and add the null model
  all_forms <- data.table::rbindlist(forms, use.names = TRUE, fill = TRUE)
  all_forms <- unique(all_forms, by = "form", fromLast = TRUE)
  null_mod <- data.table::data.table(form = paste(dataset, "~ 1", collapse = ""))
  all_forms <- data.table::rbindlist(list(all_forms, null_mod), use.names = TRUE, fill = TRUE)

  return(as.data.frame(all_forms))
}

