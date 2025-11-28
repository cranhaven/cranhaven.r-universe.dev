##############################################################
#' Fit Copula Models to Data
#'
#' Automatically fits various copula models specified in a list to the provided data.
#' This function is a wrapper around the underlying copula fitting function, facilitating
#' the exploration of multiple copula families to identify the best fitting model based
#' on criteria such as AIC.
#'
#' @name fit_copula_pbox
#' @docType methods
#' @export
#' @include pbox.R
#' @param data A data frame or data table; the data will be coerced to a `data.table` internally.
#' @param .copula_families A list specifying copula families to evaluate.
#'        The list should be structured with names corresponding to the type of copula
#'        (e.g., 'archmCopula', 'evCopula', 'ellipCopula') and elements being vectors
#'        of strings naming the copula families (e.g., "clayton", "frank").
#' @return A data table summarizing the AIC and parameter estimates for each copula family evaluated.
#' @examples
#'   data("SEAex")
#'   .copula_families <- list(
#'     archmCopula = c("clayton", "frank", "gumbel", "joe"),
#'     evCopula = c("galambos", "gumbel", "huslerReiss"),
#'     ellipCopula = c("normal")
#'   )
#'   distFits <- fit_copula_pbox(data = SEAex, .copula_families)
#'   print(distFits)
#' @importFrom stats setNames
#' @importFrom utils stack
setGeneric("fit_copula_pbox",
           def = function(data, .copula_families) {
             standardGeneric("fit_copula_pbox")
           })

#' @rdname fit_copula_pbox
#' @description
#' `fit_copula_pbox` method to fit a variety of copula models to data.
#' This method performs a grid search over specified copula families to find the best fit.
#' It employs the pseudoinverse of the empirical distribution functions to standardize the data.


setMethod("fit_copula_pbox",
          definition=function(data,.copula_families){

  u <- copula::pobs(data)
  dfCopula <- stats::setNames(utils::stack(.copula_families), c('family','copula'))

  # Perform grid search
  results <- apply(dfCopula, 1, function(row) {
    .fit_copula(copula=row["copula"], family=row["family"], dim = ncol(data), u)

  })
  # Convert results to data frame
  results_df <- do.call(rbind, results)
  return(results_df)
})


