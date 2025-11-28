##############################################################
#' Create a Probability Box from Data
#'
#' Constructs a probability box (pbox) by automatically selecting the best marginal distribution
#' and copula for a given dataset. This function facilitates the creation of a pbox object,
#' which encapsulates the uncertainty and dependencies of the input data.
#'
#'
#' @docType methods
#' @name set_pbox
#' @docType methods
#' @export
#' @include pbox.R
#'
#' @export
#' @param data A data frame or data table. The data will be coerced to a `data.table` internally.
#' @param verbose control verbosity of the output. Default to TRUE.
#' @param ... Other arguments to be passed to the `fitDist` function.
#' @return An object of class `pbox` with the following slots:
#'         - `@data`: The original data coerced into a `data.table`.
#'         - `@copula`: The selected copula object, typically of class `mvdc`.
#'         - `@fit`: A list containing results from the automated selection processes for
#'           both the marginal distributions and the copula.
#' @examples
#'   data("SEAex")
#'   pbx <- set_pbox(data = SEAex)
#'   print(pbx)
#'   print(class(pbx))
#' @importFrom gamlss fitDist
#' @import gamlss.dist
setGeneric("set_pbox",
           def = function(data,verbose=TRUE, ...) {
             standardGeneric("set_pbox")
           })

#' @rdname set_pbox
#' @description
#' `set_pbox` method that utilizes data frames or data tables to configure a comprehensive
#' pbox structure. The method involves stages of distribution fitting and copula selection,
#' executed through external functions presumed to be available in the working environment
#' or described in the package.
#'

setMethod("set_pbox",
          definition = function(data,verbose=TRUE,...) {

  if (!inherits(data, c("data.frame","data.table"))) {
    stop("Input must be a data frame or a data.table")
  }

  if (sum(dim(data))==0) {
    stop("Empty data input!")
    }

  nn_cont <- names(data)[sapply(data, function(column) {
    is.factor(column) || is.character(column) || all(column %in% c(0, 1))
  })]

  if (length(nn_cont) > 0) {
    stop("It seems that in your data there are non-continuous features. pbox is not able to handle these features currently. Non-continuous variables: ", paste(nn_cont, collapse = ", "))
  } else {
    message("All features are continuous.")
  }

  data.table::setDT(data)
  distSearch<-fit_dist_pbox(data,...)
  CopulaSearch<-fit_copula_pbox(data,.copula_families)

  finalCopula<-final_pbox(CopulaSearch,distSearch$allDitrs,data, verbose)
  message("pbox object generated!\n")

  obj <- new("pbox", data =data, copula=finalCopula,fit=list(distSearch,CopulaSearch))

})

