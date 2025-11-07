new_GPPM <- function(mFormula, cFormula, myData, control) {
  stopifnot(is.character(mFormula))
  stopifnot(is.character(cFormula))
  stopifnot(is.data.frame(myData))
  stopifnot(class(control) == "GPPMControl")

  structure(list(
    mFormula = mFormula, # formula for the mean
    cFormula = cFormula, # formula for the covariance
    data = myData, # data must be a data frame
    control = control, # list of controls
    parsedModel = NA, # model in a parsed format
    dataForStan = NA, # data as used for stan
    stanModel = NA, # generated stan Model
    stanOut = NA, # stan output
    fitRes = NA # all the fitting results
  ), class = "GPPM")
}


#' Define a Gaussian process panel model
#'
#' This function is used to specify a Gaussian process panel model (GPPM),
#' which can then be fit using \code{\link{fit.GPPM}}.
#'
#' @param mFormula character string. Contains the specification of the mean function. See details for more information.
#'
#' @param cFormula character string. Contains the specification of the covariance function. See details for more information.
#'
#' @param myData data frame. Contains the data to which the model is fitted. Must be in the long-format.
#'
#' @param ID character string. Contains the column label in myData which describes the subject ID.
#'
#' @param DV character string. Contains the column label in myData which contains the to be modeled variable.
#'
#' @param control object of class GPPMControl. Used for storing technical settings. Default should only be changed by advanced users. Generated via \code{\link{gppmControl}}.
#'
#' @return A (unfitted) Gaussian process panel model, which is an object of class 'GPPM'
#' @details
#' mFormula and cFormula contain the specification of the mean and the covariance function respectively.
#' These formulas are defined using character strings. Within these strings there are four basic elements:
#'  \itemize{
#'   \item Parameters
#'   \item Functions and operators
#'   \item References to observed variables in the data frame myData
#'    \item Mathematical constants
#' }
#' The gppm function automatically recognizes which part of the string refers to which elements. To be able to do this certain relatively common rules need to be followed:
#'
#' Parameters: Parameters may not have the same name as any of the columns in myData to avoid confusing them with a reference to an observed variable.
#'  Furthermore, to avoid confusing them with functions, operators, or constants, parameter labels must always begin with a lower case letter and only contain letters and digits.
#'
#' Functions and operators: All functions and operators that are supported by stan can be used; see \url{https://mc-stan.org/docs/} for a full list. In general, all basic operators and functions are supported.
#'
#' References: A reference must be the same as one of the elements of the output of \code{names(myData)}. For references, the same rules apply as for parameters. That is, the column names of myData may only contain letters and digits and must start with a letter.
#'
#' Constants: Again, all constants that are supported by stan can be used and in general the constants are available by their usual name.
#' @seealso \code{\link{fit.GPPM}} for how to fit a GPPM
#' @examples
#' # Defintion of a latent growth curve model
#' \donttest{
#' data("demoLGCM")
#' lgcm <- gppm(
#'   "muI+muS*t", "varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma",
#'   demoLGCM, "ID", "y"
#' )
#' }
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_minimal
#' geom_ribbon scale_color_manual scale_alpha_manual scale_shape_manual xlab ylab
#' @importFrom mvtnorm dmvnorm
#' @importFrom MASS mvrnorm
#' @importFrom utils capture.output
#' @importFrom stats simulate vcov coef qnorm predict coef fitted AIC BIC runif logLik confint
#' @import Rcpp
#' @import methods
#' @importFrom rstan sampling
#' @importFrom rstantools rstan_config
#' @importFrom RcppParallel RcppParallelLibs
#' @export
gppm <- function(mFormula, cFormula, myData, ID, DV, control = gppmControl()) {
  myData <- as_LongData(myData, ID, DV)
  validate_gppm(mFormula, cFormula, myData, control)

  theModel <- new_GPPM(mFormula, cFormula, myData, control)
  theModel$dataForStan <- as_StanData(myData)
  theModel$parsedModel <- parseModel(theModel$mFormula, theModel$cFormula, theModel$dataForStan)
  theModel$stanModel <- toStan(theModel$parsedModel, control)
  return(theModel)
}

subsetData <- function(gpModel, rowIdxs) {
  newModel <- gpModel
  newModel$data <- gpModel$data[rowIdxs, ]
  newModel$dataForStan <- as_StanData(newModel$data)
  return(newModel)
}

updateData <- function(gpModel, newData) {
  newModel <- gpModel
  oldData <- getData(newModel)
  stopifnot(identical(names(oldData), names(newData)))
  newModel$data <- as_LongData(newData, getID(oldData), getDV(oldData))
  newModel$dataForStan <- as_StanData(newModel$data)
  # remove invalid fitting results
  newModel$stanOut <- NA
  return(newModel)
}

validate_gppm <- function(mFormula, cFormula, myData, control) {
  ID <- attr(myData, "ID")
  DV <- attr(myData, "DV")
  stopifnot(!is.null(ID), !is.null(DV))
  # type checks
  if (!is.character(mFormula)) {
    stop("mFormula must contain a string")
  }

  if (length(mFormula) != 1) {
    stop("mFormula must only have length 1")
  }

  if (!is.character(cFormula)) {
    stop("cFormula must contain a string")
  }

  if (!is.data.frame(myData)) {
    stop("myData must be a data frame")
  }

  if (!(is.character(ID))) {
    stop("ID must contain a string")
  }

  if (!is.character(DV)) {
    stop("DV must contain a string")
  }

  if (!inherits(control, "GPPMControl")) {
    stop("control must be of class GPPMControl")
  }

  varNames <- names(myData)
  allValid <- grepl("^[A-Za-z]+[0-9A-Za-z]*$", varNames)
  if (any(!allValid)) {
    stop(sprintf("Invalid variable name %s in your data frame. See ?gppm for naming conventions\n",
                 varNames[!allValid]
                 ))
  }

  if (!ID %in% varNames) {
    stop(sprintf("ID variable %s not in data frame", ID))
  }

  if (!DV %in% varNames) {
    stop(sprintf("DV variable %s not in data frame", DV))
  }
}
