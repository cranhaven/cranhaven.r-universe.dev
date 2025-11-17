#' Simulated data with multi-categorical outcome
#'
#' The simulated data contains 600 observations, 800 predictors, 10 covariates,
#' and an multiclass outcome with three categories. The external information Z contains five indicator prior covariates.
#'
#' @docType data
#'
#' @usage data(example.multiclass)
#'
#' @keywords datasets
#'
#' @format The \code{example.multiclass} object is a list containing three elements:
#' \itemize{
#' \item X: A simulated 600 by 800 matrix
#' \item Y: Categorical outcome with three levels
#' \item U: Covariates matrix with 600 by 10 dimension, will be forced in the model
#' \item Z: A 800 by 5 matrix with indicator entries.
#' }
#'
#' @examples
#' data(example.multiclass)
#' X <- example.multiclass$X
#' Y <- example.multiclass$Y
#' U <- example.multiclass$U
#' Z <- example.multiclass$Z
#' \donttest{fit <- xtune(X = X,Y = Y, U = U, Z = Z, family = "multiclass", c = 0.5)}
#' \donttest{fit$penalty.vector}
"example.multiclass"
