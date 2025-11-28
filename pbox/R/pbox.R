##############################################################
#' Class "pbox": Main S4 class of the library \bold{pbox}.
#'
#' "pbox" is a class representing the probabilistic space which combines data, copula and margins.
#'
#' @export
#' @name pbox-class
#' @docType class
#' @slot data The original data coerced to a \code{data.table}.
#' @slot copula The copula object of class \code{mvdc}.
#' @slot fit The results of the automated selection for both the marginal distribution and the copula.
#' @import data.table
#' @import copula
#'
#' @importClassesFrom data.table data.table
#' @importClassesFrom copula mvdc
#'
setClass("pbox",
         slots = c(data = "data.table",
                   copula="mvdc",
                   fit="list"))


globalVariables(c(".", "..varnames", "Operator","Value", "Varnames" ,"Varnames2" ,"pbx",
                  '..co', '..cols', '..mj'))
