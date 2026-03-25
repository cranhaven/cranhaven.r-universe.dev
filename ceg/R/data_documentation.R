#' test dataset - artificial.chds.
#'
#' A dataset with dummy data, based on Child Health and Development Studies (CHDS).
#'
#' @docType data
#'
#' @usage data(artificial.chds)
#'
#' @format a data.frame with 1500 rows and 4 categorical variables. The variables
#'   names and values are compliant with CHDS, but the values are randomly filled.
#'    \describe{
#'    \item{Social}{High, Low}
#'    \item{Economic}{High, Low}
#'    \item{Event}{High, Average, Low}
#'    \item{Admission}{No, Yes}
#'    }
#' @keywords datasets
#'
#' @examples
#' data(artificial.chds)
"artificial.chds"


#' test stratified event tree
#'
#' A Stratified.event.tree S4 object, generated using the command
#' \code{set <- Stratified.event.tree(artificial.chds)}
#'
#' @docType data
#'
#' @usage data(set)
#'
#' @format a Stratified.event.tree S4 object
#'
#' @examples
#' data(set)
"set"


#' test stratified event tree (manualy constructed)
#'
#' A Stratified.event.tree S4 object, generated using manual input. \cr
#' See Stratified.event.tree documentation examples.
#'
#' @docType data
#'
#' @usage data(set)
#'
#' @format a Stratified.event.tree S4 object
#'
#' @examples
#' data(set.manual)
"set.manual"


#' test stratified staged tree
#'
#' A Stratified.staged.tree S4 object, generated using the command
#' \code{sst <- Stratified.staged.tree(artificial.chds)}
#'
#' @docType data
#'
#' @usage data(sst)
#'
#' @format a Stratified.staged.tree S4 object
#'
#' @examples
#' data(sst)
"sst"


#' test stratified ceg model
#'
#' A Stratified.ceg.model S4 object, generated using the command
#' \code{scm <- Stratified.ceg.model(sst)}
#'
#' @docType data
#'
#' @usage data(scm)
#'
#' @format a Stratified.ceg.model S4 object
#'
#' @examples
#' data(scm)
"scm"
