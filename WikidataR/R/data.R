#' @name WD.globalvar
#' 
#' @title Global variables for Wikidata properties
#'
#' @description A dataset of Wikidata global variables.
#'
#' @format A list of tibbles documenting key property constraints from wikidata  
#' \describe{
#'   \item{SID.valid}{valid reference source properties}
#'   \item{PID.datatype}{required data type for each property}
#'   \item{PID.constraint}{expected regex match for each property}
#'   \item{lang.abbrev}{language abbreviations}
#'   \item{lang.abbrev.wiki}{language abbreviations for current wikis}
#'   \item{abbrev.wiki}{Wikimedia abbreviations for current wikis}
#'   ...
#' }

utils::globalVariables(c("WD.globalvar"))