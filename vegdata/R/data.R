#' Taxonomic hierarchy levels
#'
#' hierarchy of taxon levels
#'
#' @docType data
#' @name taxlevels
#' @format A data frame with rows and 4 variables:
#' \describe{
#'   \item{rank}{Layer code, i.e. 0:9 for Turboveg}
#'   \item{level}{Combinations. Same integer means, they will be combined}
#'   \item{Euro.Med}{taxon level name in http://www.europlusmed.org/ database}
#'   \item{description}{Explanation of level codes}
#' }
#'
'taxlevels'


#' Layer combinations
#'
#' Example tables with layer codes and how they should be combined
#'  in vegetation analyses. lc.0: do not combine any layers
#'
#' @docType data
#' @name lc.0
#' @format A data frame with rows and 2 variables:
#' \describe{
#'   \item{LAYER}{Layer code, i.e. 0:9 for Turboveg}
#'   \item{COMB}{Combinations. Same integer means, they will be combined}
#' }
#'
'lc.0'

#' Layer combinations
#'
#' datasets with layer codes and how they should be combined
#'  in vegetation analyses.
#'
#' @docType data
#' @name lc.1
#' @format A data frame with rows and 3 variables:
#' \describe{
#'   \item{LAYER}{Layer code, i.e. 0:9 for Turboveg}
#'   \item{COMB}{Combinations. Same integer means, they will be combined}
#' }
#'
'lc.1'

#' Layer combinations
#'
#' A data.frame to combine all layers in a vegetation plot
#'
#' @docType data
#' @name lc.all
#' @format A data frame with rows and 2 variables:
#' \describe{
#'   \item{LAYER}{Layer code, i.e. 0:9 for Turboveg}
#'   \item{COMB}{Combinations. Same integer means, they will be combined}
#' }
#'
'lc.all'


#' This is an example vegetation dataset to be included in package vegdata. Use tv.veg('elbaue') and tv.site('elbaue') to load.
#'
#' @name elbaue
#' @docType data
#' @author Florian Jansen
#' @source \url{https://www.vegetweb.de}
#' @keywords data
NULL
