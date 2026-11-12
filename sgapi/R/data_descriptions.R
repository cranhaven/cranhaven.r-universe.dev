#' List of available ONS boundaries
#'
#' A list of the available boundary layers that the Office for National Statistics (ONS) has for ArcGIS queries 
#'
#' @format A list of with 3348 rows and 1 variable:
#' \describe{
#'   \item{boundary}{name of boundary table within 'ONS Open geography'}
#' }
#' 
#' @docType data
#' @keywords data
#' @name availableBoundaries.rda
#' 
#' 
#' @source {availableBoundaries.rda} 
"availableBoundaries"

#' Lookup to match Office for National Statistics (ONS) and 'nomis' boundary names
#' 
#' 
#' Lookup table providing ONS and 'nomis' references for a given ONS boundary layer. The variables are as follows:
#'
#' @format A data frame with 30 rows and 4 variables:
#' \describe{
#'   \item{resolution}{boundary layer, written in interpretable manner}
#'   \item{ons}{corresponding boundary layer in the format that the 'ONS Open Geography' API will interpret}
#'   \item{nomis}{corresponding boundary layer in the format that the 'nomis' API will interpret}
#'   \item{Names_and_Codes}{lookup file containing the constituency names and codes of the boundary layer}
#' }
#' 
#' @docType data
#' @keywords data
#' @name lookup.rda
#' 
#' 
#' @source {lookup.rda} 
"lookup"

#' 
#' #' List of tables on 'nomis'
#'
#' A dataset containing the available tables, codes and sources of data available on 'nomis'. The variables are as follows:
#'
#' @format A data frame with 1605 rows and 3 variables:
#' \describe{
#'   \item{name}{name of dataset}
#'   \item{id}{code corresponing to dataset}
#'   \item{sourceName}{source of dataset}
#' }
#' 
#' @docType data
#' @keywords data
#' @name nomisTables.rda
#' 
#' 
#' @source {nomisTables.rda} 
"nomisTables"

#' 
#' #' List of tables available at each ONS resolution
#'
#' A dataset containing the available tables on 'nomis', and the boundary layers at which this data is held, e.g. demographic information may only be held in the
#' Census output areas and not NHS regions. The variables are as follows:
#'
#' @format A data frame with 22564 rows and 3 variables:
#' \describe{
#'   \item{name}{name of boundary layer}
#'   \item{value}{type of data}
#'   \item{table}{nomis code for the table}
#' }
#' 
#' @docType data
#' @keywords data
#' @name scalesForEachDataset.rda
#' 
#' 
#' @source {scalesForEachDataset.rda} 
"scalesForEachDataset"