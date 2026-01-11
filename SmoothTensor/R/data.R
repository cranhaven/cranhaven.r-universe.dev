# This goes in R/data.R

#' @title A list of mode information of the Chicago crime tensor dataset
#' @description A list of mode information of order-3 tensor dataset \code{ltns}.
#' @format A list consisting of crime areas, crime hours, and crime types:
#' \describe{
#'   \item{\code{hour_map}}{24 hours of crimes}
#'   \item{\code{area_map}}{77 areas of crimes}
#'   \item{\code{crimetype_map}}{32 types of crimes}
#'}
#' @source \url{http://frostt.io/tensors/chicago-crime/}
"mode_info"


#' @title Chicago crime tensor dataset
#' @description Chicago crime dataset consists of crime counts reported in the city of Chicago, ranging from January 1st, 2001 to December 11th, 2017.
#' @format An order-3 tensor with entries representing the log counts of crimes from 24 hours, 77 community areas, and 32 crime types.
#' @source \url{http://frostt.io/tensors/chicago-crime/}
"ltns"

