#' iGAP to MM
#'
#' @name iGAP_to_MM
#' @aliases iGAP_to_MM
#' @description To convert iGAP format to MM format.
#' @usage iGAP_to_MM(data, location)
#' @param data The dataframe with the iGAP format.
#' @param location The location of the symbolic variable in the data.
#' @returns Return a dataframe with the MM format.
#' @importFrom tidyr separate
#' @importFrom magrittr %>%
#' @examples
#' data(Abalone.iGAP)
#' Abalone <- iGAP_to_MM(Abalone.iGAP, 1:7)
#' @export

iGAP_to_MM <- function(data, location = NULL){
  location <- sort(location)
  x <- 0
  for (i in location){
    y <- i + x
    data <- data %>%
      tidyr::separate(names(data)[y], c(paste(names(data)[y], '_min', sep = ''),
                                 paste(names(data)[y], '_max', sep = '')), ",")
    x <- x + 1
  }
  return(data)
}

