#' Check if a given object belongs to class 'ARPALdf'
#'
#' @description 'is_ARPALdf' checks if the input object belongs to the class 'ARPALdf'
#'
#' @param Data Object to check if the class of a dataframe is 'ARPALdf', i.e. ARPAL dataframe.
#'
#' @return The function returns 'True' if the object is of class 'ARPALdf' and it returns 'False' if the
#'     object isn't of class 'ARPALdf'
#'
#' @examples
#' d <- get_ARPA_Lombardia_AQ_registry()
#' is_ARPALdf(d)
#'
#' @export

is_ARPALdf <- function(Data) {
  is.element("ARPALdf", attr(Data,"class"))
}
