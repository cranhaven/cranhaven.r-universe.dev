#' Check if a given object belongs to class 'ARPALdf_W'
#'
#' @description 'is_ARPALdf_W' checks if the input object belongs to the class 'ARPALdf_W'
#'
#' @param Data Object to check if the class of a dataframe is 'ARPALdf_W', i.e. ARPAL
#' dataframe for weather data.
#'
#' @return The function returns 'True' if the object is of class 'ARPALdf_W' and it returns 'False' if the
#'     object isn't of class 'ARPALdf_W'
#'
#' @examples
#' d <- get_ARPA_Lombardia_W_registry()
#' is_ARPALdf_W(d)
#'
#' @export
is_ARPALdf_W <- function(Data) {
  is.element("ARPALdf_W", attr(Data,"class"))
}
