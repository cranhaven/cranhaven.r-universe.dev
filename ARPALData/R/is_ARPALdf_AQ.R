#' Check if a given object belongs to class 'ARPALdf_AQ'
#'
#' @description 'is_ARPALdf_AQ' checks if the input object belongs to the class 'ARPALdf_AQ'
#'
#' @param Data Object to check if the class of a dataframe is 'ARPALdf_AQ', i.e. ARPAL
#' dataframe for air quality data.
#'
#' @return The function returns 'True' if the object is of class 'ARPALdf_AQ' and it returns 'False' if the
#'     object isn't of class 'ARPALdf_AQ'
#'
#' @examples
#' d <- get_ARPA_Lombardia_AQ_registry()
#' is_ARPALdf_AQ(d)
#'
#' @export

is_ARPALdf_AQ <- function(Data) {
  is.element("ARPALdf_AQ", attr(Data,"class"))
}

