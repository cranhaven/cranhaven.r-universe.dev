#' Check if a given object is an \code{EEAaq_df} class object
#'
#' Given an object as input, \code{is_EEAaq_df} verify that the given object belongs
#' to the \code{EEAaq_df} class.
#' @param data the object for which verify the if it belongs to the \code{EEAaq_df} class.
#' @return logical value (T ot F). If \code{TRUE} the object given in input is an \code{EEAaq_df} object.
#' If \code{FALSE} the object doesn't belong to the \code{EEAaq_df} class.
#' @examples
#' \donttest{
#' #Download a dataset with the function EEAaq_get_data, which generate an EEAaq_df object.
#' data <- EEAaq_get_data(zone_name = "Milano", NUTS_level = "LAU", pollutant = "PM10",
#'   from = 2021, to = 2021, verbose = TRUE)
#' #Check if the imported object belongs to the EEAaq_df class
#' is_EEAaq_df(data = data)
#' }
#' @export



is_EEAaq_df <- function(data) {


  if("EEAaq_df" %in% attributes(data)$class | "EEAaq_df_sfc" %in% attributes(data)$class) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}

