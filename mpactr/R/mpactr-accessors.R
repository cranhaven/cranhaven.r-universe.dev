#' Return the peak table data.table from the mpactr object.
#'
#' @description
#' `get_peak_table()` a wrapper function to return the peak table
#' object of the given mpactr object.
#'
#' @param mpactr_object The mpactr object that is created by calling
#' the import_data() function.
#'
#' @return a `data.table`.
#' @export
#'
#' @examples
#' data <- import_data(
#'   example_path("coculture_peak_table.csv"),
#'   example_path("metadata.csv"),
#'   format = "Progenesis"
#' )
#'
#' peak_table <- get_peak_table(data)
#'
get_peak_table <- function(mpactr_object) {
  return(data.table::copy(mpactr_object$mpactr_data$get_peak_table()))
}

#' Return the meta_data data.table from the mpactr object.
#'
#' @description
#' `get_meta_data()` a wrapper function to return the meta data object
#' of the given mpactr object.
#'
#' @param mpactr_object The mpactr object that is created by calling
#' the import_data() function.
#'
#' @return a `data.table`.
#' @export
#'
#' @examples
#' data <- import_data(
#'   example_path("coculture_peak_table.csv"),
#'   example_path("metadata.csv"),
#'   format = "Progenesis"
#' )
#'
#' meta_data <- get_meta_data(data)
#'
get_meta_data <- function(mpactr_object) {
  return(data.table::copy(mpactr_object$mpactr_data$get_meta_data()))
}

#' Return the input peak table from mpactr object.
#'
#' @description
#' `get_raw_data` a wrapper function to return the meta data object of the
#' given mpactr object.
#'
#' @param mpactr_object The mpactr object that is created by calling the
#' import_data() function.
#'
#' @return a `data.table`.
#' @export
#'
#' @examples
#' data <- import_data(
#'   example_path("coculture_peak_table.csv"),
#'   example_path("metadata.csv"),
#'   format = "Progenesis"
#' )
#'
#' raw_data <- get_raw_data(data)
#'
get_raw_data <- function(mpactr_object) {
  return(data.table::copy(mpactr_object$mpactr_data$get_raw_data()))
}
