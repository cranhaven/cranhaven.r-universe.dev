#' Clones the mpactr object
#' @description
#' This function allows us to clone the mpactr object to give
#' our users a more r-based flow.
#'
#' @param mpactr_object the object that we are cloning.
#' @noRd

clone <- function(mpactr_object) {
  new_object <- mpactr_object$clone(deep = TRUE)
  new_object$mpactr_data <- mpactr_object$mpactr_data$clone(deep = TRUE)
  new_object$mpactr_data$set_peak_table(data.table::copy(
    new_object$mpactr_data$get_peak_table()
  ))
  new_object$logger <- as.environment(as.list(mpactr_object$logger,
    all.names = TRUE
  ))
  return(new_object)
}
