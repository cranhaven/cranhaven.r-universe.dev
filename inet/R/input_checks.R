


input_checks <- function(data) {

  if(any(is.na(data))) stop("No missing values permitted.")
  if(any(is.infinite(data))) stop("No infinite values permitted.")
  if(!("matrix" %in% class(data))) stop("Please provide the data as a matrix object.")

}
