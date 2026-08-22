#' Taking a vector, carry value over ('persistence')
#'
#' This function takes a value, and then replaces
#' empty elements (NA or zero-length character values)
#' with the last non-empty preceding element it
#' encountered.
#'
#' @param x The vector
#' @param noId The value to add for the first empty elements
#'
#' @return The vector with the carries over elements
#' @export
#'
#' @examples
#' rock::carry_over_values(
#'   c(
#'     NA, NA, 3, NA, NA, 7, NA, NA
#'   )
#' );
carry_over_values <- function(x,
                              noId = "no_id") {

  if (!is.vector(x)) {
    stop("As `x`, you have to provide a vector. However, the class of ",
         "the object you provided is ", vecTxtQ(class(x)), ".");
  }

  if (length(x) < 2) {
    return(x);
  }

  x <-
    ifelse(is.na(x) | (nchar(x) == 0),
           noId,
           x);

  if (all(x == "no_id")) {
    return(x);
  }

  for (i in 2:length(x)) {
    if ((x[i] == "no_id")) {
      x[i] <- x[i-1];
    }
  }

  return(x);
}
