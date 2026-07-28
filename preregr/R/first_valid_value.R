#' Select the first valid value
#'
#' From a vector or list of values, select the first valid value, valid being
#' defined as a value that is not NULL or NA and has, after being trimmed of
#' whitespace, a nonzero length. Optionally, only look at the element with a
#' given name.
#'
#' @param x The vector or list.
#' @param selectName Optionally, the name to look at.
#'
#' @return The first valid value (or NULL).
#' @export
first_valid_value <- function(x,
                              selectName = NULL) {

  res <- NULL;

  if (is.null(selectName)) {

    for (i in seq_along(x)) {
      if ((!is.null(x[[i]])) &&
          (!is.na(x[[i]])) &&
          (nchar(trimws(x[[i]])) > 0) &&
          (is.null(res))) {
        res <- x[[i]];
      }
    }

  } else {

    for (i in seq_along(x)) {
      if (selectName %in% names(x[[i]])) {
        if ((!is.null(x[[i]][[selectName]])) &&
            (!is.na(x[[i]][[selectName]])) &&
            (nchar(trimws(x[[i]][[selectName]])) > 0) &&
            (is.null(res))) {
          res <- trimws(x[[i]][[selectName]]);
        }
      }
    }

  }

  return(res);

}
