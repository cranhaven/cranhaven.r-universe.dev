#' Convert a "bipiped" value (or vector of values) to a vector
#'
#' "Bipiped" means that different values are separated by a pair of
#' pipes (`||`), like the logical OR operator in R. Use
#' `bipiped_value_to_vector()` for single values, and
#' `bipiped_values_to_vector()` for a vector of values, in which case a list
#' is returned.
#'
#' @param x The value or vector of values.
#'
#' @return A vector or list of vectors.
#' @rdname bipiped_value_to_vector
#' @export
#'
#' @examples exampleValue <-
#'   paste0('"Purposefully select" || "Aselect" || ',
#'          '"Likely self-selected" || "Ameliorated self-selection"');
#' bipiped_value_to_vector(exampleValue);
bipiped_value_to_vector <- function(x) {
  if (is.null(x)) {
    return(x);
  } else if (is.na(x)) {
    return(x);
  } else if (length(x) == 0) {
    return(x);
  } else if (length(x) > 1) {
    warning("For vectors, use `bipiped_value_to_vector()` instead - ",
            "doing that for you now, and so returning a list of ",
            "vectors instead of a vectors.");
    return(bipiped_values_to_vector(x));
  }
  res <- strsplit(x, "\\|\\|")[[1]];
  res <- trimws(res);
  res <-
    ifelse(
      grepl('^"(.*)"$', res),
      gsub('^"(.*)"$', "\\1", res),
      res
    );
  res <-
    ifelse(
      grepl("^'(.*)'$", res),
      gsub("^'(.*)'$", "\\1", res),
      res
    );
  return(res);
}

#' @rdname bipiped_value_to_vector
#' @export
bipiped_values_to_vector <- function(x) {
  return(
    lapply(
      x,
      bipiped_value_to_vector
    )
  );
}
