#' Create a filter to select utterances in a source
#'
#' This function takes a character vector with regular expressions,
#' a numeric vector with numeric indices, or a logical vector that is either
#' as long as the source or has length 1; and then always returns a logical
#' vector of the same length as the source.
#'
#' @param source The source to produce the filter for.
#' @param filter THe filtering criterion: a character vector with regular expressions,
#' a numeric vector with numeric indices, or a logical vector that is either
#' as long as the source or has length 1.
#' @param ignore.case Whether to apply the regular expression case sensitively
#' or not (see [base::grepl()]).
#' @param invert Whether to invert the result or not (i.e. whether the filter
#' specifies what you want to select (`invert=FALSE`) or what you don't want
#' to select (`invert=TRUE`)).
#' @param perl Whether the regular expression (if `filter` is a character
#' vector) is a perl regular expression or not (see [base::grepl()]).
#' @param ... Any additional arguments are passed on to [base::grepl()].
#'
#' @return A logical vector of the same length as the source.
#' @export
#'
get_source_filter <- function(source,
                              filter,
                              ignore.case = TRUE,
                              invert = FALSE,
                              perl = TRUE,
                              ...) {

  ### Probably not needed; we search by regex anyway, but let's keep it
  ### as a comment for convenience
  # codeDelimiters <- rock::opts$get(codeDelimiters);

  if (is.logical(filter)) {
    if (length(filter) == 1) {
      res <- rep(filter, length(source));
    } else if (length(filter) == length(source)) {
      res <- filter;
    } else {
      stop("The vector you passed (", deparse(substitute(filter)),
           ") is a logical vector, but its length (", length(filter),
           ") is not the same as the length of the source you passed (",
           length(source), ").");
    }
  } else if (is.numeric(filter)) {
    if ((min(filter) >= 1) && (max(filter) <= length(source))) {
      res <- rep(FALSE, length(source));
      res[filter] <- TRUE;
    } else {
      stop("The vector you passed (", deparse(substitute(filter)),
           ") is a numeric vector, but it contains indices that are under 1 ",
           "or higher than the length of the source you passed (",
           length(source),
           ").");
    }
  } else if (is.character(filter)) {
    res <-
      multigrepl(
        patterns = filter,
        x = source,
        returnMatchesForPatterns = FALSE,
        ignore.case = ignore.case,
        perl = perl,
        ...
      );
  } else {
    stop("As `filter`, pass a logical, numeric, or character vector. ",
         "You passed a vector of class(es) ", vecTxtQ(class(filter)), ".");
  }

  if (invert) {
    res <- !res;
  }

  class(res) <- c("rock_source_filter", "logical");
  return(res);

}
