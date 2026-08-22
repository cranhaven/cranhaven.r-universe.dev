#' Expand a vector or data frame
#'
#' @param x The vector or data frame
#' @param newLength The new length (or number of rows for a data frame)
#' @param fill When expanding streams, whether to duplicate elements to fill
#' the resulting vector. Ignored if `fillFun` is specified.
#' @param expandFun If specified, when expanding streams, instead of potentially
#' filling the new larger vector with elements (if `fill` is `TRUE`), the
#' vectors are passed to function `expandFun`, which must accept a vector (to
#' compress) and a single integer (with the desired resulting length of
#' the vector).
#' @param paddingValue The value to insert for rows when not filling (by
#' default, filling carries over the value from the last preceding row that
#' had a value specified).
#' @param silent Whether to be silent or chatty.
#'
#' @return The expanded vector
#' @rdname expanding_vectors_or_dataframes
#' @export
#'
#' @examples rock::syncing_vector_expand(letters[1:10], 15);
#' rock::syncing_vector_expand(letters[1:10], 15, fill=FALSE);
syncing_vector_expand <- function(x,
                                  newLength,
                                  fill = TRUE,
                                  paddingValue = NA,
                                  expandFun = NULL,
                                  silent = rock::opts$get('silent')) {

  oldLength <- length(x);
  oldIndices <- seq_along(x);
  newIndices <- floor((oldIndices - .01) / (oldLength / newLength));

  ### Correct for cases where the lowest newIndex is 2 or higher by shifting
  ### the new indices 'down'
  newIndices <- (newIndices - (min(newIndices) - 1));

  if (oldLength >= newLength) {
    stop("Currently, with length ", oldLength,
         ", `x` is longer than (or the same length as) `newlength` (",
         newLength, "). ",
         "Use `rock::sync_vector` to automatically detect whether the vector ",
         "should be shrunk or expanded.");
  }

  if (is.null(expandFun)) {

    if (oldLength == 1) {
      if (fill) {
        return(rep(x, newLength));
      } else {
        return(c(x, rep(paddingValue, newLength-1)));
      }
    }

    newVector <- c();

    # cat0(
    #   "\noldLength: ", oldLength,
    #   "\nnewLength: ", newLength,
    #   "\noldIndices: ", vecTxt(oldIndices),
    #   "\nnewIndices: ", vecTxt(newIndices)
    # );

    for (newIndex in 1:newLength) {
      if (newIndex %in% newIndices) {
        newVector[newIndex] <- x[newIndices == newIndex];
        oldValue <- newVector[newIndex];
      } else {
        if (fill) {
          newVector[newIndex] <- oldValue;
        } else {
          newVector[newIndex] <- paddingValue;
        }
      }
    }

  } else {

    if (!is.function(expandFun)) {
      stop("As `expandFun`, you must pass a function. You currently passed ",
           "and object of class(es) ", vecTxtQ(class(expandFun)), ".");
    }

    newVector <-
      expandFun(
        x,
        newLength
      );

    if (length(newVector) != newLength) {
      stop("The `expandFun` you specified did not deliver a vector of ",
           "the correct length! `newLength` is ", newLength, ", but the ",
           "vector that was returned had length ", length(newVector), ".");
    }

  }

  return(newVector);

}
