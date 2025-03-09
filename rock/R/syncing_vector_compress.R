#' Compress a vector or data frame
#'
#' @param x The vector or data frame
#' @param newLength The new length (or number of rows for a data frame)
#' @param sep When not specifying `compressFun` and `compressFunPart`, the
#' `paste` function is used to combine elements, and in that case, `sep` is
#' passed to `paste` as separator.
#' @param compressFun If specified, when compressing streams, instead of pasting
#' elements together using separator `sep`, the vectors are passed to function
#' `compressFun`, which must accept a vector (to compress) and a single integer
#' (with the desired resulting length of the vector).
#' @param compressFunPart A function to apply to the segments that are
#' automatically created; this can be passed instead of `compressFun`.
#' @param silent Whether to be silent or chatty.
#'
#' @rdname compressing_vectors_or_dataframes
#'
#' @return The compressed vector or data frame
#' @export
#'
#' @examples rock::syncing_vector_compress(
#'   1:10,
#'   3
#' );
#'
#' rock::syncing_df_compress(
#'   mtcars[, 1:4],
#'   6
#' );
#'
#' rock::syncing_df_compress(
#'   mtcars[, 1:4],
#'   6,
#'   compressFunPart = mean
#' );
syncing_vector_compress <- function(x,
                                    newLength,
                                    sep = " ",
                                    compressFun = NULL,
                                    compressFunPart = NULL,
                                    silent = rock::opts$get('silent')) {

  oldLength <- length(x);
  oldIndices <- seq_along(x);
  newIndices <- 1 + floor((oldIndices - .01) / (oldLength / newLength));

  if (oldLength <= newLength) {
    stop("Currently, with length ", oldLength, ", `x` is shorter than ",
         "(or the same length as) `newLength` (", newLength, "). ",
         "Use `rock::sync_vector` to automatically detect whether the vector ",
         "should be shrunk or expanded.");
  }

  if (is.null(compressFun)) {

    newVector <-
      c(
        unlist(
          lapply(
            1:newLength,
            function(newIndex) {
              if (is.null(compressFunPart)) {
                return(
                  paste0(
                    x[newIndices == newIndex],
                    collapse = sep
                  )
                );
              } else {
                res <- compressFunPart(x[newIndices == newIndex]);
                msg(
                  "     - Compressed ",
                  vecTxtQ(x[newIndices == newIndex]),
                  " into '", res, "'.\n",
                  silent = silent
                );
                return(res);
              }
            }
          )
        )
      );

  } else {

    if (!is.function(compressFun)) {
      stop("As `compressFun`, you must pass a function. You currently passed ",
           "and object of class(es) ", vecTxtQ(class(compressFun)), ".");
    }

    newVector <-
      compressFun(
        x,
        newLength
      );

    if (length(newVector) != newLength) {
      stop("The `compressFun` you specified did not deliver a vector of ",
           "the correct length! `newLength` is ", newLength, ", but the ",
           "vector that was returned had length ", length(newVector), ".");
    }

  }

  return(newVector);

}
