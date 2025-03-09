#' @rdname compressing_vectors_or_dataframes
#' @export
syncing_df_compress <- function(x,
                                newLength,
                                sep = " ",
                                compressFun = NULL,
                                compressFunPart = NULL,
                                silent = rock::opts$get('silent')) {

  res <-
    lapply(
      names(x),
      function(currentCol) {
        msg("    - Processing column: '", currentCol, "'.\n",
            silent = silent);
        return(
          syncing_vector_compress(
            x[, currentCol],
            newLength = newLength,
            sep = sep,
            compressFun = compressFun,
            compressFunPart = compressFunPart,
            silent = silent
          )
        )
      }
    );

  res <- as.data.frame(res);

  names(res) <- names(x);

  return(res);

}
