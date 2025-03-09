#' @rdname expanding_vectors_or_dataframes
#' @export
syncing_df_expand <- function(x,
                              newLength,
                              fill = TRUE,
                              expandFun = NULL,
                              silent = rock::opts$get('silent')) {

  res <-
    lapply(
      names(x),
      function(currentCol) {
        msg("    - Processing column: '", currentCol, "'.\n",
            silent = silent);
        return(
          syncing_vector_expand(
            x[, currentCol],
            newLength = newLength,
            fill = fill,
            expandFun = expandFun,
            silent = silent
          )
        )
      }
    );

  res <- as.data.frame(res);

  names(res) <- names(x);

  return(res);

}
