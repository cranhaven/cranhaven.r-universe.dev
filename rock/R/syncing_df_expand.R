#' @rdname expanding_vectors_or_dataframes
#' @param neverFill Columns to never fill regardless of whether fill is `TRUE`.
#' @export
syncing_df_expand <- function(x,
                              newLength,
                              fill = TRUE,
                              neverFill = NULL,
                              paddingValue = NA,
                              expandFun = NULL,
                              silent = rock::opts$get('silent')) {

  res <-
    lapply(
      names(x),
      function(currentCol) {
        msg("    - Processing column: '", currentCol, "'.\n",
            silent = silent);
        if ((!is.null(neverFill)) && (currentCol %in% neverFill)) {
          fillThisCol = FALSE;
        } else {
          fillThisCol = fill;
        }
        return(
          syncing_vector_expand(
            x[, currentCol],
            newLength = newLength,
            fill = fillThisCol,
            paddingValue = paddingValue,
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
