#' @rdname serialization
#' @export
serialize_df <- function(x,
                         idCol = NULL) {

  if (!is.null(idCol)) {
    tryCatch(
      row.names(x) <- x[, idCol],
      error = function(e) {
        stop("I ran into an error when trying to serialize one of the five ",
             "dataframes that make up the form specification. Most likely, ",
             "there are duplicate values in the column with identifiers. ",
             "For this dataframe, that column is named '", idCol, "', and ",
             "the values are: ", vecTxtQ(x[, idCol]), ".");
      }
    );
  }

  return(
    apply(
      x,
      1,
      as.list,
      simplify=FALSE
    )
  );

}
