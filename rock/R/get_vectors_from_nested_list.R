#' Return one or more values from a nested list in a list of vectors
#'
#' @param x The nested list
#' @param valuesIn The names holding the values to return (in vectors)
#' @param nestingIn The name containing the nested lists
#'
#' @return A list of vectors.
#' @export
#'
#' @examples nestedList <-
#'   list(
#'     id = "x",
#'     value = "value for x",
#'     children = list(
#'       list(
#'         id = "y",
#'         value = "value for y"
#'       ),
#'       list(
#'         id = "z",
#'         value = "value for z"
#'       )
#'     )
#'   );
#' str(nestedList);
#' get_vectors_from_nested_list(
#'   nestedList,
#'   c("id", "value")
#' );
get_vectors_from_nested_list <- function(x,
                                         valuesIn = NULL,
                                         nestingIn = "children") {
  if (is.null(valuesIn)) {
    valuesIn <- setdiff(names(x), nestingIn);
  }
  if ((nestingIn %in% names(x)) &&
      (!is.null(x[[nestingIn]])) &&
      (!all(is.na(x[[nestingIn]]))) &&
      (length(x[[nestingIn]]) > 0)) {
    return(
      c(
        list(unlist(x[valuesIn])),
        unlist(
          lapply(x[[nestingIn]],
                 get_vectors_from_nested_list,
                 valuesIn = valuesIn,
                 nestingIn = nestingIn),
          recursive = FALSE
        )
      )
    );
  } else {
    return(list(unlist(x[valuesIn])));
  }
}
