#' Return all values from a nested list in a dataframe
#'
#' @param x The nested list
#' @param nestingIn The name containing the nested lists
#'
#' @return A dataframe
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
#' get_dataframe_from_nested_list(nestedList);
get_dataframe_from_nested_list <- function(x,
                                           nestingIn = "children") {

  vectors <- get_vectors_from_nested_list(x=x,
                                          nestingIn = nestingIn);
  dfs <-
    lapply(
      lapply(
        vectors,
        as.list
      ),
      as.data.frame
    );
  return(
    rbind_df_list(
      dfs
    )
  );

}
