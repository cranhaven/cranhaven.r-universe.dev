#' Bind lots of dataframes together rowwise
#'
#' @param x A list of dataframes
#'
#' @return A dataframe
#' @export
#'
#' @examples rbind_df_list(list(Orange, mtcars, ChickWeight));
rbind_df_list <- function(x) {
  if (length(x) < 2) {
    if (is.data.frame(x)) {
      return(x);
    } else if (is.list(x)) {
      if (is.data.frame(x[[1]])) {
        return(x[[1]]);
      } else {
        warning("You passed a list that contained one object of class(es) ",
                vecTxtQ(class(x[[1]])), ". Returning that object.");
        return(x[[1]]);
      }
    }
  } else if (length(x) == 2) {
    return(rbind_dfs(x[[1]], x[[2]]));
  } else {
    return(rbind_dfs(x[[1]],
                     rbind_df_list(x[2:length(x)])));
  }
}
