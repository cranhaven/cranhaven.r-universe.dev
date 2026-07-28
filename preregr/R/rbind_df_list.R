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
    return(x);
  } else if (length(x) == 2) {
    return(rbind_dfs(x[[1]], x[[2]]));
  } else {
    return(rbind_dfs(x[[1]],
                     rbind_df_list(x[2:length(x)])));
  }
}
