#' Convert tables into a single column in a dataframe.
#'
#' Accepts a table or matrix or a list of tables and matrices
#' and converts them into dataframe columns.
#'
#' @param tab A table/matrix or a list of tables/matrices.
#' @param na_omit Logical; if true, will remove rows with NA.
#'
#' @return A dataframe column.
#'
#' @importFrom tidyr gather
#' @importFrom dplyr select
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test.xlsx",
#'   package = "quicR"
#' )
#' tabs <- organize_tables(file)
#' convert_tables(tabs)
#'
#' @export
convert_tables <- function(tab, na_omit = TRUE) {
  df_list <- data.frame()
  if (is.vector(tab)) {
    for (i in 1:length(tab)) {
      message(paste0(i, ": ", names(tab[i])))
      column <- tab[[i]] |>
        t() |>
        as.data.frame() |>
        tidyr::gather() |>
        dplyr::select("value")
      df_list <- append(df_list, column)
    }
    df_ <- as.data.frame(df_list)
    colnames(df_) <- names(tab)
    if (na_omit) df_ <- na.omit(df_)
    return(df_)
  }
}
