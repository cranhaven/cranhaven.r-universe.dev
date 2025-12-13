#' Drop Original Cols
#'
#' Drops the original column from the dataframe once bins are made.  Throws an error if the same column has multiple bin cols.
#'
#'
#' @param .data dataframe output from bin_cols
#' @param ... tidyselect. default chooses all cols created from binning
#' @param restore_names Logical, default FALSE. rename the binned cols with the original column names?
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
#' iris %>%
#'  bin_cols(Sepal.Length) %>%
#'  bin_cols(Sepal.Width, pretty_labels = TRUE) -> iris1
#'
#' iris1
#'
#' iris1 %>%
#'  drop_original_cols(restore_names = TRUE)
#'
#' iris1 %>%
#'  drop_original_cols(restore_names = FALSE)
drop_original_cols <- function(.data, ..., restore_names = FALSE){

  .data %>%
    select_otherwise(..., otherwise = tidyselect::matches("_[a-z][a-z][0-9]*$"), return_type = "names") -> new_cols

  new_cols %>% stringr::str_remove("_[a-z][a-z][0-9]*$")  -> org_col

  .data %>%
    dplyr::select(-tidyselect::any_of(org_col)) -> .data

  if(restore_names){
    .data %>%
      dplyr::rename_with(.fn = ~stringr::str_remove(., "_[a-z][a-z][0-9]*$"), .cols = tidyselect::any_of(new_cols)) -> .data
  }

  .data
}
