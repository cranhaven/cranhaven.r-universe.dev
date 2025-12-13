#' summarize bins
#'
#' Returns a summary of all bins created by `bin_cols` in a data frame. Takes no arguments other than the data frame
#' but relies on regular expressions based of the `bin_cols` output in order to identify the corresponding columns.
#'
#'
#' @param mdb dataframe output from bin_cols
#' @param ... optional tidyselect specification for specific cols
#'
#' @return a tibble
#' @export
#'
#' @examples
#'
#' iris %>%
#' bin_cols(Sepal.Width) %>%
#' bin_summary()
bin_summary <- function(mdb, ...){

  column <- .rank <- .label <-  NULL


  mdb %>%
    framecleaner::select_otherwise(...,
                     otherwise = tidyselect::everything(),
                     return_type = "names") %>%
    enc2utf8() %>%
    stringr::str_subset("_[a-z][a-z][0-9]*$") -> cols


  if(rlang::is_empty(cols)){
    rlang::abort("you only supplied columns that weren't created by bin_cols")
  }

  bucket_rgx <- stringr::str_c(cols,  collapse = "|")

  cols %>%  rlang::syms()  -> bucks


  blist <- list()


  for(buck in bucks){

  mdb %>% dplyr::select(!!buck) %>% names() %>% enc2utf8() -> bnames

  bnames %>% stringr::str_extract("[a-z][a-z][0-9]*$")-> suffix
  suffix %>% stringr::str_remove("[0-9]*$")  -> suffix_letter
  suffix %>% stringr::str_remove("^[a-z][a-z]")  -> suffix_number

  bnames %>% stringr::str_remove("_[a-z][a-z][0-9]*$") %>% rlang::sym() -> org_col

    switch(suffix_letter,
           "wi" = "equal width",
           "fr" = "equal freq",
           "va" = "equal value",
           "km" = "kmeans",
           "xg" = "xgboost",
           "ca" = "cart",
           "wo" = "woe",
           "lr" = "logistic regression",
           "ci" = "caim",
           "cc" = "cacc",
           "am" = "ameva",
           "ch" = "chi2",
           "cm" = "chimerge",
           "ec" = "extendedchi2",
           "mh" = "modchi2",
           "md" = "mdlp"
           ) -> method

    mdb %>%
      numeric_summary(original_col = !!org_col, bucket_col = !!buck) %>%
      dplyr::mutate(column := rlang::as_name(org_col),
                    method = method,
                    n_bins = as.integer(suffix_number),
                    .before = 1) %>%
      dplyr::rename_with(function(x)c(".rank"), c(4)) %>%
      dplyr::mutate(.rank = as.integer(.rank)) -> mdb1

  blist %>% rlist::list.append(mdb1) -> blist
  }


  blist %>% purrr::reduce(dplyr::bind_rows)
}
