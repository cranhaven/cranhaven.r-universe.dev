

make_pretty <- function(.data, abbv, pretty_labels) {
  if (!pretty_labels) {
    rgx <- stringr::str_c("_", abbv, "[0-9]*$")

    .data %>%
      dplyr::mutate(dplyr::across(tidyselect::matches(rgx), as.integer))
  } else{
    .data
  }
}

rename_bin_lens <- function(bin_df, abbv, cols){

  bin_df %>%
    dplyr::summarize(dplyr::across(.cols = cols, .fns =  ~dplyr::n_distinct(framecleaner::filter_missing(.)))) %>%
    purrr::map_chr(1) %>%
    stringr::str_c("_", abbv, .) -> bin_lens


  bin_df %>%
    dplyr::rename_with( .fn = ~stringr::str_c(., bin_lens), .cols = cols)
}

#' one_wrapper
#'
#' @param bin_cols cols
#' @param .data dataframe
#' @param abbv char
#' @param bin_method char. bin method.
#' @param n_bins integer. number of bins
#' @param pretty_labels pretty_labels
#' @keywords internal
#'
#' @return output
#'
oner_wrapper <- function(bin_cols, .data, abbv, bin_method, n_bins = n_bins, pretty_labels = pretty_labels) {

  bin_cols %>%
    OneR::bin(nbins = n_bins, method = bin_method, na.omit = F) %>%
    framecleaner::make_na(tidyselect::everything(), vec = "NA")  -> bin_df

  bin_df %>% rename_bin_lens(abbv = abbv, cols = tidyselect::everything()) -> bin_df

  bin_df  %>% dplyr::bind_cols(.data) -> .data

  .data %>% make_pretty(abbv = abbv, pretty_labels = pretty_labels)
}

