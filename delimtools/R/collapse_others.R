#' Summarise Haplotype Metadata Down to One Row
#'
#' @description
#' `collapse_others()` returns a [tbl_df][tibble::tbl_df] summarising
#' all unique haplotype frequencies, duplicates and selected metadata into a single row.
#'
#' @param data An object of class [tbl_df][tibble::tbl_df] containing sequence metadata.
#' @param hap_tbl Output from [haplotype_tbl].
#' @param labels Column name which contains sequence names.
#' @param cols A character vector of variables to collapse.
#'
#' @details
#' `collapse_others()` is a helper function to summarise metadata along with
#' [haplotype_tbl]. For any given `cols`, `collapse_others()` flattens its content
#' by unique haplotypes and its duplicates in `hap_tbl`.
#'
#' @return
#' an object of class [tbl_df][tibble::tbl_df].
#'
#' @author
#' Pedro S. Bittencourt, Rupert A. Collins.
#'
#' @examples
#' # summarise haplotypes
#' hap_tbl <- haplotype_tbl(geophagus)
#'
#' # summarise country
#' others_df <- collapse_others(geophagus_info, hap_tbl, "gbAccession", "country")
#'
#' @export
collapse_others <- function(data, hap_tbl, labels, cols) {
  collapse_tbl <- purrr::map(
    cols,
    ~ {
      hap_tbl |>
        dplyr::mutate(collapsed = stringr::str_split(collapsed, ", ")) |>
        tidyr::unnest(c(collapsed, n_seqs)) |>
        dplyr::mutate(
          x = dplyr::pull(data, .x)[match(labels, dplyr::pull(data, {{ labels }}))],
          y = dplyr::pull(data, .x)[match(collapsed, dplyr::pull(data, {{ labels }}))]
        ) |>
        dplyr::mutate("collapsed_{.x}" := dplyr::if_else(is.na(y),
          x,
          paste(unique(c(x, y)), collapse = ", ")
        ), .by = labels) |>
        dplyr::select(c(labels, n_seqs, tidyselect::ends_with(cols))) |>
        dplyr::distinct(labels, .keep_all = TRUE) |>
        dplyr::full_join(hap_tbl, ., by = c("labels", "n_seqs"))
    }
  ) |>
    purrr::reduce(dplyr::full_join, by = c("labels", "n_seqs", "collapsed")) |>
    dplyr::relocate("collapsed", .after = "n_seqs")

  return(collapse_tbl)
}
