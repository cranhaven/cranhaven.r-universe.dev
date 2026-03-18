#' Unite Haplotype Summaries with Species Delimitation Outputs
#' 
#' @description
#' `hap_unite()` returns a single [tbl_df][tibble::tbl_df] combining all
#' results from [haplotype_tbl] or [collapse_others] with results from [delim_join]
#' or [delim_consensus].
#' 
#' @param hap_tbl output from [haplotype_tbl] or [collapse_others]. 
#' @param delim output from [delim_join] or [delim_consensus].
#' 
#' @details
#' Many functions in this package relies on the usage of unique haplotypes due to 
#' known issues when using identical or duplicated sequences for species delimitation analysis. 
#' Thus, these outputs will very often refer only to unique haplotypes within a given dataset, 
#' which can be determined by using functions like [hap_collapse]. Assuming that a 
#' duplicated or identical sequence should share the same properties as the first 
#' sequence of the group has, `hap_unite()` combines the output of [haplotype_tbl] 
#' with the output of [delim_join]. Alternativelly, one may use [collapse_others] and
#' [delim_consensus] as well. This output may be used for downstream analysis or 
#' to determine in which cluster a given sequence belongs.
#' 
#' @return
#' an object of class [tbl_df][tibble::tbl_df].
#' 
#' @author
#' Pedro S. Bittencourt
#' 
#' @examples
#' 
#' # get haplotype table
#' hap_tbl <- haplotype_tbl(geophagus)
#' 
#' # unite
#' hap_unite(hap_tbl, geophagus_delims)
#' 
#' @export
hap_unite <- function(hap_tbl, delim){
  
  delim <- delim |>
    dplyr::mutate(status= "haplotype")
  
  collapsed_df <- hap_tbl |>
    dplyr::select(tidyselect::all_of(c("labels", "collapsed"))) |>
    dplyr::mutate(collapsed= stringr::str_split(.data$collapsed, pattern= ", ")) |>
    tidyr::unnest(cols= c("collapsed")) |>
    dplyr::left_join(delim, by= "labels") |>
    tidyr::drop_na("collapsed") |>
    dplyr::select(-labels) |>
    dplyr::rename(labels= "collapsed") |>
    dplyr::mutate(status= "collapsed")
  
  all_haps <- dplyr::left_join(hap_tbl, delim, by= "labels") |>
    dplyr::bind_rows(collapsed_df)
  
  return(all_haps)
}
