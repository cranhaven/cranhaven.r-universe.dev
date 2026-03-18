#' Estimate a Majority-Vote Consensus
#'
#' @description
#' `delim_consensus()` estimates a majority-vote consensus over the output of
#' [delim_join] in a row-wise manner.
#'
#' @param delim Output from [delim_join].
#' @param n_match An integer. Threshold for Majority-Vote calculations. If not specified,
#' returns a warning and the threshold will be defined as `ceiling(ncol(delim[, -1])/2)`.
#'
#' @details
#' `delim_consensus()` iterates row-by-row, counting the number of matching species
#' partition names across all species delimitations methods in [delim_join] output.
#' If the sum of identical partition names is greater or equal `n_match`,
#' the consensus column will be filled with its partition name. Otherwise,
#' consensus column will be filled with [NA][base::NA].
#'
#' @return
#' an object of class [tbl_df][tibble::tbl_df].
#'
#' @author
#' Pedro S. Bittencourt
#'
#' @examples
#' 
#' # estimate a majority vote consensus
#' delim_consensus <- delim_consensus(geophagus_delims, n_match= 5)
#' 
#' # check
#' delim_consensus
#' 
#'
#' @export
delim_consensus <- function(delim, n_match=NULL){
  if(is.null(n_match)){
    
    n_match <- ceiling(ncol(delim[, -1])/2)
    
    cli::cli_warn("{cli::col_yellow({cli::symbol$warning})} {.arg n_match} was not found. Using {.arg n_match= {.val {n_match}}}  instead.")
    
  }
  
  if(n_match <= 1 || n_match >= ncol(delim[,-1])+1){
    
    cli::cli_abort(c("{.arg n_match} must be a value between {.val {as.numeric(2)}} and {.val {ncol(delim[, -1])}}.",
                     "i" = "You've supplied an input file with {.val {ncol(delim)}} column{?s}."))
    
  } 
  
  if(n_match >= 1 & n_match <= ncol(delim[, -1])){
    
    cons_delim <- delim |> 
      dplyr::rowwise() |> 
      dplyr::mutate(consensus= list(vctrs::vec_count(dplyr::c_across(2:ncol(delim))))) |> 
      tidyr::unnest("consensus") |> 
      dplyr::mutate(count= dplyr::if_else(.data$count >= {{ n_match }}, .data$key, NA)) |> 
      dplyr::distinct(labels, .keep_all = TRUE) |> 
      dplyr::select(-.data$key) |> 
      dplyr::rename(consensus= "count")
    
  }
  
  return(cons_delim)
}
