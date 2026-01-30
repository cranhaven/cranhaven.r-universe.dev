#' Adjust Data Table with Candidate Elements for Elimination
#' 
#' Adjusts positions of all candidates for elimination in data.table after 
#' removing a candidate (due to the fact, that the positions may shift).
#'
#' @param cand \code{data.table} with candidates for elimination given by their
#' position indices.
#' @param cand_top_idx \code{numeric} position index of candidate to be removed.
#'
#' @return \code{data.table} candidates after any adjustments to position 
#' indices of candidates.
adjust_candidates <- function(cand, cand_top_idx) {
  
  # match on 'cand_top_idx' to identify entries that will have to be adjusted
  # after having removed this specific entry.
  if (length(cand_top_idx) > 1) {
    dt <- as.data.table(as.list(cand_top_idx))
    names(dt) <- paste0("i", seq_along(cand_top_idx))
    # only match on everything except last column.
    on_cols <- names(dt)[seq_along(cand_top_idx) - 1]
    # identify col that will have to be adjusted - last column.
    col_adjust <- names(dt)[length(names(dt))]
    # identify rows, that match on these cols. These are columns where 
    # adjustments have to be made.
    cand_adjust <- cand[dt[, ..on_cols, ], on = on_cols, nomatch = 0]
    value_adjust <- cand_top_idx[length(cand_top_idx)]
    # subset only rows, that need to be adjusted consequently.
    cand_adjust <- cand_adjust[cand_adjust[[col_adjust]] > value_adjust, , drop = FALSE]
    
    # identify rows, that are just fine as is - no need for adjustments.
    cand_ok <- cand[!cand_adjust, on = on_cols]
    
    # adjust position indices.
    cand_adjust[[col_adjust]] <- cand_adjust[[col_adjust]] - 1
    
  } else {
    # handle trivial case - most shallow layer.
    cand_adjust <- cand[i1 > cand_top_idx, , drop = FALSE]
    cand_adjust[["i1"]] <- cand_adjust[["i1"]] - 1
    cand_ok <- cand[i1 <= cand_top_idx, , drop = FALSE]
  }
  
  # join rows with and without adjustments together.
  cand <- rbind(cand_ok, cand_adjust, fill = TRUE)
  
  # order after size.
  order_after_size(cand)
  
}