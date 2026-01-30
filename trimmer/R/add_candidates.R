add_candidates <- function(x, cand, cand_top, cand_length) {
  
  # pre-allocate data.table with appropriate number of rows.
  cand_top <- cand_top[rep(1, cand_length), , ]
  
  # extract all cols except 'size'.
  cols <- names(cand_top)[!names(cand_top) %in% "size"]
  cand_top <- cand_top[, ..cols, drop = FALSE]
  
  # insert new candidates into table.
  cand_top[[paste0("i", ncol(cand_top) + 1)]] <- seq_len(cand_length)
  
  # compute object sizes.
  cand_top <- get_obj_sizes_dt(cand_top, x)
  
  # bind to original results.
  cand <- rbind(cand_top, cand, fill = TRUE)
  
  # order after size in order to prepare for next iteration.
  cand <- order_after_size(cand)
  
}