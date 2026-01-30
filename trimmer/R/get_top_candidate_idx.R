# get position index of top candidate.
get_top_candidate_idx <- function(dt, obj, verbose = TRUE) {
  
  # extract size.
  size <- dt$size
  
  # extract all cols except 'size'.
  cols <- names(dt)[!names(dt) %in% "size"]
  
  dt <- dt[, ..cols, drop = FALSE]
  
  # get position index as vector.
  idx <- as.numeric(dt)
  # remove any NA's from position index.
  idx <- idx[!is.na(idx)]
  
  # convert index to name if possible.
  idx_name <- tryCatch(convert_idx_to_name(idx, obj),
                       error = function(e) {idx})
  
  # prepare status message, depending on whether position has named index or
  # not.
  is_name <- is.character(idx_name)
  prefix <- paste0("c(", if (is_name) {"'"} else {NULL})
  postfix <- paste0(if (is_name) {"'"} else {NULL}, ")")
  coll <- if (is_name) {"','"} else {","}
  
  # print
  if (verbose) {
    cat_bullet("Trying to remove element [[", blue(prefix),
               blue(paste0(idx_name, collapse = coll)), 
               blue(postfix), "]], element size = ", 
               blue(pf_obj_size(as.numeric(size))), sep = "", 
               bullet = "continue", bullet_col = "gray")
  }
  
  idx
  
}