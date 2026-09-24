

split_int_ <- function(data, f) {
  nr <- .row_names_info(data, type = 2L)
  f_ <- .formula2varlist(formula = f, data = data) # see ?base::split.data.frame
  rid <- split.default(x = seq_len(nr), f = f_)
  return(rid[lengths(rid, use.names = FALSE) > 0L])
}