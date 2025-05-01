AftEr_myfUn <- function(x, pa = FALSE) {
  x <- as.character2(x)
  if (length(x) == 0) {
    return("")
  }
  else {
    x[is.na(x)] <- ""
    if (pa) {
      if (length(x) > 1) {
        x <- paste0(x, collapse = " ")
      }
    }
	x <- whetherencode(x)
    return(x)
  }
}
