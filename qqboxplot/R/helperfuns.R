library(grid)

has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    return(rep(FALSE, length(x)))
  }

  !is.na(nms) & nms != ""
}

ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}
